### A script for compiling and building flagged species occurrence data sets 
### Author: JT Miller
### Date: 08-27-2025

# 1. Build Raw Datasets from parquet file downloads of iDigBio, GBIF, and Symbiota ############################
library(data.table)
library(duckdb)
library(DBI)
library(dplyr)
library(arrow)
library(sf)
library(rgnparser)
library(ggplot2)
source("/blue/guralnick/millerjared/PlantSweepeR/code/finished/R/sweeper-fxns.R")

# 1. Setup ####################################################################

# gnparser path
my_path <- Sys.getenv("PATH") 
Sys.setenv(PATH = paste0(my_path, "/home/millerjared/gnparser"))

# array logic
start_num <- as.numeric(Sys.getenv("START_NUM"))
task_id <- as.numeric(start_num)
part <- paste0("part", task_id)

# read harmonized names + synonyms
name_list <- readRDS("/blue/guralnick/millerjared/PlantSweepeR/data/processed/name_list.rds")
#name_list <- readRDS("/blue/guralnick/millerjared/PlantSweepeR/data/processed/unfinished_sp_tables_names_chkpt.rds")
# retrieve name for this task
names_to_retrieve <- name_list[[task_id]]
accepted_name <- names_to_retrieve[1]
accepted_name_filestyle <- gsub(" ", "_", accepted_name)

### Use Exact Match Based On Archived Name Matches #############################################
# load matched names from species parquet
matches_file <- file.path("/blue/guralnick/millerjared/PlantSweepeR/data/processed/archived-name-matches/", 
                          paste0(accepted_name_filestyle, "_matches.parquet"))
match_df <- arrow::read_parquet(matches_file)
# only keep matches whose like pattern has at least 3 '%' characters (a shortcut to dealing with the issue of some names having non-species words as the like-pattern currently.)
filtered_match_df <- match_df[stringr::str_count(match_df$like_pattern, "%") >= 3,]
matched_names_vec <- unique(toupper(match_df$like_pattern_matched))
# add escape for single quotes that are sometimes present in names
matched_names_vec_escaped <- gsub("'", "''", matched_names_vec)
## parquet path(s) (old paths, update when /blue/guralnick has more space)
gbif_file_path <- "/blue/guralnick/millerjared/BoCP/plant-db-pipeline/parquet-files/gbif_full_occ.parquet"
idigbio_file_path <- "/blue/guralnick/millerjared/BoCP/plant-db-pipeline/parquet-files/idigio_full_occ.parquet"
symbiota_file_path <- "/blue/guralnick/millerjared/BoCP/plant-db-pipeline/parquet-files/symbiota_occ.parquet"

## preform matching 
if(length(matched_names_vec) > 0){
  pattern_for_sql <- paste0("'", matched_names_vec_escaped, "'", collapse = ", ")
  cat("Opening DB connection\n")
  con <- dbConnect(duckdb::duckdb())
  query_gbif <- paste0(
    "SELECT * FROM '", gbif_file_path, "' WHERE UPPER(verbatimScientificName) IN (", 
    pattern_for_sql, ")"
  )
  query_idigbio <- paste0(
    "SELECT * FROM '", idigbio_file_path, "' WHERE UPPER(verbatimScientificName) IN (", 
    pattern_for_sql, ")"
  )
  query_symbiota <- paste0(
    "SELECT * FROM '", symbiota_file_path, "' WHERE UPPER(verbatimScientificName) IN (", 
    pattern_for_sql, ")"
  )
  cat("Retrieving", accepted_name, "occurrence data from GBIF parquet\n")
  gbif_df <- dbGetQuery(con, query_gbif)
  cat("Retrieving", accepted_name, "occurrence data from iDigBio parquet\n")
  idigbio_df <- dbGetQuery(con, query_idigbio)
  cat("Retrieving", accepted_name, "occurrence data from Symbiota parquet\n")
  symbiota_df <- dbGetQuery(con, query_symbiota)
  
  ## Disconnect
  dbDisconnect(con, shutdown = TRUE)
}

##########################################################################################################

### Combine datasets and correct classes #################################################################
occ_data <- combine_occ_data(gbif_df = gbif_df, 
                             idigbio_df = idigbio_df, 
                             symbiota_df = symbiota_df, 
                             accepted_name = accepted_name)
occ_data <- correct_classes(occ_data)
##########################################################################################################

### Create rounded georefs for SDM models ################################################################
occ_data <- occ_data %>% 
  mutate(roundedLatitude = ifelse(!is.na(decimalLatitude) & decimalLatitude != 0.00, round(decimalLatitude, 2), NA),
         roundedLongitude = ifelse(!is.na(decimalLongitude) & decimalLongitude != 0.00, round(decimalLongitude, 2), NA))
##########################################################################################################

### Identify Duplicates ##################################################################################
occ_data <- id_duplicates(occ_data)
##########################################################################################################
### Apply Flags ##########################################################################################
occ_data <- flag_taxonomy(occ_data = occ_data, exact_names_to_match = names_to_retrieve)
occ_data <- flag_wgs84(occ_data = occ_data)
occ_data <- flag_coordinateIssue(occ_data = occ_data)
occ_data <- flag_trueCoordsWithheld(occ_data = occ_data)
occ_data <- flag_coordinateCleaning(occ_data = occ_data, max_dist_to_consider_outliers = 250)
##########################################################################################################
### Delimit Range Status #################################################################################

## bring in name aligment, this is specific to BoCP where we only really want NA species, see 003A for more details on building this... 
na_plants_alignment <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/processed/wcvp-ncbi-alignment-na.csv") 
na_plants_alignment <- na_plants_alignment %>% filter(source != "ncbi") # we cant do this for names w/ncbi origin, none are accepted so proceed
crs_aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m" # albers equal area centered on NA
# delimit range status for occs
occ_data <- delimit_pts_wcvp_range_statuses(occ_data = occ_data, 
                                            plant_alignment = na_plants_alignment, 
                                            proj_crs = crs_aea, 
                                            visual = TRUE, 
                                            saveMaps = TRUE, 
                                            full_map_file_path = "/blue/guralnick/millerjared/PlantSweepeR/figures/wcvp-distributions-w-occs/full/", 
                                            native_map_file_path = "/blue/guralnick/millerjared/PlantSweepeR/figures/wcvp-distributions-w-occs/native/")
##########################################################################################################

### Write out data #######################################################################################
data.table::fwrite(occ_data, paste0("/blue/guralnick/millerjared/PlantSweepeR/data/processed/flagged-datasets/", gsub(" ", "_", accepted_name), ".csv"))
##########################################################################################################

rm(occ_data)
gc() # clean up!


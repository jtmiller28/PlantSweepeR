# 002B ncbi only NA Name Delimitation 
# Author: JT Miller 
# Date: 02-18-2024
# Project: BoCP 

# load packages
library(data.table)
library(tidyverse)
library(sf)
library(duckdb)
library(arrow)
library(DBI)
library(rgnparser)

#### Set up pathing to use gnparser
my_path <- Sys.getenv("PATH") # grab our path
Sys.setenv(PATH = paste0(my_path, "/home/millerjared/gnparser"))

# Set up array logic
start_num <- as.numeric(Sys.getenv("START_NUM"))
task_id <- as.numeric(start_num)
part <- paste0("part", task_id)

## Delimit the region of BoCP's North America 
# Load botanical region shp files
bot_regions <- read_sf("/blue/guralnick/millerjared/PlantSweepeR/data/raw/level3-wgsrpd/level3.shp")
# strings of relevant area for study
na_string <- c("ALA", "ABT", "ASK", "ARI", "ARK", "BRC", "CAL", "COL", "CNT", "DEL",
               "GEO", "FLA", "IDA", "IOW", "ILL","INI", "KAN", "MAN", "LOU", "KTY", "MAI",
               "MNT", "MIN", "MIC", "MAS", "MSO", "MSI", "MRY", "MXC", "MXE", "MXG", "MXN",
               "MXS", "MXT", "NDA", "NCA", "NBR", "NEV", "NEB", "NFL", "NUN", "NSC", 
               "NWT", "NWM", "OHI", "NWJ", "NWH", "NWY", "ORE", "ONT", "OKL", "PEN", "PEI", "QUE",
               "RHO", "SAS", "SDA", "SCA", "TEX", "TEN", "UTA", "VRG", "VER", "WAS", "WIS", "WDC", "WVA",
               "YUK", "WYO", "LAB")
# filter down to these NA regions 
na_bot_regions <- filter(bot_regions, LEVEL3_COD %in% na_string)

## Bring in ncbi names and set up for north america chcek
# load ncbi names to check
names_to_check_if_na <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/processed/wcvp-ncbi-alignment-ncbi-needs-na-check.csv")
# generate list of names, with the accepted name parent as the first name, with synonyms as the alternatives
accepted_name_v <- unique(names_to_check_if_na$alignedParentName)
name_list <- list() # initialize an empty list
# reorganize into a nested list
bench::bench_time({
  for(i in 1:length(accepted_name_v)){
    p <- names_to_check_if_na[alignedParentName == accepted_name_v[[i]]] # create a for-loop that goes through by the accepted name and grabs synonyms, storing them both as a vector in a list. 
    s <- p[!is.na(name)]
    x <- print(s$name)
    a <- print(unique(p$alignedParentName))
    name_list[[i]] <- unique(c(a,x))
  }
})
## Use duckdb retrieval to grab data for relevant name 
connect <- dbConnect(duckdb::duckdb())
names_to_retrieve <- name_list[[task_id]]
names_to_retrieve_query <- toupper(names_to_retrieve)
names_to_retrieve_query <- gsub(" ", "%", names_to_retrieve_query)
#names_to_retrieve <- name_list[[13418]]
accepted_name <- accepted_name_v[task_id]
#accepted_name <- accepted_name_v[13418]
idigbio_file_path <- "/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/idigio_full_occ.parquet"
gbif_file_path <- "/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/gbif_full_occ.parquet"
symbiota_file_path <- "/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/symbiota_occ.parquet"
# Construct the SQL query that grabs the accepted name & all of its valid synonyms
query_idigbio <- paste0("SELECT verbatimScientificName, verbatimDecimaLlatitude AS decimalLatitude, verbatimDecimalLongitude AS decimalLongitude FROM '", idigbio_file_path, "' WHERE UPPER(verbatimScientificName) LIKE ", 
                        paste0("'%", names_to_retrieve_query, "%'", collapse = " OR UPPER(verbatimScientificName) LIKE "))
query_gbif <- paste0("SELECT verbatimScientificName, decimaLlatitude, decimalLongitude FROM '", gbif_file_path, "' WHERE UPPER(verbatimScientificName) LIKE ", 
                     paste0("'%", names_to_retrieve_query, "%'", collapse = " OR UPPER(verbatimScientificName) LIKE "))
query_symbiota <- paste0("SELECT verbatimScientificName, decimaLlatitude, decimalLongitude FROM '", symbiota_file_path, "' WHERE UPPER(verbatimScientificName) LIKE ", 
                         paste0("'%", names_to_retrieve_query, "%'", collapse = " OR UPPER(verbatimScientificName) LIKE "))
# Execute query via priority. Our question is just whether the taxon exists within NA, therefore we'll try each dataset in order of smallest -> greatest (symbiota < idigbio < gbif)
query_list <- list(query_symbiota, query_idigbio, query_gbif)
dataset_names <- c("symbiota", "idigbio", "gbif")
inNA <- FALSE 

for(i in seq_along(query_list)){
  # load query
  query <- query_list[[i]]
  df <- dbGetQuery(connect, query)
  if(nrow(df) > 0){
    # clean taxonomy, parse the output into the fullcanonical name and filter 
    df_names_parsed <- gn_parse_tidy(na.omit(df$verbatimScientificName))
    df_names_parsed <- df_names_parsed %>% mutate(n = 1:n()) %>% select(canonicalfull, n)
    df <- df %>% mutate(n = 1:n())
    df <- merge(df, df_names_parsed, by = "n")
    df <- df %>% filter(canonicalfull %in% names_to_retrieve) 
    if(nrow(df) > 0){
      # remove records without spatial coords
      df <- df %>% 
        filter(!is.na(decimalLatitude) & decimalLatitude != "") %>% 
        filter(!is.na(decimalLongitude) & decimalLongitude != "") %>% 
        mutate(decimalLongitude = gsub(",", ".", decimalLongitude), # problem with providers placing commas rather than periods for decimals.
               decimalLatitude = gsub(",", ".", decimalLatitude))
      # if there is still any data for that taxon within the dataset, make spatial
      if(nrow(df) > 0){
        # transform to wgs84 
        df_sf <- st_as_sf(df, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)
        # check to see if any points fall within NA range
        sf_use_s2(FALSE) # turn off
        na_occ <- df_sf[na_bot_regions,]
        # assign aggregator for print statement
        aggregator <- dataset_names[i]
        # if any occur in NA mark boolean field inNA as TRUE
        if(nrow(na_occ) > 0){
          inNA <- TRUE
          break # STOP querying if this becomes true 
        } else{
          print(paste(accepted_name, "not found in", aggregator))
        }
      }
    }
  }
}

# Close the connection
dbDisconnect(connect)

# Save results
check_na_info_df <- data.frame(
  acceptedParentName = accepted_name_v[task_id],
  inNA = inNA, 
  aggregatorFoundID = ifelse(inNA == TRUE, aggregator, NA)
)

fwrite(check_na_info_df, file = "/blue/guralnick/millerjared/PlantSweepeR/data/processed/ncbi_na_name_check.csv", 
       append = TRUE, col.names = !file.exists("/blue/guralnick/millerjared/PlantSweepeR/data/processed/ncbi_na_name_check.csv"))

# turns out the majority of these are either hybrids lacking 'x' or misspelling of valid names in WCVP. We're going to drop these for BoCP purposes, but I'll consider what to do with them for PlantSweepeR
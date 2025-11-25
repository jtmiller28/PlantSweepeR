# Title: wcvp north america delimitation 
# Author: JT Miller 
# Date: 02-02-2024 (updated 11-24-2025)
# Project: PlantSweeper

### Purpose: A) Filter down name aligment to only include plant species from BoCP's definition of North America. 

## Load libraries
library(data.table)
library(tidyverse)
library(sf)
library(ape)
library(dbplyr)
library(duckdb)
library(arrow)
library(DBI)
## Load in name alignment, wcvp geographic info, and the ids as we need to append these back to link them
name_alignment <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/processed/wcvp-ncbi-alignment-2025.csv")
# Replace strings ending with × with ""
name_alignment <- name_alignment%>%  # note that this is an update, as our previous alignment truncated the hyphanated names. Just going to update the ones that are necessary. 
  mutate(alignedParentName = ifelse(
    grepl("×\\s*$", alignedParentName),
    "",    # Replace with empty string
    alignedParentName
  ) )
wcvp_geo <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/raw/wcvp_distribution_2025_update.csv")
wcvp_name_ids <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/raw/wcvp_names_2025_update.csv")
bot_regions <- read_sf("/blue/guralnick/millerjared/PlantSweepeR/data/raw/level3-wgsrpd/level3.shp")
wcvp_backbone <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/raw/wcvp_names_2025_update.csv")
## Botanical Regions strings that are Yukatan through Canada
## Keep the following string for filtering
na_string <- c("ALA", "ABT", "ASK", "ARI", "ARK", "BRC", "CAL", "COL", "CNT", "DEL",
               "GEO", "FLA", "IDA", "IOW", "ILL","INI", "KAN", "MAN", "LOU", "KTY", "MAI",
               "MNT", "MIN", "MIC", "MAS", "MSO", "MSI", "MRY", "MXC", "MXE", "MXG", "MXN",
               "MXS", "MXT", "NDA", "NCA", "NBR", "NEV", "NEB", "NFL", "NUN", "NSC", 
               "NWT", "NWM", "OHI", "NWJ", "NWH", "NWY", "ORE", "ONT", "OKL", "PEN", "PEI", "QUE",
               "RHO", "SAS", "SDA", "SCA", "TEX", "TEN", "UTA", "VRG", "VER", "WAS", "WIS", "WDC", "WVA",
               "YUK", "WYO", "LAB")
# plotted for clarity
ggplot() + 
  geom_sf(bot_regions, mapping = aes()) +
  geom_sf(filter(bot_regions, LEVEL3_COD %in% na_string), mapping = aes(fill = "red")) +
  ggtitle("North American Extent for BoCP Implementation")

# apply filter to shapefile regions to only include shapes from NA 
na_bot_regions <- filter(bot_regions, LEVEL3_COD %in% na_string)

## filter ids to only accepted names, then retain only names that include NA
wcvp_name_ids <- wcvp_name_ids %>% 
  filter(taxon_status == "Accepted")

# affix wcvp_geo and wcvp_names togther
wcvp_info <- merge(wcvp_name_ids, wcvp_geo, by.x = "plant_name_id", by.y = "plant_name_id")

# organize by names that occur in NA
wcvp_info <- wcvp_info %>% 
  group_by(plant_name_id) %>% 
  mutate(inNA = ifelse(any(area_code_l3 %in% na_bot_regions$LEVEL3_COD), TRUE, FALSE))

# create an info summary table noting which species exist in Nrth America
wcvp_info_summary <- wcvp_info %>% 
  select(taxon_name, inNA) %>% 
  distinct()

# filter down name alignment to scope of the project: Seedplants
angio_orders <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/raw/APG5.csv")
gymno_orders <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/raw/gymnosperms-fams-to-orders.csv")
seed_plants <- rbind(angio_orders, gymno_orders)
seed_plant_orders_v <- seed_plants$order
seed_plant_families_v <- seed_plants$family
# add two families that are not always valid but found during my search
seed_plant_families_v <- c(seed_plant_families_v, 
                           "Cephalotaxaceae",
                           "Cochlospermaceae")
name_alignment <- name_alignment %>% 
  filter(ncbiAlignedOrder %in% seed_plant_orders_v | ncbiAlignedFamily %in% seed_plant_families_v |
           wcvpAlignedFamily %in% seed_plant_families_v)


# We can only use POWO/WCVP's designations for names that were derived from WCVP/POWO, therefore limit the alignment to these names only
name_alignment_wcvp <- name_alignment %>% 
  filter(source == "wcvp")

# Isolate NCBI taxonomy that are lone names, these will need to be checked via shapefile plotting methods. 
name_alignment_ncbi <- name_alignment %>% 
  filter(source == "ncbi") %>% 
  filter(nameStatus == "Accepted")

# Create a summary df for WCVP linking aligned names & their parents
wcvp_alignment_summary <- name_alignment_wcvp %>% 
  select(alignedName, alignedParentName) %>% 
  distinct()

# merge this summary df for wcvp with the geographic info
alignment_w_geo <- merge(wcvp_alignment_summary,
                         wcvp_info_summary, by.x = "alignedName", by.y = "taxon_name", all.x = TRUE)

# use a group_by statement that assures that if any component of a alignedParentName falls within NA, we're calling it NA
# this step is important, as taxon with subspecific designations *may* be in NA, while other members of the parentTaxon do not. For our work, we're only recongizing genus+specificEpithet, so this is a required assumption.
alignment_w_geo <- alignment_w_geo %>% 
  group_by(alignedParentName) %>% 
  mutate(parentInNA = ifelse(any(inNA == TRUE), TRUE, FALSE)) %>% 
  ungroup() %>% 
  filter(alignedParentName != "")

# filter the geo name list to parent aligned names that occur within NA 
na_name_alignment <- alignment_w_geo %>% 
  filter(parentInNA == TRUE)

# now filter down the actual alignment to match only these NA names
name_alignment_in_na <- name_alignment %>% 
  filter(alignedParentName %in% na_name_alignment$alignedParentName)

# build a list of names to check if in NA with occurrence data, as they originate from NCBI taxonomy
names_to_check_if_na <- name_alignment %>% 
  filter(alignedParentName %in% name_alignment_ncbi$alignedParentName)

# check to see if these names have the same aligned parent name (a double check just to be sure)
names_to_check_if_na <- names_to_check_if_na %>% 
  group_by(alignedParentName) %>% 
  mutate(duplicateSourceForAlignedParentName = ifelse(source == "wcvp", TRUE, FALSE ))

# if TRUE these can be removed as they should be taken care of with previous wcvp geo steps
names_to_check_if_na <- names_to_check_if_na %>% 
  filter(duplicateSourceForAlignedParentName == FALSE)

# complication, some of the multiple mapping names have made there way into the dataset. Remove these by preforming a secondary check
assess_m_maps <- names_to_check_if_na$alignedParentName
wcvp_overlap <- wcvp_backbone %>% filter(taxon_name %in% assess_m_maps)
wcvp_overlap <- wcvp_overlap %>% group_by(taxon_name) %>% summarize(n = n())
# remove these overlaps from names to check
names_to_check_if_na <- names_to_check_if_na %>% 
  filter(!alignedParentName %in% wcvp_overlap$taxon_name)
# write out these files, they will be used as source
fwrite(name_alignment_in_na, "/blue/guralnick/millerjared/PlantSweepeR/data/processed/wcvp-ncbi-alignment-na.csv")
fwrite(names_to_check_if_na, "/blue/guralnick/millerjared/PlantSweepeR/data/processed/wcvp-ncbi-alignment-ncbi-needs-na-check.csv")


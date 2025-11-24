# 002F Symbiota data formatting & Parquetting
# Author: JT Miller 
# Date: 03-06-2024 (updated 11-24-2025)
# Project: PlantSweepeR

# Load packages 
library(tidyverse)
library(data.table)
library(arrow)
library(DBI)
library(duckdb)
library(lubridate)

### Load symbiota data

# read in data on vascular plants obtained from Ed Gilbert at Symbiota. Append colnames as those didnt quite make it on the data
colnames_df <- fread("/blue/guralnick/millerjared/BoCP/data/symbiota-dws/symbiota/field_heading.csv")
new_cols <- names(colnames_df)
cch2 <- fread("/blue/guralnick/millerjared/BoCP/data/symbiota-dws/symbiota/cch2_export.csv")
colnames(cch2) <- new_cols
cnh <- fread("/blue/guralnick/millerjared/BoCP/data/symbiota-dws/symbiota/cnh_export.csv")
colnames(cnh) <- new_cols
sienet <- fread("/blue/guralnick/millerjared/BoCP/data/symbiota-dws/symbiota/seinet_export.csv")
colnames(sienet) <- new_cols

# combine data
symbiota_data <- rbind(cch2, cnh, sienet)

# restructure NAs (currently are \Ns)
symbiota_data[symbiota_data == "\\N"] <- NA

### Select for necessary fields, format verbatimScientificName 
symbiota_data <- symbiota_data %>% 
  select(
    occurrenceID, 
    sciname, 
    eventDate, 
    institutionCode,
    recordedBy, 
    locality, 
    decimalLatitude, 
    decimalLongitude, 
    coordinateUncertaintyInMeters, 
    informationWithheld, 
    
    
  ) %>% 
  # Add some fields, build date info. 
  mutate(
    basisOfRecord = "PreservedSpecimen", 
    geodeticDatum = NA, 
    year = lubridate::year(lubridate::ymd(eventDate)),
    month = lubridate::month(lubridate::ymd(eventDate)),
    day = lubridate::day(lubridate::ymd(eventDate))
  ) %>% 
  # rename sciname field to standardize agaisnt our other data
  rename(verbatimScientificName = sciname)

# create parquet file to index 
write_parquet(symbiota_data, "/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/symbiota_occ.parquet")




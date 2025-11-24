# 003B gbif join raw and processed occurrene data
# Author: JT Miller 
# Date: 03-06-2024 (updated 11-24-2025)
# Project: PlantSweepeR

# Purpose: Take our snapshots of idigbio and gbif with raw and processed data and join them by their record keys

# Load libraries 
library(tidyverse)
library(duckdb)
library(data.table)
library(dplyr)
library(dbplyr)
library(arrow)

# Load the parquet file as a table reference
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Register the raw & processed idigbio parquet files as a temporary view 
dbExecute(con, "CREATE VIEW gbif_raw AS SELECT * FROM read_parquet('/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/gbif_verbatim_occ.parquet')")
dbExecute(con, "CREATE VIEW gbif_processed AS SELECT * FROM read_parquet('/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/gbif_processed_occ.parquet')")

# Reference as a dbplyr tbl
gbif_raw <- tbl(con, "gbif_raw")
gbif_processed <- tbl(con, "gbif_processed")

# Use dbplyr to join data via the coreid field 
gbif_data <- gbif_raw %>% 
  inner_join(gbif_processed, by = "gbifID") %>% 
  select(-occurrenceID.x,
         -verbatimDecimalLatitude, # we'll take processed geocoords as these are likely corrected for internal issues
         -verbatimDecimalLongitude,
         -verbatimCoordinateUncertaintyInMeters,
         -scientificName, # no longer needed, we extracted the verbatimScientificName in formatting steps
         -verbatimInformationWithheld, # redundant to processed
         -processedYear, 
         -processedMonth, 
         -processedDay
  ) %>% 
  rename(year = verbatimYear, # we'll use this when possible, as aggregators induce uncertainty in date fields for processing
         month = verbatimMonth, 
         day = verbatimDay, 
         geodeticDatum = verbatimGeodeticDatum, # only a provider field
         coordinateUncertaintyInMeters = processedCoordinateUncertaintyInMeters, # shouldn't undergo changes
         institutionCode = verbatimInstitutionCode, 
         recordedBy = verbatimRecordedBy, 
         informationWithheld = processedInformationWithheld, 
         occurrenceID = occurrenceID.y, 
         decimalLatitude = processedDecimalLatitude, 
         decimalLongitude = processedDecimalLonitude
  )



# collect gbif data
gbif_data <- gbif_data %>% collect()

# write complete gbif data as its own parquet file
arrow::write_parquet(gbif_data, "/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/gbif_full_occ.parquet")
dbDisconnect(con, shutdown = TRUE)

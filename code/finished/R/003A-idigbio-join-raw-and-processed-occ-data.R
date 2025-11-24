# 003A join raw and processed idigbio datasets
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
dbExecute(con, "CREATE VIEW idigbio_raw AS SELECT * FROM read_parquet('/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/idigbio_raw_occ.parquet')")
dbExecute(con, "CREATE VIEW idigbio_processed AS SELECT * FROM read_parquet('/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/idigbio_processed_occ.parquet')")

# Reference as a dbplyr tbl
idigbio_raw <- tbl(con, "idigbio_raw")
idigbio_processed <- tbl(con, "idigbio_processed")

# Use dbplyr to join data via the coreid field 
idigbio_data <- idigbio_raw %>% 
  inner_join(idigbio_processed, by = "coreid") %>% 
  rename(occurrenceID = occurrenceID.y) %>% 
  select(-occurrenceID.x) %>%  # remove redundant occurrenceID 
  select(-`dwc:scientificName`) # remove this field as its no longer needed

# collect idigbio data
idigbio_data <- idigbio_data %>% collect()

# write complete idigbio data as its own parquet file
arrow::write_parquet(idigbio_data, "/blue/guralnick/millerjared/BoCP/data/raw/parquet-occs/idigio_full_occ.parquet")
dbDisconnect(con, shutdown = TRUE)

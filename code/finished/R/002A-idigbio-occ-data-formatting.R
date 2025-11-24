# 002A idigbio occurrence data formatting & build parquet file
# Author: JT Miller 
# Date: 02-18-2024 (updated 11-24-2025)
# Project: PlantSweepeR 

## Purpose: 
### A) Attach verbatim scientificName field info to idigbio processed data, to retain info about what the name was prior to interpretation

# Load packages 
library(tidyverse)
library(data.table)
library(arrow)
library(DBI)
library(duckdb)

# load in verbatim data snapshot (change this location at a later date)
dw_raw <- fread("/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/idigibio-tracheophyte-dw/occurrence_raw.csv")

# SELECT for fields on interest
dw_raw <- dw_raw %>% 
  select(coreid,
         'dwc:occurrenceID', 
         'dwc:scientificName', 
         'dwc:genus', 
         'dwc:specificEpithet', 
         'dwc:infraspecificEpithet', 
         'dwc:year', 
         'dwc:month', 
         'dwc:day', 
         'dwc:institutionCode', 
         'dwc:recordedBy',
         'dwc:locality', 
         'dwc:decimalLatitude', 
         'dwc:decimalLongitude', 
         'dwc:geodeticDatum', 
         'dwc:coordinateUncertaintyInMeters', 
         'dwc:informationWithheld'
  ) %>% 
  # MUTATE with a conditional, if scientificName isn't blank then use that entry, if it is blank fill with the genus + specificEpithet + infraspecificEpithet fields
  mutate(verbatimScientificName = ifelse(`dwc:scientificName` != "" & !is.na(`dwc:scientificName`), `dwc:scientificName`, trimws(paste(`dwc:genus`, `dwc:specificEpithet`, `dwc:infraspecificEpithet`)))) %>%
  # SELECT out these fields that are no longer necessary
  select(-'dwc:genus', -'dwc:specificEpithet', -'dwc:infraspecificEpithet') %>% 
  # RENAME subsequent fields to remove dwc: standard
  rename(
    occurrenceID = 'dwc:occurrenceID',
    verbatimYear = 'dwc:year', 
    verbatimMonth = 'dwc:month',
    verbatimDay = 'dwc:day', 
    verbatimInstitutionCode = 'dwc:institutionCode', 
    verbatimRecordedBy = 'dwc:recordedBy', 
    verbatimLocality = 'dwc:locality', 
    verbatimDecimalLongitude = 'dwc:decimalLongitude', 
    verbatimDecimalLatitude = 'dwc:decimalLatitude', 
    verbatimGeodeticDatum = 'dwc:geodeticDatum', 
    verbatimCoordinateUncertaintyInMeters = 'dwc:coordinateUncertaintyInMeters', 
    verbatimInformationWithheld = 'dwc:informationWithheld'
  )

# write out as a parquet file for quick SQL command retrieval
write_parquet(dw_raw, "/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/parquet-occs/idigbio_raw_occ.parquet")

# Save on RAM by removing this enviromental object before proceeding
rm(dw_raw)

# load in the processed data snapshot 
dw_processed <- fread("/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/idigibio-tracheophyte-dw/occurrence.csv")

# SELECT fields of interest
dw_processed <- dw_processed %>%
  select(coreid, 
         'dwc:occurrenceID', 
         'dwc:basisOfRecord', 
         'dwc:eventDate', 
         'dwc:coordinateUncertaintyInMeters') %>% 
  # RENAME fields 
  rename(
    occurrenceID = 'dwc:occurrenceID', 
    basisOfRecord = 'dwc:basisOfRecord',
    eventDate = 'dwc:eventDate', 
    coordinateUncertaintyInMeters = 'dwc:coordinateUncertaintyInMeters'
  )

# write out as a parquet file for quick SQL command retrieval
write_parquet(dw_processed, "/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/parquet-occs/idigbio_processed_occ.parquet")



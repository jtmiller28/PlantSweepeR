# 002D merge gbif raw occ data and parquet
# Author: JT Miller 
# Date: 03-06-2024 (updated 11-24-2025)
# Project: PlantSweepeR

# Load packages 
library(tidyverse)
library(data.table)
library(arrow)
library(DBI)
library(duckdb)

# read in the verbatim gbif cut up snapshots (change dirs)
verbatim1 <- fread("/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/gbif-tracheophyte-dw/select-verbatim1.txt")
verbatim2 <- fread("/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/gbif-tracheophyte-dw/select-verbatim2.txt")
verbatim3 <- fread("/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/gbif-tracheophyte-dw/select-verbatim3.txt")
verbatim4 <- fread("/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/gbif-tracheophyte-dw/select-verbatim4.txt")

verbatim <- verbatim1 %>% 
  rbind(verbatim2) %>% 
  rbind(verbatim3) %>% 
  rbind(verbatim4)
# change dirs
write_parquet(verbatim, "/home/millerjared/blue_guralnick/millerjared/BoCP/data/raw/parquet-occs/gbif_verbatim_occ.parquet")
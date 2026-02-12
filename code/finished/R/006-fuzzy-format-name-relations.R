### Fuzzy Format Name Relations
### Author: JT Miller
### 08/28/2025

# Purpose: Build Fuzzy Name Relation Table to later use in Exact Match Schema to speed up building species flagged tables 

## Load Libraries
library(duckdb)
library(DBI)
library(data.table)

# Run manual, with this being task 1 
task_id <- 1
## Connect to duckdb db 
con <- dbConnect(duckdb::duckdb(), dbdir = "/blue/guralnick/millerjared/PlantSweepeR/data/processed/archive_name_relations.duckdb")
## Archive Table (Build Only Once) CAREFUL, running as an array should only let the first round build this...
if(task_id == 1){
  cat("Constructive name relation archive table for the first time...\n")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS name_relation_archive;")
  DBI::dbExecute(con, 
                 "CREATE TABLE IF NOT EXISTS name_relation_archive (
               accepted_name TEXT,         -- canonical/parent name
               variant_name TEXT,          -- synonym, variation, etc
               like_pattern TEXT,          -- pattern for LIKE queries (ex: %OLNEYA%TESOTA%)
               UNIQUE(accepted_name, variant_name, like_pattern)
               );")
}
## Bring in name relations (as list, vectors of accepted name + synonyms)
name_list <- readRDS("/blue/guralnick/millerjared/PlantSweepeR/data/processed/name_list.rds")
for(j in 1:length(name_list)){
  name_list_part <- name_list[j] # remove later
  cat("Constructing Fuzzy Match Relations for", name_list_part[[1]][1], "and its synonyms\n")
  ## Loop through all possible names and construct Fuzzy Match (LIKE) Queries
  for(i in seq_along(name_list_part)){
    nvec <- name_list_part[[i]] # grab out each set of names
    accepted <- nvec[1] # set aside accepted name 
    variants <- nvec # all names, including accepted
    for(v in variants){ # loop through each variant
      ## make a LIKE pattern, e.g. "Olneya tesota" -> "%OLNEYA%TESOTA%"
      pat <- paste0("%", gsub(" ", "%", toupper(v)), "%")
      dbExecute(con, "INSERT OR IGNORE INTO name_relation_archive (accepted_name, variant_name, like_pattern) VALUES ($1, $2, $3)", 
                params=list(accepted,v,pat))
    }
  }
  
}
cat("Finished and Closing DB connection...\n")
# Check: 
db_count <- dbGetQuery(con, "SELECT COUNT(DISTINCT accepted_name) AS n FROM name_relation_archive;")
cat("Unique accepted_name count in DB:", db_count$n, "\n")
cat("Length of name_list:", length(name_list), "\n")
# Spot Check: NR
# set.seed(123)  # Optional: For reproducible sampling
# random_idx <- sample(seq_along(name_list), 1)
# random_names <- name_list[[random_idx]]
# accepted_name <- random_names[1]
# cat("Spot check for accepted_name:", accepted_name, "\n")
# synonym_check <- dbGetQuery(
#   con,
#   "SELECT * FROM name_relation_archive WHERE accepted_name = $1",
#   params = list(accepted_name)
# )
# length(synonym_check$variant_name)
dbDisconnect(con, shutdown = TRUE)



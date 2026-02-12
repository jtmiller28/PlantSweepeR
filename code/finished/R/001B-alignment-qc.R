# 001B Taxon Name Alignment Quality Control
# Author: JT Miller 
# Date: 12-10-2025
# Project: PlantSweepeR 

## Purpose: A secondary quality control script to identify and flag possible issues in the merged taxonomies 

# load libraries
library(data.table)
library(rgnparser)
library(tidyverse)
library(taxadb)

final_table <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/processed/wcvp-ncbi-alignment-2025.csv")

## Part 1) Check for large 'leaps' in higher level relationships, to better flag and diagnose odd cases where name relations have had dramatic changes that could alter tree construction
# create aligned name field (wcvp only) note we use backbone due to Viburnum filtering leaving out HETEROTYPIC synonyms. 
wcvp_accepted_mapping <- wcvp_backbone[taxon_status == "Accepted"]
wcvp_synonym_mapping <- wcvp_backbone[taxon_status %in% c("Synonym", "Illegitimate", "Invalid", "Misapplied", "Orthographic", "Unplaced")]
wcvp_relations <- merge(wcvp_accepted_mapping, wcvp_synonym_mapping, # Create a relational table for wcvp relations
                        by.x = "accepted_plant_name_id",
                        by.y = "accepted_plant_name_id", 
                        all.x = TRUE)

# Order and select fields for simplicity
wcvp_relations <- wcvp_relations[, .(
  wcvpAlignedGenus = genus.x,
  wcvpAlignedFamily = family.x,
  wcvpNameFamily = family.y,
  wcvpAcceptedNameUsageID = accepted_plant_name_id,
  wcvpAlignedName = taxon_name.x,
  wcvpAlignedNameAuthors = taxon_authors.x,
  wcvpAlignedNameStatus = taxon_status.x,
  wcvpName = taxon_name.y,
  wcvpNameAuthors = taxon_authors.y,
  wcvpNameStatus = taxon_status.y
)]

aligned_as_name <- wcvp_relations[, .(
  wcvpAlignedFamily,
  wcvpNameFamily,
  wcvpAlignedGenus,
  wcvpAcceptedNameUsageID,
  wcvpAlignedName,
  wcvpAlignedNameAuthors,
  wcvpAlignedNameStatus,
  wcvpName = wcvpAlignedName,
  wcvpNameAuthors = wcvpAlignedNameAuthors,
  wcvpNameStatus = wcvpAlignedNameStatus
)]


# Combine the original relations with the aligned names as names
wcvp_relations <- rbindlist(list(wcvp_relations, aligned_as_name), use.names = TRUE, fill = TRUE)
# Remove any duplicate rows to avoid redundancy
wcvp_relations <- unique(wcvp_relations)
# Removal of unplaced names. Note that those names that register as NA for wcvpAlignedName CANNOT arrive at a name due to being unplaced in wcvp's taxonomy. 
wcvp_relations <- wcvp_relations[wcvpNameStatus != "Unplaced" & !is.na(wcvpAlignedName)] # note that OR statement is because for whatever reason a name can be a synonym or otherwise attached to a unplaced name, however that name is not to be used regardless. 

# Now apply a check
check <- wcvp_relations %>% mutate(multFams = ifelse(wcvpAlignedFamily == wcvpNameFamily, FALSE, TRUE))

check_names <- check %>% filter(multFams == TRUE)

problem_names <- wcvp_relations %>% filter(wcvpName %in% check_names$wcvpName) %>%  distinct(wcvpAlignedName)

name_fams<- select(ncbi_parsed_plant_names_df, nameFamily = ncbiNameFamily, name = ncbiName)

final_table_wcvp <- final_table %>% filter(source == "wcvp")
final_table_ncbi <- final_table %>% filter(source == "ncbi")
check_names <- check_names %>% rename(name = wcvpName, nameFamily = wcvpNameFamily)

final_table_wcvp <- final_table_wcvp %>% mutate(multFams = ifelse(name %in% problem_names, TRUE, FALSE)) %>%
  left_join(select(check_names, name, nameFamily), by = "name") %>% distinct()
final_table_ncbi <- final_table_ncbi %>% left_join(name_fams, by = "name") %>% distinct()

final_table_ncbi <- final_table_ncbi %>% mutate(multFams = ifelse(nameFamily == ncbiAlignedFamily, FALSE, TRUE))

check_full <- rbind(final_table_wcvp, final_table_ncbi)

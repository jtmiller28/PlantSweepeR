finished_sp_tables <- list.files("/blue/guralnick/millerjared/PlantSweepeR/data/processed/flagged-datasets//")
fin_species <- gsub(".csv", "", finished_sp_tables)
fin_species <- gsub("_", " ", fin_species)
name_list <- readRDS("/blue/guralnick/millerjared/PlantSweepeR/data/processed/name_list.rds")
accepted_names <- sapply(name_list, function(x) x[1])
unfinished_accepted_names <- setdiff(accepted_names, fin_species)
matching_indices <- which(accepted_names %in% unfinished_accepted_names)
unfinished_names <- name_list[matching_indices]

# load in zero match file
saveRDS(unfinished_names, "/blue/guralnick/millerjared/PlantSweepeR/data/processed/unfinished_sp_tables_names_chkpt.rds")


## Check if there are names available to match
library(arrow)

unfinished_names <- readRDS("/blue/guralnick/millerjared/PlantSweepeR/data/processed/unfinished_sp_tables_names_chkpt.rds")
if (is.list(unfinished_names)) unfinished_names <- sapply(unfinished_names, `[`, 1)

for (accepted_target in unfinished_names) {
  accepted_name_filestyle <- gsub(" ", "_", accepted_target)
  matches_parquet_file <- file.path(
    "/blue/guralnick/millerjared/PlantSweepeR/data/processed/archived-name-matches/",
    paste0(accepted_name_filestyle, "_matches.parquet")
  )
  if (file.exists(matches_parquet_file)) {
    result <- arrow::read_parquet(matches_parquet_file)
    cat("Read", nrow(result), "matches for", accepted_target, "\n")
    # Optionally, do something with `result` here
  } else {
    cat("No archived match file for", accepted_target, "\n")
  }
}

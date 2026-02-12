finished_archived_species <- list.files("/blue/guralnick/millerjared/PlantSweepeR/data/processed/archived-name-matches/")
fin_species <- gsub("_matches.parquet", "", finished_archived_species)
fin_species <- gsub("_", " ", fin_species)
name_list <- readRDS("/blue/guralnick/millerjared/PlantSweepeR/data/processed/name_list_reordered.rds")
accepted_names <- sapply(name_list, function(x) x[1])
unfinished_accepted_names <- setdiff(accepted_names, fin_species)
matching_indices <- which(accepted_names %in% unfinished_accepted_names)
unfinished_names <- name_list[matching_indices]

# reorder for priority
# Count the number of names at each position
counts <- sapply(unfinished_names, length)

# Visualize the distribution as a histogram
hist(counts, main = "Distribution of Number of Names per Position",
     xlab = "Number of Names at Position", ylab = "Frequency",
     col = "steelblue", border = "white")

# num of array tasks for next step
n <- 60

# set up groupings
group_sizes <- sapply(unfinished_names, length) # counts up the num of names per position 
group_indices <- seq_along(unfinished_names) # extracts indices for later

# Sort groups by size largest to small
sorted_indices <- order(group_sizes, decreasing = TRUE)

# Initialize N empty bins
bins <- vector("list", n)
bin_counts <- rep(0, n)
bin_groups <- vector("list", n)

# Use bin packing, assigns names to the lightest packed bin.
for (idx in sorted_indices) {
  lightest <- which.min(bin_counts)
  bin_groups[[lightest]] <- c(bin_groups[[lightest]], idx)
  bin_counts[lightest] <- bin_counts[lightest] + group_sizes[idx]
}

# To get the actual name groups for each task:
task_lists <- lapply(bin_groups, function(indices) unfinished_names[indices])

# Visualize how many names are in each task
task_sizes <- sapply(task_lists, function(x) sum(sapply(x, length)))
barplot(task_sizes, main = "Total Number of Names per Array Task",
        xlab = "Task Index", ylab = "Number of Names",
        col = "purple", border = "white")

# Concatenate all indices in task order
reordered_indices <- unlist(bin_groups)

# Reorder your name_list accordingly
reordered_unfinished_names <- unfinished_names[reordered_indices]

saveRDS(reordered_unfinished_names, "/blue/guralnick/millerjared/PlantSweepeR/data/processed/unfinished_names_chkpt.rds")

### Title: Partition Name List Array
### Author: JT Miller
### Date: 11-25-2025

## Purpose: When fuzzy searching and building datasets, we should partition names density evenly across array tasks for efficiency

# Reorient namelist with a uniform distribution
name_list <- readRDS("/blue/guralnick/millerjared/PlantSweepeR/data/processed/name_list.rds")

# Count the number of names at each position
counts <- sapply(name_list, length)

# Visualize the distribution as a histogram
hist(counts, main = "Distribution of Number of Names per Position",
     xlab = "Number of Names at Position", ylab = "Frequency",
     col = "steelblue", border = "white")

# num of array tasks for next step
n <- 60

# set up groupings
group_sizes <- sapply(name_list, length) # counts up the num of names per position 
group_indices <- seq_along(name_list) # extracts indices for later

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
task_lists <- lapply(bin_groups, function(indices) name_list[indices])

# Visualize how many names are in each task
task_sizes <- sapply(task_lists, function(x) sum(sapply(x, length)))
barplot(task_sizes, main = "Total Number of Names per Array Task",
        xlab = "Task Index", ylab = "Number of Names",
        col = "purple", border = "white")

# Concatenate all indices in task order
reordered_indices <- unlist(bin_groups)

# Reorder your name_list accordingly
reordered_name_list <- name_list[reordered_indices]

# Write this reordered list out
saveRDS(reordered_name_list, "/blue/guralnick/millerjared/PlantSweepeR/data/processed/name_list_reordered.rds")




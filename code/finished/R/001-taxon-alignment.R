# 001 Taxon Name Alignment
# Author: JT Miller 
# Date: 02-02-2024 (updated 11/24/2025)
# Project: PlantSweepeR 

### Purpose: align names from POWO/WCVP and NCBI to build species occurrence datasets that can be effectively linked to Stephen's phylogenies
# Load packages
library(data.table)
library(rgnparser)
library(tidyverse)
library(taxadb)

#### Set up pathing to use gnparser
my_path <- Sys.getenv("PATH") # grab our path
Sys.setenv(PATH = paste0(my_path, "/home/millerjared/gnparser"))

#### Load Taxonomic Backbones
wcvp_backbone <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/raw/wcvp_names_2025_update.csv")
td_create("ncbi")
ncbi_names <- taxa_tbl("ncbi") # load in ncbi's names according to taxadb's last pull
ncbi_names <- ncbi_names %>%  collect() # collects into a df 

#### Clean database to relevant taxonomic scope
ncbi_names <- as.data.table(ncbi_names) 
ncbi_plant_names <- ncbi_names[kingdom == "Viridiplantae"]
ncbi_plant_names <- ncbi_plant_names %>% 
  filter(!grepl(" cf", specificEpithet) | !grepl(" cf", scientificName) ) %>% 
  filter(!grepl(" cf  ", specificEpithet) | !grepl(" cf  ", scientificName))# note that specificEpithet is the aligned name. 
ncbi_plant_names <- ncbi_plant_names %>% # we wont deal with hybrids for simplicity
  filter(!grepl(" x ", specificEpithet ) | !grepl(" x ", scientificName)) %>% 
  filter(!grepl(" sect ", specificEpithet) | !grepl(" sect ", scientificName)) %>% 
  filter(!grepl("/", specificEpithet) | !grepl("/", scientificName))

#### Standardize Backbones for joining
ncbi_plant_names <- ncbi_plant_names[,.(genus, order, family, scientificName, taxonomicStatus, ncbiAcceptedNameUsageID = acceptedNameUsageID, ncbiAlignedName = specificEpithet)]
# parse the scientificName field into the name and authorship fields
ncbi_parsed_plant_names <- gn_parse_tidy(ncbi_plant_names$scientificName)  # break up names
# remove duplicates where present 
ncbi_parsed_plant_names <- ncbi_parsed_plant_names %>% 
  distinct(, .keep_all=TRUE)
# merge
ncbi_parsed_plant_names_df <- merge(ncbi_plant_names, ncbi_parsed_plant_names, by.x = 'scientificName', by.y = 'verbatim')
# we're not going to worry about complications related to names that CANNOT be parsed, so anything that gets truncated 
ncbi_parsed_plant_names_df <- ncbi_parsed_plant_names_df %>% 
  filter(!cardinality == 0) # 0 represents things that fail at parsing. 
# remove duplicates again due to merge
ncbi_parsed_plant_names_df <- ncbi_parsed_plant_names_df %>% 
  distinct(, .keep_all = TRUE)
# update field names for clarity
ncbi_parsed_plant_names_df <- ncbi_parsed_plant_names_df[,.(ncbiNameGenus = genus, ncbiNameOrder = order, ncbiNameFamily = family, ncbiVerbatimName = scientificName, ncbiName = canonicalfull, ncbiAuthor = authorship, ncbiTaxonomicStatus = taxonomicStatus, ncbiAcceptedNameUsageID, ncbiAlignedName)]
# create a relation that notes whether the authority is accepted or synonym & deal with other name details
ncbi_parsed_plant_names_df <- ncbi_parsed_plant_names_df %>% 
  filter(!ncbiTaxonomicStatus == "in-part") %>% # this maps to multiple aligned names by definition, which is a problem for alignment. 
  group_by(ncbiName) %>%  # ID multiple mapping names, as these will cause issues 
  mutate(ncbiNameMultipleMaps = ifelse(sum(ncbiTaxonomicStatus == "accepted") > 1, TRUE, FALSE)) %>% 
  ungroup() 
# Remove Multiple Mappings (As checked prior), Standardize statuses
ncbi_parsed_plant_names_df <- ncbi_parsed_plant_names_df %>% 
  filter(ncbiNameMultipleMaps == FALSE) %>% # removed for simplicity. Check
  mutate(ncbiTaxonomicStatus = case_when(
    ncbiTaxonomicStatus == "synonym" ~ ncbiTaxonomicStatus, # return same relation
    ncbiTaxonomicStatus == "accepted" ~ ncbiTaxonomicStatus, # return same relation
    ncbiTaxonomicStatus == "common name" ~ "synonym", # alter field
    ncbiTaxonomicStatus == "equivalent name" ~ "synonym", # alter field
    ncbiTaxonomicStatus == "includes" ~ "synonym", # alter field
    ncbiTaxonomicStatus == "type material" ~ "synonym", # alter field 
    ncbiTaxonomicStatus == "genbank common name" ~ "synonym", # alter field
    ncbiTaxonomicStatus == "authority" & ncbiName == ncbiAlignedName & !is.na(ncbiAuthor) ~ "accepted with authorship", 
    ncbiTaxonomicStatus == "authority" & ncbiName != ncbiAlignedName & !is.na(ncbiAuthor) ~ "synonym with authorship"
  )) %>% 
  mutate(ncbiTaxonomicStatus = case_when( # double check as there are odd cases where these are missed
    ncbiName == ncbiAlignedName & !is.na(ncbiAuthor) ~ "accepted with authorship", 
    ncbiName != ncbiAlignedName & !is.na(ncbiAuthor) ~ "synonym with authorship",
    TRUE ~ ncbiTaxonomicStatus # else cases return 
  ))
# remove redundancy, if we have a synonym with authorship we dont need just the synonym, same logic for accepted names with authorship 
ncbi_parsed_plant_names_df <- ncbi_parsed_plant_names_df %>% 
  group_by(ncbiName) %>% 
  mutate(redundant = ifelse(
    # check for redundancy in synonym
    all(c("synonym", "synonym with authorship") %in% ncbiTaxonomicStatus) |
      # check for redundancy in accepted
      all(c("accepted", "accepted with authorship") %in% ncbiTaxonomicStatus),
    TRUE, # if there is redundancy
    FALSE # else return
  )) %>% 
  ungroup()
# when redundancy is TRUE, remove the name that has the least info (w/out author)
ncbi_parsed_plant_names_df <- ncbi_parsed_plant_names_df %>%
  filter(!(redundant == TRUE & is.na(ncbiAuthor))) # note that this isnt perfect, it'll remove names that fail parsing past the genus level, but im okay with this simplification. 
ncbi_parsed_plant_names_df <- as.data.table(ncbi_parsed_plant_names_df)
# create a relational table
ncbi_relations <- ncbi_parsed_plant_names_df %>% 
  rename(ncbiAlignedGenus = ncbiNameGenus, ncbiAlignedOrder = ncbiNameOrder, ncbiAlignedFamily = ncbiNameFamily) %>% # fixing a slight correction and renaming for clarity
  group_by(ncbiAlignedName) %>% 
  mutate(ncbiAlignedAuthor = ifelse(
    ncbiTaxonomicStatus == "accepted with authorship", 
    ncbiAuthor,
    NA_character_ # deal with NAs
  )) %>% 
  fill(ncbiAlignedAuthor, .direction = "downup") %>% # fill ncbiAligned author for all rows sharing the same "ncbiAlignedName 
  mutate(ncbiAlignedNameStatus = ifelse(
    ncbiTaxonomicStatus == "accepted" | ncbiTaxonomicStatus == "accepted with authorship", 
    "accepted",
    NA_character_
  )) %>% 
  fill(ncbiAlignedNameStatus, .direction = "downup") %>% 
  ungroup() %>% 
  as.data.table()
# simplify fields to the ones that we need
ncbi_relations <- ncbi_relations[, .( 
  ncbiAlignedOrder,
  ncbiAlignedFamily,
  ncbiAlignedGenus,
  ncbiAcceptedNameUsageID,
  ncbiAlignedName,
  ncbiAlignedAuthor,
  ncbiAlignedNameStatus,
  ncbiName,
  ncbiNameTaxonomicStatus = ncbiTaxonomicStatus,
  ncbiAuthor
)]

# wcvp standardize
wcvp_names <- wcvp_backbone[, .(genus, order, family, plant_name_id, taxon_status, taxon_name, taxon_authors, accepted_plant_name_id)]

# create aligned name field (wcvp only) note we use backbone due to Viburnum filtering leaving out HETEROTYPIC synonyms. 
wcvp_accepted_mapping <- wcvp_backbone[taxon_status == "Accepted"]
wcvp_synonym_mapping <- wcvp_backbone[taxon_status %in% c("Synonym", "Illegitimate", "Invalid", "Misapplied", "Orthographic", "Unplaced")]
wcvp_relations <- merge(wcvp_accepted_mapping, wcvp_synonym_mapping, # Create a relational table for wcvp relations
                        by.x = "accepted_plant_name_id",
                        by.y = "accepted_plant_name_id", 
                        all.x = TRUE) #all.y = TRUE)
# Order and select fields for simplicity
wcvp_relations <- wcvp_relations[, .(
  wcvpAlignedGenus = genus.x,
  wcvpAlignedFamily = family.x,
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

#### Identify multiple mapping for WCVP, remove as shown the perc effect is Low.
# cases where a wcvpName can go to different wcpvAlignedNames 
wcvp_relations <- wcvp_relations %>% 
  group_by(wcvpName) %>% 
  mutate(nNamePaths = n_distinct(wcvpAlignedName)) %>% 
  mutate(multipleNameAlignmentsPossible = ifelse(nNamePaths > 1, TRUE, FALSE))
# remove, EXCEPT in cases where its the accepted name (see md for details)
wcvp_relations <- wcvp_relations %>% 
  filter(!(multipleNameAlignmentsPossible == TRUE & !wcvpNameStatus == "Accepted")) # remove, unless its an accepted name as that would cause issues. (We're ignoring this for simplicity.)

#### Some further Standardize steps
ncbi_relations_check <- ncbi_relations %>% 
  filter(!is.na(ncbiNameTaxonomicStatus)) %>% # not entirely sure how this snuck in there
  #rename(name = ncbiName, nameStatus = ncbiNameTaxonomicStatus) %>% 
  mutate(source = "ncbi") %>% 
  rename(ncbiNameStatus = ncbiNameTaxonomicStatus) %>% 
  mutate(ncbiNameStatus = case_when(
    ncbiNameStatus == "accepted with authorship" ~ "Accepted",
    ncbiNameStatus == "synonym with authorship" ~ "Synonym", 
    ncbiNameStatus == "accepted" ~ "Accepted", 
    ncbiNameStatus == "synonym" ~ "Synonym"
  )) %>% 
  mutate(ncbiAlignedNameStatus = str_to_title(ncbiAlignedNameStatus)) %>% 
  #mutate(ncbiParentAlignedName = str_extract(ncbiAlignedName, "^\\w+ \\w+")) %>%
  mutate(ncbiParentAlignedName = word(ncbiAlignedName, 1, 2)) %>% 
  select(ncbiAlignedOrder, ncbiAlignedFamily, ncbiAlignedGenus, ncbiAlignedName, ncbiParentAlignedName, ncbiAlignedAuthor, ncbiAlignedNameStatus, ncbiName, ncbiAuthor, ncbiNameStatus, source, ncbiAcceptedNameUsageID) %>% 
  filter(str_count(ncbiAlignedName, "\\w+") > 1)

wcvp_relations_check <- wcvp_relations %>% 
  mutate(source = "wcvp") %>% 
  #mutate(wcvpParentAlignedName = str_extract(wcvpAlignedName, "^\\w+ \\w+")) %>%
  mutate(wcvpParentAlignedName = word(wcvpAlignedName, 1, 2)) %>% 
  select(wcvpAlignedFamily, wcvpAlignedGenus, wcvpAlignedName, wcvpParentAlignedName, wcvpAlignedNameAuthors, wcvpAlignedNameStatus, wcvpName, wcvpNameAuthors, wcvpNameStatus, source) %>% 
  filter(str_count(wcvpAlignedName, "\\w+") > 1) # removes genera level alignment

#### Join datasets, coalesce for priority, and backfill ncbiIDs where possible 
# Perform a full join on the names
merged_df <- full_join(wcvp_relations_check, ncbi_relations_check, by = c("wcvpName" = "ncbiName"), suffix = c("", "_ncbi"), keep = TRUE)

# Process the merged dataframe
output_df <- merged_df %>%
  mutate(
    alignedParentName = coalesce(wcvpParentAlignedName, ncbiParentAlignedName),
    alignedName = coalesce(wcvpAlignedName, ncbiAlignedName),
    name = coalesce(wcvpName, ncbiName),
    sourceConflict = ifelse(!is.na(wcvpName) & !is.na(ncbiName) & wcvpNameStatus == "Synonym" & ncbiNameStatus == "Accepted", TRUE, FALSE),
    source = ifelse(!is.na(wcvpName), "wcvp", "ncbi")
  ) %>%
  group_by(alignedParentName) %>%
  mutate(
    ncbiPossibleIDs = ifelse(all(is.na(ncbiAcceptedNameUsageID)), NA, {
      # Extract unique IDs, ensuring NA values are omitted
      unique_ids <- unique(na.omit(ncbiAcceptedNameUsageID))
      # Prioritize the ID that matches wcvp's accepted name
      if (any(!is.na(wcvpAlignedName) & !is.na(ncbiAlignedName) & ncbiAlignedName == wcvpAlignedName, na.rm = TRUE))
      {
        wcvp_id <- ncbiAcceptedNameUsageID[ncbiAlignedName == wcvpAlignedName]
        wcvp_id <- unique(wcvp_id[!is.na(wcvp_id)])
        other_ids <- unique_ids[!unique_ids %in% wcvp_id]
        paste(c(wcvp_id, other_ids), collapse = "|")
      } else {
        paste(unique_ids, collapse = "|")
      }
    }),
    mult_ncbi_ids = n_distinct(ncbiAcceptedNameUsageID, na.rm = TRUE) > 1,
    ncbiAcceptedNameUsageID = ifelse(is.na(ncbiAcceptedNameUsageID), first(ncbiAcceptedNameUsageID, na_rm = TRUE), ncbiAcceptedNameUsageID)
  ) %>%
  ungroup() %>%
  select(ncbiAlignedOrder, wcvpAlignedFamily,ncbiAlignedFamily, wcvpAlignedGenus, ncbiAlignedGenus, alignedName, alignedParentName, name, sourceConflict, wcvpName, ncbiName, ncbiAcceptedNameUsageID, mult_ncbi_ids, ncbiPossibleIDs, source)


final_table <- output_df %>% 
  distinct() %>% 
  mutate(nameStatus = ifelse(alignedName == name, "Accepted", "Synonym"))

# additional step after fixing specificEpithet truncation, removal of '×' ending hybrids...these are invalid 
final_table <- final_table %>%
  #filter(!grepl("×$", alignedParentName)) %>%  # remove things that end with '×'
  mutate(alignedParentName = ifelse(grepl("×$", alignedParentName), "", alignedParentName)) %>% 
  mutate(alignedParentName = ifelse(grepl("^×", alignedParentName), "", alignedParentName)) 
#filter(!grepl("^×", alignedParentName)) # remove things that begin with '×'

fwrite(final_table, "/blue/guralnick/millerjared/PlantSweepeR/data/processed/wcvp-ncbi-alignment-2025.csv")

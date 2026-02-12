### Title: Parquet Out Species
### Author: JT Miller
### Date: 01/28/2026

# Convert all of our species files into a large parquet for storage and processing into subsets

# Load Libraries
library(arrow)
library(data.table)

# grab data
# species_files <- list.files("/blue/guralnick/millerjared/BoCP/data/processed/plantdb-flagged-data/", full.names = TRUE)
# 
# for(i in species_files){
#   species_df <- fread(species_files[[i]])
#   arrow::write_parquet(species_df, "/blue/guralnick/millerjared/BoCP/data/processed/sp-occur-output.parquet")
# }

csv_dir <- "/blue/guralnick/millerjared/PlantSweepeR/data/processed/flagged-datasets-redone/"
ds <- arrow::open_dataset(csv_dir, format = "csv")

# set up cols to read
schema <- schema(
  uuid = utf8(),
  aggregator = utf8(),
  occurrenceID = utf8(),
  basisOfRecord = utf8(),
  species = utf8(),
  verbatimScientificName = utf8(),
  geodeticDatum = utf8(),
  decimalLatitude = float32(),
  decimalLongitude = float32(),
  coordinateUncertaintyInMeters = float32(),
  eventDate = date32(),
  year = float64(), # these need to be floats due to misentries...
  month = float64(),
  day = float64(),
  informationWithheld = utf8(),
  recordedBy = utf8(),
  institutionCode = utf8(),
  locality = utf8(), 
  roundedLatitude = float32(),
  roundedLongitude = float32(),
  withInAggDuplicate = bool(), 
  AggDuplicateGroupID = int64(), 
  amongAggDuplicate = bool(), 
  specimenDuplicateGroupID = int64(), 
  specimenDuplicate = bool(), 
  AggDuplicateRank = int64(), 
  specimenDuplicateRank = int64(), 
  parsedName = utf8(), 
  taxonomicExactMatch = bool(), 
  wgs84Datum = bool(), 
  coordinateIssue = bool(), 
  trueCoordsWithheld = bool(), 
  coordUncertaintyInc = bool(), 
  validRecord = bool(), 
  equalLatLon = bool(), 
  zeroCoords = bool(), 
  capitalCoord = bool(), 
  centroidCoord = bool(), 
  inOceanCoord = bool(), 
  inGBIFHeadquarters = bool(), 
  inInstitutionBounds = bool(), 
  landscaped = bool(), 
  distOutlier = bool(), 
  wcvpRangeStatus = utf8(), 
  LEVEL3_NAM = utf8(), 
  area_code_l3 = utf8()
)

ds <- arrow::open_dataset(csv_dir, format = "csv", schema = schema, skip_rows = 1)
arrow::write_dataset(
  ds,
  "/blue/guralnick/millerjared/PlantSweepeR/data/processed/sp-partitioned-occs-flagged_parquet",
  format = "parquet",
  compression = "zstd", 
  partitioning = c("species")
)

# 
# ds_parq <- open_dataset("/blue/guralnick/millerjared/PlantSweepeR/data/processed/sp-partitioned-occs-flagged_parquet/")
# 
# system.time({
#   test <- ds_parq |> distinct(basisOfRecord) |>  summarize(n = n()) |> collect()
# })
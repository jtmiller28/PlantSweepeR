library(terra)
library(sf)
library(dplyr)
library(arrow)
library(MASS)
library(ggplot2)
library(tidyterra)

# ── Template rasters ──────────────────────────────────────────────────────────
r_1 <- terra::rast("/blue/guralnick/millerjared/SDM_pipeline/data/vars_30s_moll/bio_1.tif")
r_5 <- terra::rast("/blue/guralnick/millerjared/SDM_pipeline/data/vars_5km_moll/bio_1.tif")

target_crs <- terra::crs(r_1)

## ── Step 1: Pull coordinates + duplicate flag columns lazily ─────────────────
message("Opening parquet dataset lazily...")
ds_parq <- arrow::open_dataset("/blue/guralnick/millerjared/PlantSweepeR/data/processed/sp-partitioned-occs-flagged_parquet/")

message("Collecting coordinates + duplicate flags only...")
xy_raw <- ds_parq |>
  dplyr::select(
    decimalLongitude, decimalLatitude,
    amongAggDuplicate, AggDuplicateGroupID, AggDuplicateRank,
    specimenDuplicate, specimenDuplicateGroupID, specimenDuplicateRank
  ) |>
  dplyr::filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  ) |>
  dplyr::collect()

message(paste("Total raw records:", nrow(xy_raw)))

# ── Step 2: Remove duplicates following your established logic ────────────────
message("Removing aggregate duplicates...")
xy_raw <- xy_raw %>%
  filter(!amongAggDuplicate | is.na(AggDuplicateGroupID)) %>%
  bind_rows(
    xy_raw %>%
      filter(amongAggDuplicate) %>%
      group_by(AggDuplicateGroupID) %>%
      filter(AggDuplicateRank == min(AggDuplicateRank, na.rm = TRUE)) %>%
      slice(1) %>%
      ungroup()
  )
message(paste(nrow(xy_raw), "records after aggregate duplicate removal"))

message("Removing specimen duplicates...")
xy_raw <- xy_raw %>%
  filter(!specimenDuplicate | is.na(specimenDuplicateGroupID)) %>%
  bind_rows(
    xy_raw %>%
      filter(specimenDuplicate) %>%
      group_by(specimenDuplicateGroupID) %>%
      filter(specimenDuplicateRank == min(specimenDuplicateRank, na.rm = TRUE)) %>%
      slice(1) %>%
      ungroup()
  )
message(paste(nrow(xy_raw), "records after specimen duplicate removal"))

# Drop flag columns now — only coordinates needed from here
xy_raw <- xy_raw %>%
  dplyr::select(decimalLongitude, decimalLatitude) %>%
  dplyr::distinct()  # final safety deduplication on coordinates alone

message(paste("Final unique coordinate records:", nrow(xy_raw)))

# ── Step 2: Remove duplicates early to reduce memory footprint ────────────────
# Round to ~1km precision before deduplication to reduce size aggressively
# xy_raw <- xy_raw |>
#   dplyr::mutate(
#     lon_r = round(decimalLongitude, 2),
#     lat_r = round(decimalLatitude, 2)
#   ) |>
#   dplyr::distinct(lon_r, lat_r) |>
#   dplyr::rename(decimalLongitude = lon_r, decimalLatitude = lat_r)
# 
# message(paste("Records after coordinate rounding + deduplication:", nrow(xy_raw)))

# ── Step 3: Project and rasterize to 1-per-cell ───────────────────────────────
# Convert to SpatVector
alldata <- terra::vect(
  xy_raw,
  geom = c("decimalLongitude", "decimalLatitude"),
  crs  = "EPSG:4326"
)
rm(xy_raw); gc()  # free memory immediately

# Project to Mollweide
alldata_proj <- terra::project(alldata, target_crs)
rm(alldata); gc()

# Crop to template extent
alldata_proj <- terra::crop(alldata_proj, terra::ext(r_1))

# Rasterize to 1km template — presence/absence only (1 per cell)
message("Rasterizing to 1km grid...")
occ_rast <- terra::rasterize(alldata_proj, r_1, fun = "count", background = NA)
occ_rast[occ_rast >= 1] <- 1
rm(alldata_proj); gc()

# ── Step 4: Extract occupied cell coordinates for KDE ────────────────────────
# Stay in Mollweide for KDE (equal area — more correct than WGS84)
message("Extracting cell coordinates for KDE...")
df <- terra::crds(occ_rast, na.rm = TRUE)
message(paste("Occupied cells for KDE:", nrow(df)))

# ── Step 5: KDE ───────────────────────────────────────────────────────────────
message("Running KDE...")
ext_r1 <- terra::ext(r_1)

dens <- MASS::kde2d(
  df[, 1], df[, 2],
  n    = c(3000, 3000),
  lims = c(ext_r1$xmin, ext_r1$xmax,
           ext_r1$ymin, ext_r1$ymax)
)
rm(df); gc()

# ── Step 6: Convert KDE to raster via XYZ ────────────────────────────────────
message("Converting KDE to raster...")

xyz_df <- expand.grid(x = dens$x, y = dens$y)
xyz_df$z <- as.vector(dens$z)
rm(dens); gc()

dens_rast <- terra::rast(xyz_df, type = "xyz", crs = terra::crs(r_1))
rm(xyz_df); gc()

# Rescale to 0-1 range BEFORE writing — KDE values like 2e-13 underflow to 0
# in 32-bit float. Relative weights are all MaxEnt needs anyway.
message("Rescaling KDE values to 0-1...")
dens_max <- terra::global(dens_rast, "max", na.rm = TRUE)[[1]]
dens_rast <- dens_rast / dens_max
message(paste("Rescaled range:", terra::global(dens_rast, "min", na.rm=TRUE)[[1]],
              "to", terra::global(dens_rast, "max", na.rm=TRUE)[[1]]))

# ── Step 7: Resample to both templates ───────────────────────────────────────
message("Resampling to 1km template...")
bias_1km <- terra::resample(dens_rast, r_1, method = "bilinear")
terra::writeRaster(
  bias_1km,
  "/blue/guralnick/millerjared/PlantSweepeR/data/processed/plant_bias_layer_1km_moll.tif",
  datatype  = "FLT4S",  # 32-bit is fine now that values are 0-1
  overwrite = TRUE
)
message(paste("1km bias range:", terra::global(bias_1km, "min", na.rm=TRUE)[[1]],
              "to", terra::global(bias_1km, "max", na.rm=TRUE)[[1]]))

message("Resampling to 5km template...")
bias_5km <- terra::resample(dens_rast, r_5, method = "bilinear")
terra::writeRaster(
  bias_5km,
  "/blue/guralnick/millerjared/PlantSweepeR/data/processed/plant_bias_layer_5km_moll.tif",
  datatype  = "FLT4S",
  overwrite = TRUE
)
message(paste("5km bias range:", terra::global(bias_5km, "min", na.rm=TRUE)[[1]],
              "to", terra::global(bias_5km, "max", na.rm=TRUE)[[1]]))
rm(dens_rast); gc()
# ── Step 7: Visualize (1km version) ──────────────────────────────────────────
na_shp <- rnaturalearth::ne_countries(
  country      = c("Canada", "United States of America", "Mexico"),
  returnclass  = "sf"
) |> sf::st_union() |> sf::st_transform(crs = target_crs)

ggplot() +
  geom_spatraster(data = bias_1km) +
  scale_fill_viridis_c(name = "Sampling\nIntensity", na.value = "transparent") +
  geom_sf(data = na_shp, fill = NA, color = "black", linewidth = 0.3) +
  labs(title = "Seed Plant Sampling Bias Layer - 1km Mollweide") +
  theme_minimal()

ggsave(
  "/blue/guralnick/millerjared/PlantSweepeR/figures/plant_bias_layer_visual.png",
  height = 10, width = 10
)
message("Done.")
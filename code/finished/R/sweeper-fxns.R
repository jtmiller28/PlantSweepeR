### Title: sweeper functions
### Author: JT Miller 
### Date: 08-27-2025

### Flag Exact Matches for Taxonomy ############################################
#' Create a Boolean Flag in occurrence dataset to indicate if the parsed species name matches that of input taxonomy
#' 
#' Takes occurrence data df that contains a verbatimScientificName field and a vector of exact names to match for flagging
#' Returns occurrence data df with flagged field taxonomicExactMatch, indicating if there was an exact match
#' 
#' @param occ_data dataframe, occurrence data for a taxon
#' @param exact_names_to_match vector, character strings to test for exact match
#' @return occ_data, returns a dataframe with added boolean field taxonomicExactMatch indicating if exact_names_to_match were found
#' examples 
#' # flag_taxonomy(idigbio_df, name_list)
flag_taxonomy <- function(occ_data, exact_names_to_match){
  # parse out names from occ data
  occ_names_parsed <- rgnparser::gn_parse_tidy(na.omit(occ_data$verbatimScientificName))
  # grab full name and index 
  occ_names_parsed <- occ_names_parsed %>% dplyr::mutate(n = 1:n()) %>% dplyr::select(canonicalfull, n)
  # apply index to occurrence data
  occ_data <- occ_data %>% dplyr::mutate(n = 1:n())
  # merge by index 
  occ_data <- base::merge(occ_data, occ_names_parsed, by = "n")
  # create boolean flag based on whether the underlying name matches any of the names to retrieve
  occ_data <- occ_data %>% dplyr::mutate(taxonomicExactMatch = ifelse(toupper(canonicalfull) %in% toupper(exact_names_to_match), TRUE, FALSE))
  # rename canonical name to parsedName for clarity and remove the index interger n 
  occ_data <- occ_data %>% rename(parsedName = canonicalfull) %>% select(-n)
}

################################################################################

### Combine Occurrence Datasets ########################################
#' Create a compiled occurrence dataset from all three data aggregates gbif, idigbio, symbiota
#' 
#' Takes occurrence data dfs from the aggregates, standardizes the fields, and brings them together 
#' 
#' @param gbif_df dataframe, occurrence data from gbif
#' @param idigbio_df dataframe, occurrence data from idigbio
#' @param symbiota_df dataframe, occurrence data from symbiota 
#' @param accepted_name characterstring, name of species field to add
#' @return full_occ_df dataframe, occurrence data standardized across the three aggregates
#' examples 
#' # combine_occ_data()
combine_occ_data <- function(gbif_df = NULL, idigbio_df = NULL, symbiota_df = NULL, accepted_name = NULL){
  if(!is.null(idigbio_df)){
    idigbio_df <- idigbio_df %>% 
      rename( 
        year = verbatimYear,
        month = verbatimMonth, 
        day = verbatimDay, 
        uuid = coreid, 
        informationWithheld = verbatimInformationWithheld,
        decimalLatitude = verbatimDecimalLatitude, 
        decimalLongitude = verbatimDecimalLongitude, 
        geodeticDatum = verbatimGeodeticDatum,
        institutionCode = verbatimInstitutionCode, 
        recordedBy = verbatimRecordedBy, 
        locality = verbatimLocality
      ) %>% 
      mutate(species = accepted_name, 
             aggregator = "idigbio") %>% 
      select(uuid,
             aggregator,
             occurrenceID,
             basisOfRecord, 
             species, 
             verbatimScientificName, 
             geodeticDatum, 
             decimalLatitude, 
             decimalLongitude,
             coordinateUncertaintyInMeters, 
             eventDate, 
             year, 
             month, 
             day, 
             informationWithheld, 
             recordedBy, 
             institutionCode, 
             locality
      )
  }
  if(!is.null(gbif_df)){
    gbif_df <- gbif_df %>% 
      rename( 
        uuid = gbifID
      ) %>% 
      mutate(species = accepted_name, 
             aggregator = "gbif") %>% 
      select(uuid,
             aggregator,
             occurrenceID,
             basisOfRecord, 
             species, 
             verbatimScientificName, 
             geodeticDatum, 
             decimalLatitude, 
             decimalLongitude,
             coordinateUncertaintyInMeters, 
             eventDate, 
             year, 
             month, 
             day, 
             informationWithheld, 
             recordedBy, 
             institutionCode, 
             locality
      )
  }
  if(!is.null(symbiota_df)){
    symbiota_df <- symbiota_df %>% 
      mutate(species = accepted_name, 
             aggregator = "symbiota", 
             uuid = NA) %>% 
      select(uuid,
             aggregator,
             occurrenceID,
             basisOfRecord, 
             species, 
             verbatimScientificName, 
             geodeticDatum, 
             decimalLatitude, 
             decimalLongitude,
             coordinateUncertaintyInMeters, 
             eventDate, 
             year, 
             month, 
             day, 
             informationWithheld, 
             recordedBy, 
             institutionCode, 
             locality
      )
  }
  # list out the dataframes 
  data_list <- list(symbiota_df, idigbio_df, gbif_df)
  # Filter out anything that contains nothing.
  non_null_data_list <- Filter(Negate(is.null), data_list)
  # Combine all non-null dataframes
  if(length(non_null_data_list) > 0){
    raw_occur_data <- do.call(rbind, non_null_data_list)
  } else { 
    raw_occur_data <- NULL # else return NULL object
  }
}

################################################################################

### Ensure Field Class Standardization #########################################
#' Check occurrence df to ensure that all fields have correct typing 
#' 
#' Takes occurrence data df and conditionally changes class if they are not already correct. Also does some correction on commas included in georefs
#' Returns occurrence data df with correct classes. Also modifies eventDate field to be standardized if applicable
#' 
#' @param occ_data dataframe, occurrence data for a taxon
#' @return occ_data, returns a dataframe with corrected classes
#' examples 
#' # correct_classes(occ_data)
correct_classes <- function(occ_data){
  if(class(occ_data$uuid) != "character"){
    occ_data$uuid <- as.character(occ_data$uuid)
  }
  if(class(occ_data$aggregator) != "character"){
    occ_data$aggregator <- as.character(occ_data$aggregator)
  }
  if(class(occ_data$occurrenceID) != "character"){
    occ_data$occurrenceID <- as.character(occ_data$occurrenceID)
  }
  if(class(occ_data$basisOfRecord) != "character"){
    occ_data$basisOfRecord <- as.character(occ_data$basisOfRecord)
  }
  if(class(occ_data$species) != "character"){
    occ_data$species <- as.character(occ_data$basisOfRecord)
  }
  if(class(occ_data$verbatimScientificName) != "character"){
    occ_data$verbatimScientificName <- as.character(occ_data$verbatimScientificName)
  }
  if(class(occ_data$geodeticDatum) != "character"){
    occ_data$geodeticDatum <- as.character(occ_data$geodeticDatum)
  }
  # require georef fields to be character to remove ','s 
  occ_data <- occ_data %>% 
    dplyr::mutate(decimalLatitude = as.character(decimalLatitude), # due to input errors, we'll first change the class to character so that commas may be removed. 
                  decimalLongitude = as.character(decimalLongitude)) %>% 
    dplyr::mutate(decimalLatitude = gsub(",", ".", decimalLatitude),
                  decimalLongitude = gsub(",", ".", decimalLongitude)) %>% 
    dplyr::mutate(decimalLatitude = as.numeric(decimalLatitude),
                  decimalLongitude = as.numeric(decimalLongitude))
  if(class(occ_data$coordinateUncertaintyInMeters) != "numeric"){
    occ_data$coordinateUncertaintyInMeters <- as.numeric(occ_data$coordinateUncertaintyInMeters)
  }
  if ("character" %in% class(occ_data$eventDate)) { 
    occ_data$eventDate <- as.Date(occ_data$eventDate, format = "%Y-%m-%d")
  } 
  # Conditionally backfills the year, month, day fields if there is nothing in these fields initially, but eventDate has info
  occ_data <- occ_data %>% # note this doesnt solve partials, but too complex for our needs
    mutate( # if there is data present in the day,month,year fields keep it, if not try the event date
      year = ifelse(is.na(year) & !is.na(eventDate), as.numeric(format(as.Date(eventDate), "%Y")), year),
      year = as.numeric(year),
      month = ifelse(is.na(month) & !is.na(eventDate), as.numeric(format(as.Date(eventDate), "%m")), month),
      month = as.numeric(month), 
      day = ifelse(is.na(day) & !is.na(eventDate), as.numeric(format(as.Date(eventDate), "%d")), day), 
      day = as.numeric(day)
    ) 
  if(class(occ_data$informationWithheld) != "character"){
    occ_data$informationWithheld <- as.character(occ_data$informationWithheld)
  }
  if(class(occ_data$recordedBy) != "character"){
    occ_data$recordedBy <- as.character(occ_data$recordedBy)
  }
  if(class(occ_data$institutionCode) != "character"){
    occ_data$institutionCode <- as.character(occ_data$institutionCode)
  }
  if(class(occ_data$locality) != "character"){
    occ_data$locality <- as.character(occ_data$locality)
  }
  return(occ_data)
  
}

################################################################################

### Extract Dates 

### Flag non-Wgs84 Data ########################################################
#' Check occurrences for non WGS84 entries in their geodeticDatums
#' 
#' Takes occurrence data df and creates a boolean field for if their was any evidence of non-WGS84 geodeticDatums 
#' CAUTION!! We assume empty/NA entries are WGS84 (since the vast majority should be)
#' Returns occurrence data with boolean field wgs84Datum
#' 
#' @param occ_data dataframe, occurrence data for a taxon
#' @return occ_data, returns a dataframe with new boolean flag field wgs84Datum added
#' examples 
#' # flag_wgs84(occ_data)
flag_wgs84 <- function(occ_data){
  occ_data <- occ_data %>% 
    mutate(wgs84Datum = ifelse(is.na(geodeticDatum) | 
                                 toupper(geodeticDatum) == "WGS84" | 
                                 toupper(geodeticDatum) == "WORLD GEODETIC SYSTEM 1984" |
                                 geodeticDatum == "" | 
                                 toupper(geodeticDatum) == "NO DISPONIBLE" | 
                                 toupper(geodeticDatum) == "EPSG:4326", TRUE, FALSE))
}
################################################################################

### Flag Coordinate Issues #########################################
#' Check occurrences for empty, zeroed, or unrealstic coordinate values in their georef fields
#' 
#' Takes occurrence data df and creates a boolean field for based on if the georeference fields are valid
#' Returns occurrence data with boolean field coordinateIssue
#' 
#' @param occ_data dataframe, occurrence data for a taxon
#' @return occ_data, returns a dataframe with new boolean flag field coordinateIssue added
#' examples 
#' # flag_coordinateIssue(occ_data)
flag_coordinateIssue <- function(occ_data){
  occ_data <- occ_data %>% 
    mutate(coordinateIssue = ifelse(is.na(decimalLatitude) | 
                                      is.na(decimalLongitude) |
                                      decimalLatitude == 0.00 |
                                      decimalLongitude == 0.00 | 
                                      decimalLatitude > 90 |
                                      decimalLatitude < -90 |
                                      decimalLongitude > 180 | 
                                      decimalLongitude < -180 |
                                      decimalLatitude == "" | 
                                      decimalLongitude == "", TRUE, FALSE)) 
}
################################################################################

### Flag True Coordinates Withheld & Coordinate Uncertainty Inc ################
#' Check occurrences for georefences being purposefully obscured past useablility (>1km) obscural
#' 
#' Takes occurrence data df and creates 2 boolean fields for based on whether informationWithheld field indicates obscural
#' Or increased Uncertainty in Coordinates
#' Returns occurrence data with boolean field trueCoordsWithheld & coordUncertaintyInc
#' 
#' @param occ_data dataframe, occurrence data for a taxon
#' @return occ_data, returns a dataframe with new boolean fields trueCoordsWithheld & coordUncertaintyInc fields added
#' examples 
#' # flag_trueCoordsWithheld(occ_data)
flag_trueCoordsWithheld <- function(occ_data){
  occ_data <- occ_data %>% 
    mutate(informationWithheld = as.character(informationWithheld)) %>% # when there is nothing in this field for all records in the csv it gets converted to a logical output
    mutate(informationWithheld = ifelse(is.na(informationWithheld), "", informationWithheld)) %>% # NAs will cause issues if unaccounted for in the proceeding line of code. 
    mutate(trueCoordsWithheld = ifelse(
      toupper(informationWithheld) == "SPECIFIC LOCALITY INFORMATION MAY BE WITHHELD" |
        toupper(informationWithheld) == "PRECISE LOCATION INFORMATION AVAILABLE ON REQUEST" |
        toupper(informationWithheld) == "LOCATION INFORMATION NOT GIVEN FOR ENDANGERED SPECIES" |
        toupper(informationWithheld) == "SENSITIVE LOCATION DATA WITHHELD" |
        toupper(informationWithheld) == "LA UBICACIÓN NO ES PROVISTA PARA ESPECIES AMENAZADAS" |
        toupper(informationWithheld) == "LOCALITY AND PRECISE COORDINATES WITHHELD TO PROTECT SENSITIVE DATA." |
        toupper(informationWithheld) == "LOCALITY, COLLECTORS, FULL EVENT DATE, AND OTHER FIELDS WITHHELD TO PROTECT SENSITIVE INFORMATION." |
        toupper(informationWithheld) == "LOCATION DATA AVAILABLE TO QUALIFIED RESEARCHERS ON REQUEST." |
        toupper(informationWithheld) == "PRECISE LOCALITY INFORMATION NOT PROVIDED FOR ENDANGERED SPECIES." |
        toupper(informationWithheld) == "LOCALITY DATA HIDDEN DUE TO SENSITITIVE STATUS OF TAXON" |
        toupper(informationWithheld) == "MASK PART ATTRIBUTE LOCATION" |
        toupper(informationWithheld) == "LOCATION INFORMATION NOT GIVEN PRECISELY : SENSITIVE TAXON" |
        toupper(informationWithheld) == "SE OCULTARON COORDENADAS. SE TRATA DE ESPECIE SENSIBLE. SI REQUIERE MÁS INFORMACIÓN CONTÁCTESE CON EL CREADOR DEL RECURSO." |
        toupper(informationWithheld) == "OCCURRENCE IS PLACED IN THE GRID SQUARE CENTROID; REAL COORDINATES, HABITAT DETAILS AND VOUCHER INFORMATION (IF PRESENT) ARE OBSCURED." |
        toupper(informationWithheld) == "ORIGINAL LOCATIONS AVAILABLE UPON REQUEST"  |
        toupper(informationWithheld) == "LOCATION, COORDINATES, INDIVIDUALS, LIFESTAGE, BEHAVIOUR ETC" |
        toupper(informationWithheld) == "BELOW COUNTY-LEVEL LOCALITY DATA" |
        toupper(informationWithheld) == "ASK FOR PRECISE COORDINATES" |
        toupper(informationWithheld) == "OCCURRENCE BLURRED" |
        toupper(informationWithheld) == "LOCALITY AND EXACT COORDINATES NOT GIVEN FOR ENDANGERED SPECIES." |
        toupper(informationWithheld) == "LAS COORDENADAS (VERBATIMCOORDINATES, VERBATIMLATITUDE, VERBATIMLONGITUDE), ALTITUD (VERBATIMELEVATION) Y EXPOSICIÓN (ASPECT) DE LAS PARCELAS DE MONITOREO Y SUS ESPECIES, HAN SIDO RESTRINGIDAS A SOLICITUD DE LOS ADMINISTRADORES DE ESTAS ÁREAS PROTEGIDAS EN FAVOR DE LA CONSERVACIÓN DE LAS MISMAS." |
        toupper(informationWithheld) == "LOCATION INFORMATION NOT GIVEN FOR SPECIES AT RISK"|
        toupper(informationWithheld) == "SUB-COUNTRY 3 AND BELOW SUPPRESSED"|
        grepl("DETAIL LOCATION DATA UNDISCLOSED", toupper(informationWithheld)) |
        grepl("ASK ABOUT DECIMALLATITUDE & DECIMALLONGITUDE", toupper(informationWithheld)) |
        grepl("OCCURRENCE IS PLACED IN THE GRID SQUARE CENTROID", toupper(informationWithheld)) |
        grepl("MASK COORDINATES", toupper(informationWithheld)) |
        grepl("MASK LOCALITY", toupper(informationWithheld)) |
        grepl("SOME LOCALITY DATA IS WITHHELD", toupper(informationWithheld)) |
        grepl("LOCALITY DETAILS WITHHELD", toupper(informationWithheld)) |
        grepl("COORDINATES HIDDEN", toupper(informationWithheld)) |
        grepl("PRECISE LOCATION INFORMATION WITHHELD", toupper(informationWithheld)) |
        grepl("COORDINATES HIDDEN", toupper(informationWithheld)) |
        grepl("LOCALITY NOT GIVEN", toupper(informationWithheld)) |
        grepl("GEOGRAPHIC INFORMATION GENERALIZED DURING AGGREGATION AT THE REQUEST OF THE PRODUCER", toupper(informationWithheld)) |
        grepl("FIELD VALUES REDACTED.*(DECIMALLATITUDE|DECIMALLONGITUDE)", toupper(informationWithheld)),
      TRUE, FALSE)) %>% 
    mutate(coordUncertaintyInc = ifelse(
      grepl("COORDINATE UNCERTAINTY INCREASED", toupper(informationWithheld)),
      TRUE, FALSE))
}
################################################################################

### Identify Aggregator and Specimen Duplicates ########################################################
#' Check occurrences for duplication across aggregators and specimen duplicates
#' 
#' Tests and identifies within aggregate, among aggregate duplicates (a product of multiple data products)
#' Additionally tests and identifies probable specimen duplicates based on identical georefs and temporal sampling
#' Finally, builds a relational ranking among identified duplicates, with low values indicating more relevant data
#' 
#' @param occ_data dataframe, occurrence data for a taxon
#' @return occ_data, returns a dataframe with 7 new fields withinAggDuplicate, amongAggDuplicate, specimenDuplicate, AggDuplicateGroupID, specimenDuplicateGroupID, AggDuplicateRank, specimenDuplicateRank
#' examples 
#' # id_duplicates(occ_data)
id_duplicates <- function(occ_data){
  occ_data <- occ_data %>% 
    dplyr::group_by(uuid, aggregator) %>% # check to see if there are multiple uuids present within an aggregator
    dplyr::mutate(withInAggDuplicate = ifelse(!is.na(uuid) & uuid != "" & n() > 1, TRUE, FALSE)) %>% 
    dplyr::ungroup() %>%
    ## if there are ever more than one aggregator associated with the same occurrenceID, we're going to assume these are duplicates across aggregates
    dplyr::mutate(occID_lower = tolower(occurrenceID)) %>% # try and standardize 
    dplyr::group_by(occID_lower) %>%
    dplyr::mutate(AggDuplicateGroupID = ifelse(n_distinct(aggregator) > 1 & !is.na(occurrenceID), cur_group_id(), NA)) %>% # assign a uniq group id to such records 
    dplyr::mutate(amongAggDuplicate = !is.na(AggDuplicateGroupID)) %>% # if a uniq group id exists for this field, its a duplicate so return TRUE
    ## identify specimenDuplicates via looking whether there are specimens that share occurrenceIDs, all temporal info, and are not aggregate duplicates
    dplyr::mutate(specimenDuplicateGroupID = ifelse(n_distinct(year) == 1 &
                                                      n_distinct(month) == 1 &
                                                      n_distinct(day) == 1 &
                                                      n() > 1 &
                                                      !is.na(occurrenceID) &
                                                      amongAggDuplicate == FALSE, 
                                                    cur_group_id(), NA)) %>% 
    dplyr::ungroup() %>% 
    ## identify specimen duplicates via looking whether there are specimens without occurrenceIDs that contain the samespatial coords or date collection
    dplyr::group_by(decimalLatitude, decimalLongitude, day, month, year) %>% 
    dplyr::mutate(specimenDuplicateGroupID = ifelse(is.na(occurrenceID) &
                                                      (sum(!is.na(decimalLatitude)) > 0 & n_distinct(decimalLatitude) == 1) & 
                                                      (sum(!is.na(decimalLongitude)) > 0 & n_distinct(decimalLongitude) == 1) & 
                                                      (sum(!is.na(day)) > 0 & n_distinct(day) == 1) & 
                                                      (sum(!is.na(month)) > 0 & n_distinct(month) == 1) & 
                                                      (sum(!is.na(year)) > 0 & n_distinct(year) == 1) & 
                                                      n() > 1, 
                                                    cur_group_id(), specimenDuplicateGroupID)) %>% 
    dplyr::ungroup() %>% 
    mutate(specimenDuplicate = ifelse(!is.na(specimenDuplicateGroupID), TRUE, FALSE))
  
  ## Next, assign rank to filter based on data completeness
  occ_data <- occ_data %>%
    group_by(AggDuplicateGroupID) %>%
    mutate(
      completeness_score = rowSums(
        cbind(
          !is.na(geodeticDatum) & geodeticDatum != "",
          !is.na(decimalLatitude) & decimalLatitude != "",
          !is.na(decimalLongitude) & decimalLongitude != "",
          !is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters != "",
          !is.na(year) & year != "",
          !is.na(month) & month != "",
          !is.na(day) & day != ""
        )
      ),
      AggDuplicateRank = ifelse(amongAggDuplicate == TRUE, dense_rank(desc(completeness_score)), NA) # rank with 1 being highest priority
    ) %>%
    ungroup() %>% 
    # do the same for specimen duplicates
    group_by(specimenDuplicateGroupID) %>%
    mutate(
      completeness_score = rowSums(
        cbind(
          !is.na(geodeticDatum) & geodeticDatum != "",
          !is.na(decimalLatitude) & decimalLatitude != "",
          !is.na(decimalLongitude) & decimalLongitude != "",
          !is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters != "",
          !is.na(year) & year != "",
          !is.na(month) & month != "",
          !is.na(day) & day != ""
        )
      ),
      specimenDuplicateRank = ifelse(specimenDuplicate == TRUE, dense_rank(desc(completeness_score)), NA) # rank with 1 being highest priority
    ) %>%
    ungroup() 
  # Select to clean up df
  occ_data <- occ_data %>% 
    select(-occID_lower, -completeness_score)
}
################################################################################

### Delimit Range Status ################
#' Categorize occurrences by native, introduced, or undocumented range status according to WCVP known distributions
#' 
#' Takes an occurrence df and creates a new field called wcvpRangeStatus, which tests each point (that is georef) 
#' for native, introduced, or undocumented location status. Note that we'll be applying a buffer
#' to each of the known regions and record in hierarchy of native > introduced > undocumented
#' Returns occurrence data with field wcvpRangeStatus with possible contents being: native, introduced, or undocumented. 
#' if NA, there are no usable georefs for these pts
#' 
#' @param occ_data dataframe, occurrence data for a taxon
#' @return occ_data, returns a dataframe with new wcvpRangeStatus field added
#' examples 
#' # delimit_pts_wcvp_range_status(occ_data)
delimit_pts_wcvp_range_statuses <- function(occ_data, plant_alignment, proj_crs, visual = TRUE, saveMaps = TRUE, full_map_file_path = NULL, native_map_file_path = NULL){
  ## bring in wcvp distribution, name status tables
  wcvp_distribution_tb <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/raw/wcvp_distribution_2025_update.csv") # 2025 because distributions from 2024 are not great!
  wcvp_name_tb <- fread("/blue/guralnick/millerjared/PlantSweepeR/data/raw/wcvp_names_2025_update.csv") # name alignment 
  ## shapefiles of the level3 regions used by wcvp
  level3_regions <- sf::read_sf("/blue/guralnick/millerjared/BoCP/data/raw/level3-wgsrpd/level3.shp")
  ## turn off spherical geom mapping
  sf_use_s2(FALSE)
  ## grab the accepted name for these occs
  accepted_name <- unique(occ_data$species)
  ## filter down alignment to capture all the alignedNames under our alignedParentName.
  plant_alignment <- plant_alignment %>% 
    filter(alignedParentName == accepted_name)
  ## select for relevant info & filter to the relevant name info for this extraction
  wcvp_names <- wcvp_name_tb %>% 
    select(plant_name_id, taxon_name, taxon_authors, taxon_status) %>% 
    filter(taxon_name %in% plant_alignment$alignedName) # grab all possible 
  ## merge to grab geographic info according to plant_name_id
  wcvp_names_w_dist <- merge(wcvp_names, wcvp_distribution_tb, by = "plant_name_id")
  ## select for relevant info
  wcvp_names_w_dist <- select(wcvp_names_w_dist, plant_name_id, taxon_name, continent, 
                              region_code_l2, region, area_code_l3,
                              area, introduced, extinct, location_doubtful)
  ## determine parentTaxon using the name alignment's criteria 
  wcvp_parentTaxon_w_dist <- wcvp_names_w_dist %>% 
    left_join(plant_alignment, by = join_by(taxon_name == alignedName)) %>% 
    rename(parentTaxon = alignedParentName) %>% 
    select(names(wcvp_names_w_dist), parentTaxon) %>% 
    distinct()
  # Delimit occ pts to regions 
  ## create a temporary id for merge during final steps
  occ_data <- occ_data %>% 
    mutate(tempID = 1:n()) # use for end merge.
  ## remove records that do not have the attributes needed for looking at them spatially
  occ_spatial_data <- occ_data %>% 
    filter(wgs84Datum == TRUE) %>%  # necessary for projecting correctly
    filter(coordinateIssue == FALSE) %>%  # needs to be clean and correct coords
    filter(trueCoordsWithheld == FALSE) # only want true clean coords 
  ## make the data spatial 
  occ_data_sf <- sf::st_as_sf(occ_spatial_data, coords = c("roundedLongitude", "roundedLatitude"), crs = 4326, remove = FALSE)
  ## project coordinates into desired projected coordinate system 
  occ_data_reproj <- st_transform(occ_data_sf, crs = proj_crs)
  ## buffer points to account for some wiggle room in regards to the ranges we're categorizing by
  occ_data_b <- st_buffer(occ_data_reproj, dist = 10000) # allow 10km of wiggle room
  ## break anti-meridian for botanical region shapes, then reproj in desired projected coordinate system
  lon_0_val <- stringr::str_match(proj_crs, "\\+lon_0=([-]?[0-9.]+)")[,2] 
  level3_regions <- sf::st_break_antimeridian(level3_regions, lon_0 = as.numeric(lon_0_val))
  level3_regions_reproj <- sf::st_transform(level3_regions, crs = proj_crs)
  ## Summarize the possible botanical region codes for a parentTaxon (the alignedName)
  statuses_for_name <- wcvp_parentTaxon_w_dist %>% 
    distinct(across(-c(plant_name_id, taxon_name)), .keep_all = TRUE) %>% # Take distinct values for parentTaxon rather than the taxon
    group_by(parentTaxon, area_code_l3) %>% # group up values of the parentTaxon and the location
    summarize(across(everything(), ~ first(.)), # Priortize natives vs introduced values per grouping  
              introduced = min(introduced)) %>% # take the 0 value instead of 1 if it exists
    ungroup()
  ## join occ data with these regions. 
  occ_data_regioned <- st_join(occ_data_b, level3_regions_reproj, join = st_intersects)
  occ_data_regioned <- st_drop_geometry(occ_data_regioned)
  ## and join statuses with occur data, note that this deals with the duplications of records created by the buffering
  occ_data_statuses <- occ_data_regioned %>% 
    rename(area_code_l3 = LEVEL3_COD) %>% 
    left_join(statuses_for_name, by = "area_code_l3") %>% 
    mutate(introduced = ifelse(!is.na(introduced), introduced, 2)) %>%  # Note that novel occurrences will be noted as 2. 
    group_by(tempID) %>% # locate duplicates where present
    dplyr::summarize(across(everything(), ~ first(.)), # find the lowest value to take in priority 
              introduced = min(introduced)) %>% # we're interested if it becomes native, so take the min (0) being native, (1) being introduced, (2) being novel    ungroup() %>% 
    mutate(wcvpRangeStatus = case_when(
      introduced == 0 ~ "native",
      introduced == 1 ~ "introduced", 
      introduced == 2 ~ "undocumented"
    ))
  
  if(visual == TRUE){
    ## create fills for native, introduced, and undocumented ranges
    true_native_regions <- statuses_for_name %>% 
      filter(introduced == 0) # we're looking at all ranges for taxon
    true_introduced_regions <- statuses_for_name %>% 
      filter(introduced == 1)
    region_fills <- level3_regions_reproj %>% # create fills accordingly 
      mutate(regionStatus = case_when(
        LEVEL3_COD %in% true_native_regions$area_code_l3 ~ "native range", 
        LEVEL3_COD %in% true_introduced_regions$area_code_l3 ~ "introduced range",
        TRUE ~ "undocumented"
      ))
    ## create a plotting view, allowing for some conditional buffering based on the point occurrences extent
    occ_data_statuses_sf <- st_as_sf(occ_data_statuses, coords = c("roundedLongitude", "roundedLatitude"), crs = 4326, remove = FALSE)
    occ_data_statuses_sf <- st_transform(occ_data_statuses_sf, crs = st_crs(region_fills))
    occ_data_bbox <- st_bbox(occ_data_statuses_sf) ## determine the zoom ratio for mapping purposes
    region_fills_bbox <- st_bbox(region_fills) ### calculate the current bounding box and region fills bounding box
    occ_native_data_bbox <- st_bbox(filter(occ_data_statuses_sf, wcvpRangeStatus == "native"))
    # conditional to adjust for plotting complexity...
    occ_data_sf_d <- distinct(occ_data_sf, decimalLongitude, decimalLatitude, .keep_all = TRUE)
    if(nrow(occ_data_sf_d) > 1){
      ### calculate the actual area of region_fills and the intersection with occur_data_bbox
      area_region_fills <- st_area(st_union(region_fills))
      area_intersection <- st_area(st_intersection(st_union(region_fills), st_as_sfc(occ_data_bbox)))
      
      ### convert areas to numeric for proportion calculation
      area_region_fills_numeric <- as.numeric(area_region_fills)
      area_intersection_numeric <- as.numeric(area_intersection)
      
      ### proportion of region_fills shown in the current occurrence data bounding box
      proportion_shown <- area_intersection_numeric / area_region_fills_numeric
      
      hull <- st_convex_hull(occ_data_statuses_sf)
      hull_bbox <- st_bbox(hull)
      ### check if the proportion is less than 20%
      if (proportion_shown < 0.20) {
        
        hull <- st_convex_hull(occ_data_statuses_sf)
        hull_b <- st_buffer(hull, 10000)
        hull_bbox <- st_bbox(hull_b)
      }
    } else{
      # else make it the region the point sits in
      regions <- unique(occ_data_statuses_sf$LEVEL3_NAM)
      hull_bbox <- st_bbox(filter(level3_regions, LEVEL3_NAM %in% regions))
    }
    
    ## and map
    color_mapping <- c("native range" = "darkgreen", "introduced range" = "goldenrod", "undocumented" = "grey")
    full_map <- ggplot() + 
      geom_sf(region_fills, mapping = aes(fill = regionStatus)) +
      scale_fill_manual(values = color_mapping) +
      geom_sf(occ_data_statuses_sf, mapping = aes(), fill = "darkorange", alpha = 0.9) + 
      coord_sf(xlim = c(hull_bbox[1], hull_bbox[3]), ylim = c(hull_bbox[2], hull_bbox[4])) +
      theme_bw() +
      ggtitle(paste("Occurrence Records for", accepted_name)) + 
      xlab("Longitude") + 
      ylab("Latitude") +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(fill=guide_legend(title="Region Status"))
    
    # Native range adjusted map
    # Extract bounding box
    xmin <- occ_native_data_bbox["xmin"]
    xmax <- occ_native_data_bbox["xmax"]
    ymin <- occ_native_data_bbox["ymin"]
    ymax <- occ_native_data_bbox["ymax"]
    
    # Apply buffer based on latitude values
    lat_buffer <- ifelse(abs(ymax) - abs(ymin) > 10, 0.5, 3)
    lon_buffer <- ifelse(abs(xmin) - abs(xmax) > 10, 0.5, 3)
    
    # Expand bounding box
    expanded_bbox <- c(
      xmin - lon_buffer,  # Longitude buffer (constant)
      ymin - lat_buffer,  # Latitude buffer (conditional)
      xmax + lon_buffer,  
      ymax + lat_buffer
    )
    
    # Plot with adjusted bounding box
    native_map <- ggplot() +
      geom_sf(region_fills, mapping = aes(fill = regionStatus)) +
      scale_fill_manual(values = color_mapping) +
      geom_sf(occ_data_statuses_sf, mapping = aes(), fill = "darkorange", alpha = 0.9) + 
      coord_sf(xlim = c(expanded_bbox[1], expanded_bbox[3]), 
               ylim = c(expanded_bbox[2], expanded_bbox[4])) +
      theme_bw() +
      ggtitle(paste("Adjusted Window Occurrence Records for", accepted_name)) + 
      xlab("Longitude") + 
      ylab("Latitude") +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(fill=guide_legend(title="Region Status"))
    
    if(saveMaps == TRUE){
      ggsave(plot = full_map, filename = paste0(full_map_file_path, gsub(" ", "_", accepted_name), ".png"), height = 10, width = 10)
      ggsave(plot = native_map, filename = paste0(native_map_file_path, gsub(" ", "_", accepted_name), ".png"), height = 10, width = 10)
    }
    
    #print(full_map)
    #print(native_map)
    
  }
  ## create new field in occ_data that adds these statuses
  occ_data_statuses <- occ_data_statuses %>% 
    #st_drop_geometry() %>% # remove the geom col before writing out these data
    select(tempID, wcvpRangeStatus, LEVEL3_NAM, area_code_l3)
  
  occ_data <- occ_data %>% 
    left_join(occ_data_statuses, by = "tempID") %>% 
    distinct() %>% 
    select(-tempID)
}
################################################################################

### Flag Coords ################################################################
#' Uses the package coordinate cleaner to identify and flag occurrences with error, or fall within institutions. 
#' Additionally tests the locality field for botanical gardens/green houses
#' 
#' 
#' 
#' 
#' 
#' 
#' @param occ_data dataframe, occurrence data for a taxon
#' @param max_dist_to_consider_outliers numeric, distance in km that if buffered boundary does not overlap with at least one other point define it as a outlier
#' @return occ_data, returns a dataframe with new wcvpRangeStatus field added
#' examples 
#' # flag_coordinateCleaning(occ_data)
flag_coordinateCleaning <- function(occ_data, max_dist_to_consider_outliers = 250){
  ##  remove data without coords
  occ_data_w_coords <- occ_data %>% 
    filter(!is.na(decimalLatitude)) %>% 
    filter(!is.na(decimalLongitude)) %>%  # both must be clean
    filter(coordinateIssue == FALSE) %>% 
    filter(wgs84Datum == TRUE) # coordinate cleaner requires WGS84 records for its checks.
  ## retain these for later
  occ_data_wout_coords <- occ_data %>% 
    filter(is.na(decimalLatitude) | is.na(decimalLongitude) | wgs84Datum == FALSE | coordinateIssue == TRUE) 
  ## if theres more than 0 data with coords, run coordinate cleaner 
  if(nrow(occ_data_w_coords) > 0){
    occ_data_w_coords <- CoordinateCleaner::clean_coordinates(occ_data_w_coords, 
                                                              lon = "decimalLongitude", 
                                                              lat = "decimalLatitude", 
                                                              species = "species", 
                                                              tests = c("capitals", "centroids", "equal", "gbif", "institutions", "seas",
                                                                        "zeros"))
    ## rename for my sanity
    occ_data_w_coords <- occ_data_w_coords %>% 
      rename(validRecord = `.val`,
             equalLatLon = `.equ`,
             zeroCoords = `.zer`,
             capitalCoord = `.cap`,
             centroidCoord = `.cen`,
             inOceanCoord = `.sea`,
             inGBIFHeadquarters = `.gbf`, 
             inInstitutionBounds = `.inst`) %>% 
      select(-`.summary`) %>% 
      # swap way the flags are presented for simplicity 
      mutate(equalLatLon = !equalLatLon, 
             zeroCoords = !zeroCoords, 
             capitalCoord = !capitalCoord,
             centroidCoord = !centroidCoord,
             inOceanCoord = !inOceanCoord, 
             inGBIFHeadquarters = !inGBIFHeadquarters, 
             inInstitutionBounds = !inInstitutionBounds)
    
    ## additional locality string screen to check for botanical gardens & for landscaped occurrences
    botanical_garden_strings <- c("Greenhouse", "greenhouse", "invernadero", "Invernadero", "Botanical Garden", "Botanical garden", 
                                  "botanical garden", "botanical Garden", "Jardín Botánico", "Jardín botánico", "jardín botánico", 
                                  "jardín Botánico", "Jardin Botanico", "Jardin botanico", "jardin botanico", "jardin Botanico", 
                                  "Arboretum", "arboretum", "Arboreto", "arboreto")
    possible_landscaped <- c("Introduced", "introduced", "Planted", 
                             "planted", "Landscaped", "landscaped", "Landscaping", "landscaping", "Plantings", "plantings", 
                             "adventive", "Adventive", "escaped", "Escaped", "non-native", "Non-native")
    
    occ_data_w_coords <- occ_data_w_coords %>%
      mutate(inInstitutionBounds = inInstitutionBounds | 
               (locality %>% grepl(pattern = paste(botanical_garden_strings, collapse="|"), ., ignore.case=FALSE)))
    occ_data_w_coords <- occ_data_w_coords %>% 
      mutate(landscaped = (locality %>% grepl(pattern = paste(possible_landscaped, collapse="|"), ., ignore.case=FALSE)))
    
    ## perform outlier detection 
    # convert to data.frame to avoid unknown issues
    occ_data_w_coords <- as.data.frame(occ_data_w_coords)
    dist_flag <- CoordinateCleaner::cc_outl(occ_data_w_coords, lon = "decimalLongitude", lat = "decimalLatitude", species = "species",
                                            method = "distance", mltpl = 5, tdi = max_dist_to_consider_outliers, value = "flagged", sampling_thresh = 0, 
                                            verbose = TRUE, min_occs = 7)
    # Invert flag values
    dist_flag <- !dist_flag
    
    # append to data 
    occ_data_w_coords$distOutlier = dist_flag
  } else {
    ## otherwise fill in NAs
    occ_data_w_coords <- occ_data_w_coords %>% 
      mutate(validRecord = NA,
             equalLatLon = NA, 
             zeroCoords = NA, 
             capitalCoord = NA,
             centroidCoord = NA,
             inOceanCoord = NA, 
             inGBIFHeadquarters = NA, 
             inInstitutionBounds = NA,
             landscaped = NA,
             distOutlier = NA)
  }
  # prep binding the dataset back together via NA values
  occ_data_wout_coords <- occ_data_wout_coords %>% 
    mutate(validRecord = NA,
           equalLatLon = NA, 
           zeroCoords = NA, 
           capitalCoord = NA,
           centroidCoord = NA,
           inOceanCoord = NA, 
           inGBIFHeadquarters = NA, 
           inInstitutionBounds = NA,
           landscaped = NA,
           distOutlier = NA)
  
  ## bring occurrences with and without occurrences back together
  occ_data <- rbind(occ_data_w_coords, occ_data_wout_coords)
  
  
}
################################################################################
#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## ------ R SCRIPT TO IDENTIFY MISSING SUBDISTRICTS BASED ON MATCHING TECHNIQUES ------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Aug 2021)

                                          # francesco.checchi@lshtm.ac.uk 



#.........................................................................................
### Preparatory steps
#.........................................................................................

  #...................................
  ## Establish how many and which subdistricts are missing

    # Label missing subdistricts
    dtm[, "missing_subdis"] <- ifelse(is.na(dtm[, "subdis_en_ocha"]), "yes", "no")
    dtm[, "missing_subdis_ori"] <- ifelse(is.na(dtm[, "subdis_ori_en_ocha"]), "yes", "no")

    # Extent of missingness
    table(dtm[, "missing_subdis"])
    prop.table(table(dtm[, "missing_subdis"]))
    table(dtm[, "missing_subdis_ori"])
    prop.table(table(dtm[, "missing_subdis_ori"]))
    
    # Describe pattern of missingness
    dtm[, "year_obs"] <- ifelse(dtm[, "type"] == "prevalent", dtm[, "year_assessment"], dtm[, "year_move"])
    prop.table(table(dtm[, c("missing_subdis", "year_obs")]), margin = 2)
    prop.table(table(dtm[, c("missing_subdis_ori", "year_obs")]), margin = 2)    
    prop.table(table(dtm[, c("missing_subdis", "type")]), margin = 2)
    prop.table(table(dtm[, c("missing_subdis_ori", "type")]), margin = 2)    

        
  #...................................
  ## Establish options for finding missing subdistricts
  
    # Districts with a single subdistrict
    x1 <- table(ocha[, "dis_en_ocha"])
    x1 <- names(x1)[which(x1 == 1)]
    x2 <- which(dtm[, "missing_subdis"] == "yes" & dtm[, "dis_en_ocha"] %in% x1)
    length(x2)
    x2 <- which(dtm[, "missing_subdis_ori"] == "yes" & dtm[, "dis_ori_en_ocha"] %in% x1)
    length(x2)
      # could resolve up to 19,967 subdistricts of arrival and 65,627 subdistricts of origin
    
    # Missing records with coordinates of site of arrival
    dtm[, "coordinates"] <- ifelse(! is.na(dtm[, "latitude"]) & ! is.na(dtm[, "longitude"]), TRUE, FALSE)    
    table(dtm[, c("coordinates", "missing_subdis")])
      # could resolve up to 2976 missing subdistricts of arrival

    # Using locality names in English or Arabic by matching these across DTM or to CSO (only subdistricts of arrival)
      # locality names available where subdistrict is missing
      dtm[, "locality_option"] <- ifelse(is.na(dtm[, "locality_en"]) & is.na(dtm[, "locality_ar"]), FALSE, TRUE)
    table(dtm[, c("locality_option", "missing_subdis")])
      # could resolve up to 52,105 missing subdistricts of arrival
    
    # Clean up
    dtm <- dtm[, ! colnames(dtm) %in% c("year_obs", "coordinates", "locality_option")]


  #...................................      
  ## Label DTM data as analysis-eligible
    
    # Define analysis eligibility
    dtm[, "eligible"] <- NA

      # no - returnee data
      dtm[, "eligible"] <- ifelse(dtm[, "who"] == "ret", "no - returnees", dtm[, "eligible"])
        
      # no - missing number of IDP households
      dtm[, "eligible"] <- ifelse(is.na(dtm[, "eligible"]) & is.na(dtm[, "n_hh"]), 
        "no - missing IDP number", dtm[, "eligible"])

      # no - missing date of move
      dtm[, "eligible"] <- ifelse(is.na(dtm[, "eligible"]) & is.na(dtm[, "year_move"]), 
        "no - missing year or month of move", dtm[, "eligible"])
      dtm[, "eligible"] <- ifelse(is.na(dtm[, "eligible"]) & is.na(dtm[, "month_move"]), 
        "no - missing year or month of move", dtm[, "eligible"])

      # no - missing district of arrival or origin
      dtm[, "eligible"] <- ifelse(is.na(dtm[, "eligible"]) & is.na(dtm[, "dis_en_ocha"]), 
        "no - missing district of arrival or origin", dtm[, "eligible"])
      dtm[, "eligible"] <- ifelse(is.na(dtm[, "eligible"]) & is.na(dtm[, "dis_ori_en_ocha"]), 
        "no - missing district of arrival or origin", dtm[, "eligible"])
      
      # otherwise eligible
      dtm[, "eligible"] <- ifelse(is.na(dtm[, "eligible"]), "yes", dtm[, "eligible"])

      # tabulate
      table(dtm[, "eligible"])
      prop.table(table(dtm[, "eligible"]))

    # Add time units
    colnames(dtm) <- gsub("_move", "", colnames(dtm))    
    dtm <- merge(dtm, t_units, by = c("month", "year"), all.x = TRUE)
      
    # Subdistrict missingness at this stage       
    table(dtm[, c("eligible", "missing_subdis")])
    prop.table(table(dtm[, c("eligible", "missing_subdis")]), margin = 1) * 100
    table(subset(dtm, eligible == "yes")[, c("year_assessment", "missing_subdis")])
    prop.table(table(subset(dtm, eligible == "yes")[, c("year_assessment", "missing_subdis")]), margin = 1) * 100
    table(subset(dtm, eligible == "yes")[, c("year", "missing_subdis")])
    prop.table(table(subset(dtm, eligible == "yes")[, c("year", "missing_subdis")]), margin = 1) * 100
    
    table(dtm[, c("eligible", "missing_subdis_ori")])
    prop.table(table(dtm[, c("eligible", "missing_subdis_ori")]), margin = 1) * 100
    table(subset(dtm, eligible == "yes")[, c("year_assessment", "missing_subdis_ori")])
    prop.table(table(subset(dtm, eligible == "yes")[, c("year_assessment", "missing_subdis_ori")]), margin = 1) * 100
    table(subset(dtm, eligible == "yes")[, c("year", "missing_subdis_ori")])
    prop.table(table(subset(dtm, eligible == "yes")[, c("year", "missing_subdis_ori")]), margin = 1) * 100
    
    prop.table(table(subset(dtm, eligible == "yes")[, c("type", "missing_subdis")]), margin = 1) * 100
    prop.table(table(subset(dtm, eligible == "yes")[, c("type", "missing_subdis_ori")]), margin = 1) * 100
    
    
    
#.........................................................................................                            
### Finding missing subdistricts: Method 1 (based on districts with a single subdistrict)
#.........................................................................................    
  
  #...................................
  ## Identify OCHA gazetteer districts with a single subdistrict
    
    # Rename OCHA variables again
    colnames(ocha) <- gsub("pcode", "pcode_ocha", colnames(ocha))
    
    # Singleton districts...
    x1 <- table(ocha[, "dis_en_ocha"])
    x1 <- names(x1)[which(x1 == 1)]

    # ...and their corresponding subdistricts
    x2 <- subset(ocha, dis_en_ocha %in% x1)[, c("dis_en_ocha", "subdis_en_ocha", "subdis_pcode_ocha")]
    colnames(x2) <- c("dis_en_ocha", "subdis_en_ocha_single", "subdis_pcode_ocha_single")

  #...................................
  ## Fill in missing subdistricts based on singleton districts, and update missingness
    
    # Missing subdistricts of arrival
    dtm <- merge(dtm, x2, by = "dis_en_ocha", all.x = TRUE)  
    dtm[, "subdis_en_ocha"] <- ifelse(is.na(dtm[, "subdis_en_ocha_single"]), dtm[, "subdis_en_ocha"], dtm[, "subdis_en_ocha_single"])
    dtm[, "subdis_pcode_ocha"] <- ifelse(is.na(dtm[, "subdis_pcode_ocha_single"]), dtm[, "subdis_pcode_ocha"], dtm[, "subdis_pcode_ocha_single"])
    
    # Missing subdistricts of origin
    colnames(x2) <- c("dis_ori_en_ocha", "subdis_ori_en_ocha_single", "subdis_ori_pcode_ocha_single")
    dtm <- merge(dtm, x2, by = "dis_ori_en_ocha", all.x = TRUE)  
    dtm[, "subdis_ori_en_ocha"] <- ifelse(is.na(dtm[, "subdis_ori_en_ocha_single"]), dtm[, "subdis_ori_en_ocha"], 
      dtm[, "subdis_ori_en_ocha_single"])
    dtm[, "subdis_ori_pcode_ocha"] <- ifelse(is.na(dtm[, "subdis_ori_pcode_ocha_single"]), dtm[, "subdis_ori_pcode_ocha"], 
      dtm[, "subdis_ori_pcode_ocha_single"])

    # Update  missingness
    dtm[, "missing_subdis"] <- ifelse(is.na(dtm[, "subdis_en_ocha"]), "yes", "no")
    dtm[, "missing_subdis_ori"] <- ifelse(is.na(dtm[, "subdis_ori_en_ocha"]), "yes", "no")
    table(dtm[, c("eligible", "missing_subdis")])
    prop.table(table(dtm[, c("eligible", "missing_subdis")]), margin = 1) * 100
    table(dtm[, c("eligible", "missing_subdis_ori")])
    prop.table(table(dtm[, c("eligible", "missing_subdis_ori")]), margin = 1) * 100
    
    # Remove extra variables
    dtm <- dtm[, - grep("_single", colnames(dtm))]
    
    
#.........................................................................................                            
### Finding missing subdistricts: Method 2 (based on coordinates in DTM dataset)
#.........................................................................................    

  #...................................
  ## Verify equivalence between OCHA gazetteer and OCHA shape file
  x1 <- ocha
  x2 <- ocha_shape[, c("gov_en_ocha_shape", "dis_en_ocha_shape", "subdis_en_ocha_shape")]
  colnames(x2) <- sapply(colnames(x2), function(x) {gsub("_shape", "", x)})  
  x3 <- merge(x1, x2, by = c("gov_en_ocha", "dis_en_ocha", "subdis_en_ocha"))
  nrow(x3)
  View(x3)
    # all OK
  rm(x1, x2, x3)
      
  #...................................
  ## Figure out OCHA subdistrict from DTM coordinates
    # from https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package 
    # also see https://spatialreference.org/ref/epsg/?search=Yemen&srtext=Search for chosen spatial projection
    
    # Prepare OCHA boundary shape file
      # apply planar transformation
      ocha_trans <- st_transform(ocha_shape, 4981)
      
    # Prepare DTM coordinate points
      # prepare a points collection
      pnts <- dtm[which(! is.na(dtm[, "latitude"]) & ! is.na(dtm[, "longitude"]) & dtm[, "missing_subdis"] == "yes"), 
        c("longitude", "latitude")]
      colnames(pnts) <- c("x", "y")
      pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
        function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 

      # apply planar transformation
      pnts_trans <- st_transform(pnts_sf, 4981)

    # Find intersection of points and polygons, and extract OCHA subdistrict pcode
    pnts$subdis_pcode_ocha <- apply(st_intersects(ocha_trans, pnts_trans, sparse = FALSE), 2, 
      function(col) { ocha_trans[which(col), ]$subdis_pcode_ocha_shape})
        
    # Merge with main dataset
      # manage points dataset for merging
      x1 <- tidyr::unnest(pnts, cols = c(subdis_pcode_ocha))
      x1 <- as.data.frame(x1)
      colnames(x1) <- c("longitude", "latitude", "subdis_pcode_ocha")
      x1 <- unique(x1)
      x1[, "subdis_pcode_ocha"] <- sapply(x1[, "subdis_pcode_ocha"], function(x) {gsub("YE", "", x)})
    
      # add other OCHA variables
      x1 <- merge(x1, ocha[, c("gov_en_ocha", "gov_pcode_ocha", "dis_en_ocha", "dis_pcode_ocha", "subdis_en_ocha", "subdis_pcode_ocha")], 
        by = "subdis_pcode_ocha", all.x = TRUE)

      # merge into DTM
      x2 <- colnames(x1) %in% c("gov_en_ocha", "gov_pcode_ocha", "dis_en_ocha", "dis_pcode_ocha", "subdis_en_ocha", "subdis_pcode_ocha")
      colnames(x1)[x2] <- paste(colnames(x1)[x2], "_coord", sep = "")
      dtm <- merge(dtm, x1, by = c("longitude", "latitude"), all.x = TRUE )
      for (i in colnames(x1)[grep("coord", colnames(x1))]) {
        dtm[, gsub("_coord", "", i)] <- ifelse(is.na(dtm[, gsub("_coord", "", i)]), dtm[, i], dtm[, gsub("_coord", "", i)])
      }
      
    # Update  missingness
    dtm[, "missing_subdis"] <- ifelse(is.na(dtm[, "subdis_en_ocha"]), "yes", "no")
    table(dtm[, c("eligible", "missing_subdis")])
    prop.table(table(dtm[, c("eligible", "missing_subdis")]), margin = 1) * 100

    # Remove extra variables
    dtm <- dtm[, - grep("_coord", colnames(dtm))]
    rm(pnts, pnts_sf, pnts_trans, ocha_trans) 
              
#.........................................................................................                            
### Finding missing subdistricts: Method 3 (based on matching localities within DTM dataset)
#.........................................................................................    
    
  #...................................      
  ## Create unique combinations of (non-missing) districts, subdistricts and localities
    
    # Arabic locality names to ASCII
    dtm[, "locality_ascii"] <- stri_trans_general(dtm[, "locality_ar"], "Arab-Latin")        
    dtm[, "locality_ascii"] <- stri_trans_general(tolower(dtm[, "locality_ascii"]), "Latin-ASCII")
    
    # Where Arabic locality names are missing, supplement with transliterated locality names
    dtm[, "locality_ascii"] <- ifelse(is.na(dtm[, "locality_ascii"]), stri_trans_general(tolower(dtm[, "locality_en"]), 
      "Latin-ASCII"), dtm[, "locality_ascii"])
    
    # Exclude special symbols, vowels and zero-length strings
    for (i in c("ʿ", "ʾ") ) {dtm[, "locality_ascii"] <- gsub(i, "'", dtm[, "locality_ascii"]) }
    for (i in c("a", "e", "i", "o", "u", "-") ) {dtm[, "locality_ascii"] <- gsub(i, "", dtm[, "locality_ascii"]) }
    dtm[, "locality_ascii"] <- trimws(dtm[, "locality_ascii"])
    dtm[, "locality_ascii"] <- ifelse(dtm[, "locality_ascii"] == "", NA, dtm[, "locality_ascii"])
    
    # Unique combinations
    equi_loc <- unique(dtm[, c("dis_en_ocha", "subdis_en_ocha", "locality_ascii")])
      # all must be non-missing
      equi_loc <- equi_loc[complete.cases(equi_loc), ]    

      # there cannot be any duplicates (same locality names within any given district)
      nrow(equi_loc)
      x1 <- na.omit(equi_loc[, c("dis_en_ocha", "locality_ascii")])
      x1[, "count"] <-1
      x1 <- aggregate(x1[, "count"], by = x1[, c("dis_en_ocha", "locality_ascii")], FUN = sum)
      table(x1[, "x"])
      equi_loc <- merge(equi_loc, x1, by = c("dis_en_ocha", "locality_ascii"), all.x = TRUE)    
      equi_loc <- subset(equi_loc, x == 1)
      nrow(equi_loc)
      
      
  #...................................      
  ## Perform approximate string matching
    
    # Do fuzzy matching by district
    dtm[, "locality_matched"] <- NA
    x1 <- na.omit(unique(dtm[, "dis_en_ocha"]))
      
    for (i in 1:length(x1) ) {

      # track progress
      loop.tracker(i, length(x1) ) 
      
      # select only DTM observations for this district that have a missing subdistrict but non-missing locality 
      x2 <- which(dtm[, "dis_en_ocha"] == x1[i] & is.na(dtm[, "subdis_en_ocha"]) & ! is.na(dtm[, "locality_ascii"]) )
      
      # if there are any observations in the district that can possibly be matched through this method...
      if (length(x2) > 0) {
        x3 <- dtm[x2, "locality_ascii"]
        
        # select only match candidates for this district
        x4 <- equi_loc[which(equi_loc[, "dis_en_ocha"] == x1[i]), "locality_ascii"]
  
        # fuzzy matching
        dtm[x2, "locality_matched"]  <- f_fuzzy(x3, x4, methods)
      }
    }
  
    # Convert "NO MATCH" values to NA
    dtm[, "locality_matched"]  <- ifelse(dtm[, "locality_matched"] == "NO MATCH", NA, dtm[, "locality_matched"] )

  #...................................      
  ## Calculate string distance and apply maximum tolerated distance to identify matches
    
    # Calculate distance
    x1 <- which(! is.na(dtm[, "locality_matched"]) )
    dtm[x1, "distance"] <-  apply(dtm[x1, c("locality_ascii", "locality_matched")], 1, function(x) {stringdist(x[1], x[2])})
    table(dtm[x1, "distance"])
      # review a selection of matches near the distance threshold
      x2 <- sample(which(dtm[, "distance"] %in% (distance_max - 1):(distance_max + 1) ), 100) 
      View(dtm[x2, c("locality_ascii", "locality_matched", "distance")])
        # based on this review, distance_max should be set at 1: too many mismatches above this threshold
      
    # Apply threshold
    dtm[x1, "match"] <- ifelse(dtm[x1, "distance"]  <= 1, "yes", "no")
    table(dtm[x1, "match"])
    dtm[which(dtm[, "match"] == "no"), "locality_matched"] <- NA
    x1 <- which(! is.na(dtm[, "locality_matched"]) )
    
  #...................................      
  ## Attribute OCHA place names
  for (i in x1) {
    # Find and attribute corresponding subdistrict
    dtm[i, "subdis_en_ocha"] <- unique(equi_loc[which(equi_loc[, "dis_en_ocha"] == dtm[i, "dis_en_ocha"] & 
        equi_loc[, "locality_ascii"] == dtm[i, "locality_matched"]), "subdis_en_ocha"])
    
    # Attribute missing subdistrict pcodes
    dtm[i, "subdis_pcode_ocha"] <- unique(ocha[which(ocha[, "subdis_en_ocha"] == dtm[i, "subdis_en_ocha"]), "subdis_pcode_ocha"])
  }  

    # Update  missingness
    dtm[, "missing_subdis"] <- ifelse(is.na(dtm[, "subdis_en_ocha"]), "yes", "no")
    table(dtm[, c("eligible", "missing_subdis")])
    prop.table(table(dtm[, c("eligible", "missing_subdis")]), margin = 1) * 100

    # Remove extra variables
    dtm <- dtm[, !colnames(dtm) %in% c("match", "distance", "locality_matched", "locality_ascii")]
    rm(equi_loc)

    
#.........................................................................................                            
### Finding missing subdistricts: Method 4 (based on matching localities in CSO dataset)
#.........................................................................................    

  #...................................      
  ## Prepare DTM locality names for matching
    
    # Arabic locality names to ASCII
    dtm[, "locality_ascii"] <- stri_trans_general(dtm[, "locality_ar"], "Arab-Latin")        
    dtm[, "locality_ascii"] <- stri_trans_general(tolower(dtm[, "locality_ascii"]), "Latin-ASCII")
    
    # Where Arabic locality names are missing, supplement with transliterated locality names
    dtm[, "locality_ascii"] <- ifelse(is.na(dtm[, "locality_ascii"]), stri_trans_general(tolower(dtm[, "locality_en"]), 
      "Latin-ASCII"), dtm[, "locality_ascii"])
    
    # Exclude special symbols, vowels and zero-length strings
    for (i in c("ʿ", "ʾ") ) {dtm[, "locality_ascii"] <- gsub(i, "'", dtm[, "locality_ascii"]) }
    for (i in c("a", "e", "i", "o", "u", "-") ) {dtm[, "locality_ascii"] <- gsub(i, "", dtm[, "locality_ascii"]) }
    dtm[, "locality_ascii"] <- trimws(dtm[, "locality_ascii"])
    dtm[, "locality_ascii"] <- ifelse(dtm[, "locality_ascii"] == "", NA, dtm[, "locality_ascii"])
    
    
  #...................................      
  ## Prepare CSO locality columns for matching
  for (i in match_units) {
    
    # Arabic locality names to ASCII
    cso[, paste(i, "_ascii", sep = "")] <- stri_trans_general(cso[, paste(i, "_ar", sep = "")], "Arab-Latin")
    cso[, paste(i, "_ascii", sep = "")] <- stri_trans_general(cso[, paste(i, "_ascii", sep = "")], "Latin-ASCII")
    
    # Exclude special symbols, vowels and zero-length strings
    for (j in c("ʿ", "ʾ") ) {cso[, paste(i, "_ascii", sep = "")] <- 
      gsub(j, "'", cso[, paste(i, "_ascii", sep = "")]) }
    for (j in c("a", "e", "i", "o", "u") ) {cso[, paste(i, "_ascii", sep = "")] <- 
      gsub(j, "", cso[, paste(i, "_ascii", sep = "")]) }      
    cso[, paste(i, "_ascii", sep = "")] <- trimws(cso[, paste(i, "_ascii", sep = "")])
    cso[, paste(i, "_ascii", sep = "")] <- ifelse(cso[, paste(i, "_ascii", sep = "")] == "", NA, 
      cso[, paste(i, "_ascii", sep = "")])
  }

  #...................................      
  ## By DTM district, check if there is a match for the DTM locality in each of the CSO locality columns
    # Preparatory steps
    for (i in match_units) { dtm[, paste(i, "_dtm_cso_match", sep = "")] <- NA}
    x1 <- unique(dtm[, "dis_pcode_ocha"])
    
    # Do fuzzy matching
    for (i in 1:length(x1) ) {

      # track progress
      loop.tracker(i, length(x1) ) 
      
      # select only DTM data for this district and needing and able to be matched
      x2 <- which(dtm[, "dis_pcode_ocha"] == x1[i] & is.na(dtm[, "subdis_en_ocha"]) 
        & (! is.na(dtm[, "locality_en"]) | ! is.na(dtm[, "locality_ar"])))
      
      # if there are no data to be matched, advance to next loop
      if (length(x2) == 0) {next}
      
      # fuzzy matching for this district, for each locality geographic unit in CSO dataset
      for (j in match_units) {
        
        # in CSO, there cannot be more than one of the same locality name across the subdistricts; otherwise, no matching
        x3 <- unique(na.omit(cso[which(cso[, "dis_pcode"] == x1[i]), c("subdis_pcode", paste(j, "_ascii", sep = "") )]))
        x3 <- table(x3[, paste(j, "_ascii", sep = "")])
        x3 <- names(x3[x3 == 1])
        
        # perform matching search
        if (length(x3) > 0) {
          dtm[x2, paste(j, "_dtm_cso_match", sep = "")] <- f_fuzzy(dtm[x2, "locality_ascii"], x3, methods)
        }
      }
    }
  
    # Convert "NO MATCH" output to NA
    x2 <- which(is.na(dtm[, "subdis_en_ocha"]) & (! is.na(dtm[, "locality_en"]) | ! is.na(dtm[, "locality_ar"])))
    for (i in match_units) {
      dtm[x2, paste(i, "_dtm_cso_match", sep = "")]  <- ifelse(dtm[x2, paste(i, "_dtm_cso_match", sep = "")] == "NO MATCH",
        NA, dtm[x2, paste(i, "_dtm_cso_match", sep = "")] )
    }

        
  #...................................      
  ## Calculate string distance for each matched column: if at least one column has a distance below a threshold,
    # there is a match
    
    # Calculate distance
    for (i in match_units) {
      print(paste("now computing distance for this geographic unit: ", i, sep = " "))
      dtm[x2, paste(i, "_distance", sep = "")] <- 
        apply(dtm[x2, c("locality_ascii", paste(i, "_dtm_cso_match", sep = "") )], 1, function(x) {stringdist(x[1], x[2])})
    }

    # Tabulate distance
    for (i in match_units) {
      print(paste("table of match distances for ", i, sep = " ") )
      print(table(dtm[x2, paste(i, "_distance", sep = "")]) ) 
    }
    
    # For which geographic unit is there a match?
      # calculate smallest distance across all match columns
      dtm[x2, "distance_min"] <- apply(dtm[x2, grep("distance", colnames(dtm))], 1, function(x) {min(x, na.rm = TRUE)})
      dtm[x2, "distance_min"] <- ifelse(dtm[x2, "distance_min"] == Inf, NA, dtm[x2, "distance_min"])
      
      # apply threshold
      dtm[, "match"] <- FALSE
      dtm[x2, "match"] <- ifelse(is.na(dtm[x2, "distance_min"]) == FALSE & dtm[x2, "distance_min"]  < distance_max, TRUE, FALSE)
      table(dtm$match)

      
  #...................................      
  ## Identify OCHA subdistrict for matches
      
    # Identify which CSO geographic unit provides the closest match
      x1 <- which(dtm[, "match"])
      
      dtm[, "match_unit"] <- NA
      dtm[x1, "match_unit"] <- apply(dtm[x1, grep("distance", colnames(dtm))], 
        1, function(x) {return(names(which.min(x)))} )
      dtm[x1, "match_unit"] <- gsub("distance", "",  dtm[x1, "match_unit"] )
      dtm[x1, "match_unit"] <- gsub("_", "",  dtm[x1, "match_unit"] )
      table(dtm[, "match_unit"])
      
    # Identify subdistrict
    for (i in x1) {
      # find corresponding subdistrict and subdistrict pcode
       x2 <- cso[which(cso[, "dis_en_ocha"] == dtm[i, "dis_en_ocha"] &
        cso[, paste(dtm[i, "match_unit"], "_ascii", sep = "")] == dtm[i, paste(dtm[i, "match_unit"], "_dtm_cso_match", sep = "")]), 
        c("subdis_en_ocha", "subdis_pcode")]
      dtm[i, c("subdis_en_ocha", "subdis_pcode")] <- unique(x2)
    }  
    dtm[x1, "subdis_pcode_ocha"] <- dtm[x1, "subdis_pcode"]
      
        
    # Update  missingness
    dtm[, "missing_subdis"] <- ifelse(is.na(dtm[, "subdis_en_ocha"]), "yes", "no")
    table(dtm[, c("eligible", "missing_subdis")])
    prop.table(table(dtm[, c("eligible", "missing_subdis")]), margin = 1) * 100

    # Remove extra variables and objects
    dtm <- dtm[, - grep("distance", colnames(dtm))]
    dtm <- dtm[, - grep("match", colnames(dtm))]
    dtm <- dtm[, ! colnames(dtm) %in% c("subdis_pcode")]
    rm(cso)
    
#.........................................................................................                            
### Writing dataset so far
#.........................................................................................    

    # Write
    write_excel_csv(dtm, "yem_displacement_dataset_clean_raw.csv", na = "")
    

    
#..........................................................................................
### ENDS
#..........................................................................................
    

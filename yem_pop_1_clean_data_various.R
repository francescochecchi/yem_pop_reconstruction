#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## ------------ R SCRIPT TO CLEAN AND PREPARE VARIOUS DATASETS FOR ANALYSIS ------------ ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Oct 2021)

                                          # francesco.checchi@lshtm.ac.uk 


     
#.........................................................................................
### Preparing OCHA shape file, gazetteers and population datasets
#.........................................................................................

  #...................................      
  ## Prepare OCHA shape file for subdistricts
    # Column names
    colnames(ocha_shape) <- tolower(colnames(ocha_shape))
    colnames(ocha_shape) <- gsub("adm1", "gov", colnames(ocha_shape))
    colnames(ocha_shape) <- gsub("adm2", "dis", colnames(ocha_shape))
    colnames(ocha_shape) <- gsub("adm3", "subdis", colnames(ocha_shape))
    colnames(ocha_shape)[colnames(ocha_shape) != "geometry"] <- paste(colnames(ocha_shape)[colnames(ocha_shape) != "geometry"], "_ocha_shape", sep = "")
    
    # Delete unnecessary columns
    ocha_shape <- ocha_shape[, c("gov_en_ocha_shape", "gov_ar_ocha_shape", "gov_pcode_ocha_shape", "dis_en_ocha_shape", "dis_ar_ocha_shape", "dis_pcode_ocha_shape",
      "subdis_ar_ocha_shape", "subdis_pcode_ocha_shape", "subdisref_en_ocha_shape", "geometry")]
    colnames(ocha_shape)[colnames(ocha_shape) == "subdisref_en_ocha_shape"] <- "subdis_en_ocha_shape"
      # like LSHTM, ocha_shape has dealt with instances of sub-districts with the same name by renaming them as 
      # "subdistrict - district" thereby creating a unique instance: keep this version
    
    # Check for duplicates
    ocha_shape[duplicated(ocha_shape[, c("gov_en_ocha_shape", "dis_en_ocha_shape", "subdis_en_ocha_shape")]), ]
      # (none identified)

  #...................................      
  ## Prepare OCHA shape file for districts
    # Column names
    colnames(ocha_shape_dis) <- tolower(colnames(ocha_shape_dis))
    colnames(ocha_shape_dis) <- gsub("adm2", "dis", colnames(ocha_shape_dis))
    colnames(ocha_shape_dis)[colnames(ocha_shape_dis) != "geometry"] <- paste(colnames(ocha_shape_dis)[colnames(ocha_shape_dis) != "geometry"], "_ocha_shape_dis", sep = "")
    
    # Delete unnecessary columns
    ocha_shape_dis <- ocha_shape_dis[, c("dis_en_ocha_shape_dis", "geometry")]
    colnames(ocha_shape_dis)[colnames(ocha_shape_dis) == "subdisref_en_ocha_shape_dis"] <- "subdis_en_ocha_shape_dis"
  
    # Check for duplicates
    ocha_shape_dis[duplicated(ocha_shape_dis[, "dis_en_ocha_shape_dis"]), ]
      # (none identified)
        
  #...................................      
  ## Prepare OCHA shape file for governorates
    # Column names
    colnames(ocha_shape_gov) <- tolower(colnames(ocha_shape_gov))
    colnames(ocha_shape_gov) <- gsub("adm1", "gov", colnames(ocha_shape_gov))
    colnames(ocha_shape_gov)[colnames(ocha_shape_gov) != "geometry"] <- paste(colnames(ocha_shape_gov)[colnames(ocha_shape_gov) != "geometry"], "_ocha_shape_gov", sep = "")
    
    # Delete unnecessary columns
    ocha_shape_gov <- ocha_shape_gov[, c("gov_en_ocha_shape_gov", "geometry")]
    colnames(ocha_shape_gov)[colnames(ocha_shape_gov) == "subdisref_en_ocha_shape_gov"] <- "subdis_en_ocha_shape_gov"
   
    # Check for duplicates
    ocha_shape_gov[duplicated(ocha_shape_gov[, "gov_en_ocha_shape_gov"]), ]
      # (none identified)
    
               
  #...................................      
  ## Prepare OCHA gazetteer
    # Check for duplicates
    ocha[duplicated(ocha[, c("gov_en", "dis_en", "subdis_en")]), ]
      # (none identified)

    # Modify pcode variables
    x1 <- grep("pcode", colnames(ocha), value = TRUE)
    ocha[, x1] <- apply(ocha[, x1], 2, function (x) {gsub("YE", "", x)} )
    
    # Modify variable names
    colnames(ocha)[grep("en|ar", colnames(ocha))] <- paste(colnames(ocha)[grep("en|ar", colnames(ocha))], "_ocha", sep = "")
    
  #...................................      
  ## Prepare CSO gazetteer
    # COlumn formats
    cso[, "locality_pcode"] <- as.character(cso[, "locality_pcode"])
    
    # Check for duplicates
    cso[duplicated(cso[, "locality_pcode"]), ]
      # (none identified)
 
    # Change column names
    x1 <- c("gov_ar", "dis_ar", "subdis_ar")
    colnames(cso)[ colnames(cso) %in% x1] <- paste(x1, "_cso", sep = "")
    
    # Add governorate, district and subdistrict pcodes, based on locality pcode
    cso[, "gov_pcode_cso"] <- substr(cso[, "locality_pcode"], 1, 2)
    cso[, "dis_pcode_cso"] <- substr(cso[, "locality_pcode"], 1, 4)
    cso[, "subdis_pcode_cso"] <- substr(cso[, "locality_pcode"], 1, 6)
    

  #...................................
  ## Prepare population dataset
    # Column formats
    pop_worldpop[, "pop"] <- as.integer(pop_worldpop[, "pop"])    
      # warning message is OK - removing some values marked as "NA" as string

    # Resolve a few NA values by setting them arbitrarily to 10 (these are small, possibly uninhabited islands)
    table(is.na(pop_worldpop[, "pop"]))
    unique(pop_worldpop[which(is.na(pop_worldpop[, "pop"])), "subdis"])
    pop_worldpop[ "pop"] <- ifelse(is.na(pop_worldpop[, "pop"]), 10, pop_worldpop[, "pop"])
    table(is.na(pop_worldpop[, "pop"]))

    
#.........................................................................................
### Converting health facility shape file into a data frame by subdistrict
#.........................................................................................
    
  #...................................      
  ## Prepare health facility shape file

    # Delete unnecessary columns
    health_shape <- health_shape[, c("osm_id", "amenity", "geometry")]
    
    # Check for duplicates
    health_shape[duplicated(health_shape[, c("amenity", "geometry")]), ]
      # delete duplicates
      health_shape <- health_shape[! duplicated(health_shape[, c("amenity", "geometry")]), ]

  #...................................      
  ##  Map coordinates to OCHA subdistrict and clean up
    # from https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package 
    # also see https://spatialreference.org/ref/epsg/?search=Yemen&srtext=Search for chosen spatial projection
    
    # Prepare OCHA boundary shape file
      # apply planar transformation
      ocha_trans <- st_transform(ocha_shape, 4981)
      
    # Prepare health facility coordinate points
      # prepare a points collection
      pnts <- matrix(unlist(health_shape[, "geometry"]), byrow= TRUE, nrow = nrow(health_shape))
      pnts <- as.data.frame(pnts)
      colnames(pnts) <- c("x", "y")
      pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
        function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 

      # apply planar transformation
      pnts_trans <- st_transform(pnts_sf, 4981)

    # Find intersection of points and polygons, and extract OCHA subdistrict pcode
    pnts$subdis_pcode_ocha <- apply(st_intersects(ocha_trans, pnts_trans, sparse = FALSE), 2, 
      function(col) { ocha_trans[which(col), ]$subdis_pcode_ocha_shape})
        
    # Merge with other OCHA variables
      # manage points dataset for merging
      x1 <- tidyr::unnest(pnts, cols = c(subdis_pcode_ocha))
      x1 <- as.data.frame(x1)
      colnames(x1) <- c("longitude", "latitude", "subdis_pcode")
      x1[, "subdis_pcode"] <- sapply(x1[, "subdis_pcode"], function(x) {gsub("YE", "", x)})

      # add other OCHA variables
      x1 <- merge(x1, ocha[, c("subdis_en_ocha", "subdis_pcode", "dis_en_ocha", "dis_pcode")], 
        by = "subdis_pcode", all.x = TRUE)
      
    # Create predictor dataset (number of health facilities per subdistrict)
    x1[, "n_hf"] <- 1
    health_pred <- aggregate(x1[, "n_hf"], by = x1[, c("subdis_en_ocha", "subdis_pcode", "dis_en_ocha", "dis_pcode")],
      FUN = sum)
    colnames(health_pred)[colnames(health_pred) == "x"] <- "n_hf"
      
      # add subdistricts with zero mapped health facilities
      health_pred <- merge(health_pred, ocha[, c("subdis_en_ocha", "dis_en_ocha")], 
        by = c("subdis_en_ocha", "dis_en_ocha"), all.y = TRUE)
      health_pred[which(is.na(health_pred[, "n_hf"])), "n_hf"] <- 0
      
    # Remove unnecessary columns and objects
    health_pred <- health_pred[, c("subdis_en_ocha", "dis_en_ocha", "n_hf")]
    rm(pnts, pnts_sf, pnts_trans, ocha_trans, health_shape) 
     
    
#.........................................................................................
### Converting road network shape file into a data frame by subdistrict
#.........................................................................................
    
  #...................................      
  ## Prepare road network shape file

    # Delete unnecessary columns
    road_shape <- road_shape[, c("OBJECTID", "geometry")]
    colnames(road_shape) <- c("id", "geometry")
        
  #...................................      
  ##  Map road segments to OCHA subdistrict and calculate road length
    # from https://stackoverflow.com/questions/56993193/road-length-within-polygons-in-r
    # also see https://spatialreference.org/ref/epsg/?search=Yemen&srtext=Search for chosen spatial projection
    
    # Prepare OCHA boundary shape file
      # apply planar transformation
      ocha_trans <- st_transform(ocha_shape, 4981)
      
    # Prepare road facility coordinate points
      # apply planar transformation
      road_trans <- st_transform(road_shape, 4981)
      
    # Find intersection between two
    x1 <- sf::st_intersection(ocha_trans, road_trans)
    
    # Calculate road distance (in Km) of each subdistrict segment
    x1[, "distance"] <- unlist(sf::st_length(x1[, "geometry"]) ) / 1000
    
    # Sum total road distance by subdistrict
    x2 <- sf::st_drop_geometry(x1)
    x2 <- aggregate(x2[, "distance"], by = x2[, c("dis_en_ocha_shape", "subdis_en_ocha_shape")], FUN = sum)
    x2[, "distance"] <- as.numeric(x2[, "x"])  

  #...................................      
  ##  Compute surface area of subdistrict and calculate road density
    
    # Subdistrict surface area in Km^2
    ocha_trans[, "area"] <- as.numeric(unlist(sf::st_area(ocha_trans)) / (1000^2) )
    
    # Calculate density (Km road per Km^2 land area)
    x1 <- sf::st_drop_geometry(ocha_trans)
    x2 <- merge(x2, x1[, c("subdis_en_ocha_shape", "area")], by = "subdis_en_ocha_shape", all.y = TRUE)
    x2[which(is.na(x2[, "distance"])), "distance"] <- 0
    x2[, "road_density"] <- x2[, "distance"] / x2[, "area"]
    
    # Clean up and remove unnecessary columns and objects
    road_pred <- x2[, c("subdis_en_ocha_shape", "area", "distance", "road_density")]
    colnames(road_pred) <- c("subdis_en_ocha", "subdis_area", "road_distance", "road_density")
    road_pred <- merge(road_pred, ocha[, c("subdis_en_ocha", "dis_en_ocha")], by = "subdis_en_ocha")
    rm(x1, x2, ocha_trans, road_trans) 
     
    

#.........................................................................................
### Preparing ACLED insecurity dataset
#.........................................................................................

  #...................................      
  ## Check completeness and validity of latitude and longitude data 
    
    # To numeric
  	acled[, "latitude"] <- as.numeric(acled[, "latitude"])  
  	acled[, "longitude"] <- as.numeric(acled[, "longitude"])  
  	
  	# Range and distribution
  	range(acled$latitude, na.rm = TRUE)
  	hist(acled$latitude)
  	range(acled$longitude, na.rm = TRUE)
  	hist(acled$longitude)
  	
    # Eliminate missing or impossible coordinates
    acled[, "latitude"] <- ifelse(acled[, "latitude"] < 12 | acled[, "latitude"] > 19, NA, acled[, "latitude"])
    acled[, "longitude"] <- ifelse(acled[, "longitude"] < 41 | acled[, "longitude"] > 55, NA, acled[, "longitude"])
  
    # Check completeness
    table(is.na(acled[, "latitude"]) | is.na(acled[, "longitude"]))
      # 99.99% complete, barring two observations to delete
        acled <- subset(acled, ! is.na(latitude) & ! is.na(longitude))
  
  #...................................      
  ## Manage variables

    # Restrict to variables of interest
    table(is.na(acled[, "event_date"]))
    acled <- acled[, c("event_date", "event_type", "latitude", "longitude", "fatalities")]   
        
    # Create date variables
    acled[, "date"] <- lubridate::parse_date_time(acled[, "event_date"], orders = "%d/%b/%y")
    acled[, "month"] <- month(acled[, "date"])
    acled[, "year"] <- year(acled[, "date"])
    table(is.na(acled[, "month"]))
    table(is.na(acled[, "year"]))

    # Create / change aggregation variables
    colnames(acled)[colnames(acled) == "fatalities"] <- "n_fatalities"
    acled[, "n_events"] <- 1
    
  #...................................      
  ##  Map coordinates to OCHA subdistrict and clean up
    # from https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package 
    # also see https://spatialreference.org/ref/epsg/?search=Yemen&srtext=Search for chosen spatial projection
    
    # Prepare OCHA boundary shape file
      # apply planar transformation
      ocha_trans <- st_transform(ocha_shape, 4981)
      
    # Prepare ACLED coordinate points
      # prepare a points collection
      pnts <- acled[, c("longitude", "latitude")]
      colnames(pnts) <- c("x", "y")
      pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
        function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 

      # apply planar transformation
      pnts_trans <- st_transform(pnts_sf, 4981)

    # Find intersection of points and polygons, and extract OCHA subdistrict pcode
    pnts$subdis_pcode_ocha <- apply(st_intersects(ocha_trans, pnts_trans, sparse = FALSE), 2, 
      function(col) { ocha_trans[which(col), ]$subdis_pcode_ocha_shape})
        
    # Merge with ACLED dataset
      # manage points dataset for merging
      x1 <- tidyr::unnest(pnts, cols = c(subdis_pcode_ocha))
      x1 <- as.data.frame(x1)
      colnames(x1) <- c("longitude", "latitude", "subdis_pcode")
      x1 <- unique(x1)
      x1[, "subdis_pcode"] <- sapply(x1[, "subdis_pcode"], function(x) {gsub("YE", "", x)})

      # add other OCHA variables
      x1 <- merge(x1, ocha[, c("subdis_en_ocha", "subdis_pcode", "dis_en_ocha", "dis_pcode")], 
        by = "subdis_pcode", all.x = TRUE)

      # merge into ACLED
      acled <- merge(acled, x1, by = c("longitude", "latitude"), all.x = TRUE )

      # double-check completeness
      table(is.na(acled[, "subdis_en_ocha"]))
        # 87 out of 62,627 records have a missing subdistrict, and are deleted
        acled <- subset(acled, ! is.na(subdis_en_ocha))
     
    # Remove unnecessary columns
    acled <- acled[, ! colnames(acled) %in% c("longitude", "latitude", "event_date", "date")]
    rm(pnts, pnts_sf, pnts_trans, ocha_trans) 
        
                          
    
#..........................................................................................
### ENDS
#..........................................................................................
    

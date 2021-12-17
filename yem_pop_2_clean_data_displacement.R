#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## ---------- R SCRIPT TO CLEAN AND PREPARE DISPLACEMENT DATASETS FOR ANALYSIS --------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Oct 2021)

                                          # francesco.checchi@lshtm.ac.uk 


                          
#.........................................................................................
### Preparing prevalent displacement datasets
#.........................................................................................
        
  #...................................
  ## Prepare IDP prevalent dataset
      
    # Check date of assessment, round, year and month of displacement
    range(na.omit(idp_prevalent$date_assessment))
    range(na.omit(idp_prevalent$round))
    table(idp_prevalent$year_idp) # need to do something about '2014 and before category'
    table(idp_prevalent$month_idp)

      # create month and year of assessment, rename variables
      idp_prevalent[, "month_assessment"] <- month(idp_prevalent$date_assessment)
      idp_prevalent[, "year_assessment"] <- year(idp_prevalent$date_assessment)
      
    # Check and correct geographic names and codes
      # governorate
      table(idp_prevalent$gov)
      table(idp_prevalent$gov_ori)
      
      # district
      table(idp_prevalent$dis)
      range(idp_prevalent$dis_pcode, na.rm = TRUE)
      table(idp_prevalent$dis_ori)
      range(idp_prevalent$dis_ori_pcode, na.rm = TRUE)
      
      # subdistrict
      range(idp_prevalent$subdis_pcode, na.rm = TRUE)
      range(idp_prevalent$subdis_ori_pcode, na.rm = TRUE)
        # convert a few sub-districts from Arabic to Latin
    	  idp_prevalent[, "subdis"] <- 
    	    stri_trans_general(stri_trans_general(idp_prevalent[, "subdis"], "Arab-Latin"), "Latin-ASCII")
    	  idp_prevalent[, "subdis_ori"] <- 
    	    stri_trans_general(stri_trans_general(idp_prevalent[, "subdis_ori"], "Arab-Latin"), "Latin-ASCII")
      
      # locality
      range(idp_prevalent$locality_pcode, na.rm = TRUE)
        # convert a few transliterated localities from Arabic to Latin
    	  idp_prevalent[, "locality_en"] <- 
    	    stri_trans_general(stri_trans_general(idp_prevalent[, "locality_en"], "Arab-Latin"), "Latin-ASCII")
        # eliminate non-Arabic characters from Arabic locality
    	  idp_prevalent[, "locality_ar"] <- gsub("[^\\p{Arabic}., ]+", "", idp_prevalent[, "locality_ar"], perl = TRUE)
       
    	# eliminate double white spaces
  	  for (i in c("gov", "gov_ori", "dis", "dis_ori", "subdis", "subdis_ori", "locality_en", "locality_ar")) {
  	    idp_prevalent[, i] <- gsub("  ", " ", idp_prevalent[, i])
  	   }

    # Check and correct latitude and longitude
      # to numeric
    	idp_prevalent[, "latitude"] <- as.numeric(idp_prevalent[, "latitude"])  
    	idp_prevalent[, "longitude"] <- as.numeric(idp_prevalent[, "longitude"])  
    	
    	# range and distribution
    	range(idp_prevalent$latitude, na.rm = TRUE)
    	hist(idp_prevalent$latitude)
    	range(idp_prevalent$longitude, na.rm = TRUE)
    	hist(idp_prevalent$longitude)
    	
    	# fix coordinate sets that appear to have been swapped (latitude = longitude and vice versa)
    	x1 <- row.names(subset(idp_prevalent, latitude > 30))
    	x2 <- cbind(idp_prevalent[x1, "longitude"], idp_prevalent[x1, "latitude"])
    	idp_prevalent[x1, c("latitude", "longitude")] <- x2
    	range(idp_prevalent$latitude, na.rm = TRUE)
    	range(idp_prevalent$longitude, na.rm = TRUE)

      # eliminate missing or impossible coordinates
      idp_prevalent[, "latitude"] <- ifelse(idp_prevalent[, "latitude"] < 12 | idp_prevalent[, "latitude"] > 19, NA, idp_prevalent[, "latitude"])
      idp_prevalent[, "longitude"] <- ifelse(idp_prevalent[, "longitude"] < 41 | idp_prevalent[, "longitude"] > 55, NA, idp_prevalent[, "longitude"])
    
    # Check and correct IDP numbers

      # are there any observations where the n of households overall is missing but the n  due to conflict/disaster is provided?
      nrow(subset(idp_prevalent, is.na(hh_idp) & ! is.na(hh_idp_conflict)))
      nrow(subset(idp_prevalent, is.na(hh_idp) & ! is.na(hh_idp_disaster)))
        # no
      
    # Check for duplicates
    x1 <- row.names(idp_prevalent[duplicated(idp_prevalent[, c("date_assessment", "year_idp", "month_idp", "locality_pcode", "hh_idp", "dis_ori")]), ])
    table(idp_prevalent[x1, "round"])
    sum(table(idp_prevalent[x1, "round"]))
      
      # about 380 duplicates - delete all
      idp_prevalent <- idp_prevalent[! row.names(idp_prevalent) %in% x1, ]
	 
    # Tag dataset
    idp_prevalent[, "type"] <- "prevalent"
    idp_prevalent[, "who"] <- "idp"
      
      
  #...................................
  ## Prepare returnee prevalent dataset
      
    # Check date of assessment, round, year and month of return
    range(na.omit(ret_prevalent$date_assessment))
    range(na.omit(ret_prevalent$round))
    table(ret_prevalent$year_ret) # need to do something about '2014 and before category'
    table(ret_prevalent$month_ret)

      # create month and year of assessment
      ret_prevalent[, "month_assessment"] <- month(ret_prevalent$date_assessment)
      ret_prevalent[, "year_assessment"] <- year(ret_prevalent$date_assessment)
    
    # Check and correct geographic names and codes
      # governorate
      table(ret_prevalent$gov)
      table(ret_prevalent$gov_ori)
      
      # district
      table(ret_prevalent$dis)
      range(ret_prevalent$dis_pcode, na.rm = TRUE)
      table(ret_prevalent$dis_ori)
      range(ret_prevalent$dis_ori_pcode, na.rm = TRUE)
      
      # subdistrict
      range(ret_prevalent$subdis_pcode, na.rm = TRUE)
        # convert a few sub-districts from Arabic to Latin
    	  ret_prevalent[, "subdis"] <- 
    	    stri_trans_general(stri_trans_general(ret_prevalent[, "subdis"], "Arab-Latin"), "Latin-ASCII")

      # locality
      range(ret_prevalent$locality_pcode, na.rm = TRUE)
        # convert a few transliterated localities from Arabic to Latin
    	  ret_prevalent[, "locality_en"] <- 
    	    stri_trans_general(stri_trans_general(ret_prevalent[, "locality_en"], "Arab-Latin"), "Latin-ASCII")
        # eliminate non-Arabic characters from Arabic locality
    	  ret_prevalent[, "locality_ar"] <- gsub("[^\\p{Arabic}., ]+", "", ret_prevalent[, "locality_ar"], perl = TRUE)
       
    	# eliminate double white spaces
  	  for (i in c("gov", "gov_ori", "dis", "dis_ori", "subdis", "locality_en", "locality_ar")) {
  	    ret_prevalent[, i] <- gsub("  ", " ", ret_prevalent[, i])
  	   }

    # Check and correct latitude and longitude
      # to numeric
    	ret_prevalent[, "latitude"] <- as.numeric(ret_prevalent[, "latitude"])  
    	ret_prevalent[, "longitude"] <- as.numeric(ret_prevalent[, "longitude"])  
    	
    	# range and distribution
    	range(ret_prevalent$latitude, na.rm = TRUE)
    	hist(ret_prevalent$latitude)
    	range(ret_prevalent$longitude, na.rm = TRUE)
    	hist(ret_prevalent$longitude)
    	
    	# fix coordinate sets that appear to have been swapped (latitude = longitude and vice versa)
    	x1 <- row.names(subset(ret_prevalent, latitude > 30))
    	x2 <- cbind(ret_prevalent[x1, "longitude"], ret_prevalent[x1, "latitude"])
    	ret_prevalent[x1, c("latitude", "longitude")] <- x2
    	range(ret_prevalent$latitude, na.rm = TRUE)
    	range(ret_prevalent$longitude, na.rm = TRUE)

      # eliminate missing or impossible coordinates
      ret_prevalent[, "latitude"] <- ifelse(ret_prevalent[, "latitude"] < 12 | ret_prevalent[, "latitude"] > 19, NA, ret_prevalent[, "latitude"])
      ret_prevalent[, "longitude"] <- ifelse(ret_prevalent[, "longitude"] < 41 | ret_prevalent[, "longitude"] > 55, NA, ret_prevalent[, "longitude"])
    
    # Check and correct returnee numbers
      # are there any observations where the n of households overall is missing but the n  due to conflict/disaster is provided?
      nrow(subset(ret_prevalent, is.na(hh_ret) & ! is.na(hh_ret_conflict)))
      nrow(subset(ret_prevalent, is.na(hh_ret) & ! is.na(hh_ret_disaster)))
        # no
      
    # Check for duplicates
    x1 <- row.names(ret_prevalent[duplicated(ret_prevalent[, c("date_assessment", "year_ret", "month_ret", "locality_pcode", "hh_ret", "dis_ori")]), ])
    table(ret_prevalent[x1, "round"])
    sum(table(ret_prevalent[x1, "round"]))
      
      # about 31 duplicates - delete all
      ret_prevalent <- ret_prevalent[! row.names(ret_prevalent) %in% x1, ]
	 
    # Tag dataset
    ret_prevalent[, "type"] <- "prevalent"
    ret_prevalent[, "who"] <- "ret"
    
    
#.........................................................................................
### Preparing incident displacement datasets
#.........................................................................................
      
  #...................................
  ## Prepare IDP incident dataset
      
    # Check date of displacement and create year and month of displacement
    range(na.omit(idp_incident$date_idp))
    idp_incident[, "year_idp"] <- year(idp_incident$date_idp)
    table(idp_incident$year_idp)
    idp_incident[, "month_idp"] <- month(idp_incident$date_idp)
    table(idp_incident$month_idp)

    # Check and correct geographic names and codes
      # governorate
      table(idp_incident$gov)
      table(idp_incident$gov_ori)
      
      # district
      table(idp_incident$dis)
      range(idp_incident$dis_pcode, na.rm = TRUE)
      table(idp_incident$dis_ori)

      # subdistrict
        # convert a few sub-districts from Arabic to Latin
    	  idp_incident[, "subdis"] <- 
    	    stri_trans_general(stri_trans_general(idp_incident[, "subdis"], "Arab-Latin"), "Latin-ASCII")

      # locality
      range(idp_incident$locality_pcode, na.rm = TRUE)
        # create an Arabic version of locality
    	  idp_incident[, "locality_ar"] <- idp_incident[, "locality"]
    	  idp_incident[, "locality_ar"] <- gsub("[^\\p{Arabic}., ]+", "", idp_incident[, "locality_ar"], perl = TRUE)
        
    	  # create a transliterated version of locality
    	  idp_incident[, "locality_en"] <- idp_incident[, "locality"]
    	  idp_incident[, "locality_en"] <- gsub("-", " ", idp_incident[, "locality_en"])
    	  idp_incident[, "locality_en"] <- gsub("[^[:alnum:] ]", "", idp_incident[, "locality_en"], perl = TRUE)
      
    	  # eliminate original locality variable
    	  idp_incident <- subset(idp_incident, select = - locality)
    	  
    	# eliminate double white spaces
  	  for (i in c("gov", "gov_ori", "dis", "dis_ori", "subdis", "locality_en", "locality_ar")) {
  	    idp_incident[, i] <- gsub("  ", " ", idp_incident[, i])
  	   }

    # Check for duplicates
    x1 <- row.names(idp_incident[duplicated(idp_incident[, c("date_idp", "gov", "dis", "hh_idp", "dis_ori")]), ])
    table(idp_incident[x1, "year_idp"])
    sum(table(idp_incident[x1, "gov_ori"]))
      
      # no basis on which to decide whether these could be duplicates - many could just be instances of multiple
      # displacements from the same to the same district on the same day, by the same n of households
  
    # Tag dataset
    idp_incident[, "type"] <- "incident"
    idp_incident[, "who"] <- "idp"
    
    
  #...................................
  ## Prepare returnee incident dataset
      
    # Check date of return and create year and month of return
    range(na.omit(ret_incident$date_ret))
    ret_incident[, "year_ret"] <- year(ret_incident$date_ret)
    table(ret_incident$year_ret)
    ret_incident[, "month_ret"] <- month(ret_incident$date_ret)
    table(ret_incident$month_ret)
    
    # Check and correct geographic names and codes
      # governorate
      table(ret_incident$gov)
      table(ret_incident$gov_ori)
      
      # district
      table(ret_incident$dis)
      table(ret_incident$dis_ori)

      # subdistrict
        # convert a few sub-districts from Arabic to Latin
    	  ret_incident[, "subdis"] <- 
    	    stri_trans_general(stri_trans_general(ret_incident[, "subdis"], "Arab-Latin"), "Latin-ASCII")

      # locality
        # create an Arabic version of locality
    	  ret_incident[, "locality_ar"] <- ret_incident[, "locality"]
    	  ret_incident[, "locality_ar"] <- gsub("[^\\p{Arabic}., ]+", "", ret_incident[, "locality_ar"], perl = TRUE)
        
    	  # create a transliterated version of locality
    	  ret_incident[, "locality_en"] <- ret_incident[, "locality"]
    	  ret_incident[, "locality_en"] <- gsub("-", " ", ret_incident[, "locality_en"])
    	  ret_incident[, "locality_en"] <- gsub("[^[:alnum:] ]", "", ret_incident[, "locality_en"], perl = TRUE)
      
    	  # eliminate original locality variable
    	  ret_incident <- subset(ret_incident, select = - locality)
    	  
    	# eliminate double white spaces
  	  for (i in c("gov", "gov_ori", "dis", "dis_ori", "subdis", "locality_en", "locality_ar")) {
  	    ret_incident[, i] <- gsub("  ", " ", ret_incident[, i])
  	   }

    # Check for duplicates
    x1 <- row.names(ret_incident[duplicated(ret_incident[, c("date_ret", "gov", "dis", "hh_ret", "dis_ori")]), ])
    table(ret_incident[x1, "year_ret"])
    sum(table(ret_incident[x1, "gov_ori"]))
      
      # no basis on which to decide whether these could be duplicates - many could just be instances of multiple
      # displacements from the same to the same district on the same day, by the same n of households
  
    # Tag dataset
    ret_incident[, "type"] <- "incident"
    ret_incident[, "who"] <- "ret"
    
 
#.........................................................................................
### Appending displacement datasets into one
#.........................................................................................

  #...................................
  ## Harmonise some column names
    
    # Time of displacement or return
    colnames(idp_incident)[colnames(idp_incident) == "date_idp"] <- "date_move"
    colnames(ret_incident)[colnames(ret_incident) == "date_ret"] <- "date_move"
    colnames(idp_prevalent)[colnames(idp_prevalent) == "month_idp"] <- "month_move"
    colnames(idp_prevalent)[colnames(idp_prevalent) == "year_idp"] <- "year_move"
    colnames(ret_prevalent)[colnames(ret_prevalent) == "month_ret"] <- "month_move"
    colnames(ret_prevalent)[colnames(ret_prevalent) == "year_ret"] <- "year_move"
    colnames(idp_incident)[colnames(idp_incident) == "month_idp"] <- "month_move"
    colnames(idp_incident)[colnames(idp_incident) == "year_idp"] <- "year_move"
    colnames(ret_incident)[colnames(ret_incident) == "month_ret"] <- "month_move"
    colnames(ret_incident)[colnames(ret_incident) == "year_ret"] <- "year_move"
    
    # Number of households and IDPs
    colnames(idp_prevalent)[colnames(idp_prevalent) == "hh_idp_conflict"] <- "n_hh_conflict"
    colnames(idp_prevalent)[colnames(idp_prevalent) == "hh_idp_disaster"] <- "n_hh_disaster"
    colnames(idp_prevalent)[colnames(idp_prevalent) == "hh_idp"] <- "n_hh"
    colnames(ret_prevalent)[colnames(ret_prevalent) == "hh_ret_conflict"] <- "n_hh_conflict"
    colnames(ret_prevalent)[colnames(ret_prevalent) == "hh_ret_disaster"] <- "n_hh_disaster"
    colnames(ret_prevalent)[colnames(ret_prevalent) == "hh_ret"] <- "n_hh"
    colnames(idp_incident)[colnames(idp_incident) == "hh_idp"] <- "n_hh"
    colnames(ret_incident)[colnames(ret_incident) == "hh_ret"] <- "n_hh"

  #...................................
  ## Make sure all common columns have the same type
  str(idp_prevalent)    
  str(ret_prevalent)    
  str(idp_incident)    
  str(ret_incident)    
    
    # All years and months of moving to character
    idp_prevalent[, "year_move"] <- as.character(idp_prevalent[, "year_move"])  
    idp_prevalent[, "month_move"] <- as.character(idp_prevalent[, "month_move"])  
    ret_prevalent[, "year_move"] <- as.character(ret_prevalent[, "year_move"])  
    ret_prevalent[, "month_move"] <- as.character(ret_prevalent[, "month_move"])  
    idp_incident[, "year_move"] <- as.character(idp_incident[, "year_move"])  
    idp_incident[, "month_move"] <- as.character(idp_incident[, "month_move"])  
    ret_incident[, "year_move"] <- as.character(ret_incident[, "year_move"])  
    ret_incident[, "month_move"] <- as.character(ret_incident[, "month_move"])  
   
  #...................................
  ## Append all datasets together
  dtm <- bind_rows(idp_prevalent, ret_prevalent, idp_incident, ret_incident)
    
    # Check
    nrow(idp_prevalent)
    nrow(ret_prevalent)
    nrow(idp_incident)
    nrow(ret_incident)
    table(dtm[, c("type", "who")])
          

#.........................................................................................
### Making manual corrections to DTM geographic names; wrapping up and writing dataset
#.........................................................................................
    
  #...................................      
  ## Manual geographic name corrections (mainly to facilitate matching, some to correct known errors)
    # Correct some governorate names
    dtm[which(dtm[, "gov"] == "Amanat Al Asimah"), "gov"] <- "Sana'a City"
    dtm[which(dtm[, "gov_ori"] == "Amanat Al Asimah"), "gov_ori"] <- "Sana'a City"
    dtm[which(dtm[, "gov"] == "Taizz"), "gov"] <- "Ta'iz"
    dtm[which(dtm[, "gov_ori"] == "Taizz"), "gov_ori"] <- "Ta'iz"
    dtm[which(dtm[, "gov"] == "Al Hudaydah"), "gov"] <- "Al Hodeidah"
    dtm[which(dtm[, "gov_ori"] == "Al Hudaydah"), "gov_ori"] <- "Al Hodeidah"
    dtm[which(dtm[, "gov"] == "Al Dhale'e"), "gov"] <- "Ad Dali'"
    dtm[which(dtm[, "gov_ori"] == "Al Dhale'e"), "gov_ori"] <- "Ad Dali'"
    dtm[which(dtm[, "gov"] == "Hadramaut"), "gov"] <- "Hadramawt"
    dtm[which(dtm[, "gov_ori"] == "Hadramaut"), "gov_ori"] <- "Hadramawt"
    dtm[which(dtm[, "gov"] == "Sanaa"), "gov"] <- "Sana'a"
    dtm[which(dtm[, "gov_ori"] == "Sanaa"), "gov"] <- "Sana'a"
    dtm[which(dtm[, "gov_ori"] == "Al Jawf" & dtm[, "dis_ori"] == "Hajjah City"), "gov_ori"] <-
      "Hajjah"
    dtm[which(dtm[, "gov_ori"] == "Al Maharah" & dtm[, "dis_ori"] == "Al Wahdah"), "gov_ori"] <-
      "Amanat Al Asimah"
    dtm[which(dtm[, "gov_ori"] == "Al Mahwit" & dtm[, "dis_ori"] == "Ma'ain"), "gov_ori"] <-
      "Amanat Al Asimah"
    dtm[which(dtm[, "gov_ori"] == "Shabwah" & dtm[, "dis_ori"] == "Jabal Murad"), "gov_ori"] <-
      "Marib"
    dtm[which(dtm[, "gov_ori"] == "Socotra" & dtm[, "dis_ori"] == "Sirwah"), "gov_ori"] <-
      "Marib"
	  
    # Correct some district names
    dtm[which(dtm[, "dis"] == "As Saddahgf"), "dis"] <- "As Saddah"
    dtm[which(dtm[, "dis_ori"] == "As Saddahgf"), "dis"] <- "As Saddah"
    dtm[which(dtm[, "dis"] == "Al Makhad"), "dis"] <- "Al Makhadir"
    dtm[which(dtm[, "dis_ori"] == "Al Makhad"), "dis"] <- "Al Makhadir"
	  dtm[which(dtm[, "dis"] == "Al Hawak" & dtm[, "subdis"] == "Alhadida"), "dis"] <- "Al Hali"
	  dtm[which(dtm[, "dis_ori"] == "Al Wazi'iyah" & dtm[, "subdis_ori"] == "Zinajbar"), "dis_ori"] <-
	    "Al Qahirah"
	  dtm[which(dtm[, "dis_ori"] == "Mawiyah" & dtm[, "subdis_ori"] == "Zinajbar"), "dis_ori"] <-
	    "Al Qahirah"

    # Correct some sub-district names
	  dtm[which(dtm[, "subdis"] == "Faj Atan NBhood"), "subdis"] <- "Faj Atan Nbhood"
	  dtm[which(dtm[, "subdis"] == "Al Wade'A"), "subdis"] <- "Al Wade'a"
	  dtm[which(dtm[, "subdis"] == "Ash Shu'Ayb"), "subdis"] <- "Ash Shu'ayb"
	  dtm[which(dtm[, "subdis"] == "Yafa'A"), "subdis"] <- "Yafa'a"
	  dtm[which(dtm[, "subdis"] == "Mayfa'A"), "subdis"] <- "Mayfa'a"
	  dtm[which(dtm[, "subdis"] == "Lawdar - Zarah"), "subdis"] <- "Lawdar"
	  dtm[which(dtm[, "dis"] == "Al Hali" & dtm[, "subdis"] == "Al-Hodeidah"), "subdis"] <- "Alhadida" 


  #...................................
  ## Correct a few observations where year is specified as '2014 and before' (only n = 221)
	  # assume all in January 2014 (before analysis period = makes no difference)
	table(dtm[, c("year_move", "month_move")])  
  dtm[which(dtm[, "year_move"] == "2014 and before"), "month_move"] <- 6  
  dtm[which(dtm[, "year_move"] == "2014 and before"), "year_move"] <- 2014  
	table(dtm[, c("year_move", "month_move")])  
  dtm[, "year_move"] <- as.integer(dtm[, "year_move"])

  #...................................
  ## Correct a few observations where the number of households is negative (only n = 91)
  table(dtm[, "n_hh"] < 0)
  dtm[, "n_hh"] <- abs(dtm[, "n_hh"])
  table(dtm[, "n_hh"] < 0)
    		  	  	  
  #...................................
  ## Reorder, sort and write dataset
    
    # Reorder columns
    dtm <- dtm[, c("who", "type", "date_assessment", "month_assessment", "year_assessment", "round",
      "date_move", "month_move", "year_move", "gov", "dis", "dis_pcode", "subdis", "subdis_pcode",
      "locality_en", "locality_ar", "locality_pcode", "latitude", "longitude",
      "gov_ori", "dis_ori", "dis_ori_pcode", "subdis_ori", "subdis_ori_pcode",
      "n_hh", "n_hh_conflict", "n_hh_disaster")]

    # Sort
    dtm <- dtm[order(dtm[, "who"], dtm[, "type"], dtm[, "date_assessment"], dtm[, "date_move"], dtm[, "locality_pcode"]), ]
    
    # Write
    write_excel_csv(dtm, "yem_displacement_dataset_clean_raw.csv", na = "")
    
    # Clean up unneeded objects
    rm(idp_prevalent, ret_prevalent, idp_incident, ret_incident)
    
    
#..........................................................................................
### ENDS
#..........................................................................................
    

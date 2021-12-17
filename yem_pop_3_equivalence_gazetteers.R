#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## -------- R SCRIPT TO ESTABLISH EQUIVALENCE FROM DTM DATASET TO OCHA GAZETTEER ------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Oct 2021)

                                          # francesco.checchi@lshtm.ac.uk 



#.........................................................................................                            
### Validating DTM pcodes
#.........................................................................................    

  #...................................      
  ## Locality pcodes
        
    # Test whether locality pcodes exist in CSO gazetteer
    x1 <- unique(cso[, "locality_pcode"])
    x2 <- unique(dtm[, "locality_pcode"])
    x3 <- x2 %in% x1
    table(nchar(x1))
    table(nchar(x2))
    table(x3)
      # only about 1/3rd of DTM locality pcodes are in CSO: this seems due to a difference in number of characters:
      # 99.9% of CSO pcodes are 11 characters, while 56% of DTM pcodes are 10 characters, and only 41% 11 characters

    # Which DTM locality pcodes can at least be used to identify the subdistrict, district and governorate?
    dtm[, "subdis_pcode1"] <- sapply(dtm[, "locality_pcode"], function(x) {substr(x, 1, 6)} )
    dtm[, "locality_pcode_valid"] <- dtm[, "subdis_pcode1"] %in% unique(ocha$subdis_pcode)
    table(is.na(dtm$locality_pcode), dtm$locality_pcode_valid)      
    dtm <- subset(dtm, select = -subdis_pcode1)

  #...................................      
  ## Subdistrict and district pcodes
    
    # Subdistrict pcodes
    dtm[, "subdis_pcode_valid"] <- dtm[, "subdis_pcode"] %in% unique(ocha$subdis_pcode)
    table(is.na(dtm$subdis_pcode), dtm$subdis_pcode_valid)

    dtm[, "subdis_ori_pcode_valid"] <- dtm[, "subdis_ori_pcode"] %in% unique(ocha$subdis_pcode)
    table(is.na(dtm$subdis_ori_pcode), dtm$subdis_ori_pcode_valid)
        
    # District pcodes
    dtm[, "dis_pcode_valid"] <- dtm[, "dis_pcode"] %in% unique(ocha$dis_pcode)
    table(is.na(dtm$dis_pcode), dtm$dis_pcode_valid)

    dtm[, "dis_ori_pcode_valid"] <- dtm[, "dis_ori_pcode"] %in% unique(ocha$dis_pcode)
    table(is.na(dtm$dis_ori_pcode), dtm$dis_ori_pcode_valid)

    
    
#.........................................................................................                            
### Supplementing pcodes using lower-level pcodes
#.........................................................................................    
    
  #...................................      
  ## Indices of observations with valid pcodes
    
    # Which indices contain valid pcodes?
    x1 <- which(dtm[, "dis_pcode_valid"])
    x2 <- which(dtm[, "dis_ori_pcode_valid"])
    x3 <- which(dtm[, "subdis_pcode_valid"])
    x4 <- which(dtm[, "subdis_ori_pcode_valid"])
    x5 <- which(dtm[, "locality_pcode_valid"])

    # Governorate 
    dtm[x1, "gov_pcode"] <- substr(dtm[x1, "dis_pcode"], 1, 2)
    dtm[x3, "gov_pcode"] <- substr(dtm[x3, "subdis_pcode"], 1, 2)
    dtm[x5, "gov_pcode"] <- substr(dtm[x5, "locality_pcode"], 1, 2)
    table(is.na(dtm$gov_pcode))

    dtm[x2, "gov_ori_pcode"] <- substr(dtm[x2, "dis_ori_pcode"], 1, 2)
    dtm[x4, "gov_ori_pcode"] <- substr(dtm[x4, "subdis_ori_pcode"], 1, 2)
    table(is.na(dtm$gov_ori_pcode))
    
    # District
    dtm[x1, "dis_pcode_ok"] <- dtm[x1, "dis_pcode"]
    dtm[x3, "dis_pcode_ok"] <- substr(dtm[x3, "subdis_pcode"], 1, 4)
    dtm[x5, "dis_pcode_ok"] <- substr(dtm[x5, "locality_pcode"], 1, 4)
    table(is.na(dtm$dis_pcode_ok))
    
    dtm[x2, "dis_ori_pcode_ok"] <- dtm[x2, "dis_ori_pcode"]
    dtm[x4, "dis_ori_pcode_ok"] <- substr(dtm[x4, "subdis_ori_pcode"], 1, 4)
    table(is.na(dtm$dis_ori_pcode_ok))
    
    # Subdistrict
    dtm[x3, "subdis_pcode_ok"] <- dtm[x3, "subdis_pcode"]
    dtm[x5, "subdis_pcode_ok"] <- substr(dtm[x5, "locality_pcode"], 1, 6)
    table(is.na(dtm$subdis_pcode_ok))

    dtm[x4, "subdis_ori_pcode_ok"] <- dtm[x4, "subdis_ori_pcode"]
    table(is.na(dtm$subdis_ori_pcode_ok))
    
  #...................................      
  ## Clean up
    
    # Double-check consistency among governorate, district and substrict pcodes
      # district and subdistrict
      dtm[, "cons"] <- ! is.na(dtm[, "subdis_pcode_ok"]) & ! is.na(dtm[, "dis_pcode_ok"]) & (substr(dtm[, "subdis_pcode_ok"], 1, 4) == dtm[, "dis_pcode_ok"]) 
      table(subset(dtm, ! is.na(subdis_pcode_ok) & ! is.na(dis_pcode_ok))$cons)
    
      # governorate and district
      dtm[, "cons"] <- ! is.na(dtm[, "dis_pcode_ok"]) & ! is.na(dtm[, "gov_pcode"]) & (substr(dtm[, "dis_pcode_ok"], 1, 2) == dtm[, "gov_pcode"]) 
      table(subset(dtm, ! is.na(dis_pcode_ok) & ! is.na(gov_pcode))$cons)

      # governorate and subdistrict
      dtm[, "cons"] <- ! is.na(dtm[, "subdis_pcode_ok"]) & ! is.na(dtm[, "gov_pcode"]) & (substr(dtm[, "subdis_pcode_ok"], 1, 2) == dtm[, "gov_pcode"]) 
      table(subset(dtm, ! is.na(subdis_pcode_ok) & ! is.na(gov_pcode))$cons)
    
    # Set non-valid locality pcodes to NA 
    dtm[, "locality_pcode"] <- ifelse(dtm[, "locality_pcode_valid"], dtm[, "locality_pcode"], NA)
      
    # Clean up  
    dtm <- dtm[, ! colnames(dtm) %in% c("cons", "dis_pcode", "dis_ori_pcode", "subdis_pcode", "subdis_ori_pcode")]
    x1 <- grep("_ok", colnames(dtm))
    colnames(dtm)[x1] <- gsub("_ok", "", colnames(dtm)[x1] )
    rm(x1, x2, x3, x4, x5)
  

#.........................................................................................                            
### Establishing unique combinations of governorate, district, subdistrict and  corresponding pcodes in DTM
#.........................................................................................    
        
  #...................................      
  ## Prepare equivalence dataset

    # Unique combinations in DTM
    x1 <- unique(dtm[, c("gov", "gov_pcode", "dis", "dis_pcode", "subdis", "subdis_pcode")])
    x2 <- unique(dtm[, c("gov_ori", "gov_ori_pcode", "dis_ori", "dis_ori_pcode", "subdis_ori", "subdis_ori_pcode")])
    colnames(x2) <- c("gov", "gov_pcode", "dis", "dis_pcode", "subdis", "subdis_pcode")  
    equi <- bind_rows(x1, x2)
    equi <- unique(equi)
    x1 <- apply(equi, 1, function(x) {! all(is.na(x))} )
    equi <- equi[x1, ]

    # Identify different 'cases' (missing or absent gov/dis/subdis name/pcode)
    x1 <- apply(equi, c(1,2), function (x) {if (is.na(x)) return(0) else return(1) })
    equi[, "sss"] <- paste(x1[, "gov"], x1[, "dis"], x1[, "subdis"], sep = "")
    equi[, "ppp"] <- paste(x1[, "gov_pcode"], x1[, "dis_pcode"], x1[, "subdis_pcode"], sep = "")
    equi[, "case"] <- paste(equi[, "sss"], equi[, "ppp"], sep = ".")
    table(equi$sss)
    table(equi$ppp)
    table(equi$case)    
  
    # Prepare merged names and pcodes for matching
      # names
      x1 <- which(equi[, "sss"] == "100")
      equi[x1, "names"] <- equi[x1, "gov"]
      x1 <- which(equi[, "sss"] == "110")
      equi[x1, "names"] <- paste(equi[x1, "gov"], equi[x1, "dis"], sep = ", ")
      x1 <- which(equi[, "sss"] == "111")
      equi[x1, "names"] <- paste(equi[x1, "gov"], equi[x1, "dis"], equi[x1, "subdis"], sep = ", ")
      
      # pcodes
      x1 <- which(equi[, "ppp"] == "000")
      equi[x1, "pcodes"] <- NA
      x1 <- which(equi[, "ppp"] == "110")
      equi[x1, "pcodes"] <- paste(equi[x1, "gov_pcode"], equi[x1, "dis_pcode"], sep = ", ")
      x1 <- which(equi[, "ppp"] == "111")
      equi[x1, "pcodes"] <- paste(equi[x1, "gov_pcode"], equi[x1, "dis_pcode"], equi[x1, "subdis_pcode"], sep = ", ")
      

#.........................................................................................                            
### Finding tentative OCHA matches for each case  (WARNING! SLOW - ABOUT 5 MIN ON A STANDARD LAPTOP)
#.........................................................................................    
       
  #...................................      
  ## Approximate name matches
    
    # If governorate only is available
    x1 <- which(equi[, "sss"] == "100")
    equi[x1, "match_names"] <- f_fuzzy(equi[x1, "names"], unique(ocha[, "gov_en_ocha"]), methods)
    x1 <- which(equi[, "sss"] == "110")
    
    # If governorate and district only are available
    x2 <- unique(paste(ocha[, "gov_en_ocha"], ocha[, "dis_en_ocha"], sep = ", "))
    equi[x1, "match_names"] <- f_fuzzy(equi[x1, "names"], x2, methods)
    
    # If governorate, district and subdistrict are all available
    x1 <- which(equi[, "sss"] == "111")
    x2 <- unique(paste(ocha[, "gov_en_ocha"], ocha[, "dis_en_ocha"], ocha[, "subdis_en_nonunique_ocha"], sep = ", "))
      # note: use non-unique version of OCHA subdistrict as this is how DTM is likely to spell it
    x3 <- length(x1)
    for (i in 1:x3) {
      loop.tracker(i, x3)
      equi[x1[i], "match_names"] <- f_fuzzy(equi[x1[i], "names"], x2, methods)
    }

  #...................................      
  ## Exact pcode matches
          
    # Match pcodes
      # if no pcode is available
      x1 <- which(equi[, "ppp"] == "000")
      equi[x1, "match_pcodes_09"] <- NA
      
      # if only governorate pcode is available
      x1 <- which(equi[, "ppp"] == "110")
      
      # if only governorate and district pcodes are available
      x2 <- unique(paste(ocha[, "gov_pcode"], ocha[, "dis_pcode"], sep = ", "))
      equi[x1, "match_pcodes_09"] <- x2[match(equi[x1, "pcodes"], x2)]
      
      # if governorate, district and subdistrict pcodes are all available
      x1 <- which(equi[, "ppp"] == "111")
      x2 <- unique(paste(ocha[, "gov_pcode"], ocha[, "dis_pcode"], ocha[, "subdis_pcode"], sep = ", "))
      equi[x1, "match_pcodes_09"] <- x2[match(equi[x1, "pcodes"], x2)]      

    # Find corresponding matching names
    equi[, "match_pcodes"] <- NA
    x1 <- sapply(equi[, "match_pcodes_09"], function(x) {strsplit(x, ",")})
    for (i in 1:length(x1) ) {
      x2 <- trimws(x1[[i]])
        x3 <- NA
        if (! is.na(x2[1])) {
          if (length(x2) > 0) {x3 <- unique(ocha[which(ocha[, "gov_pcode"] == x2[1]), "gov_en_ocha"]) }
          if (length(x2) > 1) {x3 <- paste(x3, unique(ocha[which(ocha[, "dis_pcode"] == x2[2]), "dis_en_ocha"]), sep = ", ") }
          if (length(x2) > 2) {x3 <- paste(x3, unique(ocha[which(ocha[, "subdis_pcode"] == x2[3]), "subdis_en_nonunique_ocha"]), sep = ", ") }
        }
      equi[i, "match_pcodes"] <- x3
    }

 
#.........................................................................................                            
### Reviewing matches so far and applying automatic match criteria
#.........................................................................................    
       
  #...................................      
  ## Preparatory steps
        
    # Prepare classifications
    equi[, c("match", "criterion")] <- "none"
    
    # Calculate distances for matches      
      # name matches' distance
      equi[, "distance_s"] <- apply(equi[, c("names", "match_names")], 1, 
        function(x) {stringdist(x[1], x[2])})
      table(equi$distance_s)
      
      # pcode matches' distance
      for (i in 1:nrow(equi)) {
        if (equi[i, "case"] %in% c("110.111", "111.110") ) {
          x1 <- trimws(unlist(strsplit(equi[i, "names"], ", ")))[c(1,2)]
          x2 <- trimws(unlist(strsplit(equi[i, "match_pcodes"], ", ")))[c(1,2)]
          equi[i, "distance_p"] <- stringdist(paste(x1[1], x1[2]), paste(x2[1], x2[2]) )
        }
        if (equi[i, "case"] %in% c("111.111", "110.110") ) {
          equi[i, "distance_p"] <- stringdist(equi[i, "names"], equi[i, "match_pcodes"])
        }
        if (equi[i, "ppp"] == "000" ) {
          equi[i, "distance_p"] <- NA
        }
      }
      table(equi$distance_p)

    # Identify probable mismatches based on maximum tolerated string distance (or equal pcode match)
      # maximum tolerated distance as a function of how many names are available
      equi[, c("distance_tol_s", "distance_tol_p")] <- NA
      equi[which(equi[, "case"] == "100.000"), "distance_tol_s"] <- distance_max
      equi[which(equi[, "case"] == "110.000"), "distance_tol_s"] <- distance_max * 2
      equi[which(equi[, "case"] == "111.000"), "distance_tol_s"] <- distance_max * 3
      equi[which(equi[, "case"] == "110.110"), "distance_tol_s"] <- distance_max * 2
      equi[which(equi[, "case"] == "110.111"), "distance_tol_s"] <- distance_max * 2
      equi[which(equi[, "case"] == "111.110"), "distance_tol_s"] <- distance_max * 3
      equi[which(equi[, "case"] == "111.111"), "distance_tol_s"] <- distance_max * 3

      equi[which(equi[, "case"] == "110.110"), "distance_tol_p"] <- distance_max * 2
      equi[which(equi[, "case"] == "110.111"), "distance_tol_p"] <- distance_max * 2
      equi[which(equi[, "case"] == "111.110"), "distance_tol_p"] <- distance_max * 2
      equi[which(equi[, "case"] == "111.111"), "distance_tol_p"] <- distance_max * 3
      
      # probable mismatches
      equi[, c("mismatch_p", "mismatch_s")] <- 999
      equi[which(equi[, "pcodes"] == equi[, "match_pcodes_09"]), "mismatch_p"] <- 0
      equi[which(equi[, "distance_s"] <= equi[, "distance_tol_s"]), "mismatch_s"] <- 0
      equi[which(equi[, "pcodes"] != equi[, "match_pcodes_09"]), "mismatch_p"] <- 1
      equi[which(equi[, "distance_s"] > equi[, "distance_tol_s"]), "mismatch_s"] <- 1
      table(equi[, c("mismatch_s", "mismatch_p")])

  #...................................      
  ## Criterion 1: if pcodes match and pcode names = DTM names, there is a definite match  
  
  for (i in 1:nrow(equi)) {
    # if mismatch, skip to next observation
    if (equi[i, "mismatch_p"] != 0) {next}
    
    # otherwise, consider distance
    if (equi[i, "distance_p"] <=  equi[i, "distance_tol_p"]) {equi[i, "match"] <- "p"; equi[i, "criterion"] <- "1"}
  }
  
  table(equi[, c("match", "criterion")])
  
  
  #...................................      
  ## Criterion 2: if there are no pcodes but string names match, there is a definite match  
  
  for (i in 1:nrow(equi)) {
    # if mismatch or match already attributed, skip to next observation
    if (equi[i, "mismatch_s"] != 0 | equi[i, "match"] == "p") {next}
    
    # otherwise check for a string match
    if (equi[i, "case"] %in% c("100.000", "110.000", "111.000") ) 
      {equi[i, "match"] <- "s"; equi[i, "criterion"] <- "2"}
  }

  table(equi[, c("match", "criterion")])

  #...................................      
  ## Criterion 3: if pcodes match and at least gov and subdis names match, there is a definite match (111.111 only)
   
  for (i in 1:nrow(equi)) {
    # if match already attributed, skip to next observation
    if (equi[i, "match"] != "none") {next}
    
    # otherwise check for a string match
    if (equi[i, "case"] == "111.111") {
      x1 <- trimws(unlist(strsplit(equi[i, "names"], ", ")))[c(1,2)]
      x2 <- trimws(unlist(strsplit(equi[i, "match_pcodes"], ", ")))[c(1,2)]
      x3 <- stringdist( paste(x1[1], x1[2]), paste(x2[1], x2[2]) )
      if (x3 <= (distance_max * 2)) {equi[i, "match"] <- "p"; equi[i, "criterion"] <- "3"}
    }
  }

  table(equi[, c("match", "criterion")])
  
  
#.........................................................................................                            
### Performing focussed matching for as-yet unmatched observations
#.........................................................................................    
       
  #...................................      
  ## Preparatory steps
    
    # Select only unmatched observations
    equi_u <- subset(equi, match == "none")
      # put aside old unmatched geographic names and pcodes
      x1 <- c("gov", "gov_pcode", "dis", "dis_pcode", "subdis", "subdis_pcode")
      equi_u[, paste(x1, "_dtm", sep = "")] <- equi_u[, x1]
    
    # Unique OCHA match candidates, by geographic level
    gov_o <- unique(ocha[, "gov_en_ocha"])
    dis_o <- unique(ocha[, c("gov_en_ocha", "dis_en_ocha")])
    subdis_o <- unique(ocha[, c("gov_en_ocha", "dis_en_ocha", "subdis_en_nonunique_ocha")])

  #...................................      
  ## First, match governorates...
    
    # Approximate matching
    gov_u <- unique(equi_u[, "gov"])
    gov_u <- data.frame("gov" = gov_u, "gov_match" = f_fuzzy(gov_u, gov_o, methods) )
    gov_u[, "distance"] <- apply(gov_u[, c("gov", "gov_match")], 1, function(x) {stringdist(x[1], x[2])})
    gov_u[, "mismatch"] <- ifelse(gov_u[, "distance"] > distance_max, 1, 0)
  
    # Apply manual corrections
    gov_u[which(gov_u[, "gov"] == "Amanat Al Asimah"), "gov_match"] <- "Sana'a City"
  
    # Now replace correct governorates in unmatched dataset
    equi_u <- merge(equi_u, gov_u[, c("gov", "gov_match")], by = "gov", all.x = TRUE)
    equi_u[, "gov"] <- equi_u[, "gov_match"]
    equi_u <- subset(equi_u, select = - gov_match)

  #...................................      
  ## Second, match districts by only searching for matches within governorates
    
    # Approximate matching
    dis_u <- unique(equi_u[, c("gov", "dis")])
    dis_u[, "gov_match"] <- dis_u[, "gov"]
    dis_u[, "dis_match"] <- NA
    for (i in unique(equi_u[, "gov"])) {
      # subset of observations within governorate
      x1 <- subset(dis_u, gov == i)
      x2 <- subset(dis_o, gov_en_ocha == i)
      dis_u[which(dis_u[, "gov"] == i), "dis_match"] <- f_fuzzy(x1[, "dis"], x2[, "dis_en_ocha"], methods)
    }  
    dis_u[, "distance"] <- apply(dis_u[, c("dis", "dis_match")], 1, function(x) {stringdist(x[1], x[2])})
    dis_u[, "mismatch"] <- ifelse(dis_u[, "distance"] > distance_max, 1, 0)
    
    # Apply manual corrections
    dis_u[which(dis_u[, "gov"] == "Sana'a" & dis_u[, "dis"] == "Sanhan"), "dis_match"] <- "Sanhan wa Bani Bahlul"
    dis_u[which(dis_u[, "gov"] == "Amran" & dis_u[, "dis"] == "Al Qaflah"), "dis_match"] <- "Qaflat Odhar"
    dis_u[which(dis_u[, "gov"] == "Ma'rib" & dis_u[, "dis"] == "Medghal"), "dis_match"] <- "Madghal Al Jid'an"  
    
    dis_u[which(dis_u[, "gov"] == "Al Maharah" & dis_u[, "dis"] == "Az'zal"), c("gov_match", "dis_match")] <- 
      c("Sana'a City", "Azaal")
    dis_u[which(dis_u[, "gov"] == "Hadramawt" & dis_u[, "dis"] == "Hidaybu"), c("gov_match", "dis_match")] <- 
      c("Socotra", "Hadibu")

    dis_u[which(dis_u[, "gov"] == "Ad Dali'" & dis_u[, "dis"] == "Al Makhadir"), "gov_match"] <- "Ibb"
    dis_u[which(dis_u[, "gov"] == "Aden" & dis_u[, "dis"] == "Tur Al Bahah"), "gov_match"] <- "Lahj"
    dis_u[which(dis_u[, "gov"] == "Sana'a" & dis_u[, "dis"] == "Al Ghaydhah"), "gov_match"] <- "Al Maharah"
    dis_u[which(dis_u[, "gov"] == "Sana'a" & dis_u[, "dis"] == "As Saddah"), "gov_match"] <- "Ibb"
    dis_u[which(dis_u[, "gov"] == "Sana'a" & dis_u[, "dis"] == "Marib City"), "gov_match"] <- "Ma'rib"

    # Now replace correct governorates and districts in unmatched dataset
    equi_u <- merge(equi_u, dis_u[, c("gov", "dis", "gov_match", "dis_match")], by = c("gov", "dis"), all.x = TRUE)
    equi_u[, "gov"] <- equi_u[, "gov_match"]
    equi_u[, "dis"] <- equi_u[, "dis_match"]
    equi_u <- subset(equi_u, select = - gov_match)
    equi_u <- subset(equi_u, select = - dis_match)

  #...................................      
  ## Third and last, match subdistricts by only searching for matches within governorates and districts
    
    # Approximate matching
    subdis_u <- unique(equi_u[, c("gov", "dis", "subdis")])
    subdis_u[, c("gov_match", "dis_match")] <- subdis_u[, c("gov", "dis")]
    subdis_u[, "subdis_match"] <- NA
    for (i in unique(equi_u[, "gov"])) {
      for (j in unique(subset(equi_u, gov == i)[, "dis"]) ) {
        # subset of observations within governorate
        x1 <- subset(subdis_u, gov == i & dis == j)
        x2 <- subset(subdis_o, gov_en_ocha == i & dis_en_ocha == j)
        subdis_u[which(subdis_u[, "gov"] == i & subdis_u[, "dis"] == j), "subdis_match"] <- 
          f_fuzzy(x1[, "subdis"], x2[, "subdis_en_nonunique_ocha"], methods)
      }  
    }  
    subdis_u[, "distance"] <- apply(subdis_u[, c("subdis", "subdis_match")], 1, function(x) {stringdist(x[1], x[2])})
    subdis_u[, "mismatch"] <- ifelse(subdis_u[, "distance"] > distance_max, 1, 0)
    
    # Apply manual corrections
    subdis_u[which(is.na(subdis_u[, "subdis"])), "subdis_match"] <- NA
    subdis_u[which(subdis_u[, "gov"] == "Al Hodeidah" & subdis_u[, "dis"] == "Jabal Ras" & subdis_u[, "subdis"] == "Alkuluh"),
      "subdis_match"] <- "Al Kawlah"
    subdis_u[which(subdis_u[, "gov"] == "Al Mahwit" & subdis_u[, "dis"] == "Melhan" & subdis_u[, "subdis"] == "Alrruduh"),
      "subdis_match"] <- "Ar Rawdah"
    subdis_u[which(subdis_u[, "gov"] == "Ibb" & subdis_u[, "dis"] == "Hobeish" & subdis_u[, "subdis"] == "Alnnahih"),
      "subdis_match"] <- "An Nahyah"
    subdis_u[which(subdis_u[, "gov"] == "Al Hodeidah" & subdis_u[, "dis"] == "Alluhayah" & subdis_u[, "subdis"] == "Albieajih"),
      "subdis_match"] <- "Al Bu'jyah"
    subdis_u[which(subdis_u[, "gov"] == "Al Hodeidah" & subdis_u[, "dis"] == "Bayt Al Faqih" & subdis_u[, "subdis"] == "Almaeazibuh"),
      "subdis_match"] <- "Bani Mohammad wa Al Ma'azibah"
    subdis_u[which(subdis_u[, "gov"] == "Ma'rib" & subdis_u[, "dis"] == "Al Abdiyah" & subdis_u[, "subdis"] == "Al Alseydy"),
      "subdis_match"] <- "Aal As Sa'idi"
    subdis_u[which(subdis_u[, "gov"] == "Sana'a City" & subdis_u[, "dis"] == "Shu'ub"),
      "subdis_match"] <- "Shu'ub"
    subdis_u[which(subdis_u[, "gov"] == "Sana'a City" & subdis_u[, "dis"] == "Al Wehdah"),
      "subdis_match"] <- "Al Wehdah"
    subdis_u[which(subdis_u[, "gov"] == "Sana'a City" & subdis_u[, "dis"] == "As Sab'in"),
      "subdis_match"] <- "As Sab'in"
    subdis_u[which(subdis_u[, "gov"] == "Sana'a City" & subdis_u[, "dis"] == "Old City"),
      "subdis_match"] <- "Old City"
    subdis_u[which(subdis_u[, "gov"] == "Sana'a City" & subdis_u[, "dis"] == "Ma'in"),
      "subdis_match"] <- "Ma'in"
    subdis_u[which(subdis_u[, "gov"] == "Sana'a City" & subdis_u[, "dis"] == "Azaal"),
      "subdis_match"] <- "Azaal"
    subdis_u[which(subdis_u[, "gov"] == "Ta'iz" & subdis_u[, "dis"] == "Al Mudhaffar"),
      "subdis_match"] <- "Al Mudhaffar"
    subdis_u[which(subdis_u[, "gov"] == "Ta'iz" & subdis_u[, "dis"] == "Al Qahirah"),
      "subdis_match"] <- "Al Qahirah"
    subdis_u[which(subdis_u[, "gov"] == "Ibb" & subdis_u[, "dis"] == "An Nadirah" & subdis_u[, "subdis"] == "Shaeb Almarysaa"),
      "subdis_match"] <- "Sha'b Al Muraysi"
    subdis_u[which(subdis_u[, "gov"] == "Ta'iz" & subdis_u[, "dis"] == "Ash Shamayatayn" & subdis_u[, "subdis"] == "Alqarishuh"),
      "subdis_match"] <- "Al Qurayshah"
    subdis_u[which(subdis_u[, "gov"] == "Ta'iz" & subdis_u[, "dis"] == "Ash Shamayatayn" & subdis_u[, "subdis"] == "Alshshamaya Algharbia"),
      "subdis_match"] <- "Ash Shamayah Al Gharbyah"
    subdis_u[which(subdis_u[, "gov"] == "Ta'iz" & subdis_u[, "dis"] == "At Ta'iziyah" & subdis_u[, "subdis"] == "Alshshueabaniuh Aleulya"),
      "subdis_match"] <- "Ash Sha'banyah Al Olya"
    subdis_u[which(subdis_u[, "gov"] == "Ta'iz" & subdis_u[, "dis"] == "At Ta'iziyah" & subdis_u[, "subdis"] == "Alshshueabaniuh Alssufli"),
      "subdis_match"] <- "Ash Sha'banyah As Sufla"
    subdis_u[which(subdis_u[, "gov"] == "Al Hodeidah" & subdis_u[, "dis"] == "Bayt Al Faqih" & subdis_u[, "subdis"] == "Alttaraf Alshamy"),
      "subdis_match"] <- "At Taraf Ash Shami"
    subdis_u[which(subdis_u[, "gov"] == "Ta'iz" & subdis_u[, "dis"] == "Al Wazi'yah" & subdis_u[, "subdis"] == "Alzzarifuh"),
      "subdis_match"] <- "Adh Dharifah"
    subdis_u[which(subdis_u[, "gov"] == "Ta'iz" & subdis_u[, "dis"] == "Mawiyah" & subdis_u[, "subdis"] == "Sayilh Suaraq"),
      "subdis_match"] <- "Sa'lat Sawraq"
    subdis_u[which(subdis_u[, "gov"] == "Abyan" & subdis_u[, "dis"] == "Rassd" & subdis_u[, "subdis"] == "Rasad_2"),
      "subdis_match"] <- "Al Qarrah"
    subdis_u[which(subdis_u[, "gov"] == "Al Hodeidah" & subdis_u[, "dis"] == "At Tuhayta" & subdis_u[, "subdis"] == "Alddarihmaa"),
      c("dis_match", "subdis_match")] <- c("Ad Durayhimi", "Ad Durayhimi")
    subdis_u[which(subdis_u[, "gov"] == "Al Maharah" & subdis_u[, "dis"] == "Shahin" & subdis_u[, "subdis"] == "Shahan_1"),
      "subdis_match"] <- "Habrut"
    subdis_u[which(subdis_u[, "gov"] == "Hadramawt" & subdis_u[, "dis"] == "Daw'an" & subdis_u[, "subdis"] == "Daw'an_1"),
      "subdis_match"] <- "Saif"
    subdis_u[which(subdis_u[, "gov"] == "Lahj" & subdis_u[, "dis"] == "Tuban"),
      "subdis_match"] <- "Al Hawtah"

    # Now replace correct governorates, districts and subdistricts in unmatched dataset
    equi_u <- merge(equi_u, subdis_u[, c("gov", "dis", "subdis", "gov_match", "dis_match", "subdis_match")], 
      by = c("gov", "dis", "subdis"), all.x = TRUE)
    equi_u[, "gov"] <- equi_u[, "gov_match"]
    equi_u[, "dis"] <- equi_u[, "dis_match"]
    equi_u[, "subdis"] <- equi_u[, "subdis_match"]
    
    # Attribute match and criterion variables
    equi_u[, "match"] <- "other"
    equi_u[, "criterion"] <- "4"
      

#.........................................................................................                            
### Cleaning up and assembling final equivalence dataset; applying equivalence to DTM
#.........................................................................................    
            
  #...................................      
  ## Clean up dataset matched through criteria 1, 2 and 3
      
    # Preparatory steps
      # select observations
      equi_m <- subset(equi, criterion %in% c("1", "2", "3"))
      
      # put aside old unmatched geographic names and pcodes
      x1 <- c("gov", "gov_pcode", "dis", "dis_pcode", "subdis", "subdis_pcode")
      equi_m[, paste(x1, "_dtm", sep = "")] <- equi_m[, x1]
    
      # define matched OCHA names
      equi_m[, c("gov_en_ocha", "dis_en_ocha", "subdis_en_nonunique_ocha")] <- NA
      
    # If match == "p", use pcode matches to come up with OCHA governorate, district and subdistrict matches
      # else use string matches
    for (i in 1:nrow(equi_m)) {
      if (equi_m[i, "match"] == "p") { x1 <- trimws(unlist(strsplit(equi_m[i, "match_pcodes"], ", "))) }
      if (equi_m[i, "match"] == "s") { x1 <- trimws(unlist(strsplit(equi_m[i, "match_names"], ", "))) }        
      if (length(x1) > 0) {equi_m[i, "gov_en_ocha"] <- x1[1]}
      if (length(x1) > 1) {equi_m[i, "dis_en_ocha"] <- x1[2]}
      if (length(x1) > 2) {equi_m[i, "subdis_en_nonunique_ocha"] <- x1[3]}
    }
    
    # Eliminate unneeded columns
    x1 <- c("gov_dtm", "gov_pcode_dtm", "dis_dtm", "dis_pcode_dtm", "subdis_dtm", "subdis_pcode_dtm",
      "criterion", "gov_en_ocha", "dis_en_ocha", "subdis_en_nonunique_ocha")
    equi_m <- equi_m[, x1]

    
  #...................................      
  ## Clean up dataset matched through criterion 4
    
    # Define matched OCHA names
    equi_u[, c("gov_en_ocha", "dis_en_ocha", "subdis_en_nonunique_ocha")] <- equi_u[, c("gov", "dis", "subdis")]

    # Eliminate unneeded columns
    x1 <- c("gov_dtm", "gov_pcode_dtm", "dis_dtm", "dis_pcode_dtm", "subdis_dtm", "subdis_pcode_dtm",
      "criterion", "gov_en_ocha", "dis_en_ocha", "subdis_en_nonunique_ocha")
    equi_u <- equi_u[, x1]
      

  #...................................      
  ## Bind both datasets together, add other OCHA variables, sort and save
      
    # Bind together
    equi_final <- bind_rows(equi_m, equi_u)
    equi_final <- unique(equi_final)
    
    # Merge in other OCHA variables
    x1 <- c("gov_pcode", "dis_pcode", "subdis_pcode")
    colnames(ocha)[colnames(ocha) %in% x1] <- paste(x1, "_ocha", sep = "")
    equi_final <- merge(equi_final, unique(ocha[, c("gov_en_ocha", "gov_ar_ocha", "gov_pcode_ocha")]), 
      by = "gov_en_ocha", all.x = TRUE)
    equi_final <- merge(equi_final, unique(ocha[, c("gov_en_ocha", "dis_en_ocha", "dis_ar_ocha", "dis_pcode_ocha")]), 
      by = c("gov_en_ocha", "dis_en_ocha"), all.x = TRUE)
    equi_final <- merge(equi_final, unique(ocha[, c("gov_en_ocha", "dis_en_ocha", "subdis_en_nonunique_ocha", 
      "subdis_ar_nonunique_ocha", "subdis_en_ocha", "subdis_ar_ocha", "subdis_pcode_ocha")]), 
      by = c("gov_en_ocha", "dis_en_ocha", "subdis_en_nonunique_ocha"), all.x = TRUE)
    
    # Column order and sort
    x1 <- c("gov_dtm", "gov_pcode_dtm", "dis_dtm", "dis_pcode_dtm", "subdis_dtm", "subdis_pcode_dtm", "criterion",
      "gov_en_ocha", "gov_ar_ocha", "gov_pcode_ocha", "dis_en_ocha", "dis_ar_ocha", "dis_pcode_ocha",
      "subdis_en_ocha", "subdis_en_nonunique_ocha", "subdis_ar_ocha", "subdis_ar_nonunique_ocha", "subdis_pcode_ocha")
    equi_final <- equi_final[, x1]
    equi_final <- equi_final[order(equi_final[, "gov_dtm"], equi_final[, "dis_dtm"], equi_final[, "subdis_dtm"]), ]

    # Write
    write_excel_csv(equi_final, "yem_pop_equi_dtm_ocha.csv", na = "")
                

  #...................................      
  ## Apply equivalence to DTM and clean up old place name columns

    # Prepare datasets for merging  
      # rename columns in DTM
      x1 <- c("gov", "gov_pcode", "dis", "dis_pcode", "subdis", "subdis_pcode", 
        "gov_ori", "gov_ori_pcode", "dis_ori", "dis_ori_pcode", "subdis_ori", "subdis_ori_pcode")
      dtm[, paste(x1, "_dtm", sep = "")] <- dtm[, x1]

      # remove unneeded columns in DTM
      x1 <- c(x1, "locality_pcode_valid", grep("_valid", colnames(dtm), value = TRUE))
      dtm <- dtm[, ! colnames(dtm) %in% x1]  
      
      # create extra equivalence databases for merging with arrival and origin places
      equi_merge <- equi_final
      x1 <- c("criterion", "gov_ar_ocha", "dis_ar_ocha", "subdis_en_nonunique_ocha", "subdis_ar_ocha",
        "subdis_ar_nonunique_ocha")
      equi_merge <- equi_merge[, ! colnames(equi_merge) %in% x1]      
      equi_merge_ori <- equi_merge
      colnames(equi_merge_ori) <- sapply(colnames(equi_merge_ori), function(x) {gsub("gov", "gov_ori", x)})
      colnames(equi_merge_ori) <- sapply(colnames(equi_merge_ori), function(x) {gsub("dis", "dis_ori", x)})
      
    # Merge
    dtm <- merge(dtm, equi_merge, by = grep("_dtm", colnames(equi_merge), value = TRUE), all.x = TRUE)
    dtm <- merge(dtm, equi_merge_ori, by = grep("_dtm", colnames(equi_merge_ori), value = TRUE), all.x = TRUE)
      
    # Remove old place columns
    x1 <- grep("_dtm", colnames(dtm))
    dtm <- dtm[, -x1]
      
    
  #...................................
  ## Reorder, sort and write dataset
    
    # Reorder columns
    dtm <- dtm[, c("who", "type", "date_assessment", "month_assessment", "year_assessment", "round",
      "date_move", "month_move", "year_move", "gov_en_ocha", "gov_pcode_ocha", "dis_en_ocha", "dis_pcode_ocha",
      "subdis_en_ocha", "subdis_pcode_ocha", "locality_en", "locality_ar", "locality_pcode", "latitude", "longitude",
      "gov_ori_en_ocha", "gov_ori_pcode_ocha", "dis_ori_en_ocha", "dis_ori_pcode_ocha", "subdis_ori_en_ocha", 
      "subdis_ori_pcode_ocha", "n_hh", "n_ppl", "n_hh_conflict", "n_ppl_conflict", "n_hh_disaster", "n_ppl_disaster")]

    # Sort
    dtm <- dtm[order(dtm[, "who"], dtm[, "type"], dtm[, "date_assessment"], dtm[, "date_move"], 
      dtm[, "gov_en_ocha"], dtm[, "dis_en_ocha"], dtm[, "subdis_en_ocha"]), ]
    
    # Write
    write_excel_csv(dtm, "yem_displacement_dataset_clean_equivalences.csv", na = "")
    
    # Clean up unneeded objects
    rm(dis_o, dis_u, equi, equi_final, equi_m, equi_merge, equi_merge_ori, equi_u, gov_u, subdis_o, subdis_u)
      
      
#.........................................................................................                            
### Creating equivalence: CSO governorates, districts and subdistricts to OCHA
#.........................................................................................    

  #...................................      
  ## Prepare databases
    # OCHA
    colnames(ocha)[grep("_pcode", colnames(ocha))] <- gsub("_ocha", "", colnames(ocha)[grep("_pcode", colnames(ocha))])
    
    # CSO
    colnames(cso)[grep("_pcode", colnames(cso))] <- gsub("_cso", "", colnames(cso)[grep("_pcode", colnames(cso))])
    for (i in c("gov", "dis", "subdis") ) {
      cso[, paste(i, "_ascii_cso", sep = "")] <- stri_trans_general(cso[, paste(i, "_ar_cso", sep = "")], "Arab-Latin")
      cso[, paste(i, "_ascii_cso", sep = "")] <- stri_trans_general(cso[, paste(i, "_ascii_cso", sep = "")], "Latin-ASCII")
    }
        
  #...................................      
  ## Explore differences between the two gazetteers
    
    # Any differences between governorates?
    length(unique(ocha[, "gov_pcode"]))
    length(unique(cso[, "gov_pcode"]))
    
    x1 <- merge(unique(cso[, c("gov_ar_cso", "gov_pcode")]), unique(ocha[, c("gov_ar_ocha", "gov_pcode")]), 
      by = "gov_pcode", all = TRUE )
    for (i in c("cso", "ocha")) {
      x1[, paste("gov_ascii_", i, sep = "")] <- stri_trans_general(x1[, paste("gov_ar_", i, sep = "")], "Arab-Latin")        
    }
    View(x1)
      # no (just minor spelling differences)
    
    # Any differences between districts?
    length(unique(ocha[, "dis_pcode"]))
    length(unique(cso[, "dis_pcode"]))
    
    x1 <- merge(unique(cso[, c("dis_ar_cso", "dis_pcode")]), unique(ocha[, c("dis_ar_ocha", "dis_pcode")]), 
      by = "dis_pcode", all = TRUE )
    for (i in c("cso", "ocha")) {
      x1[, paste("dis_ascii_", i, sep = "")] <- stri_trans_general(x1[, paste("dis_ar_", i, sep = "")], "Arab-Latin")        
    }
    x1[, "distance"] <- stringdist(x1[, "dis_ascii_ocha"], x1[, "dis_ascii_cso"])
    View(x1)
      # no (just minor spelling differences)  
    
    # Any differences between subdistricts?
    length(unique(ocha[, "subdis_pcode"]))
    length(unique(cso[, "subdis_pcode"]))
    table(unique(cso[, "subdis_pcode"]) %in% unique(ocha[, "subdis_pcode"]))
    table(unique(ocha[, "subdis_pcode"]) %in% unique(cso[, "subdis_pcode"]))
      # yes; 149 extra subdistricts in CSO - all consist of islands ('jazira') in Hajjah, Hodeidah, Shabwah and Taiz governorates
      x1 <- unique(cso[! cso[, "subdis_pcode"] %in% unique(ocha[, "subdis_pcode"]), 
        c("gov_ascii_cso", "dis_ascii_cso", "subdis_ascii_cso")])
      View(x1)
      table(x1[, "gov_ascii_cso"])

  #...................................      
  ## Merge OCHA into CSO for subdistricts shared by both

    # Merge with OCHA
    cso <- merge(cso, ocha, by = c("gov_pcode", "dis_pcode", "subdis_pcode"), all.x = TRUE)
     
      
  #...................................      
  ## Attribute extra CSO subdistricts to an OCHA subdistrict
    
    # Which governorates to look within  
    x1 <- which(! cso[, "subdis_pcode"] %in% unique(ocha[, "subdis_pcode"]) )
    x2 <- table(cso[x1, "subdis_pcode"], cso[x1, "gov_ascii_cso"])
    
    # Manual corrections
    x3 <- colnames(ocha)[colnames(ocha) != "subdis_pcode"]  
      # Hajjah governorate
      x1 <- cso[, "subdis_pcode"] %in% names(x2[x2[, "hjt"] > 0, "hjt"])
      View(cso[x1, ])  
        # all extras go under OCHA subdis_pcode = 170349 ("Midi islands")
        cso[x1, x3] <-  ocha[ocha[, "subdis_pcode"] == "170349", x3] 
        
      # Al Hodeidah governorate
      x1 <- cso[, "subdis_pcode"] %in% names(x2[x2[, "alhdydh"] > 0, "alhdydh"])
      View(cso[x1, ])  
        # district 'alslyf': all under OCHA subdis_pcode = 180421 ("As Salif")
        cso[cso[, "dis_pcode"] == "1804", x3] <-  ocha[ocha[, "subdis_pcode"] == "180421", x3] 
        # all other districts remain without OCHA match (possibly uninhabited islands)    
      
      # Shabwah governorate
      x1 <- cso[, "subdis_pcode"] %in% names(x2[x2[, "shbwh"] > 0, "shbwh"])
      View(cso[x1, ])  
        # all extras go under OCHA subdis_pcode = 211721 ("Radum")
        cso[x1, x3] <-  ocha[ocha[, "subdis_pcode"] == "211721", x3] 
      
      # Ta'iz governorate
      x1 <- cso[, "subdis_pcode"] == "150525"
      View(cso[x1, ])  
        # to remain without OCHA match (possibly uninhabited island)

  #...................................      
  ## Write file
  write_excel_csv(cso, "yem_cso_ocha_equivalences.csv")    

      
#..........................................................................................
### ENDS
#..........................................................................................
    

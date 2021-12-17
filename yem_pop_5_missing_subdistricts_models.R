#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## ------ R SCRIPT TO IDENTIFY MISSING SUBDISTRICTS BASED ON PREDICTIVE MODELLING ------ ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Oct 2021)

                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................                            
### Preparing model predictors
#.........................................................................................    
    
  #...................................      
  ## Rename OCHA variables
  colnames(ocha)[colnames(ocha) == "gov_pcode"] <- "gov_pcode_ocha"
  colnames(ocha)[colnames(ocha) == "dis_pcode"] <- "dis_pcode_ocha"
  colnames(ocha)[colnames(ocha) == "subdis_pcode"] <- "subdis_pcode_ocha"
    
  #...................................      
  ## Prepare population predictor
    
    # Select variables of interest
    pop_pred <- pop_worldpop[, c("subdis_pcode", "year", "month", "pop")]
    colnames(pop_pred)[colnames(pop_pred) == "subdis_pcode"] <- "subdis_pcode_ocha"

    # Add time units and districts
    pop_pred <- merge(pop_pred, t_units, by = c("year", "month"), all.x = TRUE)
    pop_pred <- merge(pop_pred, ocha[, c("dis_en_ocha", "subdis_en_ocha", "subdis_pcode_ocha")], by = "subdis_pcode_ocha", 
      all.x = TRUE)
    colnames(pop_pred)[colnames(pop_pred) == "subdis_pcode_ocha"] <- "subdis_pcode"
    
    # Interpolate and extrapolate so as to come up with monthly values per subdistrict
    pop_pred <- merge(ts[, c("tm", "year", "month", "subdis_pcode", "dis_en_ocha", "subdis_en_ocha")], pop_pred, 
      by = c("tm", "year", "month", "subdis_pcode", "dis_en_ocha", "subdis_en_ocha"), all.x = TRUE )
    pop_pred <- pop_pred[order(pop_pred[, "subdis_en_ocha"], pop_pred[, "tm"]), ]
    x1 <- by(pop_pred, pop_pred[, "subdis_pcode"], 
      function(i) {spline(na.omit(i[, c("tm", "pop")]), xout = i[, "tm"], method = "natural")})
    x2 <- do.call("rbind.data.frame", x1)
    colnames(x2) <- c("tm", "pop_ipol")
    x2[, "subdis_pcode"] <- substr(row.names(x2), 1, 6)
    pop_pred <- merge(pop_pred, x2, by = c("tm", "subdis_pcode"), all.x = TRUE)
    pop_pred <- pop_pred[order(pop_pred[, "subdis_en_ocha"], pop_pred[, "tm"]), ]
    pop_pred <- pop_pred[, colnames(pop_pred) != "pop"]
    colnames(pop_pred)[colnames(pop_pred) == "pop_ipol"] <- "pop"
    pop_pred[, "pop"] <- round(pop_pred[, "pop"], 0)
    
    # Tabulate populations and compute relative proportions within districts
    x1 <- aggregate(pop_pred[, "pop"], by = pop_pred[, c("tm", "dis_en_ocha")], FUN = sum )
    colnames(x1) <- c("tm", "dis_en_ocha", "pop_tot")
    pop_pred <- merge(pop_pred, x1, by = c("tm", "dis_en_ocha"), all.x = TRUE)
    pop_pred[, "prop_pop"] <- pop_pred[, "pop"] / pop_pred[, "pop_tot"]
    
    # Clean up
    pop_pred <- pop_pred[, c("tm", "subdis_en_ocha", "pop", "prop_pop")]

    
  #...................................      
  ## Prepare health facility predictor
    
    # Compute relative proportions of all health facilities within districts
    x1 <- aggregate(health_pred[, "n_hf"], by = list(health_pred[, "dis_en_ocha"]), FUN = sum )
    colnames(x1) <- c("dis_en_ocha", "n_hf_tot")
    health_pred <- merge(health_pred, x1, by = "dis_en_ocha", all.x = TRUE)
    health_pred[, "prop_hf"] <- health_pred[, "n_hf"] / health_pred[, "n_hf_tot"]
        
    # Compute ratio of health facilities per 100,000 population
    x1 <- pop_pred
    colnames(x1)[colnames(x1) == "subdis_ori_en_ocha"] <- "subdis_en_ocha"
    health_pred <- merge(health_pred, x1[, c("tm", "subdis_en_ocha", "pop")], by = "subdis_en_ocha", all = TRUE)
    health_pred[, "density_hf"] <- health_pred[, "n_hf"] * 100000 / health_pred[, "pop"]

    # Clean up
    health_pred <- health_pred[, c("subdis_en_ocha", "tm", "n_hf", "prop_hf", "density_hf")]
              

  #...................................      
  ## Prepare road network predictor
    
    # Compute relative proportion of total road length within districts
    x1 <- aggregate(road_pred[, "road_distance"], by = list(road_pred[, "dis_en_ocha"]), FUN = sum )
    colnames(x1) <- c("dis_en_ocha", "road_distance_tot")
    road_pred <- merge(road_pred, x1, by = "dis_en_ocha", all.x = TRUE)
    road_pred[, "prop_road_distance"] <- road_pred[, "road_distance"] / road_pred[, "road_distance_tot"]
        
    # Clean up
    road_pred <- road_pred[, c("subdis_en_ocha", "subdis_area", "road_distance", "road_density", "prop_road_distance")]
    
        
  #...................................      
  ## Prepare distance predictor
  
    # Identify each subdistrict arrival-origin combination
    distance_pred <- expand.grid(unique(ocha[, "subdis_en_ocha"]), unique(ocha[, "subdis_en_ocha"]))
    colnames(distance_pred) <- c("subdis_en_ocha", "subdis_ori_en_ocha")

    # Calculate centroid position of each subdistrict
      # apply planar transformation to shape file
      ocha_trans <- st_transform(ocha_shape, 4981)

      # compute centroids
      centroids <- st_centroid(ocha_trans)
      centroids <- data.frame(ocha_trans$subdis_en_ocha_shape, do.call(rbind.data.frame, st_geometry(centroids) ) )
      colnames(centroids) <- c("subdis_en_ocha", "longitude", "latitude")
      centroids[, "subdis_ori_en_ocha"] <- centroids[, "subdis_en_ocha"]
      
    # Compute distance in Km between the centroids of arrival and origin district
    x1 <- match(distance_pred$subdis_en_ocha, centroids$subdis_en_ocha)
    x1 <- centroids[x1, colnames(centroids) != "subdis_ori_en_ocha"]
    x2 <- match(distance_pred$subdis_ori_en_ocha, centroids$subdis_ori_en_ocha)
    x2 <- centroids[x2, colnames(centroids) != "subdis_en_ocha"]
    distance_pred <- data.frame("subdis_en_ocha" = x1$subdis_en_ocha, "subdis_ori_en_ocha" = x2$subdis_ori_en_ocha,
      "distance" = geodist(x1[, c("longitude", "latitude")], x2[, c("longitude", "latitude")],
      paired = TRUE, measure = "geodesic") )
    distance_pred[, "distance"] <- distance_pred[, "distance"] / 1000
    rm(x1, x2)
    
    
  #...................................      
  ## Prepare insecurity predictor
    
    # Aggregate data by subdistrict-month
      # add tm units
      acled <- merge(acled, t_units, by = c("month", "year"), all.x = TRUE)
      
      # visualise event type
      table(acled[, "event_type"])
        # will consider all
      
      # aggregate
      x1 <- aggregate(acled[, c("n_events", "n_fatalities")], by = acled[, c("subdis_en_ocha", "tm")], FUN = sum)
     
    # Merge with subdistrict-month time series
    acled_pred <- merge(ts, x1, by = c("subdis_en_ocha", "tm"), all.x = TRUE)    
      # restrict to start date of high-quality ACLED dataset (1 Jan 2015)
      acled_pred <- subset(acled_pred, tm >= 13)
    
      # reduce columns
      acled_pred <- acled_pred[, c("tm", "dis_en_ocha", "subdis_en_ocha", "n_events", "n_fatalities")]
      
      # set NA values to 0
      acled_pred[which(is.na(acled_pred[, "n_events"])), "n_events"] <- 0
      acled_pred[which(is.na(acled_pred[, "n_fatalities"])), "n_fatalities"] <- 0
      
    # Merge with population and compute incidence of events per 100,000 population
    acled_pred <- merge(acled_pred, pop_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
    acled_pred[, "density_events"] <- acled_pred[, "n_events"] * 100000 / acled_pred[, "pop"]
    acled_pred[, "density_fatalities"] <- acled_pred[, "n_fatalities"] * 100000 / acled_pred[, "pop"]
        
    # Create running sums of cumulative insecurity incidence (absolute and per 100,000 population): 
      # previous 12, 6 and 2 months (including the current month)
      
      # sort dataset
      acled_pred <- acled_pred[order(acled_pred[, "subdis_en_ocha"], acled_pred[, "tm"]), ]
              
      # for each variable that needs to be converted into a running sum...
      for (i in c("n_events", "n_fatalities") ) {
          
        # for each rolling sum period...
        for (j in c(12, 6, 2)) {
        
          # output vectors
          out1 <- c()
          out2 <- c()
          
          # for each subdistrict...
          for (k in unique(acled_pred[, "subdis_en_ocha"]) ) {
            # select time series for stratum k...
            x1 <- acled_pred[acled_pred[, "subdis_en_ocha"] == k, i]
            x2 <- acled_pred[acled_pred[, "subdis_en_ocha"] == k, "pop"]
            
            # calculate rolling sum (leave a burn-in period as NA at the start, until k values are reached)
            out1 <- c(out1, rep(NA, j - 1), rollsum(x1, j, align = "right") )
            
            # calculate rolling mean of population (leave a burn-in period as NA at the start, until k values are reached)
            out2 <- c(out2, rep(NA, j - 1), rollmean(x2, j, align = "right") )
          }
          
          # add new variables to time series
          acled_pred[, paste(i ,"_rs_", j, sep = "")] <- out1
          acled_pred[, paste(gsub("n_", "density_", i) ,"_rs_", j, sep = "")] <- out1 * 100000 / out2
        }
      }


    # Simplify predictor dataset
    x1 <- c("tm", "dis_en_ocha", "subdis_en_ocha", grep("rs", colnames(acled_pred), value = TRUE))  
    acled_pred <- acled_pred[, x1]


#.........................................................................................                            
### Preparing model data for subdistrict of origin imputation
#.........................................................................................    

  #...................................      
  ## Identify training data (subset of DTM with high completeness for subdistrict of origin)
    
    # Read DTM dataset as saved after previous scripts
    dtm <- read.csv("yem_displacement_dataset_clean_raw.csv")
    
    # Sort DTM dataset
    dtm <- dtm[order(dtm[, "who"], dtm[, "type"], dtm[, "gov_en_ocha"], dtm[, "dis_en_ocha"], dtm[, "subdis_en_ocha"],
      dtm[, "year"], dtm[, "month"]), ]

    # Describe subdistrict of origin missingness by date of assessment (trying to identify a dataset for model training)
    prop.table(table(dtm[, c("year_assessment", "month_assessment", "missing_subdis_ori")]), margin = c(1, 2))*100
      # how many observations have missingness <12% (Nov 2018)? about 40,000 -> training data
      table(dtm[, c("year_assessment", "month_assessment", "missing_subdis_ori")])

    # Identify training data
    dtm[, "train"] <- "no"  
    dtm[which(dtm[, "month_assessment"] == 11 & dtm[, "year_assessment"] == 2018 & dtm[, "missing_subdis_ori"] == "no"
      & dtm[, "missing_subdis"] == "no" & dtm[, "eligible"] == "yes"), "train"] <- "yes"  
    table(dtm[, c("eligible", "train")])
    

  #...................................      
  ## Prepare training data

    # Eliminate non-eligible observations
    dtm_train <- subset(dtm, eligible =="yes" & train == "yes")

    # Restrict columns to minimum necessary
    dtm_train <- dtm_train[, c("month", "year", "tm", "dis_en_ocha", "subdis_en_ocha", "dis_ori_en_ocha", 
      "subdis_ori_en_ocha", "n_hh" )]
    
    # Expand dataset so as to feature all candidate subdistricts of origin within the known district of origin
    x1 <- ocha[which(ocha[, "dis_en_ocha"] %in% unique(dtm_train[, "dis_ori_en_ocha"])), c("dis_en_ocha", "subdis_en_ocha")]
    colnames(x1) <- c("dis_ori_en_ocha", "subdis_ori_en_ocha")
    colnames(dtm_train)[colnames(dtm_train) == "subdis_ori_en_ocha"] <- "subdis_ori_en_ocha_obs"
    dtm_train <- merge(dtm_train, x1, by = "dis_ori_en_ocha")

      # for all subdistricts that have been added in this expansion, n of IDP households == 0
      dtm_train[, "n_hh"] <- ifelse(dtm_train[, "subdis_ori_en_ocha_obs"] == dtm_train[, "subdis_ori_en_ocha"], 
        dtm_train[, "n_hh"], 0)
       
    # Aggregate training data by month and year of IDP arrival, subdistrict and subdistrict of origin
    dtm_train <- aggregate(dtm_train[, c("n_hh")], by = dtm_train[, c("month", "year", "tm", "dis_en_ocha", "subdis_en_ocha", "dis_ori_en_ocha",
      "subdis_ori_en_ocha")], FUN = sum)   
    colnames(dtm_train)[colnames(dtm_train) == "x"] <- "n_hh"   
    dtm_train <- dtm_train[order(dtm_train[, "tm"], dtm_train[, "dis_en_ocha"], dtm_train[, "subdis_en_ocha"],
      dtm_train[, "dis_ori_en_ocha"], dtm_train[, "subdis_ori_en_ocha"]), ]
    
    # Prepare outcome variable (1 if any IDPs, 0 otherwise)
    dtm_train[, "outcome"] <- ifelse(dtm_train[, "n_hh"] == 0, 0, 1) 
    table(dtm_train[, "outcome"])
    
    
  
#.........................................................................................                            
### Readying dataset for model fitting (subdistrict of origin imputation)
#.........................................................................................    
     
  #...................................      
  ## Merge data with predictors and prepare for model fitting

    # Merge with predictors
      # population predictor
      colnames(pop_pred) <- c("tm", "subdis_ori_en_ocha", "pop", "prop_pop")
      dtm_train <- merge(dtm_train, pop_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # health facility predictor
      colnames(health_pred) <- c("subdis_ori_en_ocha", "tm", "n_hf", "prop_hf", "density_hf")
      dtm_train <- merge(dtm_train, health_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_ori_en_ocha", "subdis_area", "road_distance", "road_density", "prop_road_distance")
      dtm_train <- merge(dtm_train, road_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
            
      # distance predictor
      dtm_train <- merge(dtm_train, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor
      colnames(acled_pred)[colnames(acled_pred) == "subdis_en_ocha"] <- "subdis_ori_en_ocha"
      dtm_train <- merge(dtm_train, acled_pred[, ! colnames(acled_pred) %in% c("dis_en_ocha")], 
        by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
    

  #...................................      
  ## Explore and categorise predictors
    
    # Share of district population
    f_hist("prop_pop", dtm_train, c(NA, NA))
    dtm_train[, "prop_pop_cat"] <- cut(dtm_train[, "prop_pop"], breaks = seq(0, 1, 0.2), include.lowest = TRUE, 
      labels = c("0 to 19%", "20 to 39%", "40 to 59%", "60 to 79%", ">=80%"),
      right = FALSE)
    table(dtm_train[, "prop_pop_cat"])
    
    # Year
    table(dtm_train[, "year"])
    dtm_train[, "year_cat"] <- cut(as.numeric(dtm_train[, "year"]), breaks = c(0, 2015, 2016, 2017, 2021), include.lowest = TRUE, 
      labels = c("<=2015", "2016", "2017", "2018+"), right = FALSE )
    table(dtm_train[, "year_cat"])
    
    # Distance of subdistrict
    f_hist("distance", dtm_train, c(NA, NA))
    dtm_train[, "distance_cat"] <- cut(dtm_train[, "distance"], breaks = c(0, 0.001, 25, 50, 100, 1000), include.lowest = TRUE, 
      labels = c("same subdistrict", "1 to 24Km", "25 to 49Km", "50 to 99Km", ">= 100Km"), 
      right = FALSE )
    table(dtm_train[, "distance_cat"])
    
    # Insecurity
    for (i in c(2, 6, 12) ) {
      f_hist(paste("n_events_rs_", i, sep = ""), dtm_train, c(NA, 20)) 
      dtm_train[, paste("n_events_rs_", i, "_cat", sep = "")] <- cut(dtm_train[, paste("n_events_rs_", i, sep = "")],
        breaks = c(0, 1, 2, 5, 10, 10000), right = FALSE,
        labels = c("none", "1", "2 to 4", "5 to 9", ">= 10"))
      print(table(dtm_train[paste("n_events_rs_", i, "_cat", sep = "")]))
      
      f_hist(paste("density_events_rs_", i, sep = ""), dtm_train, c(NA, NA))
      dtm_train[, paste("density_events_rs_", i, "_cat", sep = "")] <- cut(dtm_train[, paste("density_events_rs_", i, sep = "")],
        breaks = c(0, 0.001, 10, 20, 30, 40, 50, 100000), right = FALSE, include.lowest = TRUE,
        labels = c("zero", "1 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", ">= 50"))
      print(table(dtm_train[paste("density_events_rs_", i, "_cat", sep = "")]))
    }
    
    for (i in c(2, 6, 12) ) {
      f_hist(paste("n_fatalities_rs_", i, sep = ""), dtm_train, c(NA, 10)) 
      dtm_train[, paste("n_fatalities_rs_", i, "_cat", sep = "")] <- cut(dtm_train[, paste("n_fatalities_rs_", i, sep = "")],
        breaks = c(0, 0.01, 5, 10000), right = FALSE,
        labels = c("none", "1 to 5", ">= 5"))
      print(table(dtm_train[paste("n_fatalities_rs_", i, "_cat", sep = "")]))
      
      f_hist(paste("density_fatalities_rs_", i, sep = ""), dtm_train, c(NA, NA))
      dtm_train[, paste("density_fatalities_rs_", i, "_cat", sep = "")] <- cut(dtm_train[, paste("density_fatalities_rs_", i, sep = "")],
        breaks = c(0, 0.001, 10, 20, 30, 40, 50, 100000), right = FALSE, include.lowest = TRUE,
        labels = c("zero", "1 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", ">= 50"))
      print(table(dtm_train[paste("density_fatalities_rs_", i, "_cat", sep = "")]))
    }
        
    # Health facilities
    f_hist("n_hf", dtm_train, c(0, 20))
    dtm_train[, "n_hf_cat"] <- cut(dtm_train[, "n_hf"], breaks = c(0, 0.001, 5, 10000), 
      labels = c("0", "1-4", ">= 5"), include.lowest = TRUE, right = FALSE)
    f_hist("density_hf", dtm_train, c(0, 50))
    table(dtm_train[, "n_hf_cat"])
    
    dtm_train[, "density_hf_cat"] <- cut(dtm_train[, "density_hf"], 
      breaks = c(0, 0.001, 10, 20, 30, 10000), right = FALSE,
      labels = c("0", "0.1 to 9.9", "10.0 to 19.9", "20.0 to 29.9", ">= 30.0"), include.lowest = TRUE)
    table(dtm_train[, "density_hf_cat"])
    
    # Road network
    f_hist("road_density", dtm_train, c(NA, NA))
    dtm_train[, "road_density_cat"] <- cut(dtm_train[, "road_density"], breaks = c(0, 0.001, 0.25, 0.50, 0.75, 100), 
      labels = c("0 Km/Km^2", "0.01 to 0.24 Km/Km^2", "0.25 to 0.49 Km/Km^2", "0.50 to 0.74 Km/Km^2", ">= 0.75 Km/Km^2"),
      include.lowest = TRUE, right = FALSE)
    table(dtm_train[, "road_density_cat"])    
       
  #...................................      
  ## Other miscellaneous preparations
    
    # Come up with additional predictor: 1 / number of subdistricts of origin within district of origin
    dtm_train[, "subdis_count"] <- 1
    x1 <- aggregate(dtm_train[, "subdis_count"], by = dtm_train[, c("tm", "subdis_en_ocha", "dis_ori_en_ocha")], FUN = sum)
    colnames(x1)[colnames(x1) == "x"] <- "n_subdis_ori"
    dtm_train <- merge(dtm_train, x1, by = c("tm", "subdis_en_ocha", "dis_ori_en_ocha"), all.x = TRUE)    
    table(dtm_train[, "n_subdis_ori"])
    dtm_train[, "prop_subdis_ori"] <- 1/ dtm_train[, "n_subdis_ori"]
      # categorise
      f_hist("prop_subdis_ori", dtm_train, c(NA, NA))
      dtm_train[, "prop_subdis_ori_cat"] <- cut(dtm_train[, "prop_subdis_ori"], 
        breaks = c(0, 0.10, 0.20, 0.50, 0.99, 100), 
        labels = c("<10%", "10 to 19%", "20 to 49%", "50 to 99%", "100%"),
        right = FALSE)
      table(dtm_train[, "prop_subdis_ori_cat"])

    # Define additional outcomes
      # log of IDP households
      dtm_train[, "n_hh_ln"] <- log(dtm_train[, "n_hh"])
      
      # proportion of IDP households by subdistrict of origin
        # number of IDP households per district of origin
        x1 <- aggregate(dtm_train[, "n_hh"], by = dtm_train[, c("tm", "subdis_en_ocha", "dis_ori_en_ocha")], FUN = sum, na.rm = TRUE)
        colnames(x1)[colnames(x1) == "x"] <- "n_hh_tot"
        dtm_train <- merge(dtm_train, x1, by = c("tm", "subdis_en_ocha", "dis_ori_en_ocha"), all.x = TRUE)
        
        # proportion
        dtm_train[, "prop_hh"] <- dtm_train[, "n_hh"] / dtm_train[, "n_hh_tot"]
        hist(dtm_train[, "prop_hh"])
        range(dtm_train[, "prop_hh"])
      
      # category for proportion of IDP households
      dtm_train[, "prop_hh_cat"] <- cut(dtm_train[, "prop_hh"], breaks = c(0, 0.0001, 0.2, 0.4, 0.6, 0.8, 0.9999, 1), 
        labels = c(0:6), include.lowest = TRUE,
        ordered_result = TRUE)
      table(dtm_train[, "prop_hh_cat"])
     
    # Come up with additional predictor: log of total district IDP households
    dtm_train[, "n_hh_tot_ln"] <- log(dtm_train[, "n_hh_tot"])
      # categorise
      f_hist("n_hh_tot_ln", dtm_train, c(NA, NA))
      dtm_train[, "n_hh_tot_ln_cat"] <- cut(dtm_train[, "n_hh_tot_ln"], 
        breaks = c(0, 0.00001, 1, 2, 3, 100), 
        labels = c("0", "0.01 to 0.99", "1.00 to 1.99", "2.00 to 2.99", ">= 3.00"),
        right = FALSE)
      table(dtm_train[, "n_hh_tot_ln_cat"])
       
    # Write training data
    write_excel_csv(dtm_train, "yem_displacement_train_data_subdis_ori.csv", na = "")
      
   
#.........................................................................................                            
### Fitting and evaluating models to impute number of subdistricts of origin (Model 1)
#.........................................................................................    
     
  #...................................
  ## Aggregate training data by district of origin
    
    # Read file if necessary
    if (! exists("dtm_train")) {dtm_train <- read.csv("yem_displacement_train_data_subdis_ori.csv")}
    colnames(dtm_train) <- gsub("ï..", "", colnames(dtm_train))
    
    # Aggregate
    x1 <- c("n_hh", "outcome", "pop", "n_hf", "road_distance", "subdis_area", "n_events_rs_2", "n_fatalities_rs_2", "n_subdis_ori")  
    dtm_train_dis <- aggregate(dtm_train[, x1], 
      by = dtm_train[, c("tm", "dis_en_ocha", "subdis_en_ocha", "dis_ori_en_ocha")], FUN = sum, na.rm = TRUE)
    
    # Recreate predictors
    dtm_train_dis[, c("density_hf", "density_events_rs_2", "density_fatalities_rs_2")] <-
      dtm_train_dis[, c("n_hf", "n_events_rs_2", "n_fatalities_rs_2")] * 100000 / dtm_train_dis[, "pop"]
    dtm_train_dis[, "road_density"] <- dtm_train_dis[, "road_distance"] / dtm_train_dis[, "subdis_area"]
    dtm_train_dis[, "n_hh_ln"] <- log(dtm_train_dis[, "n_hh"])
    
  #...................................
  ## Fit and evaluate random forest model
    
    # Explore and recategorise outcome
    table(dtm_train_dis[, "outcome"])
      # to simplify, recategorise all instances of >=5 as 5 (only n =2 )
      dtm_train_dis[which(dtm_train_dis[, "outcome"] %in% 5:7), "outcome"] <- 5
      table(dtm_train_dis[, "outcome"])
      # factorise outcome
      dtm_train_dis[, "outcome"] <- factor(dtm_train_dis[, "outcome"])
          
    # Grow random forest
      # define formula and select data
      outcome <- "outcome"
      formula_rf_m1_ori <- as.formula(paste(outcome, " ~ pop + density_events_rs_2 + density_fatalities_rs_2 +
        subdis_area + density_hf + road_density + n_subdis_ori + n_hh_ln", sep = ""))
      dtm_fit <- dtm_train_dis[complete.cases(dtm_train_dis[, all.vars(formula_rf_m1_ori)]), 
        all.vars(formula_rf_m1_ori)]
      
      # attribute weights to balance data
      x1 <- prop.table(table(dtm_fit[, outcome]))
      x1 <- data.frame(x1)
      colnames(x1) <- c(outcome, "prop_outcome")
      x1[, "wt"] <- 1 / x1[, "prop_outcome"]
      dtm_fit <- merge(dtm_fit, x1, by = outcome, all.x = TRUE)
      
      # grow forest  
      fit_rf <- ranger(formula = formula_rf_m1_ori, data = dtm_fit, classification = TRUE, num.trees = 500, 
        mtry = 2, case.weights = dtm_fit[, "wt"])
      print(fit_rf)
    
    # Predict on training data
    dtm_fit[, "pred"] <- predict(fit_rf, data = dtm_fit)$predictions
    
    # Check model performance on training data
    round(prop.table(t(table(dtm_fit[, c(outcome, "pred")])), margin = 2)*100, 1)

    # Check model performance on ten-fold cross-validation
      # shuffle dataset
      dtm_cv <- dtm_fit[sample(nrow(dtm_fit), nrow(dtm_fit), replace = FALSE), ]

      # split dataset into ten equal folds
      folds <- split(dtm_cv, (1:nrow(dtm_cv) %/% (nrow(dtm_cv) / 10)))
      if (length(folds) > 10) {folds <- folds[1:10]}
      
      # fit model on all the unfolded sets and track predictive accuracy of model fit on each fold
      out <- table(dtm_fit[, c(outcome, outcome)])
      out[,] <- 0
      for (i in 1:length(folds) ) {
        # control statement
        print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))

        # fit on all data but the fold
        x1 <- do.call(rbind, folds[-i])
        cv_fit <- update(fit_rf, data = x1, case.weights = x1[, "wt"])

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x2[, "pred"] <- predict(cv_fit, x2)$predictions

        # add model performance on fold data
        x3 <- table(x2[, c(outcome, "pred")])
        print(x3)
        out[1:nrow(x3), 1:ncol(x3)] <- out[1:nrow(x3), 1:ncol(x3)] + x3
      }

      # overall predictive performance on cross-validation
      x1 <- t(out)
      confusionMatrix(x1)
      x1 <- prop.table(x1, margin = 2)
      x1
      
      # write table
      x1 <- apply(x1, c(1,2), function (x) {sprintf("%1.1f%%", 100 * x)})
      x1 <- rbind(c("columns: observed number of subdistricts of origin per district", "", "", "", ""),
        c("1", "2", "3", "4", "5"), x1)
      x1 <- cbind(c("", "", "rows: predictions", "", "", "", ""), 
        c("", "", "1", "2", "3", "4", "5"), x1)
      write.csv(x1, "yem_pop_model1_ori_accuracy.csv", row.names = FALSE)
      
    # Save fit
    fit_rf_m1_ori <- fit_rf
    saveRDS(fit_rf_m1_ori, file = "fit_rf_m1_ori.rds")
        
            
#.........................................................................................                            
### Fitting and evaluating models to impute subdistrict of origin data (Models 2 and 3)
#.........................................................................................    
     
  #...................................
  ## Explore correlations among variables
    
    # Read training data if needed
    if (! exists ("dtm_train")) {dtm_train <- read.csv("yem_displacement_train_data_subdis_ori.csv")}
    colnames(dtm_train) <- gsub("ï..", "", colnames(dtm_train))
    
    # Generate boxplots of the outcome, by categorised predictor level    
      # select outcome and predictors
      outcome <- "prop_hh"
      x1 <- c("prop_pop_cat", "year_cat", "distance_cat", "n_events_rs_2_cat", "n_events_rs_6_cat",
        "density_hf_cat", "road_density_cat", "prop_subdis_ori_cat")
      
      # make plots
      for (i in x1) {
        x2 <- subset(dtm_train, n_hh != 0)[, c(outcome, i)]
        colnames(x2) <- c("outcome", "x_var")
        assign(paste("plot_", i, sep = ""), ggplot(x2, aes(y = outcome, x = x_var) ) +
          geom_boxplot(lwd = 0.5, fill = palette_cb[sample.int(length(palette_cb), 1)], alpha = 0.5 ) +
            theme_bw() +
            labs(title = i) +
            theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
              axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5, size = 10))
          )
        }
    
      # identify plot names
      x2 <- paste("plot_", x1, sep = "" )
    
      # arrange plots in a grid and save
      plot <- ggarrange(plotlist = mget(x2), ncol = 3, nrow = ceiling(length(x1) / 3),
        common.legend = TRUE, legend = "right", align = "hv")
      plot
        
    
  #...................................      
  ## Fit and evaluate a random forest model to guess the right subdistricts of origin
      # note: for subdistricts of origin, assume only 1 subdistrict per district: this is consistent with
        # training data (97% of records were of singleton subdistricts of origin)
        # also greatly simplifies task later down
        
    # Grow forest
      # specify formula and select data
      formula_rf_m2_ori <- as.formula("outcome ~ pop + density_events_rs_2 + density_fatalities_rs_2 + 
        density_hf + subdis_area + road_density + prop_subdis_ori + n_hh_tot_ln")
      dtm_fit <- dtm_train[complete.cases(dtm_train[, c(all.vars(formula_rf_m2_ori), 
        "dis_ori_en_ocha", "subdis_en_ocha", "tm")]), c(all.vars(formula_rf_m2_ori), "dis_ori_en_ocha", "subdis_en_ocha", "tm")]
    
      # create an aggregation id (by subdistrict, district of origin and time)
      x1 <- unique(dtm_fit[, c("tm", "subdis_en_ocha", "dis_ori_en_ocha")])
      x1[, "id"] <- paste("id", 1:nrow(x1), sep = "")
      dtm_fit <- merge(dtm_fit, x1, by = c("tm", "subdis_en_ocha", "dis_ori_en_ocha"), all.x = TRUE)
      
      # factorise outcome  
      dtm_fit[, "outcome"] <- factor(dtm_fit[, "outcome"])

      # attribute weights to balance data
      x1 <- prop.table(table(dtm_fit[, outcome]))
      x1 <- data.frame(x1)
      colnames(x1) <- c(outcome, "prop_outcome")
      x1[, "wt"] <- 1 / x1[, "prop_outcome"]
      dtm_fit <- merge(dtm_fit, x1, by = outcome, all.x = TRUE)
            
      # grow forest
      fit_rf <- ranger(formula = formula_rf_m2_ori, data = dtm_fit, classification = TRUE, mtry = 3, num.trees = 500, 
      max.depth = 20, probability = TRUE, case.weights = dtm_fit[, "wt"])
      print(fit_rf)
    
    # Predict probabilities on training data
    dtm_fit[, "p_pred"] <- predict(fit_rf, data = dtm_fit)$predictions[, 2]
    dtm_fit[, "pred"] <- 0
    dtm_fit <- dtm_fit[, colnames(dtm_fit) != "p_pred_max"]
    
    # Check model performance on training data: proportion of correct subdistrict guesses
      # constrained by only allowing exactly one subdistrict per district to be predicted
    x1 <- aggregate(dtm_fit[, "p_pred"], by = dtm_fit[, c("tm", "subdis_en_ocha", "dis_ori_en_ocha")],
      FUN = max)
    colnames(x1)[4] <- "p_pred_max"
    dtm_fit <- merge(dtm_fit, x1, by = c("tm", "subdis_en_ocha", "dis_ori_en_ocha"), all.x = TRUE)
    dtm_fit[, "pred"] <- ifelse(dtm_fit[, "p_pred_max"] == dtm_fit[, "p_pred"], 1, 0)
    table(dtm_fit[, c("outcome", "pred")])
    prop.table(table(dtm_fit[, c("outcome", "pred")]), margin = 1)  

    # Check model performance on ten-fold cross-validation
      # shuffle dataset (by id stratum)
      dtm_cv <- dtm_fit[, ! colnames(dtm_fit) %in% c("p_pred", "pred", "p_pred_max")]
      dtm_cv <- split(dtm_cv, dtm_cv[, "id"])
      dtm_cv <- do.call(rbind, dtm_cv[sample(names(dtm_cv), length(dtm_cv), replace = FALSE)])
      row.names(dtm_cv) <- NULL

      # split dataset into ten equal folds (by id)
      x1 <- data.frame(unique(dtm_cv[, "id"]))
      x2 <- (1:nrow(x1) %/% (nrow(x1) / 10))
      x2[x2 == 0] <- 10
      table(x2)
      x1[, "split"] <- x2
      colnames(x1)[1] <- "id"
      dtm_cv <- merge(dtm_cv, x1, by = "id", all.x = TRUE)
      folds <- split(dtm_cv, dtm_cv[, "split"])
      if (length(folds) > 10) {folds <- folds[1:10]}
      
      # fit model on all the unfolded sets and track predictive accuracy of model fit on each fold
      out <- c()
      for (i in 1:length(folds) ) {
        # control statement
        print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))

        # fit on all data but the fold
        x1 <- do.call(rbind, folds[-i])
        cv_fit <- update(fit_rf, data = x1, case.weights = x1[, "wt"])

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x2[, "p_pred"] <- predict(cv_fit, x2)$predictions[, 2]
        x3 <- aggregate(x2[, "p_pred"], by = x2[, c("tm", "subdis_en_ocha", "dis_ori_en_ocha")],
          FUN = max)
        colnames(x3)[4] <- "p_pred_max"
        x2 <- merge(x2, x3, by = c("tm", "subdis_en_ocha", "dis_ori_en_ocha"), all.x = TRUE)
        x2[, "pred"] <- ifelse(x2[, "p_pred_max"] == x2[, "p_pred"], 1, 0)

        # add model performance on fold data
        print(table(x2[, c("outcome", "pred")]))
        if (i == 1) {out <- table(x2[, c("outcome", "pred")])}
        if (i > 1) {out <- out + table(x2[, c("outcome", "pred")])}
      }

      # overall predictive performance on cross-validation
      prop.table(out, margin = 1)

    # Save fit
    fit_rf_m2_ori <- fit_rf
    saveRDS(fit_rf_m2_ori, file = "fit_rf_m2_ori.rds")

  #...................................      
  ## Fit and evaluate a random forest model to predict the category of proportion of IDP households of origin
    
    # Restrict analysis to subdistricts with IDP households and where the proportion of household < 100%
    dtm_fit <- subset(dtm_train, prop_hh != 1 & n_hh != 0)
    
    # Grow random forest
      # define formula and select data
      outcome <- "prop_hh_cat"
      formula_rf_m3_ori <- as.formula(paste(outcome, " ~ prop_pop + density_events_rs_2 + density_fatalities_rs_2 +
        density_hf + road_density + prop_subdis_ori + n_hh_tot_ln_cat", sep = ""))
      dtm_fit <- dtm_fit[complete.cases(dtm_fit[, all.vars(formula_rf_m3_ori)]), all.vars(formula_rf_m3_ori)]
      
      # attribute weights to balance data
      x1 <- prop.table(table(dtm_fit[, outcome]))
      x1 <- data.frame(x1)
      colnames(x1) <- c(outcome, "prop_outcome")
      x1[, "wt"] <- 1 / x1[, "prop_outcome"]
      dtm_fit <- merge(dtm_fit, x1, by = outcome, all.x = TRUE)

      # factorise outcome
      dtm_fit[, outcome] <- factor(dtm_fit[, outcome])
            
      # grow forest  
      fit_rf <- ranger(formula = formula_rf_m3_ori, data = dtm_fit, classification = TRUE, num.trees = 1000, max.depth = 15,
        mtry = 3, case.weights = dtm_fit[, "wt"])
      print(fit_rf)
    
     # Predict on training data
    dtm_fit[, "pred"] <- predict(fit_rf, data = dtm_fit)$predictions
    
    # Check model performance on training data
    prop.table(t(table(dtm_fit[, c(outcome, "pred")])), margin = 2)
    prop.table(t(fit_rf$confusion.matrix), margin = 2)  

    # Check model performance on ten-fold cross-validation
      # shuffle dataset
      dtm_cv <- dtm_fit[sample(nrow(dtm_fit), nrow(dtm_fit), replace = FALSE), ]

      # split dataset into ten equal folds
      folds <- split(dtm_cv, (1:nrow(dtm_cv) %/% (nrow(dtm_cv) / 10)))
      if (length(folds) > 10) {folds <- folds[1:10]}
      
      # fit model on all the unfolded sets and track predictive accuracy of model fit on each fold
      out <- c()
      for (i in 1:length(folds) ) {
        # control statement
        print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))

        # fit on all data but the fold
        x1 <- do.call(rbind, folds[-i])
        cv_fit <- update(fit_rf, data = x1, case.weights = x1[, "wt"])

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x2[, "pred"] <- predict(cv_fit, x2)$predictions

        # add model performance on fold data
        print(table(x2[, c(outcome, "pred")]))
        if (i == 1) {out <- table(x2[, c(outcome, "pred")])}
        if (i > 1) {out <- out + table(x2[, c(outcome, "pred")])}
      }

      # overall predictive performance on cross-validation
      x1 <- t(out)
      confusionMatrix(x1)
      x1 <- prop.table(x1, margin = 2)
      x1
      
      # write table
      x1 <- apply(x1, c(1,2), function (x) {sprintf("%1.1f%%", 100 * x)})
      x1 <- rbind(c("columns: observed proportion of IDP households from subdistrict", "", "", "", ""),
        c("1 to 19%", "20 to 39%", "40 to 59%", "60 to 79%", "80 to 99%"), x1)
      x1 <- cbind(c("", "", "rows: predictions", "", "", "", ""), 
        c("", "", "1 to 19%", "20 to 39%", "40 to 59%", "60 to 79%", "80 to 99%"), x1)
      write.csv(x1, "yem_pop_model3_ori_accuracy.csv", row.names = FALSE)
      
    # Save fit
    fit_rf_m3_ori <- fit_rf
    saveRDS(fit_rf_m3_ori, file = "fit_rf_m3_ori.rds")

  #...................................      
  ## Clean up
  rm(cv_fit, dtm_cv, dtm_fit, dtm_train, fit_rf, folds, x1, x2, x3)      
    

#.........................................................................................                            
### Preparing model data for subdistrict of arrival imputation
#.........................................................................................    

  #...................................      
  ## Identify training data (subset of DTM with high completeness for subdistrict of arrival)
    
    # Read DTM dataset as saved after previous scripts
    dtm <- read.csv("yem_displacement_dataset_clean_raw.csv")
    dtm <- as.data.frame(dtm)
    
    # Sort DTM dataset
    dtm <- dtm[order(dtm[, "who"], dtm[, "type"], dtm[, "gov_en_ocha"], dtm[, "dis_en_ocha"], dtm[, "subdis_en_ocha"],
      dtm[, "year"], dtm[, "month"]), ]

    # Describe subdistrict of arrival missingness by date of assessment (trying to identify a dataset for model training)
    prop.table(table(dtm[, c("year_assessment", "month_assessment", "missing_subdis")]), margin = c(1, 2))*100
      # how many observations have missingness 0% (Nov 2018)? about 47,000 -> training data
      table(dtm[, c("year_assessment", "month_assessment", "missing_subdis")])

    # Identify training data
    dtm[, "train"] <- "no"  
    dtm[which(dtm[, "month_assessment"] == 11 & dtm[, "year_assessment"] == 2018 & dtm[, "missing_subdis_ori"] == "no"
      & dtm[, "missing_subdis"] == "no" & dtm[, "eligible"] == "yes"), "train"] <- "yes"  
    table(dtm[, c("eligible", "train")])
    

  #...................................      
  ## Prepare training data

    # Eliminate non-eligible observations
    dtm_train <- subset(dtm, eligible =="yes" & train == "yes")

    # Restrict columns to minimum necessary
    dtm_train <- dtm_train[, c("month", "year", "tm", "dis_en_ocha", "subdis_en_ocha", "dis_ori_en_ocha", 
      "subdis_ori_en_ocha", "n_hh" )]
    
    # Expand dataset so as to feature all candidate subdistricts of arrival within the known district of arrival
    x1 <- ocha[which(ocha[, "dis_en_ocha"] %in% unique(dtm_train[, "dis_en_ocha"])), c("dis_en_ocha", "subdis_en_ocha")]
    colnames(x1) <- c("dis_en_ocha", "subdis_en_ocha")
    colnames(dtm_train)[colnames(dtm_train) == "subdis_en_ocha"] <- "subdis_en_ocha_obs"
    dtm_train <- merge(dtm_train, x1, by = "dis_en_ocha")

      # for all subdistricts that have been added in this expansion, n of IDP households == 0
      dtm_train[, "n_hh"] <- ifelse(dtm_train[, "subdis_en_ocha_obs"] == dtm_train[, "subdis_en_ocha"], 
        dtm_train[, "n_hh"], 0)
       
    # Aggregate training data by month and year of IDP arrival, subdistrict of origin and subdistrict of arrival
    dtm_train <- aggregate(dtm_train[, c("n_hh")], by = dtm_train[, c("month", "year", "tm", "dis_en_ocha", "subdis_en_ocha", "dis_ori_en_ocha",
      "subdis_ori_en_ocha")], FUN = sum)   
    colnames(dtm_train)[colnames(dtm_train) == "x"] <- "n_hh"   
    dtm_train <- dtm_train[order(dtm_train[, "tm"], dtm_train[, "dis_en_ocha"], dtm_train[, "subdis_en_ocha"],
      dtm_train[, "dis_ori_en_ocha"], dtm_train[, "subdis_ori_en_ocha"]), ]
    
    # Prepare outcome variable (1 if any IDPs, 0 otherwise)
    dtm_train[, "outcome"] <- ifelse(dtm_train[, "n_hh"] == 0, 0, 1)  
    table(dtm_train[, "outcome"])
    
    
  
#.........................................................................................                            
### Readying dataset for model fitting (subdistrict of arrival imputation)
#.........................................................................................    
     
  #...................................      
  ## Merge data with predictors and prepare for model fitting

    # Merge with predictors
      # population predictor
      colnames(pop_pred) <- c("tm", "subdis_en_ocha", "pop", "prop_pop")
      dtm_train <- merge(dtm_train, pop_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
      
      # health facility predictor
      colnames(health_pred) <- c("subdis_en_ocha", "tm", "n_hf", "prop_hf", "density_hf")
      dtm_train <- merge(dtm_train, health_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_en_ocha", "subdis_area", "road_distance", "road_density", "prop_road_distance")
      dtm_train <- merge(dtm_train, road_pred, by = "subdis_en_ocha", all.x = TRUE)
            
      # distance predictor
      dtm_train <- merge(dtm_train, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor
      colnames(acled_pred)[colnames(acled_pred) == "subdis_ori_en_ocha"] <- "subdis_en_ocha"
      dtm_train <- merge(dtm_train, acled_pred[, ! colnames(acled_pred) %in% c("dis_en_ocha")], 
        by = c("tm", "subdis_en_ocha"), all.x = TRUE)
    

  #...................................      
  ## Explore and categorise predictors
    
    # Share of district population
    f_hist("prop_pop", dtm_train, c(NA, NA))
    dtm_train[, "prop_pop_cat"] <- cut(dtm_train[, "prop_pop"], breaks = seq(0, 1, 0.2), include.lowest = TRUE, 
      labels = c("0 to 19%", "20 to 39%", "40 to 59%", "60 to 79%", ">=80%"),
      right = FALSE)
    table(dtm_train[, "prop_pop_cat"])
    
    # Year
    table(dtm_train[, "year"])
    dtm_train[, "year_cat"] <- cut(as.numeric(dtm_train[, "year"]), breaks = c(0, 2015, 2016, 2017, 2021), include.lowest = TRUE, 
      labels = c("<=2015", "2016", "2017", "2018+"), right = FALSE )
    table(dtm_train[, "year_cat"])
    
    # Distance of subdistrict
    f_hist("distance", dtm_train, c(NA, NA))
    dtm_train[, "distance_cat"] <- cut(dtm_train[, "distance"], breaks = c(0, 0.001, 25, 50, 100, 1000), include.lowest = TRUE, 
      labels = c("same subdistrict", "1 to 24Km", "25 to 49Km", "50 to 99Km", ">= 100Km"), 
      right = FALSE )
    table(dtm_train[, "distance_cat"])
    
    # Insecurity
    for (i in c(2, 6, 12) ) {
      f_hist(paste("n_events_rs_", i, sep = ""), dtm_train, c(NA, 20)) 
      dtm_train[, paste("n_events_rs_", i, "_cat", sep = "")] <- cut(dtm_train[, paste("n_events_rs_", i, sep = "")],
        breaks = c(0, 1, 2, 5, 10, 10000), right = FALSE,
        labels = c("none", "1", "2 to 4", "5 to 9", ">= 10"))
      print(table(dtm_train[paste("n_events_rs_", i, "_cat", sep = "")]))
      
      f_hist(paste("density_events_rs_", i, sep = ""), dtm_train, c(NA, NA))
      dtm_train[, paste("density_events_rs_", i, "_cat", sep = "")] <- cut(dtm_train[, paste("density_events_rs_", i, sep = "")],
        breaks = c(0, 0.001, 10, 20, 30, 40, 50, 100000), right = FALSE, include.lowest = TRUE,
        labels = c("zero", "1 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", ">= 50"))
      print(table(dtm_train[paste("density_events_rs_", i, "_cat", sep = "")]))
    }
    
    for (i in c(2, 6, 12) ) {
      f_hist(paste("n_fatalities_rs_", i, sep = ""), dtm_train, c(NA, 10)) 
      dtm_train[, paste("n_fatalities_rs_", i, "_cat", sep = "")] <- cut(dtm_train[, paste("n_fatalities_rs_", i, sep = "")],
        breaks = c(0, 0.01, 5, 10000), right = FALSE,
        labels = c("none", "1 to 5", ">= 5"))
      print(table(dtm_train[paste("n_fatalities_rs_", i, "_cat", sep = "")]))
      
      f_hist(paste("density_fatalities_rs_", i, sep = ""), dtm_train, c(NA, NA))
      dtm_train[, paste("density_fatalities_rs_", i, "_cat", sep = "")] <- cut(dtm_train[, paste("density_fatalities_rs_", i, sep = "")],
        breaks = c(0, 0.001, 10, 20, 30, 40, 50, 100000), right = FALSE, include.lowest = TRUE,
        labels = c("zero", "1 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", ">= 50"))
      print(table(dtm_train[paste("density_fatalities_rs_", i, "_cat", sep = "")]))
    }
        
    # Health facilities
    f_hist("n_hf", dtm_train, c(0, 20))
    dtm_train[, "n_hf_cat"] <- cut(dtm_train[, "n_hf"], breaks = c(0, 0.001, 5, 10000), 
      labels = c("0", "1-4", ">= 5"), include.lowest = TRUE, right = FALSE)
    f_hist("density_hf", dtm_train, c(0, 50))
    table(dtm_train[, "n_hf_cat"])
    
    dtm_train[, "density_hf_cat"] <- cut(dtm_train[, "density_hf"], 
      breaks = c(0, 0.001, 10, 20, 30, 10000), right = FALSE,
      labels = c("0", "0.1 to 9.9", "10.0 to 19.9", "20.0 to 29.9", ">= 30.0"), include.lowest = TRUE)
    table(dtm_train[, "density_hf_cat"])
    
    # Road network
    f_hist("road_density", dtm_train, c(NA, NA))
    dtm_train[, "road_density_cat"] <- cut(dtm_train[, "road_density"], breaks = c(0, 0.001, 0.25, 0.50, 0.75, 100), 
      labels = c("0 Km/Km^2", "0.01 to 0.24 Km/Km^2", "0.25 to 0.49 Km/Km^2", "0.50 to 0.74 Km/Km^2", ">= 0.75 Km/Km^2"),
      include.lowest = TRUE, right = FALSE)
    table(dtm_train[, "road_density_cat"])    
       
  #...................................      
  ## Other miscellaneous preparations
    
    # Come up with additional predictor: 1 / number of subdistricts of arrival within district of arrival
    dtm_train[, "subdis_count"] <- 1
    x1 <- aggregate(dtm_train[, "subdis_count"], by = dtm_train[, c("tm", "subdis_en_ocha", "dis_en_ocha")], FUN = sum)
    colnames(x1)[colnames(x1) == "x"] <- "n_subdis"
    dtm_train <- merge(dtm_train, x1, by = c("tm", "subdis_en_ocha", "dis_en_ocha"), all.x = TRUE)    
    table(dtm_train[, "n_subdis"])
    dtm_train[, "prop_subdis"] <- 1/ dtm_train[, "n_subdis"]
      # categorise
      f_hist("prop_subdis", dtm_train, c(NA, NA))
      dtm_train[, "prop_subdis_cat"] <- cut(dtm_train[, "prop_subdis"], 
        breaks = c(0, 0.10, 0.20, 0.50, 0.99, 100), 
        labels = c("<10%", "10 to 19%", "20 to 49%", "50 to 99%", "100%"),
        right = FALSE)
      table(dtm_train[, "prop_subdis_cat"])

    # Define additional outcomes
      # log of IDP households
      dtm_train[, "n_hh_ln"] <- log(dtm_train[, "n_hh"])
      
      # proportion of IDP households by subdistrict of arrival
        # number of IDP households per district of arrival
        x1 <- aggregate(dtm_train[, "n_hh"], by = dtm_train[, c("tm", "subdis_en_ocha", "dis_en_ocha")], FUN = sum, na.rm = TRUE)
        colnames(x1)[colnames(x1) == "x"] <- "n_hh_tot"
        dtm_train <- merge(dtm_train, x1, by = c("tm", "subdis_en_ocha", "dis_en_ocha"), all.x = TRUE)
        
        # proportion
        dtm_train[, "prop_hh"] <- dtm_train[, "n_hh"] / dtm_train[, "n_hh_tot"]
        hist(dtm_train[, "prop_hh"])
        range(dtm_train[, "prop_hh"])
      
      # category for proportion of IDP households
      dtm_train[, "prop_hh_cat"] <- cut(dtm_train[, "prop_hh"], breaks = c(0, 0.0001, 0.2, 0.4, 0.6, 0.8, 0.9999, 1), 
        labels = c(0:6), include.lowest = TRUE,
        ordered_result = TRUE)
      table(dtm_train[, "prop_hh_cat"])
     
    # Come up with additional predictor: log of total district IDP households
    dtm_train[, "n_hh_tot_ln"] <- log(dtm_train[, "n_hh_tot"])
      # categorise
      f_hist("n_hh_tot_ln", dtm_train, c(NA, NA))
      dtm_train[, "n_hh_tot_ln_cat"] <- cut(dtm_train[, "n_hh_tot_ln"], 
        breaks = c(0, 0.00001, 1, 2, 3, 100), 
        labels = c("0", "0.01 to 0.99", "1.00 to 1.99", "2.00 to 2.99", ">= 3.00"),
        right = FALSE)
      table(dtm_train[, "n_hh_tot_ln_cat"])
       
    # Write training data
    write_excel_csv(dtm_train, "yem_displacement_train_data_subdis.csv", na = "")
      

#.........................................................................................                            
### Fitting and evaluating models to impute number of subdistricts of arrival (Model 1)
#.........................................................................................    
     
  #...................................
  ## Aggregate training data by district of arrival

    # Read file if necessary
    if (! exists("dtm_train")) {dtm_train <- read.csv("yem_displacement_train_data_subdis.csv")}
    colnames(dtm_train) <- gsub("ï..", "", colnames(dtm_train))
    
    # Aggregate
    x1 <- c("n_hh", "outcome", "pop", "n_hf", "road_distance", "subdis_area", "n_events_rs_2", "n_fatalities_rs_2", "n_subdis")  
    dtm_train_dis <- aggregate(dtm_train[, x1], 
      by = dtm_train[, c("tm", "dis_ori_en_ocha", "subdis_ori_en_ocha", "dis_en_ocha")], FUN = sum, na.rm = TRUE)
    
    # Recreate predictors
    dtm_train_dis[, c("density_hf", "density_events_rs_2", "density_fatalities_rs_2")] <-
      dtm_train_dis[, c("n_hf", "n_events_rs_2", "n_fatalities_rs_2")] * 100000 / dtm_train_dis[, "pop"]
    dtm_train_dis[, "road_density"] <- dtm_train_dis[, "road_distance"] / dtm_train_dis[, "subdis_area"]
    dtm_train_dis[, "n_hh_ln"] <- log(dtm_train_dis[, "n_hh"])
    
  #...................................
  ## Fit and evaluate random forest model
    
    # Explore and recategorise outcome
    table(dtm_train_dis[, "outcome"])
      # to simplify, recategorise all instances of >=10 as 10 (only n = 43 )
      dtm_train_dis[which(dtm_train_dis[, "outcome"] %in% 10:50), "outcome"] <- 10
      
      # change single 0 observation to 1
      dtm_train_dis[which(dtm_train_dis[, "outcome"] == 0), "outcome"] <- 1
      table(dtm_train_dis[, "outcome"])
      
      # factorise outcome
      dtm_train_dis[, "outcome"] <- factor(dtm_train_dis[, "outcome"])
          
    # Grow random forest
      # define formula and select data
      outcome <- "outcome"
      formula_rf_m1 <- as.formula(paste(outcome, " ~ pop + density_events_rs_2 + density_fatalities_rs_2 +
        subdis_area + density_hf + road_density + n_subdis + n_hh_ln", sep = ""))
      dtm_fit <- dtm_train_dis[complete.cases(dtm_train_dis[, all.vars(formula_rf_m1)]), 
        all.vars(formula_rf_m1)]
      
      # attribute weights to balance data
      x1 <- prop.table(table(dtm_fit[, outcome]))
      x1 <- data.frame(x1)
      colnames(x1) <- c(outcome, "prop_outcome")
      x1[, "wt"] <- 1 / x1[, "prop_outcome"]
      dtm_fit <- merge(dtm_fit, x1, by = outcome, all.x = TRUE)
      
      # grow forest  
      fit_rf <- ranger(formula = formula_rf_m1, data = dtm_fit, classification = TRUE, num.trees = 500, 
        mtry = 2, case.weights = dtm_fit[, "wt"])
      print(fit_rf)
    
    # Predict on training data
    dtm_fit[, "pred"] <- predict(fit_rf, data = dtm_fit)$predictions
    
    # Check model performance on training data
    round(prop.table(t(table(dtm_fit[, c(outcome, "pred")])), margin = 2)*100, 1)

    # Check model performance on ten-fold cross-validation
      # shuffle dataset
      dtm_cv <- dtm_fit[sample(nrow(dtm_fit), nrow(dtm_fit), replace = FALSE), ]

      # split dataset into ten equal folds
      folds <- split(dtm_cv, (1:nrow(dtm_cv) %/% (nrow(dtm_cv) / 10)))
      if (length(folds) > 10) {folds <- folds[1:10]}
      
      # fit model on all the unfolded sets and track predictive accuracy of model fit on each fold
      out <- table(dtm_fit[, c(outcome, outcome)])
      out[,] <- 0
      for (i in 1:length(folds) ) {
        # control statement
        print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))

        # fit on all data but the fold
        x1 <- do.call(rbind, folds[-i])
        cv_fit <- update(fit_rf, data = x1, case.weights = x1[, "wt"])

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x2[, "pred"] <- predict(cv_fit, x2)$predictions

        # add model performance on fold data
        x3 <- table(x2[, c(outcome, "pred")])
        print(x3)
        out[1:nrow(x3), 1:ncol(x3)] <- out[1:nrow(x3), 1:ncol(x3)] + x3
      }

      # overall predictive performance on cross-validation
      x1 <- t(out)
      confusionMatrix(x1)
      x1 <- prop.table(x1, margin = 2)
      x1
      
      # write table
      x1 <- apply(x1, c(1,2), function (x) {sprintf("%1.1f%%", 100 * x)})
      x1 <- rbind(c("columns: observed number of subdistricts of arrival per district", "", "", "", "", "", "", "", "", ""),
        c("1", "2", "3", "4", "5", "6", "7", "8", "9", ">=10"), x1)
      x1 <- cbind(c("", "", "rows: predictions", "", "", "", "", "", "", "", "", ""), 
        c("", "", "1", "2", "3", "4", "5", "6", "7", "8", "9", ">=10"), x1)
      write.csv(x1, "yem_pop_model1_accuracy.csv", row.names = FALSE)
      
    # Save fit
    fit_rf_m1 <- fit_rf
    saveRDS(fit_rf_m1, file = "fit_rf_m1.rds")
        
        
        
#.........................................................................................                            
### Fitting and evaluating models to impute subdistrict of arrival data (Models 2 and 3)
#.........................................................................................    
     
  #...................................
  ## Explore correlations among variables
    
    # Read training data if needed
    if (! exists ("dtm_train")) {dtm_train <- read.csv("yem_displacement_train_data_subdis.csv")}
    colnames(dtm_train) <- gsub("ï..", "", colnames(dtm_train))
    
    # Generate boxplots of the outcome, by categorised predictor level    
      # select outcome and predictors
      outcome <- "prop_hh"
      x1 <- c("prop_pop_cat", "year_cat", "distance_cat", "n_events_rs_2_cat", "n_events_rs_6_cat",
        "density_hf_cat", "road_density_cat", "prop_subdis_cat")
      
      # make plots
      for (i in x1) {
        x2 <- subset(dtm_train, n_hh != 0)[, c(outcome, i)]
        colnames(x2) <- c("outcome", "x_var")
        assign(paste("plot_", i, sep = ""), ggplot(x2, aes(y = outcome, x = x_var) ) +
          geom_boxplot(lwd = 0.5, fill = palette_cb[sample.int(length(palette_cb), 1)], alpha = 0.5 ) +
            theme_bw() +
            labs(title = i) +
            theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
              axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5, size = 10))
          )
        }
    
      # identify plot names
      x2 <- paste("plot_", x1, sep = "" )
    
      # arrange plots in a grid and save
      plot <- ggarrange(plotlist = mget(x2), ncol = 3, nrow = ceiling(length(x1) / 3),
        common.legend = TRUE, legend = "right", align = "hv")
      plot
        
    
  #...................................      
  ## Fit and evaluate a random forest model to guess the right subdistricts of arrival

    # Grow forest
      # specify formula and select data
      formula_rf_m2 <- as.formula("outcome ~ pop + density_events_rs_2 + density_fatalities_rs_2 + 
        density_hf + subdis_area + road_density + prop_subdis + n_hh_tot_ln")
      dtm_fit <- dtm_train[complete.cases(dtm_train[, c(all.vars(formula_rf_m2), 
        "dis_en_ocha", "subdis_ori_en_ocha", "tm")]), c(all.vars(formula_rf_m2), "dis_en_ocha", "subdis_ori_en_ocha", "tm")]
    
      # create an aggregation id (by subdistrict, district of arrival and time)
      x1 <- unique(dtm_fit[, c("tm", "subdis_ori_en_ocha", "dis_en_ocha")])
      x1[, "id"] <- paste("id", 1:nrow(x1), sep = "")
      dtm_fit <- merge(dtm_fit, x1, by = c("tm", "subdis_ori_en_ocha", "dis_en_ocha"), all.x = TRUE)
      
      # create a unique id for each row (for later merging)
      dtm_fit <- dtm_fit[order(dtm_fit[, "id"]), ]
      dtm_fit[, "row"] <- as.character(1:nrow(dtm_fit))
      
      # compute the number of subdistricts of arrival per parent district (as a constraint on prediction)
      x1 <- aggregate(dtm_fit[, "outcome"], by = list(dtm_fit[, "id"]), FUN = sum)
      colnames(x1) <- c("id", "n_subdis_true")
      dtm_fit <- merge(dtm_fit, x1, by = "id", all.x = TRUE)
            
      # factorise outcome  
      dtm_fit[, "outcome"] <- factor(dtm_fit[, "outcome"])

      # attribute weights to balance data
      x1 <- prop.table(table(dtm_fit[, outcome]))
      x1 <- data.frame(x1)
      colnames(x1) <- c(outcome, "prop_outcome")
      x1[, "wt"] <- 1 / x1[, "prop_outcome"]
      dtm_fit <- merge(dtm_fit, x1, by = outcome, all.x = TRUE)
      
      # grow forest
      fit_rf <- ranger(formula = formula_rf_m2, data = dtm_fit, classification = TRUE, mtry = 3, num.trees = 500, 
        max.depth = 20, probability = TRUE, case.weights = dtm_fit[, "wt"])
      print(fit_rf)
    
    # Predict probabilities on training data
    dtm_fit[, "p_pred"] <- predict(fit_rf, data = dtm_fit)$predictions[, 2]
    dtm_fit[, "pred"] <- 0

    # Check model performance on training data: proportion of correct subdistrict guesses
      # constrained by only allowing exactly as many subdistricts per district to be predicted, as there are in reality
    x1 <- by(dtm_fit, dtm_fit[, "id"], function(x) {return(x[order(x[, "p_pred"], decreasing = TRUE), 
      c("id", "row", "n_subdis_true", "p_pred", "pred")])})
    x1 <- lapply(x1, function(x) {x[1:unique(x[, "n_subdis_true"]), "pred"] <- 1; return(x)})
    x1 <- do.call(rbind, x1)
    row.names(x1) <- NULL
    dtm_fit <- merge(dtm_fit[, colnames(dtm_fit) != "pred"], x1[, c("id", "row", "pred")], by = c("id", "row"), all.x = TRUE)
    table(dtm_fit[, c("outcome", "pred")])
    prop.table(table(dtm_fit[, c("outcome", "pred")]), margin = 1)  

    # Check model performance on ten-fold cross-validation
      # shuffle dataset (by id stratum)
      dtm_cv <- dtm_fit[, ! colnames(dtm_fit) %in% c("p_pred", "pred")]
      dtm_cv[, "pred"] <- 0
      dtm_cv <- split(dtm_cv, dtm_cv[, "id"])
      dtm_cv <- do.call(rbind, dtm_cv[sample(names(dtm_cv), length(dtm_cv), replace = FALSE)])
      row.names(dtm_cv) <- NULL

      # split dataset into ten equal folds (by id)
      x1 <- data.frame(unique(dtm_cv[, "id"]))
      x2 <- (1:nrow(x1) %/% (nrow(x1) / 10))
      x2[x2 == 0] <- 10
      table(x2)
      x1[, "split"] <- x2
      colnames(x1)[1] <- "id"
      dtm_cv <- merge(dtm_cv, x1, by = "id", all.x = TRUE)
      folds <- split(dtm_cv, dtm_cv[, "split"])
      if (length(folds) > 10) {folds <- folds[1:10]}
      rm(dtm_cv)
      
      # fit model on all the unfolded sets and track predictive accuracy of model fit on each fold
      out <- c()
      for (i in 1:length(folds) ) {
        # control statement
        print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))

        # fit on all data but the fold
        x1 <- do.call(rbind, folds[-i])
        cv_fit <- update(fit_rf, data = x1, case.weights = x1[, "wt"])

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x2[, "p_pred"] <- predict(cv_fit, x2)$predictions[, 2]
        x3 <- by(x2, x2[, "id"], function(x) {return(x[order(x[, "p_pred"], decreasing = TRUE), 
          c("id", "row", "n_subdis_true", "p_pred", "pred")])})
        x3 <- lapply(x3, function(x) {x[1:unique(x[, "n_subdis_true"]), "pred"] <- 1; return(x)})
        x3 <- do.call(rbind, x3)
        row.names(x3) <- NULL
        x2 <- merge(x2[, colnames(x2) != "pred"], x3[, c("id", "row", "pred")], by = c("id", "row"), all.x = TRUE)

        # add model performance on fold data
        print(table(x2[, c("outcome", "pred")]))
        if (i == 1) {out <- table(x2[, c("outcome", "pred")])}
        if (i > 1) {out <- out + table(x2[, c("outcome", "pred")])}
      }

      # overall predictive performance on cross-validation
      prop.table(out, margin = 1)

    # Save fit
    fit_rf_m2 <- fit_rf
    saveRDS(fit_rf_m2, file = "fit_rf_m2.rds")

    
  #...................................      
  ## Fit and evaluate a random forest model to predict the category of proportion of IDP households of arrival
    
    # Restrict analysis to subdistricts with IDP households and where the proportion of households < 100%
    dtm_fit <- subset(dtm_train, prop_hh != 1 & n_hh != 0)
    
    # Grow random forest
      # define formula and select data
      outcome <- "prop_hh_cat"
      formula_rf_m3 <- as.formula(paste(outcome, " ~ prop_pop + density_events_rs_2 + density_fatalities_rs_2 +
        density_hf + road_density + prop_subdis + n_hh_tot_ln_cat", sep = ""))
      dtm_fit <- dtm_fit[complete.cases(dtm_fit[, all.vars(formula_rf_m3)]), all.vars(formula_rf_m3)]
      
      # attribute weights to balance data
      x1 <- prop.table(table(dtm_fit[, outcome]))
      x1 <- data.frame(x1)
      colnames(x1) <- c(outcome, "prop_outcome")
      x1[, "wt"] <- 1 / x1[, "prop_outcome"]
      dtm_fit <- merge(dtm_fit, x1, by = outcome, all.x = TRUE)

      # factorise outcome
      dtm_fit[, outcome] <- factor(dtm_fit[, outcome])
            
      # grow forest  
      fit_rf <- ranger(formula = formula_rf_m3, data = dtm_fit, classification = TRUE, num.trees = 1000, max.depth = 15,
        mtry = 3, case.weights = dtm_fit[, "wt"])
      print(fit_rf)
    
     # Predict on training data
    dtm_fit[, "pred"] <- predict(fit_rf, data = dtm_fit)$predictions
    
    # Check model performance on training data
    prop.table(t(table(dtm_fit[, c(outcome, "pred")])), margin = 2)
    prop.table(t(fit_rf$confusion.matrix), margin = 2)  

    # Check model performance on ten-fold cross-validation
      # shuffle dataset
      dtm_cv <- dtm_fit[sample(nrow(dtm_fit), nrow(dtm_fit), replace = FALSE), ]

      # split dataset into ten equal folds
      folds <- split(dtm_cv, (1:nrow(dtm_cv) %/% (nrow(dtm_cv) / 10)))
      if (length(folds) > 10) {folds <- folds[1:10]}
      
      # fit model on all the unfolded sets and track predictive accuracy of model fit on each fold
      out <- c()
      for (i in 1:length(folds) ) {
        # control statement
        print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))

        # fit on all data but the fold
        x1 <- do.call(rbind, folds[-i])
        cv_fit <- update(fit_rf, data = x1, case.weights = x1[, "wt"])

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x2[, "pred"] <- predict(cv_fit, x2)$predictions

        # add model performance on fold data
        print(table(x2[, c(outcome, "pred")]))
        if (i == 1) {out <- table(x2[, c(outcome, "pred")])}
        if (i > 1) {out <- out + table(x2[, c(outcome, "pred")])}
      }

      # overall predictive performance on cross-validation
      x1 <- t(out)
      confusionMatrix(x1)
      x1 <- prop.table(x1, margin = 2)
      x1
      
      # write table
      x1 <- apply(x1, c(1,2), function (x) {sprintf("%1.1f%%", 100 * x)})
      x1 <- rbind(c("columns: observed proportion of IDP households within subdistrict", "", "", "", ""),
        c("1 to 19%", "20 to 39%", "40 to 59%", "60 to 79%", "80 to 99%"), x1)
      x1 <- cbind(c("", "", "rows: predictions", "", "", "", ""), 
        c("", "", "1 to 19%", "20 to 39%", "40 to 59%", "60 to 79%", "80 to 99%"), x1)
      write.csv(x1, "yem_pop_model3_accuracy.csv", row.names = FALSE)
      
    # Save fit
    fit_rf_m3 <- fit_rf
    saveRDS(fit_rf_m3, file = "fit_rf_m3.rds")

  #...................................      
  ## Clean up
  rm(cv_fit, dtm_cv, dtm_fit, dtm_train, dtm_train_dis, fit_rf, folds, x1, x2, x3)      
    
  
      
#.........................................................................................                            
### Applying models to impute subdistricts
#.........................................................................................    

  #...................................      
  ## Read in random forest models
  fit_rf_m1 <- readRDS("fit_rf_m1.rds")
  fit_rf_m2 <- readRDS("fit_rf_m2.rds")
  fit_rf_m3 <- readRDS("fit_rf_m3.rds")
  fit_rf_m1_ori <- readRDS("fit_rf_m1_ori.rds")
  fit_rf_m2_ori <- readRDS("fit_rf_m2_ori.rds")
  fit_rf_m3_ori <- readRDS("fit_rf_m3_ori.rds")
  
  #...................................      
  ## Identify and prepare DTM records where subdistricts are still missing
    
    # Discard ineligible observations
    dtm_agg <- subset(dtm, eligible == "yes")
  
    # Relabel missing subdistricts
    table(dtm_agg[, c("missing_subdis", "missing_subdis_ori")])
    dtm_agg[, "subdis_en_ocha"] <- ifelse(dtm_agg[, "missing_subdis"] == "yes", "missing", dtm_agg[, "subdis_en_ocha"])
    dtm_agg[, "subdis_ori_en_ocha"] <- ifelse(dtm_agg[, "missing_subdis_ori"] == "yes", "missing", dtm_agg[, "subdis_ori_en_ocha"])
    
    # Relabel dates of assessment to enable aggregation of incident data as well
    dtm_agg[, "month_assessment"] <- ifelse(is.na(dtm_agg[, "month_assessment"]), "n/a", dtm_agg[, "month_assessment"])
    dtm_agg[, "year_assessment"] <- ifelse(is.na(dtm_agg[, "year_assessment"]), "n/a", dtm_agg[, "year_assessment"])
    
    # Aggregate and reduce to essential variables
    dtm_agg <- aggregate(dtm_agg[, c("n_hh")], by = dtm_agg[, c("type", "month", "year", "tm", "month_assessment", "year_assessment",
      "dis_en_ocha", "subdis_en_ocha", "dis_ori_en_ocha", "subdis_ori_en_ocha")], FUN = sum)   
    colnames(dtm_agg)[colnames(dtm_agg) == "x"] <- "n_hh"   
    dtm_agg <- dtm_agg[order(dtm_agg[, "tm"], dtm_agg[, "dis_en_ocha"], dtm_agg[, "subdis_en_ocha"],
      dtm_agg[, "dis_ori_en_ocha"], dtm_agg[, "subdis_ori_en_ocha"]), ]
    colnames(dtm_agg)
    nrow(dtm_agg)
    
    # Create unique ID for later merging
    dtm_agg[, "id"] <- paste("id", 1:nrow(dtm_agg), sep = "")
  
    # Pull out records with missing subdistrict of arrival or origin
      # arrival
      table(dtm_agg[, "subdis_en_ocha"] == "missing")
      prop.table(table(dtm_agg[, "subdis_en_ocha"] == "missing"))
      dtm_agg_missing <- subset(dtm_agg, subdis_en_ocha == "missing")
      
      # origin
      table(dtm_agg[, "subdis_ori_en_ocha"] == "missing")
      prop.table(table(dtm_agg[, "subdis_ori_en_ocha"] == "missing"))
      dtm_agg_missing_ori <- subset(dtm_agg, subdis_ori_en_ocha == "missing")
    
      
  #...................................      
  ## Expand missing datasets so as to feature all candidate subdistricts 
        
    # Expand dataset so as to feature all candidate subdistricts of arrival within the known district of arrival
    x1 <- ocha[which(ocha[, "dis_en_ocha"] %in% unique(dtm_agg_missing[, "dis_en_ocha"])), c("dis_en_ocha", "subdis_en_ocha")]
    colnames(x1) <- c("dis_en_ocha", "subdis_en_ocha")
    dtm_agg_missing <- dtm_agg_missing[, colnames(dtm_agg_missing) != "subdis_en_ocha"]
    dtm_agg_missing <- merge(dtm_agg_missing, x1, by = "dis_en_ocha")
    colnames(dtm_agg_missing)[colnames(dtm_agg_missing) == "n_hh"] <- "n_hh_tot"
      
    # Expand dataset so as to feature all candidate subdistricts of origin within the known district of origin
    x1 <- ocha[which(ocha[, "dis_en_ocha"] %in% unique(dtm_agg_missing_ori[, "dis_ori_en_ocha"])), c("dis_en_ocha", "subdis_en_ocha")]
    colnames(x1) <- c("dis_ori_en_ocha", "subdis_ori_en_ocha")
    dtm_agg_missing_ori <- dtm_agg_missing_ori[, colnames(dtm_agg_missing_ori) != "subdis_ori_en_ocha"]
    dtm_agg_missing_ori <- merge(dtm_agg_missing_ori, x1, by = "dis_ori_en_ocha")
    colnames(dtm_agg_missing_ori)[colnames(dtm_agg_missing_ori) == "n_hh"] <- "n_hh_tot"
      
      
  #...................................      
  ## Add external predictor variables
  
    # To data with missing subdistrict of arrival
      # population predictor
      colnames(pop_pred) <- c("tm", "subdis_en_ocha", "pop", "prop_pop")
      dtm_agg_missing <- merge(dtm_agg_missing, pop_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
      
      # health facility predictor
      colnames(health_pred) <- c("subdis_en_ocha", "tm", "n_hf", "prop_hf", "density_hf")
      dtm_agg_missing <- merge(dtm_agg_missing, health_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_en_ocha", "subdis_area", "road_distance", "road_density", "prop_road_distance")
      dtm_agg_missing <- merge(dtm_agg_missing, road_pred, by = "subdis_en_ocha", all.x = TRUE)
            
      # distance predictor
      dtm_agg_missing <- merge(dtm_agg_missing, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor
      colnames(acled_pred)[colnames(acled_pred) == "subdis_ori_en_ocha"] <- "subdis_en_ocha"
      dtm_agg_missing <- merge(dtm_agg_missing, acled_pred[, ! colnames(acled_pred) %in% c("dis_en_ocha")], 
        by = c("tm", "subdis_en_ocha"), all.x = TRUE)
      
    # To data with missing subdistrict of origin
      # population predictor
      colnames(pop_pred) <- c("tm", "subdis_ori_en_ocha", "pop", "prop_pop")
      dtm_agg_missing_ori <- merge(dtm_agg_missing_ori, pop_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # health facility predictor
      colnames(health_pred) <- c("subdis_ori_en_ocha", "tm", "n_hf", "prop_hf", "density_hf")
      dtm_agg_missing_ori <- merge(dtm_agg_missing_ori, health_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_ori_en_ocha", "subdis_area", "road_distance", "road_density", "prop_road_distance")
      dtm_agg_missing_ori <- merge(dtm_agg_missing_ori, road_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
            
      # distance predictor
      dtm_agg_missing_ori <- merge(dtm_agg_missing_ori, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor
      colnames(acled_pred)[colnames(acled_pred) == "subdis_en_ocha"] <- "subdis_ori_en_ocha"
      dtm_agg_missing_ori <- merge(dtm_agg_missing_ori, acled_pred[, ! colnames(acled_pred) %in% c("dis_en_ocha")], 
        by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)


  #...................................      
  ## Generate additional internal predictor variables
    
    # Proportion of subdistricts within district
      # arrival
      dtm_agg_missing[, "subdis_count"] <- 1
      x1 <- aggregate(dtm_agg_missing[, "subdis_count"], 
        by = dtm_agg_missing[, c("id", "dis_en_ocha")], FUN = sum)
      colnames(x1)[colnames(x1) == "x"] <- "n_subdis"
      dtm_agg_missing <- merge(dtm_agg_missing, x1, by = c("id", "dis_en_ocha"), all.x = TRUE)    
      dtm_agg_missing[, "prop_subdis"] <- 1/ dtm_agg_missing[, "n_subdis"]
      range(dtm_agg_missing[, "prop_subdis"])
      
      # origin
      dtm_agg_missing_ori[, "subdis_count"] <- 1
      x1 <- aggregate(dtm_agg_missing_ori[, "subdis_count"], 
        by = dtm_agg_missing_ori[, c("id", "dis_ori_en_ocha")], FUN = sum)
      colnames(x1)[colnames(x1) == "x"] <- "n_subdis"
      dtm_agg_missing_ori <- merge(dtm_agg_missing_ori, x1, by = c("id", "dis_ori_en_ocha"), all.x = TRUE)    
      dtm_agg_missing_ori[, "prop_subdis_ori"] <- 1/ dtm_agg_missing_ori[, "n_subdis"]
      range(dtm_agg_missing_ori[, "prop_subdis_ori"])
      
    # Log of total district IDP households
      # arrival
      dtm_agg_missing[, "n_hh_tot_ln"] <- log(dtm_agg_missing[, "n_hh_tot"])
        # categorise
        dtm_agg_missing[, "n_hh_tot_ln_cat"] <- cut(dtm_agg_missing[, "n_hh_tot_ln"], 
          breaks = c(0, 0.00001, 1, 2, 3, 100), 
          labels = c("0", "0.01 to 0.99", "1.00 to 1.99", "2.00 to 2.99", ">= 3.00"),
          right = FALSE)
        table(dtm_agg_missing[, "n_hh_tot_ln_cat"])
         
      # origin
      dtm_agg_missing_ori[, "n_hh_tot_ln"] <- log(dtm_agg_missing_ori[, "n_hh_tot"])
        # categorise
        dtm_agg_missing_ori[, "n_hh_tot_ln_cat"] <- cut(dtm_agg_missing_ori[, "n_hh_tot_ln"], 
          breaks = c(0, 0.00001, 1, 2, 3, 100), 
          labels = c("0", "0.01 to 0.99", "1.00 to 1.99", "2.00 to 2.99", ">= 3.00"),
          right = FALSE)
        table(dtm_agg_missing_ori[, "n_hh_tot_ln_cat"])

 
  #...................................      
  ## Save missing datasets
  write_excel_csv(dtm_agg_missing, "yem_dtm_agg_missing.csv", na = "")
  write_excel_csv(dtm_agg_missing_ori, "yem_dtm_agg_missing_ori.csv", na = "")
   

  #...................................      
  ## Use model 1 to predict how many subdistricts per district send/receive IDPs
  
    # Read datasets if needed
    if (! exists("dtm_agg_missing")) {dtm_agg_missing <- read.csv("yem_dtm_agg_missing.csv")}
    if (! exists("dtm_agg_missing_ori")) {dtm_agg_missing_ori <- read.csv("yem_dtm_agg_missing_ori.csv")}
  
      # make sure ID column name has read well
      colnames(dtm_agg_missing) <- gsub("ï..", "", colnames(dtm_agg_missing))    
      colnames(dtm_agg_missing_ori) <- gsub("ï..", "", colnames(dtm_agg_missing_ori))    

    # Aggregate arrival dataset to district level
      # aggregate
      x1 <- c("pop", "n_hf", "road_distance", "subdis_area", "n_events_rs_2", "n_fatalities_rs_2")  
      dtm_agg_missing_dis <- aggregate(dtm_agg_missing[, x1], by = dtm_agg_missing[, c("id", "n_hh_tot", "n_subdis")], FUN = sum, na.rm = TRUE)
      
      # recreate predictors
      dtm_agg_missing_dis[, c("density_hf", "density_events_rs_2", "density_fatalities_rs_2")] <-
        dtm_agg_missing_dis[, c("n_hf", "n_events_rs_2", "n_fatalities_rs_2")] * 100000 / dtm_agg_missing_dis[, "pop"]
      dtm_agg_missing_dis[, "road_density"] <- dtm_agg_missing_dis[, "road_distance"] / dtm_agg_missing_dis[, "subdis_area"]
      colnames(dtm_agg_missing_dis)[colnames(dtm_agg_missing_dis) == "n_hh_tot"] <- "n_hh"
      dtm_agg_missing_dis[, "n_hh_ln"] <- log(dtm_agg_missing_dis[, "n_hh"])
  
    # Aggregate origin dataset to district level
      # omitted, as for subdistricts of origin assumption is only 1 subdistrict per district     
      
    # Predict number of subdistricts of arrival
    dtm_agg_missing_dis[, "n_subdis_pred"] <- predict(fit_rf_m1, data = dtm_agg_missing_dis)$predictions
    table(dtm_agg_missing_dis[, "n_subdis_pred"])
    table(is.na(dtm_agg_missing_dis[, "n_subdis_pred"]))
  
      # add predictions to subdistrict-level dataset
      dtm_agg_missing <- merge(dtm_agg_missing, dtm_agg_missing_dis[, c("id", "n_subdis_pred")], by = "id", all.x = TRUE)
      table(is.na(dtm_agg_missing[, "n_subdis_pred"]))

    # Predict number of subdistricts of origin
      # just predict 1 for all
      dtm_agg_missing_ori[, "n_subdis_pred"] <- 1

                  
  #...................................      
  ## Use model 2 to predict which candidate subdistricts send/receive IDPs
      # done in chunks to avoid R crashing
      # constrained by only allowing exactly as many subdistricts per district, as predicted by model 1
      
    # Recall formulae if needed
    if (! exists("formula_rf_m2")) {
    formula_rf_m2 <- as.formula("outcome ~ pop + density_events_rs_2 + density_fatalities_rs_2 + 
        density_hf + subdis_area + road_density + prop_subdis + n_hh_tot_ln") }
  
    if (! exists("formula_rf_m2_ori")) {
    formula_rf_m2_ori <- as.formula("outcome ~ pop + density_events_rs_2 + density_fatalities_rs_2 + 
        density_hf + subdis_area + road_density + prop_subdis_ori + n_hh_tot_ln") }

    # Subdistricts of arrival
      # predict probabilities for each subdistrict
      x1 <- all.vars(formula_rf_m2)
      x1 <- x1[-1]
      x2 <- which(complete.cases(dtm_agg_missing[, x1]))
      x2 <- split(x2, sort(x2%%10))
      x3 <- c()
      for (i in 1:length(x2)) {
        print(i)
        x4 <- predict(fit_rf_m2, data = dtm_agg_missing[x2[[i]], x1])$predictions[, 2]
      x3 <- c(x3, x4)
      }
      dtm_agg_missing[, "p_pred"] <- NA
      dtm_agg_missing[which(complete.cases(dtm_agg_missing[, x1])), "p_pred"] <- x3
      table(is.na(dtm_agg_missing[, "p_pred"]))
      dtm_agg_missing[, "pred"] <- ifelse(is.na(dtm_agg_missing[, "p_pred"]), NA, 0)

      # delete records where the prediction is missing (n = 748)
      dtm_agg_missing <- subset(dtm_agg_missing, ! is.na(p_pred) )
      
      # the n subdistricts with the highest probability are predicted, the others not (n = predicted by model 1)
      dtm_agg_missing[, "row"] <- as.character(1:nrow(dtm_agg_missing))
      x1 <- by(dtm_agg_missing, dtm_agg_missing[, "id"], function(x) {return(x[order(x[, "p_pred"], decreasing = TRUE), 
        c("id", "row", "n_subdis_pred", "p_pred", "pred")])})
      x1 <- lapply(x1, function(x) {x[1:unique(x[, "n_subdis_pred"]), "pred"] <- 1; return(x)})
      x1 <- do.call(rbind, x1)
      row.names(x1) <- NULL
      dtm_agg_missing <- merge(dtm_agg_missing[, colnames(dtm_agg_missing) != "pred"], x1[, c("id", "row", "pred")], by = c("id", "row"), all.x = TRUE)
    
      # delete records where the prediction is 0
      table(dtm_agg_missing[, "pred"])
        # do all ids have at least one predicted subdistrict?
        x1 <- aggregate(dtm_agg_missing[, "pred"], by = list("id" = dtm_agg_missing[, "id"]), FUN= sum)
        table(x1[, "x"]) # yes they do
      dtm_agg_missing <- subset(dtm_agg_missing, pred != 0)

    # Subdistricts of origin
      # predict probabilities for each subdistrict
      x1 <- all.vars(formula_rf_m2_ori)
      x1 <- x1[-1]
      x2 <- which(complete.cases(dtm_agg_missing_ori[, x1]))
      x2 <- split(x2, sort(x2%%10))
      x3 <- c()
      for (i in 1:length(x2)) {
        print(i)
        x4 <- predict(fit_rf_m2_ori, data = dtm_agg_missing_ori[x2[[i]], x1])$predictions[, 2]
      x3 <- c(x3, x4)
      }
      dtm_agg_missing_ori[, "p_pred"] <- NA
      dtm_agg_missing_ori[which(complete.cases(dtm_agg_missing_ori[, x1])), "p_pred"] <- x3
      table(is.na(dtm_agg_missing_ori[, "p_pred"]))
      dtm_agg_missing_ori[, "pred"] <- ifelse(is.na(dtm_agg_missing_ori[, "p_pred"]), NA, 0)

      # delete records where the prediction is missing (n = 3131)
      dtm_agg_missing_ori <- subset(dtm_agg_missing_ori, ! is.na(p_pred) )
      
      # the n subdistricts with the highest probability are predicted, the others not (n = predicted by model 1)
      dtm_agg_missing_ori[, "row"] <- as.character(1:nrow(dtm_agg_missing_ori))
      x1 <- by(dtm_agg_missing_ori, dtm_agg_missing_ori[, "id"], function(x) {return(x[order(x[, "p_pred"], decreasing = TRUE), 
        c("id", "row", "n_subdis_pred", "p_pred", "pred")])})
      x1 <- lapply(x1, function(x) {x[1:unique(x[, "n_subdis_pred"]), "pred"] <- 1; return(x)})
      x1 <- do.call(rbind, x1)
      row.names(x1) <- NULL
      dtm_agg_missing_ori <- merge(dtm_agg_missing_ori[, colnames(dtm_agg_missing_ori) != "pred"], x1[, c("id", "row", "pred")], by = c("id", "row"), all.x = TRUE)
    
      # delete records where the prediction is 0
      table(dtm_agg_missing_ori[, "pred"])
        # do all ids have at least one predicted subdistrict?
        x1 <- aggregate(dtm_agg_missing_ori[, "pred"], by = list("id" = dtm_agg_missing_ori[, "id"]), FUN= sum)
        table(x1[, "x"]) # yes they do
      dtm_agg_missing_ori <- subset(dtm_agg_missing_ori, pred != 0)
       
      
  #...................................      
  ## Use model 3 to predict the percent share of IDPs in the above subdistricts
      
    # Recall formulae if needed
    if (! exists("formula_rf_m3")) {
    formula_rf_m3 <- as.formula("prop_hh_cat ~ prop_pop + density_events_rs_2 + density_fatalities_rs_2 +
        density_hf + road_density + prop_subdis + n_hh_tot_ln_cat") }
  
    if (! exists("formula_rf_m3_ori")) {
    formula_rf_m3_ori <- as.formula("prop_hh_cat ~ prop_pop + density_events_rs_2 + density_fatalities_rs_2 +
        density_hf + road_density + prop_subdis_ori + n_hh_tot_ln_cat") }

    # Subdistricts of arrival
      # identify non-singleton subdistricts and predict percent share of IDP households for these
      x1 <- c(all.vars(formula_rf_m3), "n_subdis_pred")
      x1 <- x1[-1]
      x2 <- which(complete.cases(dtm_agg_missing[, x1]) & dtm_agg_missing[,  "n_subdis_pred"] != 1)
      x3 <- predict(fit_rf_m3, data = dtm_agg_missing[x2, x1])$predictions
      dtm_agg_missing[, "pred"] <- NA
      dtm_agg_missing[x2, "pred"] <- x3
      dtm_agg_missing[, "pred"] <- ifelse(dtm_agg_missing[, "n_subdis_pred"] == 1, 0, dtm_agg_missing[, "pred"])
      table(dtm_agg_missing[, "pred"])
      table(is.na(dtm_agg_missing[, "pred"]))
    
      # convert predicted category into a proportion (mid-point of category)
      x1 <- data.frame("pred" = c(0, 1, 2, 3, 4, 5), "prop_pred" = c(1, 0.1, 0.3, 0.5, 0.7, 0.9))
      dtm_agg_missing <- merge(dtm_agg_missing, x1, by = "pred", all.x = TRUE)
      table(dtm_agg_missing[, "prop_pred"])
      table(is.na(dtm_agg_missing[, "prop_pred"]))

      # scale prediction to 1 within the parent district
      x1 <- aggregate(dtm_agg_missing[, "prop_pred"], by = list(dtm_agg_missing[, "id"]), FUN = sum, na.rm = TRUE)
      colnames(x1) <- c("id", "prop_pred_tot")
      dtm_agg_missing <- merge(dtm_agg_missing, x1, by = "id", all.x = TRUE)
      dtm_agg_missing[, "prop_pred_scaled"] <- dtm_agg_missing[, "prop_pred"] / dtm_agg_missing[, "prop_pred_tot"]
      hist(dtm_agg_missing[, "prop_pred_scaled"])
      
      # compute number of IDP households and people
      dtm_agg_missing[, "n_hh"] <- dtm_agg_missing[, "n_hh_tot"] * dtm_agg_missing[, "prop_pred_scaled"]
      dtm_agg_missing[, "n_ppl"] <- round(dtm_agg_missing[, "n_hh"] * hh_size, digits = 0)
      
    # Subdistricts of origin
      # all proportions = 1 single only singleton subdistricts predicted
      dtm_agg_missing_ori[, "prop_pred_scaled"] <- 1
      
      # compute number of IDP households and people
      dtm_agg_missing_ori[, "n_hh"] <- dtm_agg_missing_ori[, "n_hh_tot"] * dtm_agg_missing_ori[, "prop_pred_scaled"]
      dtm_agg_missing_ori[, "n_ppl"] <- round(dtm_agg_missing_ori[, "n_hh"] * hh_size, digits = 0)
      
      
  #...................................      
  ## Merge imputed datasets together and with main DTM dataset
    
    # Remove unnecessary columns  
    x1 <- c("id", "type", "month", "year", "tm", "month_assessment", "year_assessment", "dis_en_ocha", "subdis_en_ocha", 
      "dis_ori_en_ocha", "subdis_ori_en_ocha", "n_hh", "n_ppl", "n_subdis") 
    dtm_agg_missing <- dtm_agg_missing[, x1]
    dtm_agg_missing_ori <- dtm_agg_missing_ori[, x1]
    
    # Start merged output
    dtm_agg_missing_merged <- c()  
    
    # Case 1: subdistrict of arrival imputed, subdistrict of origin already known
    dtm_agg_missing_merged <- subset(dtm_agg_missing, subdis_ori_en_ocha != "missing")  

    # Case 2: subdistrict of origin imputed, subdistrict of arrival already known
    dtm_agg_missing_merged <- rbind(dtm_agg_missing_merged, subset(dtm_agg_missing_ori, subdis_en_ocha != "missing") )
            
    # Case 3: subdistrict of arrival imputed, subdistrict of origin also imputed
    x1 <- subset(dtm_agg_missing, subdis_ori_en_ocha == "missing")
    x2 <- subset(dtm_agg_missing_ori, subdis_en_ocha == "missing")
    x1 <- merge(x1[, colnames(x1) != "subdis_ori_en_ocha"], x2[, c("id", "subdis_ori_en_ocha")], by = "id", all.x = TRUE)
    table(x1[, "subdis_ori_en_ocha"] == "missing")
    table(is.na(x1[, "subdis_ori_en_ocha"]))
    dtm_agg_missing_merged <- rbind(dtm_agg_missing_merged, x1)
    table(dtm_agg_missing_merged[, "subdis_en_ocha"] == "missing")
    table(dtm_agg_missing_merged[, "subdis_ori_en_ocha"] == "missing")

    # Append to main dataset
    dtm_agg_final <- subset(dtm_agg, subdis_en_ocha != "missing" & subdis_ori_en_ocha != "missing")
      # compute number of people
      dtm_agg_final[, "n_ppl"] <- round(dtm_agg_final[, "n_hh"] * hh_size, digits = 0)
    dtm_agg_missing_merged <- dtm_agg_missing_merged[, colnames(dtm_agg_final)]
      # add variable denoting which observation was imputed through models
      dtm_agg_final[, "imputed"] <- "no"
      dtm_agg_missing_merged[, "imputed"] <- "yes"
    dtm_agg_final <- rbind(dtm_agg_final, dtm_agg_missing_merged)
    
    # Verify missingness
    table(is.na(dtm_agg_final[, "subdis_en_ocha"]))
    table(is.na(dtm_agg_final[, "subdis_ori_en_ocha"]))
      # establish number of aggregate records for which missingness was not resolved
      # (n = 414, all due to ACLED starting Jan 2015)
      length(unique(dtm_agg[, "id"])) - length(unique(dtm_agg_final[, "id"]))
      x1 <- dtm_agg[, "id"] %in% dtm_agg_final[, "id"]
      View(dtm_agg[!x1, ])
      prop.table(table(x1))
      
    # Add OCHA governorates and write dataset
    x1 <- unique(ocha[, c("dis_en_ocha", "gov_en_ocha")])  
    dtm_agg_final <- merge(dtm_agg_final, x1, by = "dis_en_ocha", all.x = TRUE)
    colnames(x1) <- c("dis_ori_en_ocha", "gov_ori_en_ocha")
    dtm_agg_final <- merge(dtm_agg_final, x1, by = "dis_ori_en_ocha", all.x = TRUE)
    x1 <- c("id", "type", "month", "year", "tm", "month_assessment", "year_assessment", 
      "gov_en_ocha", "dis_en_ocha", "subdis_en_ocha",
      "gov_ori_en_ocha", "dis_ori_en_ocha", "subdis_ori_en_ocha",
      "n_hh", "n_ppl", "imputed")
    dtm_agg_final <- dtm_agg_final[, x1]
      # delete one record with 0 IDPs
      dtm_agg_final <- subset(dtm_agg_final, n_hh != 0)
    write_excel_csv(dtm_agg_final, "yem_displacement_dataset_imputed.csv")
  
    
#.........................................................................................    
### ENDS
#.........................................................................................
       
      
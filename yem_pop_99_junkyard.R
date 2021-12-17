        
           
#.........................................................................................                            
### Finding missing subdistricts: Method 5 (predictive model to impute subdistricts of origin)
#.........................................................................................    

  #...................................      
  ## Prepare dependent variable (outcome dataset)
    
    # Attribute unique ID to each DTM observation
    dtm <- dtm[order(dtm[, "who"], dtm[, "type"], dtm[, "gov_en_ocha"], dtm[, "dis_en_ocha"], dtm[, "subdis_en_ocha"],
      dtm[, "year_move"], dtm[, "month_move"]), ]
    dtm[, "id"] <- paste("dtm", 1:nrow(dtm), sep = "")
    
    # Select necessary variables of DTM and restrict to eligible observations
    df <- dtm[, c("id", "who", "type", "year_move", "month_move", "dis_en_ocha", "subdis_en_ocha", 
      "dis_ori_en_ocha", "subdis_ori_en_ocha", "n_hh")]
    df <- df[complete.cases(df[, c("id", "who", "type", "year_move", "month_move", "dis_en_ocha", "subdis_en_ocha", 
      "dis_ori_en_ocha")]), ]
    colnames(df)[colnames(df) == "subdis_ori_en_ocha"] <- "subdis_ori_en_ocha_right"
    
    # Expand dataset so as to feature all candidate subdistricts of origin within the known district of origin
    x1 <- ocha[which(ocha[, "dis_en_ocha"] %in% unique(df[, "dis_ori_en_ocha"])), c("dis_en_ocha", "subdis_en_ocha")]
    colnames(x1) <- c("dis_ori_en_ocha", "subdis_ori_en_ocha")
    df <- merge(df, x1, by = "dis_ori_en_ocha")
    
    # Further preparations ahead of merging with predictors
    colnames(df) <- gsub("_move", "", colnames(df))    
    df <- merge(df, t_units, by = c("month", "year"), all.x = TRUE)
    df <- subset(df, ! is.na(df[, "tm"])) # a few tm are missing because the year is specified as 'before or in 2014'
      
      # define outcome: NA if the subdistrict of origin is missing, 1 for the line with the right subdistrict, 0 otherwise
      df[, "outcome"] <- NA
      df[which(! is.na(df[, "subdis_ori_en_ocha_right"])), "outcome"] <- 0    
      df[which(df[, "subdis_ori_en_ocha_right"] == df[, "subdis_ori_en_ocha"]), "outcome"] <- 1    
          
    # Clean up
    df <- df[, c("id", "tm", "year", "who", "type", "n_hh", "subdis_en_ocha", "dis_ori_en_ocha", "subdis_ori_en_ocha", "outcome")]  
          

  #...................................      
  ## Merge data with predictors and prepare for model fitting

    # Merge with predictors
      # population predictor
      colnames(pop_pred) <- c("subdis_ori_en_ocha", "pop", "prop_pop")
      df <- merge(df, pop_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
      
      # health facility predictor
      colnames(health_pred) <- c("subdis_ori_en_ocha", "n_hf", "prop_hf", "density_hf")
      df <- merge(df, health_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_ori_en_ocha", "subdis_area", "road_distance", "road_density", "prop_road_distance")
      df <- merge(df, road_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
            
      # distance predictor
      df <- merge(df, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor
      colnames(acled_pred)[colnames(acled_pred) == "subdis_en_ocha"] <- "subdis_ori_en_ocha"
      df <- merge(df, acled_pred[, ! colnames(acled_pred) %in% c("dis_en_ocha")], 
        by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
    

  #...................................      
  ## Explore and categorise predictors
    
    # Share of district population
    f_hist("prop_pop", df, c(NA, NA))
    df[, "prop_pop_cat"] <- cut(df[, "prop_pop"], breaks = c(seq(0, 0.9, by = 0.1), 0.9999, 1.1), include.lowest = TRUE, 
      labels = c("0 to 9%", "10 to 19%", "20 to 29%", "30 to 39%", "40 to 49%", 
        "50 to 59%", "60 to 69%", "70 to 79%", "80 to 89%", "90 to 99%", "100%") )
    table(df[, "prop_pop_cat"])
    
    # Year
    table(df[, "year"])
    df[, "year_cat"] <- cut(as.numeric(df[, "year"]), breaks = c(0, 2015, 2016, 2017, 2021), include.lowest = TRUE, 
      labels = c("<=2015", "2016", "2017", "2018+") )
    table(df[, "year_cat"])
    
    # Distance of subdistrict
    f_hist("distance", df, c(NA, NA))
    df[, "distance_cat"] <- cut(df[, "distance"], breaks = c(0, 0.001, 25, 50, 100, 1000), include.lowest = TRUE, 
      labels = c("same subdistrict", "1 to 24Km", "25 to 49Km", "50 to 99Km", ">= 100Km") )
    table(df[, "distance_cat"])
    
    # Insecurity
    for (i in c(2, 6, 12) ) {
      f_hist(paste("n_events_rs_", i, sep = ""), df, c(NA, 20)) 
      df[, paste("n_events_rs_", i, "_cat", sep = "")] <- cut(df[, paste("n_events_rs_", i, sep = "")],
        breaks = c(0, 0.01, 1, 2, 5, 10, 10000), include.lowest = TRUE, 
        labels = c("none", "1", "2", "3 to 5", "6 to 10", ">= 10"))
    }
    for (i in c(2, 6, 12) ) {
      f_hist(paste("n_fatalities_rs_", i, sep = ""), df, c(NA, 10)) 
      df[, paste("n_fatalities_rs_", i, "_cat", sep = "")] <- cut(df[, paste("n_fatalities_rs_", i, sep = "")],
        breaks = c(0, 0.01, 5, 10000), include.lowest = TRUE, 
        labels = c("none", "1 to 5", ">= 5"))
    }
        
    # Health facilities
    f_hist("n_hf", df, c(0, 20))
    df[, "n_hf_cat"] <- cut(df[, "n_hf"], breaks = c(0, 0.001, 5, 10000), 
      labels = c("0", "1-4", ">= 5"), include.lowest = TRUE)
    
    # Road network
    f_hist("road_density", df, c(NA, NA))
    df[, "road_density_cat"] <- cut(df[, "road_density"], breaks = c(0, 0.001, 0.25, 0.50, 0.75, 100), 
      labels = c("0 Km/Km^2", "0.01 to 0.24 Km/Km^2", "0.25 to 0.49 Km/Km^2", "0.50 to 0.74 Km/Km^2", ">= 0.75 Km/Km^2"),
      include.lowest = TRUE)
    

  # #...................................
  # ## Fit and evaluate a conditional logistic model
  # 
  #   # Select training data (only IDPs, only multi-subdistrict districts)
  #   df_train <- subset(df, who == "idp" & ! is.na(outcome) & pop_prop != 1)
  # 
  #   # Fit model
  #   fit <- clogit(outcome ~ pop_prop_cat + distance_cat + road_density + density_hf + prop_events_rs_2 + strata(id), data = na.omit(df_train) )
  #   summary(fit)
  #   tidy(fit, exponentiate = TRUE)
  # 
  #   # Predict probabilities on training data
  #   df_train[complete.cases(df_train), "pred"] <- exp(predict(fit, na.omit(df_train)) )
  #   # df_train[complete.cases(df_train), "pred"] <- df_train[complete.cases(df_train), "pred"] /
  #   #   (1 + df_train[complete.cases(df_train), "pred"])
  # 
  #     # now rescale probabilities to unity (sum of all probabilities == 1 within each stratum / there can only be one 'winner')
  #     x1 <- by(df_train, df_train[, "id"], function (x) {
  #       return(cbind(x[, "id"], x[which.max(x[, "outcome"]), "subdis_ori_en_ocha"], x[, "subdis_ori_en_ocha"],
  #         x[, "pred"] / sum(x[, "pred"], na.rm = TRUE))) } )
  #     x1 <- data.frame(do.call(rbind, x1))
  #     colnames(x1) <- c("id", "correct", "candidate", "prob")
  # 
  #     # eliminate instances without a prediction
  #     x1 <- na.omit(x1)
  # 
  #   # Use probabilities to guess most likely subdistrict within each stratum (district),
  #     # and also capture the correct subdistrict
  #   x2 <- by(x1, x1[, "id"], function (x) {
  #     return(c(unique(x[, "id"]), x[which.max(x[, "prob"]), c("candidate")], unique(x[, "correct"])) )}
  #   )
  #   x2 <- data.frame(do.call(rbind, x2))
  #   colnames(x2) <- c("id", "predicted", "observed")
  # 
  #   # Check model performance on training data
  #     # Akaike information criterion
  #     AIC(fit)
  # 
  #     # proportion of correct subdistrict guesses
  #     x2[, "correct"] <- ifelse(x2[, "predicted"] == x2[, "observed"], "yes", "no")
  #     prop.table(table(x2[, "correct"]) )


  #...................................      
  ## Fit and evaluate a random forest model

    # Select training data (only IDPs, only multi-subdistrict districts)
    df_train <- subset(df, who == "idp" & ! is.na(outcome) & prop_pop != 1)
          
    # Grow forest
    formula_forest <- as.formula("outcome ~ prop_pop_cat + n_hh + distance + n_events_rs_2 + n_hf")    
    df_fit <- df_train[complete.cases(df_train[, all.vars(formula_forest)]), all.vars(formula_forest)]
    fit <- ranger(formula = formula_forest, data = df_fit, classification = TRUE, mtry = 2, 
      num.trees = 500)
    print(fit)
    
    # Predict probabilities on training data
    df_fit[, "pred"] <- predict(fit, df_fit)$predictions

    # Check model performance on training data: proportion of correct subdistrict guesses
    prop.table(table(df_fit[, c("outcome", "pred")]), margin = 1)  
    
    # Check model performance on ten-fold cross-validation
      # shuffle dataset
      df_cv <- df_fit[sample(nrow(df_fit), nrow(df_fit), replace = FALSE), ]

      # split dataset into ten equal folds
      folds <- split(df_cv, (1:nrow(df_cv) %/% (nrow(df_cv) / 5)))

      # fit model on all the unfolded sets and track predictive accuracy of model fit on each fold
      out <- c()
      for (i in 1:length(folds) ) {
        # control statement
        print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))

        # fit on all data but the fold
        x1 <- do.call(rbind, folds[-i])
        cv_fit <- ranger(formula = formula_forest, data = na.omit(x1), classification = TRUE)

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x2[complete.cases(x2), "pred"] <- predict(cv_fit, na.omit(x2))$predictions

        # add model performance on fold data
        print(table(x2[, c("outcome", "pred")]))
        if (i == 1) {out <- table(x2[, c("outcome", "pred")])}
        if (i > 1) {out <- out + table(x2[, c("outcome", "pred")])}
      }

      # overall predictive performance on cross-validation
      prop.table(out, margin = 1)


      
      
  #...................................      
  ## Impute missing subdistricts of origin using random forest model

      
      
            
      


















#.........................................................................................                            
### Finding missing subdistricts: Method 5b (predictive model to impute subdistricts of origin)
#.........................................................................................    

  #...................................      
  ## Identify model training data (subset of DTM with high completeness for subdistrict of origin)
    
    # Sort DTM dataset
    dtm <- dtm[order(dtm[, "who"], dtm[, "type"], dtm[, "gov_en_ocha"], dtm[, "dis_en_ocha"], dtm[, "subdis_en_ocha"],
      dtm[, "year_move"], dtm[, "month_move"]), ]

    # Describe subdistrict of origin missingness by date of assessment (trying to identify a dataset for model training)
    prop.table(table(dtm[, c("year_assessment", "month_assessment", "missing_subdis_ori")]), margin = c(1, 2))*100
      # how many observations have missingness <12% (Nov 2018)? = 42,578 -> training data
      table(dtm[, c("year_assessment", "month_assessment", "missing_subdis_ori")])
    
    # Select training data
    df_train <- subset(dtm, month_assessment == 11 & year_assessment == 2018 & 
        missing_subdis_ori == "no" & missing_subdis == "no" & who == "idp")
      
    # Restrict to eligible observations
    df_train <- df_train[, c("year_move", "month_move", "dis_en_ocha", "subdis_en_ocha", 
      "dis_ori_en_ocha", "subdis_ori_en_ocha", "n_hh")]
    df_train <- df_train[complete.cases(df_train[, c("year_move", "month_move", "dis_en_ocha", "subdis_en_ocha", 
      "dis_ori_en_ocha", "subdis_ori_en_ocha")]), ]
    colnames(df_train) <- gsub("_move", "", colnames(df_train))    
    df_train <- merge(df_train, t_units, by = c("month", "year"), all.x = TRUE)
    df_train <- subset(df_train, ! is.na(df_train[, "tm"])) # a few tm are missing because the year is specified as 'before or in 2014'
 
    # Expand dataset so as to feature all candidate subdistricts of origin within the known district of origin
    x1 <- ocha[which(ocha[, "dis_en_ocha"] %in% unique(df_train[, "dis_ori_en_ocha"])), c("dis_en_ocha", "subdis_en_ocha")]
    colnames(x1) <- c("dis_ori_en_ocha", "subdis_ori_en_ocha")
    colnames(df_train)[colnames(df_train) == "subdis_ori_en_ocha"] <- "subdis_ori_en_ocha_obs"
    df_train <- merge(df_train, x1, by = "dis_ori_en_ocha")
    
      # for all subdistricts that have been added in this expansion, n of IDP households == 0
      df_train[, "n_hh"] <- ifelse(df_train[, "subdis_ori_en_ocha_obs"] == df_train[, "subdis_ori_en_ocha"], 
        df_train[, "n_hh"], 0)
       
  #...................................      
  ## Aggregate training data by month and year of IDP arrival, subdistrict and subdistrict of origin
    
    # Aggregate
    df_train <- aggregate(df_train[, c("n_hh")], by = df_train[, c("month", "year", "tm", "subdis_en_ocha", "dis_ori_en_ocha",
      "subdis_ori_en_ocha")], FUN = sum)   
    colnames(df_train)[colnames(df_train) == "x"] <- "n_hh"   

    # Prepare stratum variable (unique instance of displacement date, subdistrict of arrival and district of origin)
    x1 <- unique(df_train[, c("tm", "subdis_en_ocha", "dis_ori_en_ocha")])
    x1[, "stratum"] <- paste("s", 1:nrow(x1), sep ="" )
    df_train <- merge(df_train, x1, by = c("tm", "subdis_en_ocha", "dis_ori_en_ocha"), all.x = TRUE)
    df_train[, "stratum"] <- factor(df_train[, "stratum"])
      
  #...................................      
  ## Merge data with predictors and prepare for model fitting

    # Merge with predictors
      # population predictor
      colnames(pop_pred) <- c("subdis_ori_en_ocha", "pop", "pop_prop")
      df_train <- merge(df_train, pop_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
            
      # distance predictor
      df_train <- merge(df_train, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor
      colnames(acled_pred)[colnames(acled_pred) == "subdis_en_ocha"] <- "subdis_ori_en_ocha"
      df_train <- merge(df_train, acled_pred[, ! colnames(acled_pred) %in% c("dis_en_ocha")], 
        by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
    
    # Compute alternative outcomes (proportion of all IDPs from district of origin)  
    x1 <- aggregate(df_train[, "n_hh"], by = df_train[, c("tm", "subdis_en_ocha", "dis_ori_en_ocha")], FUN = sum )
    colnames(x1) <- c("tm", "subdis_en_ocha", "dis_ori_en_ocha", "n_hh_tot")
    df_train <- merge(df_train, x1, by = c("tm", "subdis_en_ocha", "dis_ori_en_ocha"), all.x = TRUE)
    df_train[, "prop_hh"] <- df_train[, "n_hh"] / df_train[, "n_hh_tot"]
      # as beta regression does not accept values of 0 or 1 for the outcome, modify any such instances to 0.01 and 0.99
      df_train[, "prop_hh"] <- ifelse(df_train[, "prop_hh"] == 0, 0.01, df_train[, "prop_hh"])
      df_train[, "prop_hh"] <- ifelse(df_train[, "prop_hh"] == 1, 0.99, df_train[, "prop_hh"])
      hist(df_train[, "prop_hh"])
      
      # also compute categorical option
      df_train[, "prop_hh_cat"] <- cut(df_train[, "prop_hh"], breaks = c())
      
  #...................................      
  ## Explore and categorise predictors
    
    # Share of district population
    f_hist("pop_prop", df_train, c(NA, NA))
    df_train[, "pop_prop_cat"] <- cut(df_train[, "pop_prop"], breaks = seq(0, 1, by = 0.2), include.lowest = TRUE, 
      labels = c("0 to 19%", "20 to 39%", "40 to 59%", "60 to 79%", ">=80%") )
    table(df_train[, "pop_prop_cat"])
    
    # year
    table(df_train[, "year"])
    df_train[, "year_cat"] <- cut(as.numeric(df_train[, "year"]), breaks = c(0, 2015, 2016, 2017, 2021), include.lowest = TRUE, 
      labels = c("<=2015", "2016", "2017", "2018+") )
    table(df_train[, "year_cat"])
    
    # distance of subdistrict
    f_hist("distance", df_train, c(NA, NA))
    df_train[, "distance_cat"] <- cut(df_train[, "distance"], breaks = c(0, 0.001, 25, 50, 100, 1000), include.lowest = TRUE, 
      labels = c("same subdistrict", "1 to 24Km", "25 to 49Km", "50 to 99Km", ">= 100Km") )
    table(df_train[, "distance_cat"])
    
    # insecurity
    for (i in c(2, 6, 12) ) {
      f_hist(paste("n_events_rs_", i, sep = ""), df_train, c(NA, 20)) 
      df_train[, paste("n_events_rs_", i, "_cat", sep = "")] <- cut(df_train[, paste("n_events_rs_", i, sep = "")],
        breaks = c(0, 0.01, 1, 2, 5, 10, 10000), include.lowest = TRUE, 
        labels = c("none", "1", "2", "3 to 5", "6 to 10", ">= 10"))
    }
    for (i in c(2, 6, 12) ) {
      f_hist(paste("n_fatalities_rs_", i, sep = ""), df_train, c(NA, 10)) 
      df_train[, paste("n_fatalities_rs_", i, "_cat", sep = "")] <- cut(df_train[, paste("n_fatalities_rs_", i, sep = "")],
        breaks = c(0, 0.01, 5, 10000), include.lowest = TRUE, 
        labels = c("none", "1 to 5", ">= 5"))
    }
        

  #...................................      
  ## Fit and evaluate Poisson model
    
    # Restrict data
    x1 <- c("pop_prop_cat", "distance_cat", "n_events_rs_6_cat")
    df_fit <- df_train[complete.cases(df_train[, x1]), ]
    
    # Fit model  
    fit <- glm(n_hh ~ pop + distance + n_events_rs_6, data = df_fit, family = poisson)
    summary(fit)
    tidy(fit, exponentiate = TRUE)
      
    # Evaluate predictions on training data
      # predict
      df_fit[, "pred"] <- predict(fit, data = df_fit, type = "response")

      # compute Akaike information criterion
      AIC(fit)
    
      # plot predictions vs observations
      x1 <- df_fit[, c("n_hh", "pred")]  

      plot <- ggplot(x1) + geom_point(aes(x = n_hh, y = pred), size = 2, colour = "steelblue") + 
        theme_bw() +
        scale_x_continuous("observed number of households from subdistrict", limits=c(0, 20)) +
        scale_y_continuous("predicted number of household from subdistrict", limits=c(0, 20)) +  
        geom_abline(intercept = 0, slope = 1, colour="red") +
        theme(axis.title = element_text(colour = "grey20"))
      plot
    
    
          
  #...................................      
  ## Fit and evaluate conditional Poisson model
    
    # Restrict data
    x1 <- c("pop_prop_cat", "distance_cat", "n_events_rs_6_cat")
    df_fit <- subset(df_train[complete.cases(df_train[, x1]), ], pop_prop != 1)
    
    # Fit model  
    fit <- gnm(n_hh ~ pop_prop_cat + distance_cat + n_events_rs_6_cat, data = df_fit, 
      eliminate = stratum, family = poisson)
    summary(fit)
    tidy(fit, exponentiate = TRUE)
      
    # Evaluate predictions on training data
      # predict
      df_fit[, "pred"] <- predict(fit, data = df_fit, type = "response")

      # compute Akaike information criterion
      AIC(fit)
    
      # plot predictions vs observations
      x1 <- df_fit[, c("n_hh", "pred")]  
      plot <- ggplot(x1) + geom_point(aes(x = n_hh, y = pred), size = 2, colour = "steelblue") + 
        theme_bw() +
        scale_x_continuous("observed number of households from subdistrict", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +
        scale_y_continuous("predicted number of household from subdistrict", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +  
        geom_abline(intercept = 0, slope = 1, colour="red") +
        theme(axis.title = element_text(colour = "grey20"))
      plot

  #...................................      
  ## Fit and evaluate beta regression
    
    # Restrict data
    x1 <- c("pop_prop_cat", "distance_cat", "n_events_rs_6_cat")
    df_fit <- subset(df_train[complete.cases(df_train[, x1]), ], pop_prop != 1 & prop_hh != 0 & prop_hh != 1)
    
    # Fit model  
    fit <- betareg(prop_hh ~ pop_prop_cat + distance_cat + n_events_rs_6_cat, data = df_fit)
    summary(fit)
    tidy(fit, exponentiate = TRUE)
      
    # Evaluate predictions on training data
      # predict
      df_fit[, "pred"] <- predict(fit, data = df_fit, type = "response")

      # compute Akaike information criterion
      AIC(fit)
    
      # plot predictions vs observations
      x1 <- df_fit[, c("prop_hh", "pred")]  

      plot <- ggplot(x1) + geom_point(aes(x = prop_hh, y = pred), size = 2, colour = "steelblue") + 
        theme_bw() +
        scale_x_continuous("observed number of households from subdistrict", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +
        scale_y_continuous("predicted number of household from subdistrict", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +  
        geom_abline(intercept = 0, slope = 1, colour="red") +
        theme(axis.title = element_text(colour = "grey20"))
      plot

      
      
  #...................................      
  ## Fit and evaluate random forest
    
    # Restrict data
    x1 <- c("pop_prop_cat", "distance_cat", "n_events_rs_6_cat")
    df_fit <- subset(df_train[complete.cases(df_train[, x1]), ], pop_prop != 1)
    
    # Fit model  
    fit <- ranger(n_hh ~ distance + n_events_rs_6, data = df_fit, 
      keep.inbag = TRUE, importance = "permutation", num.trees = 1000)
    fit
      
    # Evaluate predictions on training data
      # predict
      df_fit[, "pred"] <- predict(fit, data = df_fit)$predictions

      # plot predictions vs observations
      x1 <- df_fit[, c("n_hh", "pred")]  

      plot <- ggplot(x1) + geom_point(aes(x = n_hh, y = pred), size = 2, colour = "steelblue") + 
        theme_bw() +
        scale_x_continuous("observed number of households from subdistrict", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +
        scale_y_continuous("predicted number of household from subdistrict", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +  
        geom_abline(intercept = 0, slope = 1, colour="red") +
        theme(axis.title = element_text(colour = "grey20"))
      plot

      
  #...................................      
  ## Fit and evaluate a gradient boosting model to predict the number of IDP households

    # Restrict analysis to subdistricts with IDP households and where the proportion of household < 100%
    dtm_fit <- subset(dtm_train, prop_hh != 1 & n_hh != 0)
          
    # Grow Gradient Boosting Machine (GBM) model
    outcome <- "prop_hh_cat"
    # formula_gbm <- as.formula(paste(outcome, " ~ pop + distance + n_events_rs_2 + year_cat + density_hf + road_density +
    #  prop_subdis_ori", sep = ""))
    formula_gbm <- as.formula(paste(outcome, " ~ prop_pop + distance + n_events_rs_2 + year + density_hf + road_density +
        prop_subdis_ori + n_hh_tot_ln_cat", sep = ""))
    
    fit_gbm <- gbm(formula = formula_gbm, data = dtm_fit, distribution = "multinomial", cv.folds = 10, n.trees = 5000,
      shrinkage = 0.01, interaction.depth = 3)
    gbm.perf(fit_gbm, method = "cv")
    print(fit_gbm)
    
    # Predict on training data
    dtm_fit[, "pred"] <- predict(fit_gbm, dtm_fit, type = "response", n.trees = 5000)
    
    # Plot model performance on training data: observed versus predicted number of IDP households
      x1 <- dtm_fit[, c(outcome, "pred")]
      colnames(x1) <- c("obs", "pred")
      plot <- ggplot(x1) + geom_point(aes(x = obs, y = pred), size = 2, colour = palette_cb[6], alpha = 0.3) + 
        theme_bw() +
        scale_x_continuous("observed", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +
        scale_y_continuous("predicted", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +  
        geom_abline(intercept = 0, slope = 1, colour = palette_cb[7], alpha = 0.5, size = 1) +
        theme(axis.title = element_text(colour = "grey20"))
      plot

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
        cv_fit <- gbm(formula = formula_gbm, data = x1, distribution = "gaussian", n.trees = 5000,
          shrinkage = 0.1, interaction.depth = 3)

        # predict on fold data
        x2 <- folds[[i]]
        x2[, "pred"] <- predict(cv_fit, x2, n.trees = 5000)

        # add model performance on fold data
        if (i == 1) {out <- x2[, c(outcome, "pred")]}
        if (i > 1) {out <- rbind(out, x2[, c(outcome, "pred")])}
      }

      # plot predictive performance on cross-validation
      x1 <- out[, c(outcome, "pred")]
      colnames(x1) <- c("obs", "pred")
      plot <- ggplot(x1) + geom_point(aes(x = obs, y = pred), size = 2, colour = palette_cb[6], alpha = 0.3) + 
        theme_bw() +
        scale_x_continuous("observed", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +
        scale_y_continuous("predicted", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +  
        geom_abline(intercept = 0, slope = 1, colour = palette_cb[7], alpha = 0.5, size = 1) +
        theme(axis.title = element_text(colour = "grey20"))
      plot

      
      
      
      
  #...................................
  ## Fit and evaluate an ordinal logistic regression model to predict the category of the proportion of IDP households

    # Restrict analysis to subdistricts with IDP households
    dtm_train2 <- subset(dtm_train, n_hh != 0)

    # Fit model
      # only retain observations where the proportion of households is < 100%
      dtm_fit <- subset(dtm_train2, prop_hh != 1)

      # define formula and select data
      outcome <- "prop_hh_cat"
      formula_glm <- as.formula(paste(outcome, " ~ road_density_cat", sep = ""))
      dtm_fit <- dtm_fit[complete.cases(dtm_fit[, all.vars(formula_glm)]), all.vars(formula_glm)]

      # fit model
      fit_glm <- polr(formula_glm, data = dtm_fit, Hess = TRUE)
      summary(fit_glm)
      x1 <- coef(summary(fit_glm))
      x1 <- cbind(x1, "p-value" = pnorm(abs(x1[, "t value"]), lower.tail = FALSE) * 2)
      x1[, c(1,2)] <- exp(x1[, c(1,2)])
      x1

    # Predict probabilities on training data
    dtm_fit[, "pred"] <- predict(fit_glm, dtm_fit, type = "class")

    # Check model performance on training data
    prop.table(table(dtm_fit[, c(outcome, "pred")]), margin = 1)

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
        cv_fit <- update(fit_glm, data = x1)

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x2[, "pred"] <- predict(cv_fit, x2, type = "class")

        # add model performance on fold data
        print(table(x2[, c(outcome, "pred")]))
        if (i == 1) {out <- table(x2[, c(outcome, "pred")])}
        if (i > 1) {out <- out + table(x2[, c(outcome, "pred")])}
      }

      # overall predictive performance on cross-validation
      prop.table(out, margin = 1)


  #...................................      
  ## Fit and evaluate a quantile regression model to predict the number of IDP households
    
    # Restrict analysis to subdistricts with IDP households
    dtm_train2 <- subset(dtm_train, n_hh != 0)

    # Fit model
      # only retain observations where the proportion of households is < 100%
      dtm_fit <- subset(dtm_train2, prop_hh != 1)

      # define formula and select data
      outcome <- "prop_hh"
      formula_glm <- as.formula(paste(outcome, " ~ road_density", sep = ""))
      dtm_fit <- dtm_fit[complete.cases(dtm_fit[, all.vars(formula_glm)]), all.vars(formula_glm)]

      # fit model
      fit_glm <- rq(formula_glm, data = dtm_fit, tau = 0.5)
      summary(fit_glm)

    # Predict on training data
    dtm_fit[, "pred"] <- predict(fit_glm, dtm_fit)
    
    # Plot model performance on training data: observed versus predicted number of IDP households
      x1 <- dtm_fit[, c(outcome, "pred")]
      colnames(x1) <- c("obs", "pred")
      plot <- ggplot(x1) + geom_point(aes(x = obs, y = pred), size = 2, colour = palette_cb[6], alpha = 0.3) + 
        theme_bw() +
        scale_x_continuous("observed", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +
        scale_y_continuous("predicted", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +  
        geom_abline(intercept = 0, slope = 1, colour = palette_cb[7], alpha = 0.5, size = 1) +
        theme(axis.title = element_text(colour = "grey20"))
      plot

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
        cv_fit <- gbm(formula = formula_gbm, data = x1, distribution = "gaussian", n.trees = 5000,
          shrinkage = 0.1, interaction.depth = 3)

        # predict on fold data
        x2 <- folds[[i]]
        x2[, "pred"] <- predict(cv_fit, x2, n.trees = 5000)

        # add model performance on fold data
        if (i == 1) {out <- x2[, c(outcome, "pred")]}
        if (i > 1) {out <- rbind(out, x2[, c(outcome, "pred")])}
      }

      # plot predictive performance on cross-validation
      x1 <- out[, c(outcome, "pred")]
      colnames(x1) <- c("obs", "pred")
      plot <- ggplot(x1) + geom_point(aes(x = obs, y = pred), size = 2, colour = palette_cb[6], alpha = 0.3) + 
        theme_bw() +
        scale_x_continuous("observed", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +
        scale_y_continuous("predicted", limits=c(0, 1.1*max(x1[, 1:2], na.rm=TRUE))) +  
        geom_abline(intercept = 0, slope = 1, colour = palette_cb[7], alpha = 0.5, size = 1) +
        theme(axis.title = element_text(colour = "grey20"))
      plot

      
  #...................................
  ## Fit and evaluate a gradient boosting model to predict the number of IDP households

    # Restrict analysis to subdistricts with IDP households and where the proportion of household < 100%
    dtm_fit <- subset(dtm_train, prop_hh != 1 & n_hh != 0)
          
    # Grow Gradient Boosting Machine (GBM) model
    outcome <- "prop_hh_cat"
    formula_gbm <- as.formula(paste(outcome, " ~ prop_pop + distance + n_events_rs_2 + year + density_hf + road_density +
      prop_subdis_ori + n_hh_tot_ln_cat", sep = ""))
    dtm_fit <- dtm_fit[complete.cases(dtm_fit[, all.vars(formula_gbm)]), all.vars(formula_gbm)]
    
    fit_gbm <- gbm(formula = formula_gbm, data = dtm_fit, distribution = "multinomial", cv.folds = 10, n.trees = 500,
      shrinkage = 0.01, interaction.depth = 3)
    gbm.perf(fit_gbm, method = "cv")
    print(fit_gbm)
    
    # Predict categories on training data
    x1 <- predict(fit_gbm, data = dtm_fit, n.trees = 251, type = "response")
    x1 <- matrix(x1, ncol = 5)
    dtm_fit[, "pred"] <- apply(x1, 1, which.max)
    
    # Check model performance on training data: proportion of correct subdistrict guesses
    table(dtm_fit[, c(outcome, "pred")])
    prop.table(table(dtm_fit[, c(outcome, "pred")]), margin = 1)
     
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
        cv_fit <- update(fit_gbm, data = x1)

        # predict probabilities on fold data
        x2 <- folds[[i]]
        x3 <- predict(cv_fit, x2, n.trees = 500, type = "response")
        x3 <- matrix(x3, ncol = 5)
        x2[, "pred"] <- apply(x3, 1, which.max)

        # add model performance on fold data
        print(table(x2[, c(outcome, "pred")]))
        if (i == 1) {out <- table(x2[, c(outcome, "pred")])}
        if (i > 1) {out <- out + table(x2[, c(outcome, "pred")])}
      }

      # overall predictive performance on cross-validation
      x1 <- prop.table(out, margin = 1)
      x1
      

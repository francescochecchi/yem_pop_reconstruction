#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## ----------------------- R SCRIPT TO COMPUTE DISPLACEMENT FLOWS ---------------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Nov 2021)

                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................                            
### Generating exploratory graphs
#.........................................................................................    
    
  #.........................................
  ## Read imputed displacement dataset
  dtm <- read.csv("yem_displacement_dataset_imputed.csv")
  colnames(dtm) <- gsub("ï..", "", colnames(dtm))

  #.........................................
  ## Graph prevalent data availability by subdistrict of arrival
    # (number of subdistricts with data, by governorate and date of assessment)

    # Prepare data for plotting
      # select observations
      x1 <- subset(dtm, type == "prevalent")
      
      # aggregate number of subdistricts of arrival by assessment date and governorate
      x1 <- unique(x1[, c("subdis_en_ocha", "year_assessment", "month_assessment", "gov_en_ocha")])
      x1[, "n_subdis"] <- 1
      x1 <- aggregate(x1[, "n_subdis"], by = x1[, c("year_assessment", "month_assessment", "gov_en_ocha")], FUN = sum)
      colnames(x1) <- c("year", "month", "gov_en_ocha", "n_subdis")

      # merge with governorate and time unit time series
      x2 <- unique(ts[, c("gov_en_ocha", "year", "month")])
      x2 <- merge(x2, x1, by = c("year", "month", "gov_en_ocha"), all.x = TRUE)
      
      # add dates and grid lines
      x2[, "date"] <- dmy(paste("1", x2[, "month"], x2[, "year"], sep = "-"))
      vlines <- seq(dmy("15-01-2016"), dmy("15-12-2018"), by = "months")
      hlines <- seq(1:length(unique(x2[, "gov_en_ocha"]))) - 0.5
      
    # Plot
    plot <- ggplot(subset(x2, year %in% 2016:2018 ), aes(x = date, y = factor(gov_en_ocha), fill = n_subdis)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = n_subdis), size = 3, colour = "grey20") +
      theme_bw() +
      geom_vline(xintercept = vlines, colour = palette_cb[6], size = 0.5) +
      geom_hline(yintercept = hlines, colour = palette_cb[6], size = 0.5) +
      scale_y_discrete("governorate", expand = c(0,0) ) +
      scale_fill_gradient(na.value = "white", low = "darkseagreen1", high = "darkseagreen4" ) +
      labs(x = "date", y = "governorate", fill = "number of subdistricts") +
      scale_x_date(date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
      theme(plot.margin = unit(c(0,1,0,0), "cm"), legend.position = "none") +
      theme(axis.text = element_text(size = 9, colour = "grey20"),
        axis.title = element_text(size = 10, colour = "grey20") )
    plot
    ggsave("yem_pop_idp_data_coverage_subdis.png", height = 15, width = 25, units = "cm", dpi = "print")

    
  #.........................................
  ## Graph prevalent data availability by district of arrival
    # (number of districts with data, by governorate and date of assessment)

    # Prepare data for plotting
      # select observations
      x1 <- subset(dtm, type == "prevalent")
      
      # aggregate number of districts of arrival by assessment date and governorate
      x1 <- unique(x1[, c("dis_en_ocha", "year_assessment", "month_assessment", "gov_en_ocha")])
      x1[, "n_dis"] <- 1
      x1 <- aggregate(x1[, "n_dis"], by = x1[, c("year_assessment", "month_assessment", "gov_en_ocha")], FUN = sum)
      colnames(x1) <- c("year", "month", "gov_en_ocha", "n_dis")

      # merge with governorate and time unit time series
      x2 <- unique(ts[, c("gov_en_ocha", "year", "month")])
      x2 <- merge(x2, x1, by = c("year", "month", "gov_en_ocha"), all.x = TRUE)
      
      # add dates and grid lines
      x2[, "date"] <- dmy(paste("1", x2[, "month"], x2[, "year"], sep = "-"))
      vlines <- seq(dmy("15-01-2016"), dmy("15-12-2018"), by = "months")
      hlines <- seq(1:length(unique(x2[, "gov_en_ocha"]))) - 0.5
      
    # Plot
    plot <- ggplot(subset(x2, year %in% 2016:2018 ), aes(x = date, y = factor(gov_en_ocha), fill = n_dis)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = n_dis), size = 3, colour = "grey20") +
      theme_bw() +
      geom_vline(xintercept = vlines, colour = palette_cb[6], size = 0.5) +
      geom_hline(yintercept = hlines, colour = palette_cb[6], size = 0.5) +
      scale_y_discrete("governorate", expand = c(0,0) ) +
      scale_fill_gradient(na.value = "white", low = "darkseagreen1", high = "darkseagreen4" ) +
      labs(x = "date", y = "governorate", fill = "number of districts") +
      scale_x_date(date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
      theme(plot.margin = unit(c(0,1,0,0), "cm"), legend.position = "none") +
      theme(axis.text = element_text(size = 9, colour = "grey20"),
        axis.title = element_text(size = 10, colour = "grey20") )
    plot
    ggsave("yem_pop_idp_data_coverage_dis.png", height = 15, width = 25, units = "cm", dpi = "print")


  #.........................................
  ## Graph theoretical decline of IDP populations, due to returns, by time of prevalence assessment
    
    # Create dummy data
      # data
      x1 <- data.frame(
        "date" = seq(dmy("01-05-2016"), dmy("31-12-2018"), by = "months"),
        "A" = c(rep(345, 17), rep(321, 15)),
        "B" = c(rep(326, 4), rep(276, 6), 254, 233, rep(220, 4), 198, rep(187, 10), rep(176, 5)),
        "C" = c(rep(253, 8), rep(232, 2), 211, rep(162, 2), 154, 133, rep(97, 4), 65, rep(23, 2), rep(0, 10)),
        "D" = c(rep(109, 5), rep(26, 3), rep(0, 24))
        )
      x1 <- reshape2::melt(x1, id.vars = "date")
      colnames(x1) <- c("date", "scenario", "n_hh")
      
      # departure and assessment rounds
      x2 <- c(dmy("01-05-2016"), dmy("17-02-2017"), dmy("12-09-2018"))
    
    # Plot
    plot <- ggplot(x1, aes(x = date, y = n_hh, colour = scenario)) +
      geom_step(size = 1, alpha = 0.5) +
      theme_bw() +
      scale_colour_manual(values = palette_cb[c(3, 6, 7, 8)]) +
      scale_x_date(name = "date", date_labels = "%b %Y", breaks = "3 months", expand = c(0,0), limits = c(dmy("01-04-2016"), NA) ) +
      scale_y_continuous(name = "number of IDPs", breaks = seq(0, 350, by = 50), expand = c(0,0), limits = c(0, 355)) +
      theme(text = element_text(colour = "grey20", size = 10), legend.position = "none" ) +
      geom_vline(xintercept = x2, colour = palette_cb[4], size = 1, linetype = "twodash") +
      annotate(geom = "label", x = c(x2[1] + 15, x2[2:3]), y = 75, fill = "white",
        label = c(stringr::str_wrap("arrival to subdistrict", 2), "assessment 1", "assessment 2"), 
        colour = palette_cb[4], size = 3, label.size = 0.5) +
      annotate(geom = "label", x = dmy("15-04-2016"), y = c(345, 326, 253, 109), label = c("A", "B", "C", "D"),
        fill = "white", colour = palette_cb[c(3, 6, 7, 8)], size = 3, label.size = 1, fontface = "bold")
    plot
    ggsave("yem_pop_idp_evolution_scenarios.png", height = 12, width = 20, units = "cm", dpi = "print")


#.........................................................................................                            
### Modelling the rate of return of IDPs, based on prevalent data
#.........................................................................................    
    
  #.........................................
  ## Prepare outcome data for modelling
    
    # Select data
    dtm_p <- subset(dtm, type == "prevalent")
    
    # Prepare data
      # generate months of displacement and assessment
      dtm_p[, "t_move"] <- dtm_p[, "tm"]
      x1 <- t_units
      colnames(x1) <- c("t_assess", "month_assessment", "year_assessment")
      dtm_p <- merge(dtm_p, x1, by = c("year_assessment", "month_assessment"), all.x = TRUE)

      # generate unique IDs for subdistrict of origin - subdistrict of arrival - time of displacement combinations
      x1 <- unique(dtm_p[, c("subdis_en_ocha", "subdis_ori_en_ocha", "t_move")])
      x1[, "instance"] <- paste("instance", 1:nrow(x1), sep = "")
      dtm_p <- merge(dtm_p, x1, by = c("subdis_en_ocha", "subdis_ori_en_ocha", "t_move"), all.x = TRUE)
      # dtm_p[, "instance"] <- factor(dtm_p[, "instance"])

    # Identify duplicate instances and deal with them
    x1 <- duplicated(dtm_p[, c("instance", "t_assess")])
    table(x1) # 2118 duplicates out of 111,987 observations, so any adjustment will have negligible effects
      # delete duplicates: all other solutions are too complex
      dtm_p <- dtm_p[! x1, ]
  
    # Compute number of repeat observations per instance
    dtm_p[, "n_assess"] <- 1
    x1 <- aggregate(dtm_p[, "n_assess"], by = list("instance" = dtm_p[, "instance"]), FUN = sum)
    colnames(x1) <- c("instance", "n_assess")
    table(x1[, "n_assess"])
    dtm_p <- merge(dtm_p[, colnames(dtm_p) != "n_assess"], x1, by = "instance", all.x = TRUE)
    dtm_p <- dtm_p[order(dtm_p[, "instance"], dtm_p[, "t_assess"]), ]
      
    # Calculate time distances
      # minimise columns
      dtm_p <- dtm_p[, c("instance", "t_move", "t_assess", "subdis_en_ocha", "subdis_ori_en_ocha", "n_hh",
        "n_assess", "imputed")]
      
      # calculate time distances and identify assessment rounds
      dtm_p <- dtm_p[order(dtm_p[, "instance"], dtm_p[, "t_assess"]), ]
      dtm_p[, "t_idp"] <- dtm_p[, "t_assess"] - dtm_p[, "t_move"]
      x1 <- unique(dtm_p[, c("instance", "n_assess")])
      x2 <- apply(x1, 1, function(x) {1:x["n_assess"]})
      dtm_p[, "round"] <- unlist(x2)
      x1 <- dtm_p[which(dtm_p[, "round"] == 1), c("instance", "t_assess")]
      colnames(x1) <- c("instance", "t_assess1")
      dtm_p <- merge(dtm_p, x1, by = "instance", all.x = TRUE)
      dtm_p[, "t_dtm"] <- dtm_p[, "t_assess"] - dtm_p[, "t_assess1"]
          
    # Resolve 'wrong' proportional change values (series that do not decrease monotonically or remain constant)
      # identify instances in which the time series is not monotonically decreasing or stable
      x1 <- subset(dtm_p, n_assess > 1)
      x1 <- by(x1, x1[, "instance"], function(x) {all(sign(diff(x[, "n_hh"])) <= 0)} )
      x1 <- data.frame("instance" = names(x1), "monotonic" = as.vector(unlist(x1)) )
      dtm_p <- merge(dtm_p, x1, by = "instance", all.x = TRUE)
      table(x1[, "monotonic"])
      prop.table(table(x1[, "monotonic"]))
      dtm_p[, "monotonic"] <- ifelse(is.na(dtm_p[, "monotonic"]), "n/a", dtm_p[, "monotonic"])
      dtm_p[which(dtm_p[, "monotonic"] == TRUE), "monotonic"] <- "yes"
      dtm_p[which(dtm_p[, "monotonic"] == FALSE), "monotonic"] <- "no"
      table(dtm_p[, "monotonic"])
      dtm_p_ok <- subset(dtm_p, monotonic != "no")
      dtm_p_prob <- subset(dtm_p, monotonic == "no")
      dtm_p_prob <- dtm_p_prob[order(dtm_p_prob[, "instance"], dtm_p_prob[, "t_assess"]), ]
        
      # reasonable low scenario
      f_low <- function(x) {
        out <- x[, "n_hh"]
        for (i in 2:length(x[, "n_hh"])) {if (out[i] > out[i-1]) {out[i] <- out[i-1]} }
        return(out)
      }      
      x1 <- by(dtm_p_prob, dtm_p_prob[, "instance"], f_low)
      dtm_p_prob[, "n_hh_low"] <- as.vector(unlist(x1))
     
      # reasonable high scenario
      f_high <- function(x) {
        out <- x[, "n_hh"]
        for (i in (length(x[, "n_hh"])-1):1) {if (out[i] < out[i+1]) {out[i] <- out[i+1]} }
        return(out)
      }      
      x1 <- by(dtm_p_prob, dtm_p_prob[, "instance"], f_high)
      dtm_p_prob[, "n_hh_high"] <- as.vector(unlist(x1))
      
      # main analysis approach
      dtm_p_prob[, "n_hh_mid"] <- rowMeans(dtm_p_prob[, c("n_hh_low", "n_hh_high")])
      
      # re-bind all pieces of the dataset together
      dtm_p_ok[, c("n_hh_low", "n_hh_high", "n_hh_mid")] <- dtm_p_ok[, "n_hh"]
      dtm_p <- rbind(dtm_p_ok, dtm_p_prob)
      dtm_p <- dtm_p[order(dtm_p[, "instance"], dtm_p[, "round"]), ]
      rm(dtm_p_ok, dtm_p_prob)
      
      # write
      write_excel_csv(dtm_p, "dtm_p.csv")

  #.........................................
  ## Investigate overlap between the two main assessment rounds, in terms of instances with date of displacement
      # predating both assessments (November 2016, November 2018)
    
    # Select and identify data
    x1 <- subset(dtm_p, t_move < 35)
    x1[, "nov_2016"] <- ifelse(x1[, "t_assess"] %in% 35:37, 1, 0)
    x1[, "nov_2018"] <- ifelse(x1[, "t_assess"] == 59, 1, 0)
  
    # Describe overlap
    x1 <- aggregate(x1[, c("nov_2016", "nov_2018")], by = list("instance" = x1[, "instance"]), FUN = sum)    
    x1[which(x1[, "nov_2016"] > 1), "nov_2016"] <- 1
    x2 <- table(x1[, "nov_2016"])
    x2
    prop.table(x2)
    x2 <- table(x1[, "nov_2018"])
    x2
    prop.table(x2)
    x2 <- table(x1[, c("nov_2016", "nov_2018")])
    x2
    sum(x2)
    prop.table(x2)
        
    # Chapman estimate of unlisted instances (two-list capture-recapture)
    x3 <- ((sum(x2[2, ]) + 1) * (sum(x2[, 2]) + 1) / (x2[2, 2] + 1)) - 1 # unlisted instances
    x4 <- x3 + x2[2, 1] + x2[1,2] + x2[2,2]    
    x3 / x4 # proportion of instances unlisted
    sum(x2[2, ]) / x4 # sensitivity of Nov 2016 assessment
    sum(x2[, 2]) / x4 # sensitivity of Nov 2018 assessment
    
    
  #.........................................
  ## Graph number of IDP households, by date of displacement and prevalent assessment round

    # Prepare data for plotting
      # select observations and add dates
      x1 <- dtm_p
      x2 <- t_units
      colnames(x2) <- c("t_move", "month_move", "year_move")
      x1 <- merge(x1, x2, by = "t_move", all.x = TRUE)
      x1[, "date"] <- dmy(paste("15", x1[, "month_move"], x1[, "year_move"], sep = "-"))
      
      # aggregate number of households by date of displacement and round
      x1 <- aggregate(x1[, "n_hh"], by = x1[, c("date", "round")], FUN = sum, na.rm = TRUE)
      colnames(x1) <- c("date", "round", "n_hh")
      x1[, "round"] <- factor(x1[, "round"], levels = 1:6)
      
    # Plot
    plot <- ggplot(x1, aes(x = date, y = n_hh, fill = round)) +
      geom_col(alpha = 0.5) +
      theme_bw() +
      scale_y_continuous("number of IDP households", expand = c(0,0), labels = comma, 
        breaks = seq(0, 400000, by = 50000) ) +
      scale_fill_manual(values = palette_cb[2:7]) +
      labs(x = "date of displacement", fill = "assessment") +
      scale_x_date(date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
      theme(plot.margin = unit(c(0,1,0,0), "cm"), legend.position = c(0.8,0.5)) +
      theme(axis.text = element_text(size = 9, colour = "grey20"), 
        legend.text = element_text(size = 9, colour = "grey20"),
        axis.title = element_text(size = 10, colour = "grey20"),
        legend.title = element_text(size = 10, colour = "grey20"))
    plot
    ggsave("yem_pop_idp_dates_displacement.png", height = 15, width = 25, units = "cm", dpi = "print")

    
  #.........................................
  ## Prepare predictor data for modelling

    # Re-prepare ACLED dataset
      # rename OCHA variables
      colnames(ocha)[colnames(ocha) == "gov_pcode"] <- "gov_pcode_ocha"
      colnames(ocha)[colnames(ocha) == "dis_pcode"] <- "dis_pcode_ocha"
      colnames(ocha)[colnames(ocha) == "subdis_pcode"] <- "subdis_pcode_ocha"
    
      # aggregate data by subdistrict-month
      acled <- merge(acled, t_units, by = c("month", "year"), all.x = TRUE)
      x1 <- aggregate(acled[, "n_events"], by = acled[, c("subdis_en_ocha", "tm")], FUN = sum)
      colnames(x1)[colnames(x1) == "x"] <- "n_events"
      
      # merge with subdistrict-month time series
      acled_pred <- merge(ts, x1, by = c("subdis_en_ocha", "tm"), all.x = TRUE)    
        # restrict to start date of high-quality ACLED dataset (1 Jan 2015)
        acled_pred <- subset(acled_pred, tm >= 13)
      
        # reduce columns
        acled_pred <- acled_pred[, c("tm", "subdis_en_ocha", "n_events")]
        
        # set NA values to 0
        acled_pred[which(is.na(acled_pred[, "n_events"])), "n_events"] <- 0

      # merge with population and compute incidence of events per 100,000 population
      acled_pred <- merge(acled_pred, pop_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
      acled_pred[, "density_events"] <- acled_pred[, "n_events"] * 100000 / acled_pred[, "pop"]
      acled_pred <- acled_pred[, c("tm", "subdis_en_ocha", "density_events")]
      rm(acled)
            
    # Other predictors
    health_pred <- health_pred[, c("subdis_en_ocha", "tm", "density_hf")]
    road_pred <- road_pred[, c("subdis_en_ocha", "road_density")]      
      
   
  #.........................................
  ## Prepare training dataset for model
    
    # Select data for model
    # dtm_m <- subset(dtm_p, n_assess >= 2)
    dtm_m <- subset(dtm_p, n_assess >= 2 & imputed == "no")
    dtm_m[, "tm"] <- dtm_m[, "t_move"] + dtm_m[, "t_idp"]
    
    # Make sure all numbers are integers
    dtm_m[, grep("n_hh", colnames(dtm_m))] <- round(dtm_m[, grep("n_hh", colnames(dtm_m))], 0)
    dtm_m[, "instance"] <- factor(dtm_m[, "instance"])
    
    # Add predictors
      # health facility predictor
      colnames(health_pred) <- c("subdis_en_ocha", "tm", "density_hf")
      dtm_m <- merge(dtm_m, health_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
      colnames(health_pred) <- c("subdis_ori_en_ocha", "tm", "density_hf_ori")
      dtm_m <- merge(dtm_m, health_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_en_ocha", "road_density")
      dtm_m <- merge(dtm_m, road_pred, by = "subdis_en_ocha", all.x = TRUE)
      colnames(road_pred) <- c("subdis_ori_en_ocha", "road_density_ori")
      dtm_m <- merge(dtm_m, road_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
            
      # distance predictor (also applies to subdistricts of origin)
      dtm_m <- merge(dtm_m, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor - mean insecurity density over period in between each assessment
      colnames(acled_pred) <- c("tm", "subdis_en_ocha", "density_events")
      dtm_m <- dtm_m[order(dtm_m[, "instance"], dtm_m[, "round"]), ]
      x1 <- by(dtm_m, dtm_m[, "instance"], function(x) 
        {return(c(x[1, "t_assess"] - x[1, "t_move"], diff(x[, "t_assess"])))})
      dtm_m[, "t_diff"] <- as.vector(unlist(x1))
      dtm_m[, c("density_events", "density_events_ori")] <- NA
      for (i in 1:nrow(dtm_m)) {
        loop.tracker(i, nrow(dtm_m) )
        x1 <- dtm_m[i, "t_assess"]
        x1 <- (x1 - dtm_m[i, "t_diff"] + 1):x1
        x2 <- acled_pred[which(acled_pred[, "subdis_en_ocha"] == dtm_m[i, "subdis_en_ocha"] & 
            acled_pred[, "tm"] %in% x1), "density_events"]
        x3 <- acled_pred[which(acled_pred[, "subdis_en_ocha"] == dtm_m[i, "subdis_ori_en_ocha"] & 
            acled_pred[, "tm"] %in% x1), "density_events"]
        dtm_m[i, c("density_events", "density_events_ori")] <- c(mean(x2, na.rm = TRUE), mean(x3, na.rm = TRUE))
      }
      
        # categorise (to allow for prediction)
        dtm_m[, "density_events_cat"] <- cut(dtm_m[, "density_events"], 
          breaks = c(-0.1, 0, 1, 2, 3, 4, 5, 10000))
        dtm_m[, "density_events_cat"] <- as.integer(dtm_m[, "density_events_cat"])
        dtm_m[, "density_events_ori_cat"] <- cut(dtm_m[, "density_events_ori"], 
          breaks = c(-0.1, seq(0, 15, by = 2), 10000))
        dtm_m[, "density_events_ori_cat"] <- as.integer(dtm_m[, "density_events_ori_cat"])
      
    # Save dataset
    write_excel_csv(dtm_m, "yem_pop_gamm_training_df.csv")
  
              
  #.........................................
  ## Fit and evaluate general additive mixed growth model (WARNING: VERY SLOW!)

    # Read dataset if needed
    if (! exists("dtm_m")) {dtm_m <- read.csv("yem_pop_gamm_training_df.csv")}
    colnames(dtm_m) <- gsub("ï..", "", colnames(dtm_m))
    dtm_m[, "instance"] <- as.factor(dtm_m[, "instance"])
    
    # Fit models
      # model for main analysis    
      fit_gamm_mid <- gamlss(n_hh_mid ~ pbm(t_idp, mono = "down") + pbm(density_events_ori_cat, mono = "down") + distance + 
        density_hf + road_density + road_density_ori + random(instance), sigma.formula = ~1,
        data = dtm_m, family = "NBI", control = gamlss.control(c.crit = 0.01) )
      summary(fit_gamm_mid)
      plot(fit_gamm_mid)

      # model for reasonable high scenario  
      fit_gamm_high <- gamlss(n_hh_high ~ pbm(t_idp, mono = "down") + pbm(density_events_ori_cat, mono = "down") + distance + 
        density_hf + road_density + road_density_ori + random(instance), sigma.formula = ~1,
        data = dtm_m, family = "NBI", control = gamlss.control(c.crit = 0.01) )
      summary(fit_gamm_high)
      plot(fit_gamm_high)
  
      # model for reasonable low scenario  
      fit_gamm_low <- gamlss(n_hh_low ~ pbm(t_idp, mono = "down") + pbm(density_events_ori_cat, mono = "down") + distance + 
        density_hf + road_density + road_density_ori + random(instance), sigma.formula = ~1,
        data = dtm_m, family = "NBI", control = gamlss.control(c.crit = 0.01) )
      summary(fit_gamm_low)
      plot(fit_gamm_low)

    # Save models
    saveRDS(fit_gamm_mid, file = "fit_gamm_mid.rds")
    saveRDS(fit_gamm_high, file = "fit_gamm_high.rds")
    saveRDS(fit_gamm_low, file = "fit_gamm_low.rds")
                  
    # fit_cv <- gamlssCV(n_hh ~ pbm(t_dtm, mono = "down") + random(instance), sigma.formula = ~1,
    #   data = dtm_m, family = "NBI", K.fold = 3)
    # CV(fit_cv)

    

#.........................................................................................                            
### Applying growth model to within-sample (training) prevalent displacement data
#.........................................................................................    

  #.........................................
  ## Create dataset for later prediction (WARNING: LARGE FILE, TAKES A LONG TIME)
    
    # All combinations of instances and times
    ts_pred_p <- expand.grid(unique(dtm_p[, "instance"]), 1:tm_analysis_end )
    colnames(ts_pred_p) <- c("instance", "tm")
    ts_pred_p <- merge(ts_pred_p, unique(dtm_p[, c("instance", "subdis_en_ocha", "subdis_ori_en_ocha")]), by = "instance", all.x = TRUE)
    
    # Merge predictors with subdistricts of arrival
      # health facility predictor
      colnames(health_pred) <- c("subdis_en_ocha", "tm", "density_hf")
      ts_pred_p <- merge(ts_pred_p, health_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_en_ocha", "road_density")
      ts_pred_p <- merge(ts_pred_p, road_pred, by = "subdis_en_ocha", all.x = TRUE)
            
      # distance predictor (also applies to subdistricts of origin)
      ts_pred_p <- merge(ts_pred_p, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor
      colnames(acled_pred)[colnames(acled_pred) == "subdis_ori_en_ocha"] <- "subdis_en_ocha"
      ts_pred_p <- merge(ts_pred_p, acled_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
        # categorise (to allow for prediction)
        ts_pred_p[, "density_events_cat"] <- cut(ts_pred_p[, "density_events"], 
          breaks = c(-0.1, 0, 1, 2, 3, 4, 5, 10000))
        ts_pred_p[, "density_events_cat"] <- as.integer(ts_pred_p[, "density_events_cat"])
      
    # Merge predictors with subdistricts of origin
      # health facility predictor
      colnames(health_pred) <- c("subdis_ori_en_ocha", "tm", "density_hf_ori")
      ts_pred_p <- merge(ts_pred_p, health_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_ori_en_ocha", "road_density_ori")
      ts_pred_p <- merge(ts_pred_p, road_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
            
      # ACLED insecurity predictor
      colnames(acled_pred) <- c("tm", "subdis_ori_en_ocha", "density_events_ori")
      ts_pred_p <- merge(ts_pred_p, acled_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
        # categorise (to allow for prediction)
        ts_pred_p[, "density_events_ori_cat"] <- cut(ts_pred_p[, "density_events_ori"], 
          breaks = c(-0.1, seq(0, 15, by = 2), 10000))
        ts_pred_p[, "density_events_ori_cat"] <- as.integer(ts_pred_p[, "density_events_ori_cat"])
      
    # Write dataset
    write_excel_csv(ts_pred_p, "yem_pop_gamm_predictor_prev_df.csv")  
      
    
  #.........................................
  ## Prepare within-sample prediction dataframe
    
    # Read dataset if needed
    if (! exists("ts_pred_p")) {
      ts_pred_p <- read.csv("yem_pop_gamm_predictor_prev_df.csv")
      colnames(ts_pred_p) <- gsub("ï..", "", colnames(ts_pred_p))
    }
    
    # Select within-sample data
    pred_in <- subset(ts_pred_p, instance %in% unique(dtm_m[, "instance"]))
      
    # Further preparations
      # times of displacement / assessment
      pred_in <- merge(pred_in, unique(dtm_m[, c("instance", "t_move")]), by = "instance", all.x = TRUE)
      pred_in[, "t_idp"] <- pred_in[, "tm"] - pred_in[, "t_move"]
      pred_in <- subset(pred_in, t_idp >= 0)
      
      # factorise instance
      pred_in[, "instance"] <- as.factor(pred_in[, "instance"])
        
      # set 20 missing insecurity records to 0 (probably pre-Jan 2015)
      pred_in[, "density_events"] <- ifelse(is.na(pred_in[, "density_events"]), 0, pred_in[, "density_events"])
      pred_in[, "density_events_ori"] <- ifelse(is.na(pred_in[, "density_events_ori"]), 0, pred_in[, "density_events_ori"])
      pred_in[, "density_events_cat"] <- ifelse(is.na(pred_in[, "density_events_cat"]), 1, pred_in[, "density_events_cat"])
      pred_in[, "density_events_ori_cat"] <- ifelse(is.na(pred_in[, "density_events_ori_cat"]), 1, pred_in[, "density_events_ori_cat"])

    # Write data
    write_excel_csv(pred_in, "pred_in.csv")

       
  #.........................................
  ## Predict on within-sample data
    
    # Read datasets if needed
    if (! exists("pred_in")) {
      pred_in <- read.csv("pred_in.csv")
      colnames(pred_in) <- gsub("ï..", "", colnames(pred_in))      
    }          

    if (! exists("dtm_m")) {
      dtm_m <- read.csv("yem_pop_gamm_training_df.csv")
      colnames(dtm_m) <- gsub("ï..", "", colnames(dtm_m))
      dtm_m[, "instance"] <- as.factor(dtm_m[, "instance"])
    }

    # Read model fits if needed
    if (! exists(("fit_gamm_mid"))) {fit_gamm_mid <- readRDS("fit_gamm_mid.rds")}
    if (! exists(("fit_gamm_high"))) {fit_gamm_high <- readRDS("fit_gamm_high.rds")}
    if (! exists(("fit_gamm_low"))) {fit_gamm_low <- readRDS("fit_gamm_low.rds")}
      
    # Prepare prediction dataframes
    pred_in_mid <- pred_in
    pred_in_high <- pred_in
    pred_in_low <- pred_in
    
    # Predict to all instances and time points in range, by scenario (only point estimates)
    pred_in_mid[, "pred"] <- predict(fit_gamm_mid, newdata = pred_in, type = "response")
    pred_in_high[, "pred"] <- predict(fit_gamm_high, newdata = pred_in, type = "response")
    pred_in_low[, "pred"] <- predict(fit_gamm_low, newdata = pred_in, type = "response")
      
    # Save predictions
    pred_in_mid[, "scenario"] <- "mid"
    pred_in_high[, "scenario"] <- "high"
    pred_in_low[, "scenario"] <- "low"
    pred_in_agg <- rbind(pred_in_mid, pred_in_high)
    pred_in_agg <- rbind(pred_in_agg, pred_in_low)
    pred_in_agg <- pred_in_agg[, c("instance", "tm", "subdis_ori_en_ocha", "subdis_en_ocha", "t_idp", "pred", "scenario")]
    write_excel_csv(pred_in_agg, "predictions_in_prev.csv")
    rm(pred_in_mid, pred_in_high, pred_in_low)
    
  #.........................................
  ## Generate prediction graphs

    # Predict to all instances and time points in range, and compute 95%CIs of prediction, by scenario
    pred_in_mid <- f_interval(fit_gamm_mid, 10, pred_in, FALSE)
    pred_in_high <- f_interval(fit_gamm_high, 10, pred_in, FALSE)
    pred_in_low <- f_interval(fit_gamm_low, 10, pred_in, FALSE)

    # Sum predictions across all instances, and convert to percent change from time of displacement
      # aggregate by time since displacement
      pred_agg_mid <- aggregate(pred_in_mid[, c("pred", "pred_lci", "pred_uci")], 
        by = list("t_idp" = pred_in_mid[, "t_idp"]), FUN = sum)
      pred_agg_high <- aggregate(pred_in_high[, c("pred", "pred_lci", "pred_uci")], 
        by = list("t_idp" = pred_in_high[, "t_idp"]), FUN = sum)
      pred_agg_low <- aggregate(pred_in_low[, c("pred", "pred_lci", "pred_uci")], 
        by = list("t_idp" = pred_in_low[, "t_idp"]), FUN = sum)
      
      # compute change since t = 0
      for (i in c("", "_lci", "_uci")) {
        pred_agg_mid[, paste("change", i, sep = "")] <- pred_agg_mid[, paste("pred", i, sep = "")] * 100 /  pred_agg_mid[1, paste("pred", i, sep = "")]
      }
      for (i in c("", "_lci", "_uci")) {
        pred_agg_high[, paste("change", i, sep = "")] <- pred_agg_high[, paste("pred", i, sep = "")] * 100 /  pred_agg_high[1, paste("pred", i, sep = "")]
      }
      for (i in c("", "_lci", "_uci")) {
        pred_agg_low[, paste("change", i, sep = "")] <- pred_agg_low[, paste("pred", i, sep = "")] * 100 /  pred_agg_low[1, paste("pred", i, sep = "")]
      }
      
      # label scenarios
      pred_agg_mid[, "scenario"] <- "main analysis"
      pred_agg_high[, "scenario"] <- "high"
      pred_agg_low[, "scenario"] <- "low"
      
      # bind all scenarios
      pred_agg <- rbind(pred_agg_mid, pred_agg_high, pred_agg_low)
      pred_agg[, "scenario"] <- factor(pred_agg[, "scenario"], levels = c("main analysis", "low", "high"))
    
    # Sum predictions and observations for each time point, and for all instances observed at that time point
      # just for main analysis
      pred_vs_obs <- as.data.frame(matrix(NA, ncol = 5, nrow = nrow(pred_agg_mid)))
      colnames(pred_vs_obs) <- c("t_idp", "obs", "pred", "pred_lci", "pred_uci")
      pred_vs_obs[, "t_idp"] <- pred_agg_mid[, "t_idp"]
      for (i in 1:nrow(pred_vs_obs)) {
        x1 <- pred_vs_obs[i, "t_idp"]
        pred_vs_obs[i, "obs"] <- sum(dtm_m[which(dtm_m[, "t_idp"] == x1), "n_hh"])
        x2 <- unique(dtm_m[which(dtm_m[, "t_idp"] == x1), "instance"])
        pred_vs_obs[i, "pred"] <- sum(pred_in_mid[which(pred_in_mid[, "t_idp"] == x1 & pred_in_mid[, "instance"] %in% x2), "pred"])
        pred_vs_obs[i, "pred_lci"] <- sum(pred_in_mid[which(pred_in_mid[, "t_idp"] == x1 & pred_in_mid[, "instance"] %in% x2), "pred_lci"])
        pred_vs_obs[i, "pred_uci"] <- sum(pred_in_mid[which(pred_in_mid[, "t_idp"] == x1 & pred_in_mid[, "instance"] %in% x2), "pred_uci"])
      }
      pred_vs_obs[pred_vs_obs[, "t_idp"] == 0, c("obs", "pred", "pred_lci", "pred_uci")] <- NA
      
    # Plot predicted evolution of IDP populations, by scenario
    plot1 <- ggplot(pred_agg, aes(x = t_idp, group = scenario, colour = scenario, fill = scenario, size = scenario)) +
      geom_line(aes(y = change ), alpha = 0.5) +
      scale_size_manual(values = c(1.5, 0.75, 0.75)) +
      geom_ribbon(aes(ymin = change_lci, ymax = change_uci), alpha = 0.1, linetype = 0) +
      scale_colour_manual(values = palette_cb[c(4, 6, 7)]) +
      scale_fill_manual(values = palette_cb[c(4, 6, 7)]) +
      scale_y_continuous("percent of starting IDP population (%)", minor_breaks = NULL, 
        breaks = seq(0, 100, by = 10), limits = c(0, 100) ) +
      theme_bw() +
      scale_x_continuous("months since arrival to subdistrict", minor_breaks = NULL,
        breaks = seq(0, 80, 5), limits = c(0, 75), expand = c(0,0) ) +
      theme(text = element_text(colour = "grey20", size = 9), legend.position = "top",
        legend.title = element_text(colour = "grey20", size = 9), 
        axis.text = element_text(colour = "grey20", size = 9))
    plot1
    ggsave("yem_pop_gamm_prop_change.png", height = 12, width = 18, units = "cm", dpi = "print")

    # Plot predictions versus observations for each time point
    plot2 <- ggplot(pred_vs_obs, aes(x = t_idp)) +
      geom_point(aes(y = obs ), size = 2, stroke = 1.5, alpha = 0.7, shape = 0, colour = palette_cb[7]) +
      geom_point(aes(y = pred), alpha = 0.3, colour = palette_cb[6], size = 1.5) +
      geom_errorbar(aes(ymin = pred_lci, ymax = pred_uci), colour = palette_cb[6], size = 0.5) +
      scale_y_continuous("number of IDPs assessed", minor_breaks = NULL, 
        breaks = seq(0, max(pred_vs_obs[, "pred_uci"], na.rm = TRUE) + 500, by = 10000), labels = comma ) +
      theme_bw() +
      scale_x_continuous("months since arrival to subdistrict", minor_breaks = NULL,
        breaks = seq(0, 45, 5), limits = c(0, 47), expand = c(0,0) ) +
      theme(text = element_text(colour = "grey20", size = 9), legend.position = "top",
        legend.title = element_text(colour = "grey20", size = 9), 
        axis.text = element_text(colour = "grey20", size = 9))
    plot2
    ggsave("yem_pop_gamm_pred_vs_obs_mid.png", height = 10, width = 18, units = "cm", dpi = "print")

    # Combine plots
    plot <- ggarrange(plot1 + rremove("xlab"), plot2, nrow = 2, labels = c("A", "B"), align = "v", heights = c(1.25, 1.25) )
    ggsave("yem_pop_gamm_combi_plot.png", height = 18, width = 18, units = "cm", dpi = "print")
  

#.........................................................................................                            
### Applying growth model to out-of-sample prevalent displacement data (i.e. instances with only 1 assessment)
#.........................................................................................    
      
  #.........................................
  ## Prepare out-of-sample prediction frame
  
    # Read data if needed
    if (! exists("ts_pred_p")) {
      ts_pred_p <- read.csv("yem_pop_gamm_predictor_prev_df.csv")
      colnames(ts_pred_p) <- gsub("ï..", "", colnames(ts_pred_p))
      ts_pred_p[, "instance"] <- as.factor(ts_pred_p[, "instance"])
    }

    if (! exists("pred_in")) {
      pred_in <- read.csv("pred_in.csv")
      colnames(pred_in) <- gsub("ï..", "", colnames(pred_in))
      pred_in[, "instance"] <- as.factor(pred_in[, "instance"])
    }

    if (! exists("dtm_p")) {
      dtm_p <- read.csv("dtm_p.csv")
      colnames(dtm_p) <- gsub("ï..", "", colnames(dtm_p))
      dtm_p[, "instance"] <- as.factor(dtm_p[, "instance"])
    }
        
    if (! exists("dtm_m")) {
      dtm_m <- read.csv("yem_pop_gamm_training_df.csv")
      colnames(dtm_m) <- gsub("ï..", "", colnames(dtm_m))
      dtm_m[, "instance"] <- as.factor(dtm_m[, "instance"])
    }

    # Read model fits if needed
    if (! exists(("fit_gamm_mid"))) {fit_gamm_mid <- readRDS("fit_gamm_mid.rds")}
    if (! exists(("fit_gamm_high"))) {fit_gamm_high <- readRDS("fit_gamm_high.rds")}
    if (! exists(("fit_gamm_low"))) {fit_gamm_low <- readRDS("fit_gamm_low.rds")}
        
    # Identify out-of-sample instances (single-assessment instances not included in training data)
    dtm_out <-  subset(dtm_p, ! instance %in% unique(dtm_m[, "instance"]))
    pred_out <- subset(ts_pred_p, instance %in% unique(dtm_out[, "instance"]))
  
    # Add times and select out times before displacement
    pred_out <- merge(pred_out, unique(dtm_out[, c("instance", "t_move")]), by = "instance", all.x = TRUE)
    pred_out[, "t_idp"] <- pred_out[, "tm"] - pred_out[, "t_move"]
    pred_out <- subset(pred_out, t_idp >= 0)
    
    # Set a few missing insecurity records to 0 (probably pre-Jan 2015)
    pred_out[, "density_events"] <- ifelse(is.na(pred_out[, "density_events"]), 0, pred_out[, "density_events"])
    pred_out[, "density_events_ori"] <- ifelse(is.na(pred_out[, "density_events_ori"]), 0, pred_out[, "density_events_ori"])
    pred_out[, "density_events_cat"] <- ifelse(is.na(pred_out[, "density_events_cat"]), 1, pred_out[, "density_events_cat"])
    pred_out[, "density_events_ori_cat"] <- ifelse(is.na(pred_out[, "density_events_ori_cat"]), 1, pred_out[, "density_events_ori_cat"])
  
    # write
    write_excel_csv(pred_out, "pred_out.csv")
    
    # Prepare one model frame per scenario
    pred_out_mid <- pred_out
    pred_out_high <- pred_out
    pred_out_low <- pred_out
    
    # clean up
    rm(ts_pred_p, pred_out)

    # if (! exists("pred_out")) {
    #   pred_out <- read.csv("pred_out.csv")
    #   colnames(pred_out) <- gsub("ï..", "", colnames(pred_out))
    #   pred_out[, "instance"] <- as.factor(pred_out[, "instance"])
    # }

    
  #.........................................
  ## Identify data from which to extract spline predictions for each t_idp 
  pred_in[, "instance"] <- as.factor(pred_in[, "instance"])
  x3 <- which(pred_in[, "t_idp"] == max(pred_in[, "t_idp"]))
  x3 <- unique(pred_in[x3, "instance"])
  x4 <- which(pred_in[, "t_idp"] == min(pred_in[, "t_idp"]))
  x4 <- unique(pred_in[x4, "instance"])
  x3 <- subset(pred_in, instance == x3[1] )
  x4 <- subset(pred_in, instance == x4[1] )
  spline_t_idp <- rbind(x3, subset(x4, ! t_idp %in% x3[, "t_idp"]) )

  #.........................................
  ## Identify data from which to extract spline predictions for each value of insecurity intensity    
  x3 <- by(pred_in, pred_in[, "instance"], function(x) {length(unique(x[, "density_events_ori_cat"]))})
  x3 <- cbind(names(x3), unlist(x3))
  row.names(x3) <- NULL
  colnames(x3) <- c("instance", "n_levels")
  x3 <- as.data.frame(x3)
  x3[, "n_levels"] <- as.numeric(x3[, "n_levels"])
  x4 <- which(x3[, "n_levels"] == length(unique(pred_out_mid[, "density_events_ori_cat"])))
  spline_events <- subset(pred_in, instance == x3[x4[1], "instance"])
  

for (i in c("mid", "high", "low")) {    
  #.........................................
  ## Prepare model coefficients
    
    # Select scenario data
    print(paste("now working on", i, "scenario", sep = " "))
    pred_out <- get(paste("pred_out_", i, sep = ""))
    fit_gamm <- get(paste("fit_gamm_", i, sep = ""))
    x1 <- unique(dtm_out[, c("instance", "t_idp", paste("n_hh_", i, sep = ""))])
    colnames(x1) <- c("instance", "t_idp1", "n_hh")
    
    # Extract fixed effect spline coefficient for time since displacement
    coef_spline <- cbind(spline_t_idp[, "t_idp"], 
      predict(fit_gamm, newdata = spline_t_idp, type = "terms")[, "pbm(t_idp, mono = \"down\")"])
    colnames(coef_spline) <- c("t_idp", "spline_t_idp")
    coef_spline[, "spline_t_idp"] <- round(as.numeric(coef_spline[, "spline_t_idp"]), 3)
    coef_spline <- unique(coef_spline)
    coef_spline <- coef_spline[which(coef_spline[, "t_idp"] >= 0), ]
      # merge with prediction frame
      pred_out <- merge(pred_out, coef_spline, by = "t_idp", all.x = TRUE)

    # Extract fixed effect spline coefficient for insecurity events
    coef_events <- cbind(spline_events[, "density_events_ori_cat"], 
      predict(fit_gamm, newdata = spline_events, type = "terms")[, "pbm(density_events_ori_cat, mono = \"down\")"])
    colnames(coef_events) <- c("density_events_ori_cat", "spline_events")
    coef_events <- unique(coef_events)
      # merge with prediction frame
      pred_out <- merge(pred_out, coef_events, by = "density_events_ori_cat", all.x = TRUE)
    
    # Extract other fixed effect coefficients
    intercept <- coef(fit_gamm)[1]
    x2 <- coef(fit_gamm)[-1]
    x2 <- x2[! names(x2) %in% c("pbm(t_idp, mono = \"down\")", "pbm(density_events_ori_cat, mono = \"down\")", "random(instance)")]
    coef_other <- as.data.frame(matrix(x2, ncol = length(x2), nrow = nrow(pred_out),  byrow = TRUE) )
    colnames(coef_other) <- names(x2)
    
  #.........................................
  ## Compute fixed effects predictions
    
    # Compute fixed effect prediction (number of IDP households)
    pred_out[, colnames(coef_other)] <- pred_out[, colnames(coef_other)] * coef_other
    pred_out[, "pred"] <- exp(rowSums(pred_out[, c("spline_t_idp", "spline_events",
      colnames(coef_other))]) + intercept)    
    
    # Normalise to get proportional reduction at t_x, compared to t_0
    x2 <- subset(pred_out, t_idp == 0)[, c("instance", "pred")]
    colnames(x2) <- c("instance", "pred_start")
    pred_out <- merge(pred_out, x2, by = "instance", all.x = TRUE)
    pred_out[, "change"] <- pred_out[, "pred"] / pred_out[, "pred_start"]

    # Reduce columns
    pred_out <- pred_out[, c("instance", "t_idp", "tm", "t_move", "subdis_en_ocha", "subdis_ori_en_ocha",
      "change")]
    
    # Scale up to known single-assessment number of IDP households
    pred_out[, "pred"] <- NA
    pred_out <- merge(pred_out, x1, by = "instance", all.x = TRUE)
    pred_out[, "assess"] <- ifelse(pred_out[, "t_idp"] == pred_out[, "t_idp1"], 1, 0)
    x2 <- subset(pred_out, assess == 1)[, c("instance", "n_hh", "change")]
    colnames(x2) <- c("instance", "n_hh_assess", "change_assess")
    pred_out <- merge(pred_out, x2, by = "instance", all.x = TRUE)
    pred_out[, "pred"] <- pred_out[, "change"] * pred_out[, "n_hh_assess"] / pred_out[, "change_assess"]

    # Output
    pred_out <- pred_out[, c("instance", "tm", "subdis_ori_en_ocha", "subdis_en_ocha", "t_idp", "pred")]
    pred_out <- pred_out[order(pred_out[, "instance"], pred_out[, "t_idp"]), ]    
    assign(paste("pred_out_", i, sep = ""), pred_out)
}    

    # Save predictions
    pred_out_mid[, "scenario"] <- "mid"
    pred_out_high[, "scenario"] <- "high"
    pred_out_low[, "scenario"] <- "low"
    pred_out <- rbind(pred_out_mid, pred_out_high)
    pred_out <- rbind(pred_out, pred_out_low)
    pred_out <- pred_out[, c("instance", "tm", "subdis_ori_en_ocha", "subdis_en_ocha", "t_idp", "pred", "scenario")]
    write_excel_csv(pred_out, "predictions_out_prev.csv")
    rm(pred_out, pred_out_mid, pred_out_high, pred_out_low)
  
  
#.........................................................................................                            
### Applying growth model to incident displacement data
#.........................................................................................    

  #.........................................
  ## Select and prepare observations
    
    # Select data
    dtm_i <- subset(dtm, type == "incident")
    
    # Prepare data
      # generate months of displacement
      dtm_i[, "t_move"] <- dtm_i[, "tm"]

      # generate unique IDs for subdistrict of origin - subdistrict of arrival - time of displacement combinations
      x1 <- unique(dtm_i[, c("subdis_en_ocha", "subdis_ori_en_ocha", "t_move")])
      x1[, "instance"] <- paste("instance", 1:nrow(x1), sep = "")
      dtm_i <- merge(dtm_i, x1, by = c("subdis_en_ocha", "subdis_ori_en_ocha", "t_move"), all.x = TRUE)
      # dtm_i[, "instance"] <- factor(dtm_i[, "instance"])

    # Identify duplicate instances and deal with them
    x1 <- duplicated(dtm_i[, c("instance", "t_move")])
    table(x1) # 150 duplicates out of 13,610 observations, so any adjustment will have negligible effects
      # delete duplicates: all other solutions are too complex
      dtm_i <- dtm_i[! x1, ]
  
    # Minimise columns
    dtm_i <- dtm_i[, c("instance", "t_move", "subdis_en_ocha", "subdis_ori_en_ocha", "n_hh",
      "imputed")]
      

  #.........................................
  ## Create dataset for prediction 
    
    # All combinations of instances and times
    ts_pred_i <- expand.grid(unique(dtm_i[, "instance"]), 1:tm_analysis_end )
    colnames(ts_pred_i) <- c("instance", "tm")
    ts_pred_i <- merge(ts_pred_i, unique(dtm_i[, c("instance", "subdis_en_ocha", "subdis_ori_en_ocha")]), by = "instance", all.x = TRUE)
    
    # Merge predictors with subdistricts of arrival
      # health facility predictor
      colnames(health_pred) <- c("subdis_en_ocha", "tm", "density_hf")
      ts_pred_i <- merge(ts_pred_i, health_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_en_ocha", "road_density")
      ts_pred_i <- merge(ts_pred_i, road_pred, by = "subdis_en_ocha", all.x = TRUE)
            
      # distance predictor (also applies to subdistricts of origin)
      ts_pred_i <- merge(ts_pred_i, distance_pred, by = c("subdis_en_ocha", "subdis_ori_en_ocha"), all.x = TRUE)
      
      # ACLED insecurity predictor
      colnames(acled_pred)[colnames(acled_pred) == "subdis_ori_en_ocha"] <- "subdis_en_ocha"
      ts_pred_i <- merge(ts_pred_i, acled_pred, by = c("tm", "subdis_en_ocha"), all.x = TRUE)
        # categorise (to allow for prediction)
        ts_pred_i[, "density_events_cat"] <- cut(ts_pred_i[, "density_events"], 
          breaks = c(-0.1, 0, 1, 2, 3, 4, 5, 10000))
        ts_pred_i[, "density_events_cat"] <- as.integer(ts_pred_i[, "density_events_cat"])
      
    # Merge predictors with subdistricts of origin
      # health facility predictor
      colnames(health_pred) <- c("subdis_ori_en_ocha", "tm", "density_hf_ori")
      ts_pred_i <- merge(ts_pred_i, health_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
     
      # road network predictor
      colnames(road_pred) <- c("subdis_ori_en_ocha", "road_density_ori")
      ts_pred_i <- merge(ts_pred_i, road_pred, by = "subdis_ori_en_ocha", all.x = TRUE)
            
      # ACLED insecurity predictor
      colnames(acled_pred) <- c("tm", "subdis_ori_en_ocha", "density_events_ori")
      ts_pred_i <- merge(ts_pred_i, acled_pred, by = c("tm", "subdis_ori_en_ocha"), all.x = TRUE)
        # categorise (to allow for prediction)
        ts_pred_i[, "density_events_ori_cat"] <- cut(ts_pred_i[, "density_events_ori"], 
          breaks = c(-0.1, seq(0, 15, by = 2), 10000))
        ts_pred_i[, "density_events_ori_cat"] <- as.integer(ts_pred_i[, "density_events_ori_cat"])
      
    # Write dataset
    write_excel_csv(ts_pred_i, "yem_pop_gamm_predictor_inc_df.csv")  
      

  #.........................................
  ## Read necessary data 
    
    # Read dataset if needed
    if (! exists("ts_pred_i")) {
      ts_pred_i <- read.csv("yem_pop_gamm_predictor_inc_df.csv")
      colnames(ts_pred_i) <- gsub("ï..", "", colnames(ts_pred_i))      
    }          
    ts_pred_i[, "instance"] <- as.factor(ts_pred_i[, "instance"])
      # eliminate months before the move
      ts_pred_i <- merge(ts_pred_i, dtm_i[, c("instance", "t_move")], by = "instance", all.x = TRUE)
      ts_pred_i <- subset(ts_pred_i, tm >= t_move)
      ts_pred_i <- ts_pred_i[order(ts_pred_i[, "instance"], ts_pred_i[, "tm"]), ]
      
      # create time of displacement
      ts_pred_i[, "t_idp"] <- ts_pred_i[, "tm"] - ts_pred_i[, "t_move"]
      
      # prepare scenarios
      pred_i_mid <- ts_pred_i
      pred_i_high <- ts_pred_i
      pred_i_low <- ts_pred_i
      
    # Read model fits if needed
    if (! exists(("fit_gamm_mid"))) {fit_gamm_mid <- readRDS("fit_gamm_mid.rds")}
    if (! exists(("fit_gamm_high"))) {fit_gamm_high <- readRDS("fit_gamm_high.rds")}
    if (! exists(("fit_gamm_low"))) {fit_gamm_low <- readRDS("fit_gamm_low.rds")}
      
    # Read training data if needed
    if (! exists("dtm_m")) {
      dtm_m <- read.csv("yem_pop_gamm_training_df.csv")
      colnames(dtm_m) <- gsub("ï..", "", colnames(dtm_m))
      dtm_m[, "instance"] <- as.factor(dtm_m[, "instance"])
    }
    
    # Read dataset on which to predict the spline coefficients
    if (! exists("pred_in")) {
      pred_in <- read.csv("pred_in.csv")
      colnames(pred_in) <- gsub("ï..", "", colnames(pred_in))
    }          


  #.........................................
  ## Identify data from which to extract spline predictions for each t_idp 
  pred_in[, "instance"] <- as.factor(pred_in[, "instance"])
  x3 <- which(pred_in[, "t_idp"] == max(pred_in[, "t_idp"]))
  x3 <- unique(pred_in[x3, "instance"])
  x4 <- which(pred_in[, "t_idp"] == min(pred_in[, "t_idp"]))
  x4 <- unique(pred_in[x4, "instance"])
  x3 <- subset(pred_in, instance == x3[1] )
  x4 <- subset(pred_in, instance == x4[1] )
  spline_t_idp <- rbind(x3, subset(x4, ! t_idp %in% x3[, "t_idp"]) )

  #.........................................
  ## Identify data from which to extract spline predictions for each value of insecurity intensity    
  x3 <- by(pred_in, pred_in[, "instance"], function(x) {length(unique(x[, "density_events_ori_cat"]))})
  x3 <- cbind(names(x3), unlist(x3))
  row.names(x3) <- NULL
  colnames(x3) <- c("instance", "n_levels")
  x3 <- as.data.frame(x3)
  x3[, "n_levels"] <- as.numeric(x3[, "n_levels"])
  x4 <- which(x3[, "n_levels"] == length(unique(pred_i_mid[, "density_events_ori_cat"])))
  spline_events <- subset(pred_in, instance == x3[x4[1], "instance"])
  
    
for (i in c("mid", "high", "low")) {    
  #.........................................
  ## Prepare model coefficients
    
    # Select scenario data
    print(paste("now working on", i, "scenario", sep = " "))
    pred_i <- get(paste("pred_i_", i, sep = ""))
    fit_gamm <- get(paste("fit_gamm_", i, sep = ""))
    x1 <- unique(dtm_i[, c("instance", "t_move", "n_hh")])
    colnames(x1) <- c("instance", "t_idp1", "n_hh")
    
    # Extract fixed effect spline coefficient for time since displacement
    coef_spline <- cbind(spline_t_idp[, "t_idp"], 
      predict(fit_gamm, newdata = spline_t_idp, type = "terms")[, "pbm(t_idp, mono = \"down\")"])
    colnames(coef_spline) <- c("t_idp", "spline_t_idp")
    coef_spline[, "spline_t_idp"] <- round(as.numeric(coef_spline[, "spline_t_idp"]), 3)
    coef_spline <- unique(coef_spline)
    coef_spline <- coef_spline[which(coef_spline[, "t_idp"] >= 0), ]
      # merge with prediction frame
      pred_i <- merge(pred_i, coef_spline, by = "t_idp", all.x = TRUE)

    # Extract fixed effect spline coefficient for insecurity events
    coef_events <- cbind(spline_events[, "density_events_ori_cat"], 
      predict(fit_gamm, newdata = spline_events, type = "terms")[, "pbm(density_events_ori_cat, mono = \"down\")"])
    colnames(coef_events) <- c("density_events_ori_cat", "spline_events")
    coef_events <- unique(coef_events)
      # merge with prediction frame
      pred_i <- merge(pred_i, coef_events, by = "density_events_ori_cat", all.x = TRUE)
    
    # Extract other fixed effect coefficients
    intercept <- coef(fit_gamm)[1]
    x2 <- coef(fit_gamm)[-1]
    x2 <- x2[! names(x2) %in% c("pbm(t_idp, mono = \"down\")", "pbm(density_events_ori_cat, mono = \"down\")", "random(instance)")]
    coef_other <- as.data.frame(matrix(x2, ncol = length(x2), nrow = nrow(pred_i),  byrow = TRUE) )
    colnames(coef_other) <- names(x2)
    
  #.........................................
  ## Compute fixed effects predictions
    
    # Compute fixed effect prediction (number of IDP households)
    pred_i[, colnames(coef_other)] <- pred_i[, colnames(coef_other)] * coef_other
    pred_i[, "pred"] <- exp(rowSums(pred_i[, c("spline_t_idp", "spline_events", colnames(coef_other))]) + intercept)
    
    # Normalise to get proportional reduction at t_x, compared to t_0
    x2 <- subset(pred_i, t_idp == 0)[, c("instance", "pred")]
    colnames(x2) <- c("instance", "pred_start")
    pred_i <- merge(pred_i, x2, by = "instance", all.x = TRUE)
    pred_i[, "change"] <- pred_i[, "pred"] / pred_i[, "pred_start"]

    # Reduce columns
    pred_i <- pred_i[, c("instance", "t_idp", "tm", "t_move", "subdis_en_ocha", "subdis_ori_en_ocha",
      "change")]
    
    # Scale up to known single-assessment number of IDP households
    pred_i[, "pred"] <- NA
    pred_i <- merge(pred_i, x1, by = "instance", all.x = TRUE)
    pred_i[, "assess"] <- ifelse(pred_i[, "tm"] == pred_i[, "t_idp1"], 1, 0)
    x2 <- subset(pred_i, assess == 1)[, c("instance", "n_hh", "change")]
    colnames(x2) <- c("instance", "n_hh_assess", "change_assess")
    pred_i <- merge(pred_i, x2, by = "instance", all.x = TRUE)
    pred_i[, "pred"] <- pred_i[, "change"] * pred_i[, "n_hh_assess"] / pred_i[, "change_assess"]

    # Output
    pred_i <- pred_i[, c("instance", "tm", "subdis_ori_en_ocha", "subdis_en_ocha", "t_idp", "pred")]
    pred_i <- pred_i[order(pred_i[, "instance"], pred_i[, "t_idp"]), ]    
    assign(paste("pred_i_", i, sep = ""), pred_i)
}    

    # Save predictions
    pred_i_mid[, "scenario"] <- "mid"
    pred_i_high[, "scenario"] <- "high"
    pred_i_low[, "scenario"] <- "low"
    pred_i <- rbind(pred_i_mid, pred_i_high)
    pred_i <- rbind(pred_i, pred_i_low)
    pred_i <- pred_i[, c("instance", "tm", "subdis_ori_en_ocha", "subdis_en_ocha", "t_idp", "pred", "scenario")]
    write_excel_csv(pred_i, "predictions_inc.csv")

    
#.........................................................................................                            
### Aggregating displacement projections into a single dataset and producing graphs
#.........................................................................................    

  #.........................................
  ## Bind all predictions together and aggregate

    # Read datasets
    pred_p_in <- read.csv("predictions_in_prev.csv")  
    pred_p_out <- read.csv("predictions_out_prev.csv")  
    pred_i <- read.csv("predictions_inc.csv")  
    
    # Bind all datasets together
    pred <- rbind(pred_p_in, pred_p_out)
    pred <- rbind(pred, pred_i)
    rm(pred_p_in, pred_p_out, pred_i)
    
    # Aggregate by time, subdistrict, subdistrict of origin and scenario
    colnames(pred)[colnames(pred) == "pred"] <- "n_hh"
    pred[, "n_hh"] <- ifelse(is.na(pred[, "n_hh"]), 0, pred[, "n_hh"])
    pred <- aggregate(pred[, "n_hh"], by = pred[, c("subdis_en_ocha", "subdis_ori_en_ocha", "tm", "scenario")], FUN = sum)
    colnames(pred)[colnames(pred) == "x"] <- "n_hh"
    
    # Restrict to period of analysis
    pred <- subset(pred, tm %in% 1:tm_analysis_end)
    
  #.........................................
  ## Convert households to number of IDPs and round to integer
  pred[, "n_idp"] <- NA
  pred[which(pred[, "scenario"] == "mid"), "n_idp"] <- pred[which(pred[, "scenario"] == "mid"), "n_hh"] * hh_size_mid
  pred[which(pred[, "scenario"] == "high"), "n_idp"] <- pred[which(pred[, "scenario"] == "high"), "n_hh"] * hh_size_high
  pred[which(pred[, "scenario"] == "low"), "n_idp"] <- pred[which(pred[, "scenario"] == "low"), "n_hh"] * hh_size_low
  pred[, "n_idp"] <- round(pred[, "n_idp"], digits = 0)
        
  #.........................................
  ## Produce various output datasets
  
    # Prevalent IDPs by subdistrict of origin and arrival, by month and scenario
    pred <- pred[order(pred[, "scenario"], pred[, "subdis_en_ocha"], pred[, "subdis_ori_en_ocha"], pred[, "tm"]), ]
    x1 <- c("scenario", "subdis_en_ocha", "subdis_ori_en_ocha", "tm", "n_idp")
    write_excel_csv(pred[, x1], "yem_pop_out_idps_arr_ori.csv")

    # Create time series of subdistricts by scenario  
    ts_mid <- ts
    ts_mid[, "scenario"] <- "mid"
    ts_high <- ts
    ts_high[, "scenario"] <- "high"
    ts_low <- ts
    ts_low[, "scenario"] <- "low"
    ts_all <- rbind(ts_mid, ts_high)
    ts_all <- rbind(ts_all, ts_low)
    rm(ts_mid, ts_high, ts_low)
    
    # Prevalent IDPs by subdistrict of arrival, by month and scenario
      # aggregate
      x2 <- aggregate(pred[, "n_idp"], by = pred[, c("subdis_en_ocha", "tm", "scenario")], FUN = sum)
      colnames(x2)[colnames(x2) == "x"] <- "n_idp"
      
      # make sure all subdistrict-months are featured
      x2 <- merge(ts_all, x2,  by = c("tm", "subdis_en_ocha", "scenario"), all.x = TRUE)
      x2[, "n_idp"] <- ifelse(is.na(x2[, "n_idp"]), 0, x2[, "n_idp"])
      x2 <- subset(x2, tm %in% 1:tm_analysis_end)
      
      # write
      x2 <- x2[order(x2[, "scenario"], x2[, "gov_en_ocha"], x2[, "dis_en_ocha"], x2[, "subdis_en_ocha"], x2[, "tm"]), ]
      write_excel_csv(x2[, c("scenario", "gov_en_ocha", "dis_en_ocha", "subdis_en_ocha", "subdis_pcode", "tm", "n_idp")], 
        "yem_pop_out_idps_arr.csv")
      
    # Prevalent IDPs by subdistrict of origin, by month and scenario
      # aggregate
      x2 <- aggregate(pred[, "n_idp"], by = pred[, c("subdis_ori_en_ocha", "tm", "scenario")], FUN = sum)
      colnames(x2)[colnames(x2) == "x"] <- "n_idp"
      
      # make sure all subdistrict-months are featured
      colnames(ts_all) <- gsub("gov", "gov_ori", colnames(ts_all))
      colnames(ts_all) <- gsub("dis", "dis_ori", colnames(ts_all))
      x2 <- merge(ts_all, x2, by = c("tm", "subdis_ori_en_ocha", "scenario"), all.x = TRUE)
      x2[, "n_idp"] <- ifelse(is.na(x2[, "n_idp"]), 0, x2[, "n_idp"])
      x2 <- subset(x2, tm %in% 1:tm_analysis_end)
      
      # write
      x2 <- x2[order(x2[, "scenario"], x2[, "gov_ori_en_ocha"], x2[, "dis_ori_en_ocha"], x2[, "subdis_ori_en_ocha"], x2[, "tm"]), ]
      write_excel_csv(x2[, c("scenario", "gov_ori_en_ocha", "dis_ori_en_ocha", "subdis_ori_en_ocha", "subdis_ori_pcode", "tm", "n_idp")], 
        "yem_pop_out_idps_ori.csv")
      rm(ts_all)
 
      
                       
#.........................................................................................    
### ENDS
#.........................................................................................
       
      
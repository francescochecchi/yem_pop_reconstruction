#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## -------- R SCRIPT TO PERFORM POPULATION RECONSTRUCTION BY SUBDISTRICT-MONTH --------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Dec 2021)

                                          # francesco.checchi@lshtm.ac.uk 


#..........................................................................................
### Reading and preparing different datasets
#..........................................................................................

  #...................................  
  ## Prepare main time series
  x1 <- ts[, c("gov_en_ocha", "dis_en_ocha", "subdis_en_ocha", "subdis_pcode", "tm", "year", "month")]
  ts_pop <- rbind(x1, x1)
  ts_pop <- rbind(ts_pop, x1)
  ts_pop[, "scenario"] <- c(rep("mid", nrow(x1)), rep("low", nrow(x1)), rep("high", nrow(x1)) )
  table(ts_pop[, c("year", "scenario")])
    
    
  #...................................  
  ## Read displacement datasets (IDPs by subdistricts of arrival-month, and origin-month)
  idp_arr <- read.csv("yem_pop_out_idps_arr.csv")
  colnames(idp_arr) <- gsub("ï..", "", colnames(idp_arr))
  idp_ori <- read.csv("yem_pop_out_idps_ori.csv")
  colnames(idp_ori) <- gsub("ï..", "", colnames(idp_ori))
  

  #...................................  
  ## Establish whether each subdistrict is urban or rural
    # based on population density >=300/km2 (in June 2014): https://blogs.worldbank.org/sustainablecities/how-do-we-define-cities-towns-and-rural-areas
    # Yemen official UN definition for urban: "Capitals of 17 governorates and other towns."
    # see https://population.un.org/wup/Download/ 

    # Calculate surface area of each subdistrict
      # apply planar transformation
      ocha_trans <- st_transform(ocha_shape, 4981)
      # subdistrict surface area in Km^2
      ocha_trans[, "area"] <- as.numeric(unlist(sf::st_area(ocha_trans)) / (1000^2) )
      
    # Calculate population density of each subdistrict and categorise into urban/rural
    pop_density <- subset(pop_worldpop, year == 2014 & month == 6)[, c("subdis", "pop")]
    x1 <- ocha_trans[, c("subdis_en_ocha_shape", "area")]
    st_geometry(x1) <- NULL
    colnames(x1) <- c("subdis_en_ocha", "subdis_area")
    colnames(pop_density) <- c("subdis_en_ocha", "pop")
    pop_density <- merge(pop_density, x1, by = "subdis_en_ocha", all.x = TRUE)
    pop_density[, "pop_density"] <- pop_density[, "pop"] / pop_density[, "subdis_area"]
    pop_density[, "class"] <- ifelse(pop_density[, "pop_density"] >= 300, "urban", "rural")
    table(pop_density[, "class"])
          
    # Merge with main time series
    ts_pop <- merge(ts_pop, pop_density, by = "subdis_en_ocha", all.x = TRUE)
  
      
  #...................................  
  ## Prepare percent change due non-forced migration
    
    # Figure out annual rate of change assumed in Worldpop data
    x1 <- aggregate(pop_worldpop[, "pop"], by = list("year" = pop_worldpop[, "year"]), FUN = sum)
    colnames(x1) <- c("year", "pop")
    x1[, "change"] <- c(diff(x1[, "pop"]), NA)
    x1[, "rate_change"] <- x1[, "change"] / x1[, "pop"]
    annual_r <- mean(x1[, "rate_change"], na.rm = TRUE) # about 2.79% per annum

    # Eliminate national growth trend from Worldpop annual data
    pop_worldpop[, "delta_years"] <- pop_worldpop[, "year"] - 2014    
    pop_worldpop[, "pop_detrend"] <- pop_worldpop[, "pop"] / ((1 + annual_r) ^ pop_worldpop[, "delta_years"])
    
    # Interpolate and extrapolate so as to come up with monthly values per subdistrict
    change <- pop_worldpop[, c("subdis", "year", "month", "pop_detrend")]
    colnames(change) <- c("subdis_en_ocha", "year", "month", "pop_detrend")
    change <- merge(change, t_units, by = c("month", "year"), all.x = TRUE)
    change <- merge(ts[, c("tm", "subdis_en_ocha")], change, by = c("tm", "subdis_en_ocha"), all.x = TRUE )
    change <- change[order(change[, "subdis_en_ocha"], change[, "tm"]), ]
    x1 <- by(change, change[, "subdis_en_ocha"], function(x) {cbind(x[, c("subdis_en_ocha", "tm")], 
      spline(na.omit(x[, c("tm", "pop_detrend")]), xout = x[, "tm"], method = "natural")$y)})
    x2 <- do.call("rbind.data.frame", x1)
    colnames(x2) <- c("subdis_en_ocha", "tm", "pop_ipol")
    change <- x2
    row.names(change) <- NULL
    
    # Compute proportional monthly change, per subdistrict
    x1 <- by(change, change[, "subdis_en_ocha"], function(x) {cbind(x[, c("subdis_en_ocha", "tm", "pop_ipol")], 
      c(diff(x[, "pop_ipol"]), NA))})
    x2 <- do.call("rbind.data.frame", x1)
    colnames(x2) <- c("subdis_en_ocha", "tm", "pop_ipol", "change")
    row.names(x2) <- NULL
    change <- x2
    change[, "change"] <- change[, "change"] / change[, "pop_ipol"]

    
#..........................................................................................
### Preparing crude birth rate and death rate evolution scenarios
#..........................................................................................

  #...................................  
  ## Prepare CBR and CDR trends dataset
  
    # Add last time point and calculate dates
    cbr_cdr_trends <- rbind(cbr_cdr_trends, cbr_cdr_trends[nrow(cbr_cdr_trends), ])
    cbr_cdr_trends[nrow(cbr_cdr_trends), "year"] <- t_units[nrow(t_units), "year"]
    cbr_cdr_trends[nrow(cbr_cdr_trends), "month"] <- t_units[nrow(t_units), "month"]
    cbr_cdr_trends[, "date"] <- dmy(paste("15", cbr_cdr_trends[, "month"], cbr_cdr_trends[, "year"], sep = "-"))

    # Expand to all months in time series
    x1 <- seq(cbr_cdr_trends[1, "date"], cbr_cdr_trends[nrow(cbr_cdr_trends), "date"], by = "month")
    x1 <- data.frame("date" = x1)
    trends <- merge(x1, cbr_cdr_trends, by = "date", all.x = TRUE)  
  
    # Compute smooth counterfactual trends
    trends[, "cbr_smooth"] <- spline(na.omit(trends[, c("date", "cbr")]), xout = trends[, "date"], method = "natural")$y
    trends[, "cdr_smooth"] <- spline(na.omit(trends[, c("date", "cdr")]), xout = trends[, "date"], method = "natural")$y
    
    # Inter- and extrapolate slope factors (CBR) or relative risks (CDR) for each scenario as step functions
    trends[which(trends[, "date"] < dmy("01-01-2014")), grep("sl", colnames(trends))] <- 1
    trends[which(trends[, "date"] < dmy("01-01-2014")), grep("rr", colnames(trends))] <- 1
    for (i in c("mid", "high", "low")) {
      # CBR
      trends[, paste("sl_cbr_", i, "_ipol", sep = "")] <- approx(na.omit(trends[, c("date", paste("sl_cbr_", i, sep = ""))]), 
        xout = trends[, "date"], method = "constant", f = 0)$y
      
      # CDR
      trends[, paste("rr_cdr_", i, "_ipol", sep = "")] <- approx(na.omit(trends[, c("date", paste("rr_cdr_", i, sep = ""))]), 
        xout = trends[, "date"], method = "constant", f = 0)$y
    }

  #...................................  
  ## Compute CBR and CDR for each scenario, starting June 2014
    
    # CBR instantaneous slope
    trends <- trends[order(trends[, "date"]), ]
    trends[, "cbr_slope"] <- c(diff(trends[, "cbr_smooth"]), NA ) / trends[, "cbr_smooth"]

    # CDR baseline level
    trends[, "cdr_base"] <- NA
    trends[which(trends[, "date"] >= dmy("15-06-2014")), "cdr_base"] <- trends[which(trends[, "date"] == dmy("15-06-2014")), "cdr_smooth"]
  
  for (i in c("mid", "high", "low")) {
    # CBR
    trends[, paste("cbr_slope_", i, sep = "")] <- trends[, "cbr_slope"] * trends[, paste("sl_cbr_", i, "_ipol", sep = "")]
    trends[, paste("cbr_", i, sep = "")] <- NA
    for (j in 1:nrow(trends)) {
      if (j == 1) {trends[j, paste("cbr_", i, sep = "")] <- trends[j, "cbr_smooth"]}
      if (j > 1) {trends[j, paste("cbr_", i, sep = "")] <- trends[j-1, paste("cbr_", i, sep = "")] *  
      (1 + trends[j-1, paste("cbr_slope_", i, sep = "")]) }
    }
          
    # CDR
    trends[, paste("cdr_", i, sep = "")] <- trends[, "cdr_base"] * trends[, paste("rr_cdr_", i, "_ipol", sep = "")]
    x1 <- which(trends[, "date"] < dmy("15-06-2014"))
    trends[x1, paste("cdr_", i, sep = "")] <- trends[x1, "cdr_smooth"]
  }
  

  #...................................  
  ## Graph CBR and CDR scenarios

    # CBR
      # prepare dataset
      x1 <- reshape2::melt(trends[, c("date", "cbr_smooth", "cbr_mid", "cbr_low", "cbr_high")],
        variable.name = "scenario", value.name = "cbr", id.var = "date")
      x1[, "scenario"] <- as.character(x1[, "scenario"])
      x1[which(x1[, "scenario"] == "cbr_smooth"), "scenario"] <- "secular trend"
      x1[which(x1[, "scenario"] == "cbr_mid"), "scenario"] <- "main analysis"
      x1[which(x1[, "scenario"] == "cbr_low"), "scenario"] <- "low"
      x1[which(x1[, "scenario"] == "cbr_high"), "scenario"] <- "high"
      x1[, "scenario"] <- factor(x1[, "scenario"], levels = c("secular trend", "main analysis", "low", "high"))
      table(x1[, "scenario"])
      x2 <- na.omit(cbr_cdr_trends[, c("date", "cbr")])
      colnames(x2) <- c("date", "cbr_obs")
      x1 <- merge(x1, x2, by = "date", all.x = TRUE)
      x1[which(x1[, "date"] < dmy("15-05-2014") & x1[, "scenario"] != "secular trend"), c("cbr", "cbr_obs")] <- NA
  
      # plot
      plot_cbr <- ggplot(x1, aes(x = date, y = cbr, group = scenario, colour = scenario, size = scenario, linetype = scenario)) +
        geom_line(alpha = 0.5) +
        scale_size_manual("", values = c(1, 2, 0.75, 0.75)) +
        scale_colour_manual("", values = palette_cb[c(1, 4, 6, 7)]) +
        scale_linetype_manual("", values = c("longdash", "solid", "solid", "solid")) +
        scale_y_continuous("crude birth rate per 1000 people/year", minor_breaks = NULL, 
          breaks = seq(0, 55, by = 5), limits = c(20, 55), expand = c(0, 0) ) +
        scale_x_date("date", date_labels = "%Y", date_breaks = "2 years", 
          limits = c(as.Date("1987-06-01"), as.Date("2021-09-30")), expand = c(0, 0)) +
        theme_bw() +
        theme(text = element_text(colour = "grey20", size = 10), legend.position = "top",
          axis.text = element_text(colour = "grey20", size = 10),
          legend.text = element_text(colour = "grey20", size = 10)) +
        geom_point(aes(x = date, y = cbr_obs), size = 2, stroke = 1.5, 
          colour = palette_cb[1], alpha = 0.8, shape = 0) +
        annotate("rect", xmin = as.Date("2014-09-01"), xmax = as.Date("2021-09-30"), 
          ymin = 20, ymax = 55, fill = palette_cb[8], alpha = 0.2)

    plot_cbr
    ggsave("yem_pop_cbr_scenarios.png", height = 12, width = 18, units = "cm", dpi = "print")

    # CDR
      # prepare dataset
      x1 <- reshape2::melt(trends[, c("date", "cdr_smooth", "cdr_mid", "cdr_low", "cdr_high")],
        variable.name = "scenario", value.name = "cdr", id.var = "date")
      x1[, "scenario"] <- as.character(x1[, "scenario"])
      x1[which(x1[, "scenario"] == "cdr_smooth"), "scenario"] <- "secular trend"
      x1[which(x1[, "scenario"] == "cdr_mid"), "scenario"] <- "main analysis"
      x1[which(x1[, "scenario"] == "cdr_low"), "scenario"] <- "low"
      x1[which(x1[, "scenario"] == "cdr_high"), "scenario"] <- "high"
      x1[, "scenario"] <- factor(x1[, "scenario"], levels = c("secular trend", "main analysis", "low", "high"))
      table(x1[, "scenario"])
      x2 <- na.omit(cbr_cdr_trends[, c("date", "cdr")])
      colnames(x2) <- c("date", "cdr_obs")
      x1 <- merge(x1, x2, by = "date", all.x = TRUE)
      x1[which(x1[, "date"] < dmy("15-05-2014") & x1[, "scenario"] != "secular trend"), c("cdr", "cdr_obs")] <- NA
      x2 <- na.omit(unique(c(cbr_cdr_trends[, "rr_cdr_mid"], cbr_cdr_trends[, "rr_cdr_low"], cbr_cdr_trends[, "rr_cdr_high"])))
      rr_labels <- data.frame("xpos" = as.Date("2013-06-15"), 
        "ypos" = c(x2 * na.omit(unique(trends[, "cdr_base"])), 19.2), 
        "label" = c(paste("x", format(x2, nsmall = 1)), "relative\nincrease"))
      
      # plot
      plot_cdr <- ggplot(x1, aes(x = date, y = cdr, group = scenario, colour = scenario, size = scenario, linetype = scenario)) +
        geom_line(alpha = 0.5) +
        scale_size_manual("", values = c(1, 2, 0.75, 0.75)) +
        scale_colour_manual("", values = palette_cb[c(1, 4, 6, 7)]) +
        scale_linetype_manual("", values = c("longdash", "solid", "solid", "solid")) +
        scale_y_continuous("crude death rate per 1000 people/year", minor_breaks = NULL, 
          breaks = seq(0, 20, by = 1), limits = c(5, 20), expand = c(0, 0) ) +
        scale_x_date("date", date_labels = "%Y", date_breaks = "2 years", 
          limits = c(as.Date("1987-06-01"), as.Date("2021-09-30")), expand = c(0, 0)) +
        theme_bw() +
        theme(text = element_text(colour = "grey20", size = 10), legend.position = "top",
          axis.text = element_text(colour = "grey20", size = 10),
          legend.text = element_text(colour = "grey20", size = 10)) +
        geom_point(aes(x = date, y = cdr_obs), size = 2, stroke = 1.5, 
          colour = palette_cb[1], alpha = 0.8, shape = 0) +
        annotate("rect", xmin = as.Date("2014-09-01"), xmax = as.Date("2021-09-30"), 
          ymin = 5, ymax = 20, fill = palette_cb[8], alpha = 0.2) +
        annotate("text", x = rr_labels[, "xpos"], y = rr_labels[, "ypos"], label = rr_labels[, "label"],
          size = 3.5, colour = "darkred", hjust = 0.5, lineheight = 0.75) 
    plot_cdr
    ggsave("yem_pop_cdr_scenarios.png", height = 12, width = 18, units = "cm", dpi = "print")
    
    # Combined plot
    plot_combi <- ggarrange(plot_cbr + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
      plot.margin = unit(c(0,0.5,0,0.5), "lines")), 
      plot_cdr + theme(plot.margin = unit(c(0,0.5,0,0.5), "lines")), 
      ncol = 1, labels = NULL, common.legend = TRUE, align = "hv")
    plot_combi  
    ggsave("yem_pop_cbr_cdr_scenarios_combi.png", height = 20, width = 18, units = "cm", dpi = "print")

    
  #...................................  
  ## Finish preparing national CBR and CDR scenarios
    
    # CBR scenarios to long version
    x1 <- trends[, c("date", "cbr_mid", "cbr_low", "cbr_high")]
    x1 <- reshape2::melt(x1, id.var = "date", variable.name = "scenario", value.name = "cbr")
    x1[, "scenario"] <- as.character(x1[, "scenario"])
    x1[which(x1[, "scenario"] == "cbr_mid"), "scenario"] <- "mid"
    x1[which(x1[, "scenario"] == "cbr_low"), "scenario"] <- "low"
    x1[which(x1[, "scenario"] == "cbr_high"), "scenario"] <- "high"
    
    # CDR scenarios to long version
    x2 <- trends[, c("date", "cdr_mid", "cdr_low", "cdr_high")]
    x2 <- reshape2::melt(x2, id.var = "date", variable.name = "scenario", value.name = "cdr")
    x2[, "scenario"] <- as.character(x2[, "scenario"])
    x2[which(x2[, "scenario"] == "cdr_mid"), "scenario"] <- "mid"
    x2[which(x2[, "scenario"] == "cdr_low"), "scenario"] <- "low"
    x2[which(x2[, "scenario"] == "cdr_high"), "scenario"] <- "high"

    # Merge, add dates        
    trends <- merge(x1, x2, by = c("date", "scenario"), all.x = TRUE)  
    trends[, "year"] <- year(trends[, "date"])
    trends[, "month"] <- month(trends[, "date"])    
    trends <- merge(trends, t_units, by = c("year", "month"), all.x = TRUE)
    trends <- subset(trends, ! is.na(tm))[, colnames(trends) != "date"]
    
  #...................................  
  ## Specify rural versus urban CBR and CDR
    
    # Merge with main time series
    ts_pop <- merge(ts_pop, trends[, ! colnames(trends) %in% c("year", "month")], by = c("scenario", "tm"), all.x = TRUE)
    ts_pop[, "cbr_avg"] <- ts_pop[, "cbr"]
    ts_pop[, "cdr_avg"] <- ts_pop[, "cdr"]
    
    #  Rural vs urban CBR
      # (cbr_urban * prop_urban + cbr_urban * ratio_cbr * (1 - prop_urban) = cbr_national)
      # (cbr_rural * (1 - prop_urban) + (cbr_rural / ratio_cbr) * prop_urban = cbr_national)  
    
      # ratio of rural:urban
      ratio_cbr <- cbr_rural / cbr_urban      
    
      # proportion urban (as of June 2014)
      prop_urban <- sum(pop_density[which(pop_density[, "class"] == "urban"), "pop"]) / sum(pop_density[, "pop"])
            
      # cbr_urban = cbr_national / (prop_urban + ratio_cbr - ratio_cbr * prop_urban)
      x1 <- which(ts_pop[, "class"] == "urban")
      ts_pop[x1, "cbr"] <- ts_pop[x1, "cbr_avg"] / (prop_urban + ratio_cbr - ratio_cbr * prop_urban)

      # cbr_rural = cbr_national / (1 - prop_urban + prop_urban / ratio_cbr)
      x1 <- which(ts_pop[, "class"] == "rural")
      ts_pop[x1, "cbr"] <- ts_pop[x1, "cbr_avg"] / (1 - prop_urban + prop_urban / ratio_cbr)

    #  Rural vs urban CDR
      # (cdr_urban * prop_urban + cdr_urban * ratio_cdr * (1 - prop_urban) = cdr_national)
      # (cdr_rural * (1 - prop_urban) + (cdr_rural / ratio_cdr) * prop_urban = cdr_national)  
    
      # ratio of rural:urban
      ratio_cdr <- u5mr_rural / u5mr_urban      
    
      # proportion urban (as of June 2014)
      prop_urban <- sum(pop_density[which(pop_density[, "class"] == "urban"), "pop"]) / sum(pop_density[, "pop"])
            
      # cdr_urban = cdr_national / (prop_urban + ratio_cdr - ratio_cdr * prop_urban)
      x1 <- which(ts_pop[, "class"] == "urban")
      ts_pop[x1, "cdr"] <- ts_pop[x1, "cdr_avg"] / (prop_urban + ratio_cdr - ratio_cdr * prop_urban)

      # cdr_rural = cdr_national / (1 - prop_urban + prop_urban / ratio_cdr)
      x1 <- which(ts_pop[, "class"] == "rural")
      ts_pop[x1, "cdr"] <- ts_pop[x1, "cdr_avg"] / (1 - prop_urban + prop_urban / ratio_cdr)
      
    # Clean up
    ts_pop <- ts_pop[, ! colnames(ts_pop) %in% c("subdis_area", "pop_density", "cbr_avg", "cdr_avg", "pop")]
    

#..........................................................................................
### Reconstructing population denominators
#..........................................................................................
    
  #...................................  
  ## Assemble and merge different datasets together
    
    # Add baseline population figures (June 2014)
    x1 <- subset(pop_worldpop, year == 2014 & month == 6)[, c("subdis", "year", "month", "pop")]
    colnames(x1)[colnames(x1) == "subdis"] <- "subdis_en_ocha"        
    ts_pop <- merge(ts_pop, x1, by = c("subdis_en_ocha", "year", "month"), all.x = TRUE)
    
    # Add change due to non-forced displacement
    ts_pop <- merge(ts_pop, change[, c("subdis_en_ocha", "tm", "change")], by = c("subdis_en_ocha", "tm"), all.x = TRUE)
    
    # Add assumptions on proportion of forced displacement that is captured by Worldpop change
    ts_pop[, "overlap"] <- NA
    ts_pop[which(ts_pop[, "scenario"] == "mid"), "overlap"] <- overlap_mid
    ts_pop[which(ts_pop[, "scenario"] == "low"), "overlap"] <- overlap_low
    ts_pop[which(ts_pop[, "scenario"] == "high"), "overlap"] <- overlap_high
    
    # Add displacement estimates
      # IDPs in the subdistrict
      x1 <- idp_arr[, c("scenario", "subdis_en_ocha", "tm", "n_idp")] 
      colnames(x1)[colnames(x1) == "n_idp"] <- "n_idp_in"
      ts_pop <- merge(ts_pop, x1, by = c("scenario", "subdis_en_ocha", "tm"), all.x = TRUE)
            
      # IDPs from the subdistrict
      x1 <- idp_ori[, c("scenario", "subdis_ori_en_ocha", "tm", "n_idp")] 
      colnames(x1)[colnames(x1) == "n_idp"] <- "n_idp_out"
      colnames(x1)[colnames(x1) == "subdis_ori_en_ocha"] <- "subdis_en_ocha"
      ts_pop <- merge(ts_pop, x1, by = c("scenario", "subdis_en_ocha", "tm"), all.x = TRUE)

    # Add under 5y proportion depending on rural vs urban
    ts_pop[, "u5_prop"] <- ifelse(ts_pop[, "class"] == "rural", u5_prop_rural, u5_prop_urban)

    # Restrict time to June 2014 and beyond
    ts_pop <- subset(ts_pop, tm >= 6)
          
  #...................................  
  ## Forward-calculate population, by scenario and subdistrict, starting with June 2014
  
    # Prepare a grouping variable    
    ts_pop[, "id"] <- paste(ts_pop[, "scenario"], ts_pop[, "subdis_en_ocha"], sep = "_")
    ts_pop <- ts_pop[order(ts_pop[, "id"], ts_pop[, "tm"]), ]  
    
    # Compute monthly growth rate (other than through forced displacement)  
    ts_pop[, "cbr"] <- ts_pop[, "cbr"] / (1000 * 12)
    ts_pop[, "cdr"] <- ts_pop[, "cdr"] / (1000 * 12)
    ts_pop[, "growth"] <- 1 + ts_pop[, "cbr"] - ts_pop[, "cdr"] + ts_pop[, "change"]
    
    # Compute monthly net IDP change
    ts_pop[, "n_idp_net"] <- ts_pop[, "n_idp_in"] - ts_pop[, "n_idp_out"]
    x1 <- by(ts_pop, ts_pop[, "id"], function(x) {cbind(x[, c("id", "tm")], c(diff(x[, "n_idp_net"]), NA))}  )
    x1 <- do.call("rbind.data.frame", x1)
    colnames(x1) <- c("id", "tm", "n_idp_change")
    row.names(x1) <- NULL
    ts_pop <- merge(ts_pop, x1, by = c("id", "tm"), all.x = TRUE)
    
    # Adjust baseline population figures for prevalent IDPs at that point
    x1 <- which(ts_pop[, "year"] == year_analysis_start & ts_pop[, "month"] == month_analysis_start)
    ts_pop[x1, "pop"] <- ts_pop[x1, "pop"] + ts_pop[x1, "n_idp_in"] - ts_pop[x1, "n_idp_out"]
      
    # Reconstruct population  
    f_pop <- function(x) {
      x <- x[order(x[, "tm"]), ]
      for (i in 2:nrow(x)) {
        x[i, "pop"] <- x[i-1, "pop"] * x[i-1, "growth"] + (1 - x[i-1, "overlap"]) * x[i-1, "n_idp_change"]
      }
      return(x)
    }  
    x1 <- by(ts_pop, ts_pop[, "id"], f_pop )
    x1 <- do.call("rbind.data.frame", x1)
    row.names(x1) <- NULL
        
    # Compute under 5y population
    x1[, "pop_u5"] <- x1[, "pop"] * x1[, "u5_prop"]
    
    # Tabulate negative estimates
    prop.table(table(ts_pop[, "pop"] < 0, ts_pop[, "scenario"]), margin = 2)

    # Clean up and save
    ts_pop <- x1
    ts_pop[, "pop"] <- round(ts_pop[, "pop"], digits = 0)
    ts_pop[, "pop_u5"] <- round(ts_pop[, "pop_u5"], digits = 0)
    x1 <- c("scenario", "gov_en_ocha", "dis_en_ocha", "subdis_en_ocha", "subdis_pcode", "year", "month", "tm", "pop", "pop_u5",
      "n_idp_in", "n_idp_out")
    write_excel_csv(ts_pop[, x1], "yem_pop_out_pop_estimates.csv")

          
  #...................................  
  ## Generate graphs, tables and maps of population evolution
  
    # Some statistics
      # baseline population
      x1 <- subset(ts_pop, tm == 6)
      by(x1, x1[, "scenario"], function(x) {sum(x[, "pop"], na.rm = TRUE)})
      by(x1, x1[, "scenario"], function(x) {sum(x[, "pop_u5"], na.rm = TRUE)})
  
      # endline population
      x1 <- subset(ts_pop, tm == 93)
      by(x1, x1[, "scenario"], function(x) {sum(x[, "pop"], na.rm = TRUE)})
      by(x1, x1[, "scenario"], function(x) {sum(x[, "pop_u5"], na.rm = TRUE)})
        
      # endline number of IDPs
      by(x1, x1[, "scenario"], function(x) {sum(x[, "n_idp_in"], na.rm = TRUE)})
            
      
    # Countrywide evolution, by scenario
      # prepare data
      x1 <- ts_pop
      x1 <- aggregate(ts_pop[, "pop"], by = ts_pop[, c("scenario", "year", "month")], FUN = sum, na.rm = TRUE)
      colnames(x1)[colnames(x1) == "x"] <- "pop"
      x1[, "date"] <- dmy(paste("15", x1[, "month"], x1[, "year"], sep = "-"))
      x1[which(x1[, "scenario"] == "mid"), "scenario"] <- "main analysis"
      x1[, "scenario"] <- factor(x1[, "scenario"], levels = c("main analysis", "low", "high"))
         
      # plot
      plot1 <- ggplot(x1, aes(x = date, y = pop, group = scenario, colour = scenario, size = scenario, 
        linetype = scenario)) +
        geom_line(alpha = 0.5) +
        scale_size_manual("", values = c(2, 0.75, 0.75)) +
        scale_colour_manual("", values = palette_cb[c(4, 6, 7)]) +
        scale_linetype_manual("", values = c("solid", "solid", "solid")) +
        scale_y_continuous("estimated population", minor_breaks = NULL, labels = comma,
          breaks = seq(0, 35000000, by = 1000000), limits = c(25000000, 32000000), expand = c(0, 0) ) +
        scale_x_date("", date_labels = "%Y", date_breaks = "1 year", expand = c(0, 0)) +
        theme_bw() +
        theme(text = element_text(colour = "grey20", size = 10), legend.position = "top",
          axis.text = element_text(colour = "grey20", size = 10),
          legend.text = element_text(colour = "grey20", size = 10))
      plot1
      ggsave("yem_pop_country_evolution.png", height = 8, width = 16, units = "cm", dpi = "print")

    # Evolution by governorate, by scenario
      # prepare data
      x1 <- ts_pop
      x1 <- aggregate(ts_pop[, "pop"], by = ts_pop[, c("scenario", "gov_en_ocha", "year", "month")], FUN = sum, na.rm = TRUE)
      colnames(x1)[colnames(x1) == "x"] <- "pop"
      x1[, "date"] <- dmy(paste("15", x1[, "month"], x1[, "year"], sep = "-"))
      x1[which(x1[, "scenario"] == "mid"), "scenario"] <- "main analysis"
      x1[, "scenario"] <- factor(x1[, "scenario"], levels = c("main analysis", "low", "high"))
         
      # long plot
      plot1 <- ggplot(x1, aes(x = date, y = pop, group = scenario, colour = scenario, size = scenario, 
        linetype = scenario)) +
        geom_line(alpha = 0.5) +
        scale_size_manual("", values = c(2, 0.75, 0.75)) +
        scale_colour_manual("", values = palette_cb[c(4, 6, 7)]) +
        scale_linetype_manual("", values = c("solid", "solid", "solid")) +
        scale_y_continuous("estimated population", minor_breaks = NULL, labels = comma, expand = c(0, 0) ) +
        scale_x_date("", date_labels = "%Y", date_breaks = "1 year", expand = c(0, 0)) +
        theme_bw() +
        theme(text = element_text(colour = "grey20", size = 10), legend.position = "top",
          axis.text = element_text(colour = "grey20", size = 10),
          legend.text = element_text(colour = "grey20", size = 10),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) ) +
        facet_wrap(~gov_en_ocha, scales = "free_y", ncol= 4)
      plot1
      ggsave("yem_pop_evolution_governorate_long.png", height = 25, width = 20, units = "cm", dpi = "print")

      # wide plot
      plot1 <- ggplot(x1, aes(x = date, y = pop, group = scenario, colour = scenario, size = scenario, 
        linetype = scenario)) +
        geom_line(alpha = 0.5) +
        scale_size_manual("", values = c(2, 0.75, 0.75)) +
        scale_colour_manual("", values = palette_cb[c(4, 6, 7)]) +
        scale_linetype_manual("", values = c("solid", "solid", "solid")) +
        scale_y_continuous("estimated population", minor_breaks = NULL, labels = comma, expand = c(0, 0) ) +
        scale_x_date("", date_labels = "%Y", date_breaks = "1 year", expand = c(0, 0)) +
        theme_bw() +
        theme(text = element_text(colour = "grey20", size = 10), legend.position = "top",
          axis.text = element_text(colour = "grey20", size = 10),
          legend.text = element_text(colour = "grey20", size = 10),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) ) +
        facet_wrap(~gov_en_ocha, scales = "free_y", ncol= 6)
      plot1
      ggsave("yem_pop_evolution_governorate_wide.png", height = 20, width = 30, units = "cm", dpi = "print")
      
          
    # Table of percent change in population from baseline, by governorate
      # prepare data
      for (i in c("mid", "low", "high")) {
        x1 <- subset(ts_pop, scenario == i)
        x1 <- aggregate(x1[, "pop"], by = x1[, c("gov_en_ocha", "year", "month")], FUN = sum, na.rm = TRUE)
        colnames(x1)[colnames(x1) == "x"] <- "pop"
        x1[, "date"] <- dmy(paste("15", x1[, "month"], x1[, "year"], sep = "-"))
        x1 <- subset(x1, date %in% c(min(x1[, "date"]), max(x1[, "date"])))
        x2 <- subset(x1, date == min(x1[, "date"]))[, c("gov_en_ocha", "pop")]
        colnames(x2)[2] <- "pop_base"
        x1 <- merge(x1, x2, by = "gov_en_ocha", all.x = TRUE)
        x1 <- subset(x1, date == max(x1[, "date"]))
        x1[, paste("change_", i, sep = "")] <- (x1[, "pop"] - x1[, "pop_base"]) / x1[, "pop_base"]
        x2 <- c((sum(x1[, "pop"]) - sum(x1[, "pop_base"])) / sum(x1[, "pop_base"]), sum(x1[, "pop"]))
        x1 <- x1[, c("gov_en_ocha", paste("change_", i, sep = ""), "pop")]
        x1 <- rbind(x1, c("total", x2) )
        x1[, "pop"] <- format(round(as.numeric(x1[, "pop"]), digits = -3), big.mark = ",")
        x1[, paste("change_", i, sep = "")] <- format(round(as.numeric(x1[, paste("change_", i, sep = "")]) * 100, 
          digits = 1), nsmall = 1)
        colnames(x1)[colnames(x1) == "pop"] <- paste("pop_", i, sep = "")
        assign(paste("table_", i, sep = ""), x1)
      }    
      
      x1 <- as.data.frame(matrix(ncol = ncol(table_mid), nrow = nrow(table_mid)))
      x1[, 1] <- table_mid[, "gov_en_ocha"]
      x1[, 2] <- paste(table_mid[, "pop_mid"], " (", table_low[, "pop_low"], " to ", table_high[, "pop_high"], ")", sep = "")
      x1[, 3] <- paste(table_mid[, "change_mid"], " (", table_low[, "change_low"], " to ", table_high[, "change_high"], ")", sep = "")
      colnames(x1) <- c("governorate", "estimated population (Sep 2021)", "percent change from Jun 2014")
      write.csv(x1, "yem_pop_out_change_by_gov.csv", row.names = FALSE)
          
    
  #...................................  
  ## Generate graphs of displacement evolution
    # Countrywide evolution, by scenario
      # prepare data
      x1 <- ts_pop
      x1 <- aggregate(ts_pop[, "n_idp_in"], by = ts_pop[, c("scenario", "year", "month")], FUN = sum, na.rm = TRUE)
      colnames(x1)[colnames(x1) == "x"] <- "n_idp_in"
      x1[, "date"] <- dmy(paste("15", x1[, "month"], x1[, "year"], sep = "-"))
      x1[which(x1[, "scenario"] == "mid"), "scenario"] <- "main analysis"
      x1[, "scenario"] <- factor(x1[, "scenario"], levels = c("main analysis", "low", "high"))
      
      # add IOM official estimates (https://www.internal-displacement.org/countries/yemen)
      x2 <- data.frame(
        "scenario" = "IOM estimates",
        "date" = dmy("31-12-2014", "31-12-2015", "31-12-2016", "31-12-2017", "31-12-2018", "31-12-2019", "31-12-2020"), 
        "n_idp_in" = c(334090, 2509068, 1973994, 2014026, 2323596, 3635245, 3857845)
      )
      x2[, "n_idp_rounded"] <- format(round(x2[, "n_idp_in"] / 1000000, digits = 1), nsmall = 1)
      x2[, "n_idp_rounded"] <- paste(x2[, "n_idp_rounded"], "M", sep = "")
         
      # plot
      plot1 <- ggplot(x1) +
        geom_line(aes(x = date, y = n_idp_in, group = scenario, colour = scenario, size = scenario, 
        linetype = scenario), alpha = 0.5) +
        scale_size_manual("", values = c(2, 0.75, 0.75)) +
        scale_colour_manual("", values = palette_cb[c(4, 6, 7)]) +
        scale_linetype_manual("", values = c("solid", "solid", "solid")) +
        scale_y_continuous("estimated number of IDPs", minor_breaks = NULL, labels = comma, 
          breaks = seq(0, 16000000, by = 2000000), limits = c(0, 16000000), expand = c(0, 0) ) +
        scale_x_date("", date_labels = "%Y", date_breaks = "1 year", expand = c(0, 0)) +
        geom_point(data = x2, aes(x = date, y = n_idp_in), colour = palette_cb[3], 
          shape = 0, size = 2, stroke = 1) +
        geom_text(data = x2, aes(x = date, y = n_idp_in, label = n_idp_rounded), nudge_y = 1000000,
          colour = palette_cb[3]) +
        theme_bw() +
        theme(text = element_text(colour = "grey20", size = 10), legend.position = "top",
          axis.text = element_text(colour = "grey20", size = 10),
          legend.text = element_text(colour = "grey20", size = 10))
      plot1
      ggsave("yem_pop_idp_evolution.png", height = 10, width = 20, units = "cm", dpi = "print")

      
    # Evolution by governorate, by scenario
      # prepare data
      x1 <- ts_pop
      x1 <- aggregate(ts_pop[, "n_idp_in"], by = ts_pop[, c("scenario", "gov_en_ocha", "year", "month")], FUN = sum, na.rm = TRUE)
      colnames(x1)[colnames(x1) == "x"] <- "n_idp_in"
      x1[, "date"] <- dmy(paste("15", x1[, "month"], x1[, "year"], sep = "-"))
      x1[which(x1[, "scenario"] == "mid"), "scenario"] <- "main analysis"
      x1[, "scenario"] <- factor(x1[, "scenario"], levels = c("main analysis", "low", "high"))
         
      # long plot
      plot1 <- ggplot(x1, aes(x = date, y = n_idp_in, group = scenario, colour = scenario, size = scenario, 
        linetype = scenario)) +
        geom_line(alpha = 0.5) +
        scale_size_manual("", values = c(2, 0.75, 0.75)) +
        scale_colour_manual("", values = palette_cb[c(4, 6, 7)]) +
        scale_linetype_manual("", values = c("solid", "solid", "solid")) +
        scale_y_continuous("estimated number of IDPs", minor_breaks = NULL, labels = comma, expand = c(0, 0) ) +
        scale_x_date("", date_labels = "%Y", date_breaks = "1 year", expand = c(0, 0)) +
        theme_bw() +
        theme(text = element_text(colour = "grey20", size = 10), legend.position = "top",
          axis.text = element_text(colour = "grey20", size = 10),
          legend.text = element_text(colour = "grey20", size = 10),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) ) +
        facet_wrap(~gov_en_ocha, scales = "free_y", ncol= 4)
      plot1
      ggsave("yem_pop_idp_evolution_governorate_long.png", height = 25, width = 20, units = "cm", dpi = "print")

      # wide plot
      plot1 <- ggplot(x1, aes(x = date, y = n_idp_in, group = scenario, colour = scenario, size = scenario, 
        linetype = scenario)) +
        geom_line(alpha = 0.5) +
        scale_size_manual("", values = c(2, 0.75, 0.75)) +
        scale_colour_manual("", values = palette_cb[c(4, 6, 7)]) +
        scale_linetype_manual("", values = c("solid", "solid", "solid")) +
        scale_y_continuous("estimated number of IDPs", minor_breaks = NULL, labels = comma, expand = c(0, 0) ) +
        scale_x_date("", date_labels = "%Y", date_breaks = "1 year", expand = c(0, 0)) +
        theme_bw() +
        theme(text = element_text(colour = "grey20", size = 10), legend.position = "top",
          axis.text = element_text(colour = "grey20", size = 10),
          legend.text = element_text(colour = "grey20", size = 10),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) ) +
        facet_wrap(~gov_en_ocha, scales = "free_y", ncol= 6)
      plot1
      ggsave("yem_pop_idp_evolution_governorate_wide.png", height = 20, width = 30, units = "cm", dpi = "print")
      
    # Map of IDPs as percent of population, by district (as of Sep 2021)
      # prepare data  
      x1 <- subset(ts_pop, tm == max(ts_pop[, "tm"], na.rm = TRUE))
      x1 <- subset(x1, scenario == "mid")
      x1 <- x1[, c("dis_en_ocha", "n_idp_in", "pop")]
      x1 <- aggregate(x1[, c("n_idp_in", "pop")], by = list(x1[, "dis_en_ocha"]), FUN = sum, na.rm = TRUE)
      colnames(x1)[1] <- "dis_en_ocha"
      # x1[, "pop"] <- ifelse(x1[, "pop"] < 0, 0, x1[, "pop"])
      x1[, "percent"] <- x1[, "n_idp_in"] * 100 / x1[, "pop"]
      x1[, "percent"] <- ifelse(x1[, "percent"] < 0, NA, x1[, "percent"])
            
      # merge with shape file
      x2 <- ocha_shape_dis
      colnames(x2) <- gsub("_shape_dis", "", colnames(x2))
      x1 <- merge(x2, x1, by = c("dis_en_ocha"), all.x = TRUE)
      
      # prepare for mapping
      pos_gov_x <- rep(0, times <- length(ocha_shape_gov$gov_en_ocha_shape_gov))
      pos_gov_y <- rep(0, times <- length(ocha_shape_gov$gov_en_ocha_shape_gov))
      names(pos_gov_x) <- ocha_shape_gov$gov_en_ocha_shape_gov
      names(pos_gov_y) <- ocha_shape_gov$gov_en_ocha_shape_gov
      pos_gov_y[c("Sana'a", "Aden")] <- -0.2
    
      # map
      map1 <- tm_shape(x1) +
        tm_borders(col = "grey60", lwd = 0.3) +
        tm_fill("percent", alpha = 0.5, palette = "Reds", legend.is.portrait = TRUE,
          title = "percentage of IDPs (Sep 2021)") +
        tm_shape(ocha_shape_gov) +
        tm_text("gov_en_ocha_shape_gov", col = "grey20", size = 0.6, xmod = pos_gov_x,
          ymod = pos_gov_y, fontface = "bold", alpha = 0.8) +
        tm_borders(col = "grey20", lwd = 2, alpha = 0.5) +
        tm_layout(legend.title.size = 0.7, legend.text.size =  0.5, legend.text.color = "grey20",
          legend.title.color = "grey20", legend.position = c("LEFT", "TOP"))
      map1
      tmap_save(map1, "yem_pop_map_idp_percent.png", height = 15, width = 20, units = "cm", dpi = 300)
    
    
#..........................................................................................
### ENDS
#..........................................................................................
     
#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## ---- R SCRIPT TO READ DATA AND PARAMETERS, AND CONTROL EXECUTION OF OTHER SCRIPTS --- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Oct 2021)

                                          # francesco.checchi@lshtm.ac.uk 


#..........................................................................................
### Preparatory steps
#..........................................................................................

  #...................................      
  ## Install or load required R packages
    
    # List of required packages
    x1 <- c("caret", "dismo", "geodist", "ggpubr", "gamlss", "gamlss.tr",
      "gtools", "lubridate", "MASS", "mice", "NCmisc", "patchwork", "ranger", "RColorBrewer",
      "readxl", "scales", "sf", "stringdist", "stringi", "tidyverse", "tmap", "zoo")
    
    # Install any packages not yet installed
    x2 <- x1 %in% row.names(installed.packages())
    if (any(x2 == FALSE)) { install.packages(x1[! x2]) }
    
    # Load all packages    
    lapply(x1, library, character.only = TRUE)


  #...................................      
  ## Starting setup

    # Set memory size
    memory.size(999999)
    
    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font
    windowsFonts(Arial=windowsFont("Arial"))

    # Set working directory to where this file is stored
    current_path = rstudioapi::getActiveDocumentContext()$path 
    setwd(dirname(current_path ))
    print( getwd() )
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
    palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    

#.........................................................................................
### Specify parameters
#.........................................................................................    

  #...................................      
  ## Table of matching methods and parameters
  methods <- data.frame("method" = c("hamming", "lv", "osa", "dl", "qgram", "cosine", "jaccard", "jw" ),
    "maxDist" = c(5, 5, 5, 5, 5, 0.5, 0.5, 0.5), "p" = 0.1, "q" = 1)

  #...................................      
  ## Geographic units for locality matching
  match_units <- c("city", "neighbourhood", "harrah", "village", "subvillage")   

  #...................................      
  ## Maximum string distance tolerated to declare a match between two geographic names
  distance_max <- 2
   
  
#.........................................................................................
### Reading in bespoke functions
#.........................................................................................    

source("yem_pop_0_user_functions.r", echo = TRUE)        


  
#.........................................................................................
### Reading in required files and parameters
#.........................................................................................
 
  #...................................      
  ## Read in various shape files
  
    # OCHA/CSO shape file for sub-districts
    dir_maps <- paste(getwd( ), "/mapping", sep = "")
    ocha_shape <- sf::st_read(paste(dir_maps, "/yem_admbnda_adm3_govyem_cso_20191002.shp", sep = ""))
 
    # OCHA/CSO shape file for districts
    dir_maps <- paste(getwd( ), "/mapping", sep = "")
    ocha_shape_dis <- sf::st_read(paste(dir_maps, "/yem_admbnda_adm2_govyem_cso_20191002.shp", sep = ""))
    
    # OCHA/CSO shape file for governorates
    dir_maps <- paste(getwd( ), "/mapping", sep = "")
    ocha_shape_gov <- sf::st_read(paste(dir_maps, "/yem_admbnda_adm1_govyem_cso_20191002.shp", sep = ""))
     
    # OpenStreetMap shape file for location of health facilities
    dir_maps <- paste(getwd( ), "/health_data", sep = "")
    health_shape <- sf::st_read(paste(dir_maps, "/hotosm_yem_health_facilities_points.shp", sep = ""))
    
    # Shape file for road network
    dir_maps <- paste(getwd( ), "/transport_data", sep = "")
    road_shape <- sf::st_read(paste(dir_maps, "/Ymn-Roads.shp", sep = ""))
  
  
  #...................................      
  ## Read in gazetteer datasets
  filename <- "yem_gazetteers.xlsx"
  worksheets <- c("ocha_gazetteer", "cso_gazetteer")
  df_names <- c("ocha", "cso")
  for (i in 1:length(worksheets) ) {
    x1 <- read_excel(filename, sheet = worksheets[i] )
    x1 <- as.data.frame(x1)
    assign(df_names[i], x1)
    rm(x1)
  }

  #...................................      
  ## Read in ACLED insecurity dataset
  acled <- read.csv("yem_insecurity_data.csv")  
  
  #...................................      
  ## Read in population and displacement parameters and datasets
    # File name that contains these
    filename <- "yem_demog_data.xlsx"

    # Data table
    demog_table <- read_excel(filename, sheet = "data_table")
      #get rid of tibble
      demog_table <- data.frame(demog_table)

    # Demographic parameters
    demog_pars <- read_excel(filename, sheet = "demog_pars")
      #get rid of tibble
      demog_pars <- data.frame(demog_pars)

      # assign parameter objects and their values
      demog_pars <- demog_pars[, c("parameter", "value")]
      for (i in 1:nrow(demog_pars)) {
        if (suppressWarnings(! is.na(as.numeric(demog_pars[i, "value"]) ) ) )
        { assign(demog_pars[i, "parameter"], as.numeric(demog_pars[i, "value"])) }
        else
        { assign(demog_pars[i, "parameter"], demog_pars[i, "value"]) }
      }

    # Population data
      # first read data table tab, which contains meta-data on all the other predictors...
      pop_sources <- read_excel(filename, sheet = "data_table")
        #get rid of tibble
        pop_sources <- data.frame(pop_sources)
        # exclude any datasets that are not going to be used for analysis
        pop_sources <- subset(pop_sources, used_in_analysis == "Y" )

      # ...also read variable dictionary which contains variable on whether any variable is to be read (used in analysis)
      dictionary <- read_excel(filename, sheet = "dictionary")
        #get rid of tibble
        dictionary <- data.frame(dictionary)

      # ...then read all of the population data sources and name them as per the data sources table
      for (i in 1:nrow(pop_sources) )  {
        # which predictor is being read?
        x1 <- paste(pop_sources[i, "worksheet"])

        # read file
        x2 <- read_excel(filename, sheet = x1, guess_max = 200000 )
          #get rid of tibble
          x2 <- data.frame(x2)

        # check that import has been successful
        print("+++++++++++++++++++++++++++++++++++++")
        print(paste("now importing dataset...", x1) )
        str(x2)

        # only keep variables that will be used for analysis
        x1 <- subset(dictionary, worksheet == x1 & used_in_analysis == "Y")[, "variable"]
        x2 <- x2[, c(unlist(x1)) ]

        # name the dataset as per the name of the worksheet
        assign(pop_sources[i, "worksheet"], get("x2") )
        rm(x2)
      }

   
    
#.........................................................................................
### Cleaning shape file, population data and gazetteer datasets
#.........................................................................................

source("yem_pop_1_clean_data_various.r", echo = TRUE)        


    
#.........................................................................................                            
### Generating a time series of subdistrict-months
#.........................................................................................
  
  #...................................    
  ## Create a time series of stratum-time

    # Create a time unit variable tm (from month 1 to month T of analysis period T)
      # start time series one Jan of the year of analysis start
    tm <- seq(1, (( year_analysis_end - year_analysis_start ) * 12 + month_analysis_end - 1 + 1 ), 1)
    
    # Create a time series of subdistrict-year-months
    ts <- expand.grid(unlist(ocha[, "subdis_en_ocha"] ), tm)
    colnames(ts) <- c("subdis_en_ocha", "tm")
    
    # Work out corresponding year and month values
    ts[, "year"] <- floor( (ts[, "tm"] + 1 - 2) / 12) + year_analysis_start
    ts[, "month"] <- (ts[, "tm"] + 1 - 1) - (ts[, "year"] - year_analysis_start) * 12
    
    # Merge other admin levels back in
    ts <- merge(ts, ocha, by = "subdis_en_ocha", sort=TRUE)
    
    # Sort time series
    ts <- ts[order(ts[, "subdis_en_ocha"], ts[, "tm"]), ]
  
      
  #...................................    
  ## Define subdistrict names and time units
    # Subdistrict names
    subdis_names <- as.character(unique(ocha[, "subdis_en_ocha"]))
    subdis_names <- sort(subdis_names)
    
    # Time units  
    t_units <- unique(ts[, c("tm", "month", "year")])
  
    # Period start and end points
    tm_analysis_start <- t_units[t_units$year == year_analysis_start & t_units$month == month_analysis_start, "tm"]
    tm_analysis_end <- t_units[t_units$year == year_analysis_end & t_units$month == month_analysis_end, "tm"]

  
#.........................................................................................
### Clean displacement datasets (prevalent, incident; IDPs, returnees)
#.........................................................................................
                
source("yem_pop_2_clean_data_displacement.r", echo = TRUE)        
    
        
#.........................................................................................
### Establishing equivalence between DTM and OCHA/CSO gazetteers
#.........................................................................................

source("yem_pop_3_equivalence_gazetteers.r", echo = TRUE)        
    
          
#.........................................................................................
### Identifying missing subdistricts through matching techniques
#.........................................................................................

source("yem_pop_4_missing_subdistricts_matching.r", echo = TRUE)        
          
      
#.........................................................................................    
### Identifying missing subdistricts through predictive models
#.........................................................................................
      
source("yem_pop_5_missing_subdistricts_models.r", echo = TRUE)        
          
  
#.........................................................................................    
### Preparing displacement flow estimates while adjusting for estimated rate of return
#.........................................................................................
      
source("yem_pop_6_prepare_displacement_flows.r", echo = TRUE)        
          
      
#.........................................................................................    
### Reconstructing population denominators
#.........................................................................................
      
source("yem_pop_7_reconstruct_population.r", echo = TRUE)        
          
    
    
   
#..........................................................................................
### ENDS
#..........................................................................................
    

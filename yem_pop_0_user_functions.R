#..........................................................................................
###       RECONSTRUCTING SUBDISTRICT POPULATION DENOMINATORS IN YEMEN, 2014-2021        ###
#..........................................................................................

#..........................................................................................
## ----------------- R SCRIPT TO DEFINE FUNCTIONS USED FOR THE ANALYSIS ---------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Oct 2021)

                                          # francesco.checchi@lshtm.ac.uk 


#..........................................................................................
### Function to perform fuzzy string matching using various methods, and identify most common match among methods
#..........................................................................................

f_fuzzy <- function(strings_to_match, match_candidates, methods) {
  
  #...................................  
  ## Prepare output
    out <- data.frame(matrix(NA, nrow = length(strings_to_match), ncol = nrow(methods)))
    colnames(out) <- methods$method
      
  #...................................     
  ## Fuzzy matching using various methods
  for (i in  1:nrow(methods) ) {
    x1 <- sapply(strings_to_match, function (x) 
        {amatch(x, match_candidates, maxDist = methods[i, "maxDist"], method = methods[i, "method"],
          q = methods[i, "q"], p = methods[i, "p"], matchNA = TRUE,
          useBytes = FALSE, nomatch = NA, weight = c(d = 1, i = 1, s = 1, t = 1),)} )
    out[, methods[i, "method"]] <- match_candidates[x1]
  }
    
  #...................................     
  ## Identify most common match across records
  out[, "common"] <- NA
    
    # Identify all-NA rows and assign a value of "NO MATCH" to all of these
    out[, "all_na"] <- apply(out[, methods$method], 1, FUN = function(x) { all(is.na(x)) } )
    out[out$all_na == TRUE, "common"] <- "NO MATCH"
    
    # For not all-NA rows...
    out[out$all_na == FALSE, "common"] <- 
      apply(out[out$all_na == FALSE, methods$method], 1, FUN = function(x) {return(names(which.max(table(as.character(x)))))} )
  return(out$common)
}  
    

#..........................................................................................
### Function to plot a histogram of a given variable
#..........................................................................................
  
f_hist <- function(var_x, df_plot, lims) {
      
  plot <- ggplot(df_plot)

  #...................................  
  ## If the variable has >= 20 unique values...(i.e. can be treated continuous...)          
  if (length(unique(na.omit(df_plot[, var_x]))) >= 20) {
    plot <- plot + geom_histogram(aes(x = as.numeric(df_plot[, var_x]) ), 
      color="seagreen", fill="seagreen3", alpha = 0.5 ) +
      theme_bw() + xlab(var_x) + scale_x_continuous(expand = c(0, 0), limits = lims )
  }
   
  #...................................  
  ## Otherwise...         

  if (length(unique(na.omit(df_plot[, var_x]))) < 20) {
    plot <- plot + geom_histogram(aes(x = as.factor(df_plot[, var_x]) ), stat="count", 
      color="seagreen", fill="seagreen3", alpha = 0.5) +
      theme_bw() + xlab(var_x)
  }
          
  print(plot)
}


#..........................................................................................
### Function to compute prediction confidence intervals or prediction profiles for bootstrapping, from a GAMLSS fit
  # by posterior simulation, as in https://r.789695.n4.nabble.com/Prediction-interval-with-GAM-td3460175.html
#..........................................................................................
    
f_interval <- function(f_fit, f_n_boot, f_obs, f_profile) {

  #...................................
  ## Extract coefficient estimates and variance-covariance matrix from fit
  beta <- na.omit( coef(f_fit) ) # only mu coefficients, i.e. for estimating linear predictor; omit NAs as random effects come out as NA
    # Convert to inverse (for later operation)
    beta_inv <- ginv(matrix(beta))
  vcov_matrix <- vcov(f_fit)[1:length(beta), 1:length(beta)] # only mu coefficients
    # Cholesky decomposition
    cholesky_dec <- chol(vcov_matrix)

  #...................................
  ## Simulate a large number of random beta coefficient values drawn from an assumed normal error distribution
    # around their posterior estimates
  beta_sim <- t(cholesky_dec) %*% matrix(rnorm(f_n_boot * length(beta)), length(beta), f_n_boot) + as.vector(beta)

  #...................................
  ## Predict both mu and sigma on the new data
  mu_pred <- predict(f_fit, newdata = f_obs[, all.vars(formula(f_fit))[-1]], type = "link")
  if (family(f_fit)[1] != "PO") {
    sigma_pred <- predict(f_fit, newdata = f_obs[, all.vars(formula(f_fit))[-1]], type = "link", what = "sigma")} # for later

    # Produce a 'linear prediction matrix'
      # since lp_matrix %*% coef(f_fit) = mu_pred , the code below uses the inverse (i.e. matrix 'division')
      # to get the lp_matrix, which gamlss doesn't output
    lp_matrix <- mu_pred %*% beta_inv

    # Compute point estimate linear predictions and back-transform them if appropriate, for all the random beta values
      # only exponential and logit back-transform implemented here for now
    pred_sim <- lp_matrix %*% beta_sim
    if ( f_fit$mu.link == "log" ) {pred_sim <- exp(pred_sim)}
    if ( f_fit$mu.link == "logit" ) {pred_sim <- exp(pred_sim) / (1 + exp(pred_sim))}

  #...................................
  ## Lastly, generate random predictions by combining the point estimates with the other estimated distributional parameters
    # gaussian, beta, gamma, inverse gaussian and basic 'count' distributions implemented here so far
  if (family(f_fit)[1] == "NO")
    { rand_sim <- matrix( rNO(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }
  if (family(f_fit)[1] == "IG")
    { rand_sim <- matrix( rIG(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }
  if (family(f_fit)[1] == "IG0to1")
    { rand_sim <- matrix( rIG0to1(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }    
  
  if (family(f_fit)[1] == "PO")
    { rand_sim <- matrix( rPO(n = prod(dim(pred_sim)), mu = pred_sim),
      nrow(pred_sim), ncol(pred_sim) ) }
  if (family(f_fit)[1] == "NBI")
    { rand_sim <- matrix( rNBI(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }
  if (family(f_fit)[1] == "NBII")
    { rand_sim <- matrix( rNBII(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }
  if (family(f_fit)[1] == "GA")
    { rand_sim <- matrix( rGA(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }
  if (family(f_fit)[1] == "GA0to1")
    { rand_sim <- matrix( rGA0to1(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }
  if (family(f_fit)[1] == "IGAMMA")
    { rand_sim <- matrix( rIGAMMA(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }
  if (family(f_fit)[1] == "IGAMMA0to1")
    { rand_sim <- matrix( rIGAMMA0to1(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred), f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }
  
  if (family(f_fit)[1] == "BE")
    { rand_sim <- matrix( rBE(n = prod(dim(pred_sim)), mu = pred_sim, sigma = rep(exp(sigma_pred) / (1 + exp(sigma_pred)) , f_n_boot) ),
      nrow(pred_sim), ncol(pred_sim) ) }

  #...................................
  ## If desire 95% confidence interval...
  if (f_profile == FALSE) {
    # x1 <- mu_pred
    # if ( f_fit$mu.link == "log") { x1 <- exp(mu_pred) }
    # if ( f_fit$mu.link == "logit") { x1 <- exp(mu_pred) / (1 + exp(mu_pred)) }
    # out <- cbind(f_obs, x1, t(apply(rand_sim, 1, quantile, prob = c(0.025,0.975), na.rm = TRUE) ) )
    out <- cbind(f_obs, t(apply(rand_sim, 1, quantile, prob = c(0.500, 0.025,0.975), na.rm = TRUE) ) )
    out <- as.data.frame(out)
    colnames(out) <- c(colnames(f_obs), "pred", "pred_lci", "pred_uci")
    # out[, c("pred", "pred_lci", "pred_uci")] <- round(out[, c("pred", "pred_lci", "pred_uci")], digits = 0)
    return(out)
  }

  #...................................
  ## ...else output entire prediction profile, sorted ascendingly
  if (f_profile == TRUE) {
    out <- apply(rand_sim, 1, sort)
    return(out)
  }
}



#..........................................................................................
### ENDS
#..........................................................................................
     
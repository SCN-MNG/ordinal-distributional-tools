
###############################################################
# Gordon Anderson Working Paper
# 
###############################################################

library(dplyr)
library(tidyr)
library(purrr)



###############################################################
# 1. DISTRIBUTION (any number of variables)
#    data       : vector, matrix or data.frame (columns = variables)
#    categories : NULL (inferred per variable), one vector used for
#                 every variable, or a list with one vector per variable.
#                 Defines the ordinal categories used by this variable (e.g. 1:10 for a Cantril Ladder)
###############################################################

distribution_tools <- function(data,
                               categories = NULL,
                               remove_missing = TRUE){
  
  data <- as.data.frame(data)
  k <- ncol(data)
  vars <- names(data)
  
  # Process categories into one vector per variable
  if(is.null(categories)){
    categories <- lapply(data, function(v) sort(unique(v[!is.na(v)])))
  } else if(!is.list(categories)){
    categories <- rep(list(categories), k)
  }
  names(categories) <- vars
  
  # Drop rows with any value outside the defined categories (or NA)
  if(remove_missing){
    valid <- Map(`%in%`, data, categories)   # one TRUE/FALSE vector per variable
    data <- data[Reduce(`&`, valid), , drop = FALSE]
  }
  
  n <- nrow(data)
  
  # Convert dataframe to factors; no significant modification
  data[] <- Map(factor, data, levels = categories)
  
  
  counts <- table(data)
  pdf <- array(as.numeric(counts) / n,
               dim = dim(counts),
               dimnames = dimnames(counts))
  
  # Joint CDF: cumulative sums along each dimension in turn
  cdf <- pdf
  if(k == 1){
    cdf[] <- cumsum(pdf)
  } else {
    for(d in seq_len(k)){
      others <- seq_len(k)[-d]
      res <- apply(cdf, others, cumsum)
      res <- array(res, dim = c(dim(pdf)[d], dim(pdf)[others]))
      cdf <- aperm(res, order(c(d, others)))
    }
    dimnames(cdf) <- dimnames(pdf)
  }
  
  survival <- 1 - cdf
  
  list(
    Categories = categories,
    PDF = pdf,
    CDF = cdf,
    Survival = survival
  )
}

###############################################################
# 2. EXPECTED HIGHER OUTCOME
###############################################################

expected_tools <- function(data,
                           categories = NULL){
  
  d <- distribution_tools(data, categories)
  
  list(
    EqualWeightedHO    = mean(d$Survival),
    CategoryWeightedHO = sum(d$PDF * d$Survival)
  )
}

###############################################################
# 3. MODE, INFORMITY, GGGN, AVERAGE CATEGORY AND LEFT SKEWNESS
###############################################################
mode_tools <- function(data, categories = NULL) {
  
  d <- distribution_tools(data, categories)
  
  pdf  <- d$PDF
  cdf  <- d$CDF
  cats <- d$Categories
  k    <- length(cats)
  
  # Mode (first one if tied)
  mode_idx <- as.vector(arrayInd(which.max(pdf), dim(pdf)))
  mode_category <- mapply(function(cv, i) cv[i], cats, mode_idx)
  p_mode <- max(pdf)
  
  # Odds relative to the mode, and informity
  odds <- pdf / p_mode
  informity <- 1 - sum(pdf * odds)
  
  # GGGN (Greatest Good of the Greatest Number) Index
  PM <- cdf[matrix(mode_idx, nrow = 1)]
  gggn <- sqrt(PM * informity)
  
  # Average category and left-skewness: only for one variable
  if (k == 1) {
    
    mean_category <- sum(cats[[1]] * pdf)
    
    avg_idx <- which.min(abs(cats[[1]] - mean_category))
    average_category <- cats[[1]][avg_idx]
    
    PA <- cdf[avg_idx]
    left_skewness <- PA - PM
    
  } else {
    average_category <- NA
    left_skewness <- NA
  }
  
  result <- list(
    ModalCategory = unname(mode_category),
    Informity      = unname(informity),
    GGGN           = unname(gggn)
  )
  
  if (k == 1) {
    result$AverageCategory <- unname(average_category)
    result$LeftSkewness    <- unname(PA - PM)
  }
  
  result
}

###############################################################
# 4. COMPLETE SUMMARY (any number of variables)
###############################################################

distributional_summary <- function(data,
                                   categories = NULL){
  c(
    expected_tools(data, categories),
    mode_tools(data, categories)
  )
}


###############################################################
# 5. GINI DISTRIBUTIONAL TRANSVARIATION
###############################################################

transvariation <- function(x1,
                           x2,
                           categories = NULL,
                           remove_missing = TRUE) {
  
  x1 <- as.data.frame(x1)
  x2 <- as.data.frame(x2)
  
  # Both datasets must have the same variables in the same order
  if (!identical(names(x1), names(x2))) {
    stop("x1 and x2 must have identical column names and order.")
  }
  
  # Use a common set of categories for both distributions
  if (is.null(categories)) {
    combined <- rbind(x1, x2)
    
    categories <- lapply(
      combined,
      function(v) sort(unique(v[!is.na(v)]))
    )
  }
  
  # Calculate the joint distributions
  d1 <- distribution_tools(
    x1,
    categories = categories,
    remove_missing = remove_missing
  )
  
  d2 <- distribution_tools(
    x2,
    categories = categories,
    remove_missing = remove_missing
  )
  
  # Compare the joint PDFs cell by cell
  transvariation <- sum(abs(d1$PDF - d2$PDF)) / 2
  
  list(
    Transvariation = transvariation
  )
}
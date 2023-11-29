# main simulated databases
datasets

# modelling
# 1. FABIA

# (1.a) Writing FABIA function

#############################################################################################
fabia_function <- function(datasets, num, factor_vector) {
  set.seed(123)
  library(fabia)
  
  # Saving bicluster information into a list of tables
  fab.sim_score <- list()
  fab.sim_loading <- list()
  
  for (i in 1:length(datasets)) {
    rawdata <- as.matrix(datasets[[i]])  # Convert input data to matrix
    sim.resFabia <- fabia(t(rawdata), p = num, alpha = 0,1, cyc = 1000, spl = 0.5, spz = 0.5, random = 1.0, center = 2, norm = 2, scale = 0.0, lap = 1.0, nL = 1, lL = 0, bL = 0)
    extractPlot(sim.resFabia, which = 5)  # Plot the results
    
    score_list <- list()
    loading_list <- list()
    
    for (k in 1:length(factor_vector)) {
      # Create a function to extract score
      fab_score <- function(sim_resFabia, factor_index) {
        score_FABIA <- sim_resFabia@L[, factor_index]
        return(score_FABIA)
      }
      
      # Create a function to extract loading
      fab_loading <- function(sim_resFabia, factor_index) {
        loading_FABIA <- sim_resFabia@Z[factor_index, ]
        return(loading_FABIA)
      }
      
      score_list[[k]] <- fab_score(sim.resFabia, factor_vector[k])
      loading_list[[k]] <- fab_loading(sim.resFabia, factor_vector[k])
    }
    
    fab.sim_score[[i]] <- score_list
    fab.sim_loading[[i]] <- loading_list
  }
  
  table_res <- list(score = fab.sim_score, loading = fab.sim_loading)
  return(table_res)
}

# Example:
biclusters <- fabia_function(datasets = datasets, num = 5, factor_vector = c(1))
load <- as.data.frame(biclusters$score[[1]])
plot(load$c.gene1...0..gene2...0.369619893176104..gene3...0..gene4...0..)
abs_load = abs(load$c.gene1...0..gene2...0.369619893176104..gene3...0..gene4...0..)
abs_load <- load %>% filter(load$c.gene1...0..gene2...0.369619893176104..gene3...0..gene4...0.. >0)
library(dplyr)

#############################################################################################
fabia_function <- function(datasets, num, factor_vector) {
  set.seed(123)
  library(fabia)
  
  # Saving bicluster information into a list of tables
  fab.sim_score <- list()
  fab.sim_loading <- list()
  
  for (i in 1:length(datasets)) {
    rawdata <- as.matrix(datasets[[i]])  # Convert input data to matrix
    sim.resFabia <- fabia(t(rawdata), p = num, alpha = 0,1, cyc = 1000, spl = 0.5, spz = 0.5, random = 1.0, center = 2, norm = 2, scale = 0.0, lap = 1.0, nL = 1, lL = 0, bL = 0)
    extractPlot(sim.resFabia, which = 5)  # Plot the results
    
    score_list <- list()
    loading_list <- list()
    
    for (k in 1:length(factor_vector)) {
      # Create a function to extract score
      fab_score <- function(sim_resFabia, factor_index) {
        score_FABIA <- sim_resFabia@L[, factor_index]
        return(score_FABIA)
      }
      
      # Create a function to extract loading
      fab_loading <- function(sim_resFabia, factor_index) {
        loading_FABIA <- sim_resFabia@Z[factor_index, ]
        return(loading_FABIA)
      }
      
      score_list[[k]] <- fab_score(sim.resFabia, factor_vector[k])
      loading_list[[k]] <- fab_loading(sim.resFabia, factor_vector[k])
    }
    
    fab.sim_score[[i]] <- score_list
    fab.sim_loading[[i]] <- loading_list
  }
  #### MOFA #################
  x_fabia <- sim.resFabia@X  # Convert input data to matrix
  ###########################
  table_res <- list(score = fab.sim_score, loading = fab.sim_loading)
  return(table_res)
}

# Example:
biclusters <- fabia_function(datasets = datasets, num = 5, factor_vector = c(1))

load <- as.data.frame(biclusters$score[[1]])
plot(load$c.gene1...0..gene2...0.369619893176104..gene3...0..gene4...0..)
abs_load = abs(load$c.gene1...0..gene2...0.369619893176104..gene3...0..gene4...0..)
abs_load <- load %>% filter(load$c.gene1...0..gene2...0.369619893176104..gene3...0..gene4...0.. >0)
library(dplyr)

################################################################################################
# TEST WITH PULLING DATA FOR MOFA
fabia_function <- function(datasets, num, factor_vector) {
  set.seed(123)
  library(fabia)
  
  # Saving bicluster information into a list of tables
  fab.sim_score <- list()
  fab.sim_loading <- list()
  
  for (i in 1:length(datasets)) {
    rawdata <- as.matrix(datasets[[i]])  # Convert input data to matrix
    sim.resFabia <- fabia(t(rawdata), p = num, alpha = 0,1, cyc = 1000, spl = 0.5, spz = 0.5, random = 1.0, center = 2, norm = 2, scale = 0.0, lap = 1.0, nL = 1, lL = 0, bL = 0)
    extractPlot(sim.resFabia, which = 5)  # Plot the results
    x_matrix <- matrix()
    score_list <- list()
    loading_list <- list()
    
    for (k in 1:length(factor_vector)) {
      # Create a function to extract score
      fab_score <- function(sim_resFabia, factor_index) {
        score_FABIA <- sim_resFabia@L[, factor_index]
        return(score_FABIA)
      }
      
      # Create a function to extract loading
      fab_loading <- function(sim_resFabia, factor_index) {
        loading_FABIA <- sim_resFabia@Z[factor_index, ]
        return(loading_FABIA)
      }
      
      score_list[[k]] <- fab_score(sim.resFabia, factor_vector[k])
      loading_list[[k]] <- fab_loading(sim.resFabia, factor_vector[k])
    }
    
    fab.sim_score[[i]] <- score_list
    fab.sim_loading[[i]] <- loading_list
    #### FABIA Normalized Data #################
    x_fabia[i] <- sim.resFabia@X   # Convert input data to matrix
    ###########################
  }

  table_res <- list(score = fab.sim_score, loading = fab.sim_loading, X = x_fabia)
  return(table_res)
}

# Example:
biclusters <- fabia_function(datasets = datasets, num = 5, factor_vector = c(1))

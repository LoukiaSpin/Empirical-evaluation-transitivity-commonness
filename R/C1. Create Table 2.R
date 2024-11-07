#*******************************************************************************
#*
#*
#*                        Creating Table 2 of Manuscript                                                                                                
#*              Descriptive statistics of database characteristics                                                                                           
#*                
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************



## Load libraries
library("tracenma")



## Load functions ----
source("./R/function.collection_function.R")



## Load tracenma dataset ----
# Obtain the PMID number of the datasets
pmid_index <- index$PMID

# Load all 217 datasets as data-frames
read_all_excels <- lapply(pmid_index, function(x) get.dataset(pmid = x)$Dataset)



## Dataset preparation ----
# Get 'dataset_new'
dataset_new0 <- get_dataset_new(read_all_excels)

# Remove datasets with less than four characteristics (after removing dose-related characteristics)
remove <- which(unname(unlist(lapply(dataset_new0, 
                                     function(x) dim(subset(x, select = -c(trial, treat1, treat2)))[2]))) < 4)
dataset_new <- dataset_new0[-remove] 



## Characteristics pertinent to the networks ----
# Number of studies
num_studies <- sapply(dataset_new, function(x) length(unique(x[, 1])))
summary(num_studies)

# Number of interventions
num_interv <- sapply(dataset_new, function(x) length(unique(unlist(x[, 2:3]))))
summary(num_interv)
which(num_interv == 3) # One network with three interventions (see note (a) in Table 2)

# Percentage of observed comparisons
num_obs_comp <- sapply(dataset_new, function(x) length(unique(paste(x[, 3], "vs", x[, 2]))))
perc_obs_comp <- round((num_obs_comp / 
                          sapply(num_interv, function(x) dim(combn(x, 2))[2])) * 100, 0)
summary(perc_obs_comp)
summary(perc_obs_comp[perc_obs_comp < 100]) # Excluding the 3 fully connected networks
which(perc_obs_comp == 100) # Three fully connected networks

# Percentage of single-study comparisons
num_single_comp <- sapply(dataset_new, 
                          function(x) length(which(table(paste(x[, 3], "vs", x[, 2])) == 1)))
perc_single_comp <- round((num_single_comp / num_obs_comp) * 100, 0)
summary(perc_single_comp[perc_single_comp > 0]) # Excluding datasets without single-study comparisons
which(perc_single_comp == 100) # One network with only single-study comparisons (Exclude from subsequent analyses)
length(dataset_new) - length(which(perc_single_comp == 0)) # Networks with at least one single-study comparisons



## Characteristics pertinent to the transitivity evaluation ----
# Number of extracted characteristics
num_chars <- sapply(dataset_new, function(x) dim(x[, -c(1:3)])[2])
summary(num_chars)

# Percentage of numeric characteristics
num_numer_chars <- sapply(dataset_new, function(x) length(which(lapply(x, typeof) == "double")))
perc_numer_chars <- round((num_numer_chars / num_chars) * 100, 0)
summary(perc_numer_chars)
length(which(perc_numer_chars == 100)) # 27 networks contained only numeric characteristics
length(which(perc_numer_chars == 0))   # 1 network did not contain any numeric characteristics

# Percentage of non-numeric characteristics
num_nonnumer_chars <- sapply(dataset_new, function(x) length(which(lapply(x, typeof) == "integer")))
perc_nonnumer_chars <- round((num_nonnumer_chars / num_chars) * 100, 0)
summary(perc_nonnumer_chars)
length(which(perc_nonnumer_chars == 100)) # 1 network contained only non-numeric characteristics
length(which(perc_nonnumer_chars == 0))   # 27 networks did not contain any non-numeric characteristics

# Percentage of total missing data
perc_miss_total <- 
  unname(unlist(lapply(dataset_new, 
                       function(x) round(length(which(is.na(x[, -c(1:3)]) == TRUE)) / prod(dim(x[, -c(1:3)])) * 100, 1))))
summary(perc_miss_total[perc_miss_total > 0]) # Excluding datasets without missing data
length(dataset_new) - length(which(perc_miss_total == 0)) # 177 networks with at least one missing case

# Percentage of missing data per characteristic
num_miss_char <- sapply(dataset_new, function(x) colSums(is.na(x[, -c(1:3)]))) 
perc_miss_char <- 
  unname(unlist(sapply(dataset_new, function(x) round((colSums(is.na(x[, -c(1:3)])) / dim(x)[1]) * 100, 1))))
summary(perc_miss_char[perc_miss_char > 0])

# Percentage of characteristics with at least one missing case
num_char_miss <- sapply(dataset_new, function(x) length(names(which(colSums(is.na(x[, -c(1:3)])) > 0))))
perc_char_miss <- round((as.numeric(unlist(num_char_miss)) / as.numeric(unlist(num_chars))) * 100, 1)
summary(perc_char_miss[perc_char_miss > 0])
length(which(perc_char_miss > 0))  # 177 networks with at least one missing case

# Percentage of dropped characteristics
#' Spot dropped characteristics per comparison and dataset
dropped_char_list <- 
  lapply(dataset_new, function(x) {
    lapply(split(x, f = as.character(paste(x$treat2, "vs", x$treat1))), function(y) {
      if (dim(y)[1] > 1) as.vector(which(colSums(is.na(y)) == nrow(y) | 
                                           colSums(is.na(y)) == nrow(y) - 1))
    }) 
  })

#' Vector of the number of unique dropped characteristics per dataset
dropped_char <- as.numeric(lapply(dropped_char_list, function(x) if (length(unique(unlist(x))) == 0) {
  0
} else {
  length(unique(unlist(x)))
}))

#' Get the descriptive statistics
perc_dropped_char <- round((dropped_char / num_chars) * 100, 0)
summary(dropped_char[dropped_char > 0])
length(dropped_char[dropped_char > 0]) # 124 networks with at least one dropped characteristic

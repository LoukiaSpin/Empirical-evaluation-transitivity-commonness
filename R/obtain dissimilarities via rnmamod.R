#*******************************************************************************
#*
#*
#*                 Database Preparation & Overall Dissimilarity                                  
#*            (within-comparison & between-comparisons dissimilarity)                                                
#*                
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************



## Load libraries
list.of.packages <- c("tracenma", "rnmamod")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load datasets ----
# Obtain the PMID number of the datasets (tracenma)
pmid_index <- index$PMID

# Load all 217 datasets as data-frames (tracenma)
read_all_excels <- lapply(pmid_index, function(x) get.dataset(pmid = x)$Dataset)



## Load functions ----
source("./R/function.collection_function.R")



## Dataset preparation ----
# Get 'dataset_new'
dataset_new <- get_dataset_new(read_all_excels)

# Check 'class' 
lapply(dataset_new, function(x) unlist(lapply(x, typeof)))



## Run the novel approach for each dataset ----
dissimilarities0 <- lapply(dataset_new,
                           function(x) comp_clustering(input = x, 
                                                       informative = TRUE,
                                                       threshold = 0.13)) # It does not affect the results



## Find and remove datasets with less than four characteristic (after removing dose-related characteristics)
# Find these datasets
remove <- which(unlist(lapply(dissimilarities0, function(x) dim(x$Types_used)[1])) < 4)

# Remove these datasets
dissimilarities <- dissimilarities0[-remove]



## Save as .RData
#save(dissimilarities, file = "./data/Overall Dissimilarities_Results.RData")

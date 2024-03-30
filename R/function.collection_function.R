#*******************************************************************************
#*
#*
#*                 Function to obtain 'dataset_new' for analysis                              
#*                   (Removing dose-related characteristics)                                       
#*
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************

get_dataset_new <- function (read_all_excels) {
  
  ## Dataset preparation
  # STEP 0: Remove columns relating to dose characteristics
  dataset_start <- lapply(read_all_excels, 
                          function(x) x[!names(x) %in% names(x[, startsWith(names(x), "treat.dos") | 
                                                                 startsWith(names(x), "treat.type") | 
                                                                 startsWith(names(x), "treat.interval") | 
                                                                 startsWith(names(x), "treat.route") | 
                                                                 startsWith(names(x), "treat.time") |
                                                                 startsWith(names(x), "treat.fre") |
                                                                 startsWith(names(x), "route1") |
                                                                 startsWith(names(x), "route2")])])

  # STEP 1: Turn into data.frame
  dataset_new00 <- lapply(dataset_start, as.data.frame)
  
  # STEP 2: Remove columns with treatment names
  dataset_new0 <- lapply(dataset_new00, function(x) x[, -c(4,5)])
  
  # STEP 3: Turn 'character' columns into 'integer'
  dataset_new <- lapply(dataset_new0, 
                        function(x) {x[sapply(x, class) == 'character'] <- lapply(x[sapply(x, class) == 'character'], as.factor); x})
  
  # STEP 4: Turn study and treatment columns from 'double' to 'character'
  dataset_new <- lapply(dataset_new, 
                        function(x) {x[1:3] <- lapply(x[1:3], as.character); x})
  
  return(dataset_new)
}



#*******************************************************************************
#*
#*
#*   Function with following actions: 
#*   (1) cleans datasets from characteristics with missing data that 
#*   are removed from 'comp_clustering' function. 
#*   (2) identify the datasets with less than four characteristics after
#*   dropping characteristics with too many missing data (see (1))
#*   (3) returns a 'clean' dataset after applying (1) and (2) to be used in
#*   R script 'E1. Create Figures 5.R' on statistical tests for 
#*   transitivity evaluation
#* 
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************

dataset_tests <- function (dataset) {
  
  
  ## Insert 'Comparison' in the dataset (control appears second in the compar.)
  dataset_new <- lapply(dataset, function(x) {x$Comparison <- as.character(paste0(x$treat2, "-", x$treat1)); x})
  
  
  ## Split 'dataset' by 'Comparison'
  split_dataset <- lapply(dataset_new, function(x) split(x, f = x$Comparison))
  
  
  ## Find the completely missing columns in all non-single-study comparisons
  col_all_miss <-
    lapply(1:length(split_dataset), 
           function(y) unique(unlist(lapply(split_dataset[[y]], function(x) if (dim(x)[1] > 1)
             as.vector(which(colSums(is.na(x)) == nrow(x) | colSums(is.na(x)) == nrow(x) - 1))))))
  
  
  ## Keep the names of the completely missing columns in all comparisons
  col_all_miss_names <-
    lapply(1:length(split_dataset), 
           function(y) unique(unlist(lapply(split_dataset[[y]],
                                            function(x) colnames(x)[col_all_miss[[y]]]))))
  
  
  ## Remove these columns also from the dataset for the moment
  dataset_new_final0 <- 
    lapply(1:length(col_all_miss), function(x) {if (length(col_all_miss[[x]]) > 0) {
      subset(dataset_new[[x]], select = -col_all_miss[[x]])
    } else {
      dataset_new[[x]]
    }})
  names(dataset_new_final0) <- names(dataset)
  
  
  ## Keep only datasets with at least four charactersitics after dropping those with too many missing data
  #' Number of characteristics per dataset (before dropping those with too many missing data)
  num_chars <- unname(unlist(lapply(dataset_new, function(x) dim(x[, -c(1:3)])[2] - 1)))
  
  #' Vector of the number of unique dropped characteristics per dataset
  dropped_char <- as.numeric(lapply(col_all_miss_names, function(x) if (length(unique(unlist(x))) == 0) {
    0
  } else {
    length(unique(unlist(x)))
  }))
  
  #' Get the dataset(s) with less than 4 characteristics
  exclude_datasets <- which(num_chars - dropped_char < 4) # 64 102 107 122 168
  
  # Get final database after removing datasets with less than four characteristics
  dataset_new_final <- dataset_new_final0[-exclude_datasets]
  
  return(list(dataset_new_final = dataset_new_final,
              exclude_datasets = exclude_datasets))
}



#*******************************************************************************
#*
#*
#*  Assign proper threshold to each dataset based on their design factors
#* 
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************

dataset_threshold <- function (dataset_new, index) {
  
  
  ## Load libraries ----
  library("dplyr"); library("tracenma")

  
  ## Dataset preparation ----
  # List of extracted networks
  list_extracted_networks <- index 
  colnames(list_extracted_networks)[7:8] <- c("outcome_type", "interv_comp_type")
  
  # Rename the 'interv_comp_type' levels
  list_extracted_networks$interv_comp_type <- 
    factor(plyr::revalue(list_extracted_networks$interv_comp_type,
                         c("non-pharmacological vs any" = "Non-pharma vs. Any",
                           "pharmacological vs pharmacological" = "Pharma vs. Pharma",
                           "pharmacological vs placebo" = "Pharma vs. Placebo")),
           levels = c("Pharma vs. Placebo", "Pharma vs. Pharma", "Non-pharma vs. Any"))
  
  # Rename the 'outcome_type' levels
  list_extracted_networks$outcome_type <- factor(list_extracted_networks$outcome_type,
                                                 levels = c("Objective", "Semi-objective", "Subjective"))
  
  # Define the design factors
  interv_comp_type <- unique(list_extracted_networks$interv_comp_type)
  outcome_type <- unique(list_extracted_networks$outcome_type)
  average_size <- c("small", "moderate", "large")
  
  # Define average size per network: Get the mean age per network
  mean_size_net0 <- suppressWarnings({unname(unlist(lapply(dataset_new, function(x) mean(x$sample.size, na.rm = TRUE))))})
  
  #' Define average size per network: For networks without information on the study sample, 
  #' replace NA with the median of 'mean_size_net0'
  mean_size_net <- ifelse(is.na(mean_size_net0), median(mean_size_net0, na.rm = TRUE), mean_size_net0) 
  
  # Define average size per network: Label the sample size: small, moderate and large
  size_net_label <- ifelse(mean_size_net < 50, "small", 
                           ifelse(mean_size_net > 200, "large", "moderate"))
  
  # Set the data-frame of thresholds
  threshold_set <- expand.grid(interv_comp_type, outcome_type, average_size)
  colnames(threshold_set) <- c("interv_comp_type", "outcome_type", "average_size")
  
  # Median as threshold
  threshold_set$thresh_value <- 
    c(0.25, 0.16, 0.24, 0.0007, 0.0004, 0.0007, 0.06, 0.04, 0.06, 
      0.25, 0.16, 0.23, 0.0007, 0.0004, 0.0007, 0.06, 0.04, 0.06, 
      0.28, 0.18, 0.26, 0.0008, 0.0005, 0.0007, 0.07, 0.04, 0.06)
  
  # 75% quartile as threshold
  threshold_set$thresh_value_75 <- 
    c(0.53, 0.32, 0.55, 0.01, 0.005, 0.01, 0.28, 0.16, 0.29,
      0.52, 0.32, 0.55, 0.01, 0.005, 0.01, 0.27, 0.16, 0.29,
      0.56, 0.35, 0.58, 0.01, 0.006, 0.01, 0.30, 0.18, 0.33)
  
  # Match network design factors with threshold and add 'threshold' column in 'list_extracted_networks'
  list_extracted_networks$threshold_50 <- inner_join(data.frame(list_extracted_networks[, 7:8], 
                                                                average_size = size_net_label), 
                                                     threshold_set[, -5])[, 4]
  list_extracted_networks$threshold_75 <- inner_join(data.frame(list_extracted_networks[, 7:8], 
                                                                average_size = size_net_label), 
                                                     threshold_set[, -4])[, 4]
  
  return(list_extracted_networks)
  
}



#*******************************************************************************
#*
#*
#*  Name each dataset using their ID, Year, PMID, and first Author
#*  (Applied in R script 'E1. Create Figures 5.R')
#* 
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************

name_each_dataset <- function (read_all_excels, index) {
  
  # Extract the year for each dataset
  year <- lapply(index$Year, function(x) paste("Year:", x))
  
  # Extract the PMID for each dataset
  pmid <- lapply(index$PMID, function(x) paste("PMID:", x))
  
  # Extract the first author for each dataset
  first_author <- lapply(index$First.Author, function(x) paste("Authors:", x, "et al."))
  
  # Add ID
  id <- paste0("ID: [", stringr::str_pad(1:length(year), 3, pad = "0"), "]")
  
  # Bring into a data-frame
  names_list_elements <- data.frame(id, unlist(year), unlist(pmid), unlist(first_author))
  colnames(names_list_elements) <- c("id", "year", "pmid", "first_author")
  
  return(names_list_elements)
}



#*******************************************************************************
#*
#*
#* Function to capture warning (Applied in R script 'E1. Create Figures 5.R')
#* Original source: https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
#* 
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************

myTryCatch <- function(expr) {
  warn <- err <- NA
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NA
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}

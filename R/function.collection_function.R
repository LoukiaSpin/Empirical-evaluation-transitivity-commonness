## Function to obtain 'dataset_new'
get_dataset_new <- function (read_all_excels) {
  
  ## Dataset preparation
  # STEP 00: Remove columns relating to dose characteristics
  dataset_start <- lapply(read_all_excels, 
                          function(x) x[!names(x) %in% names(x[, startsWith(names(x), "treat.dos") | 
                                                                 startsWith(names(x), "treat.type") | 
                                                                 startsWith(names(x), "treat.interval") | 
                                                                 startsWith(names(x), "treat.route") | 
                                                                 startsWith(names(x), "treat.time") |
                                                                 startsWith(names(x), "treat.fre")])])

  # STEP 0: Turn into data.frame
  dataset_new00 <- lapply(dataset_start, as.data.frame)
  
  # STEP 1: Remove columns with treatment names
  dataset_new0 <- lapply(dataset_new00, function(x) x[, -c(4,5)])
  
  # STEP 2: Turn 'character' columns into 'integer'
  dataset_new <- lapply(dataset_new0, 
                        function(x) {x[sapply(x, class) == 'character'] <- lapply(x[sapply(x, class) == 'character'], as.factor); x})
  
  # STEP 3: Turn study and treatment columns from 'double' to 'character'
  dataset_new <- lapply(dataset_new, 
                        function(x) {x[1:3] <- lapply(x[1:3], as.character); x})
  
  return(dataset_new)
}



#' Function to clean dataset from characteristics with missing data that 
#' are removed from 'comp_clustering' function. The dataset is used to apply
#' statistical tests for transitivity evaluation
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
  dataset_new_final <- 
    lapply(1:length(col_all_miss), function(x) {if (length(col_all_miss[[x]]) > 0) {
      subset(dataset_new[[x]], select = -col_all_miss[[x]])
    } else {
      dataset_new[[x]]
    }})
}


## Assign proper threshold to each dataset based on their design factors
dataset_threshold <- function (dataset_new) {
  
  
  ## Load libraries ----
  list.of.packages <- c("readxl", "dplyr")
  lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)

  
  ## Load datasets ----
  # TRACE-NMA dataset
  load("./data/TRACE-NMA Dataset.RData")
  
  # Extracted networks (one network per row)
  load("./data/index_reviews.RData")


  ## Dataset preparation ----
  # Get PMIDs from 'dataset_new'
  pmid_dataset_new <- unlist(lapply(1:length(dataset_new), 
                                    function(x) substr(names(dataset_new)[x], start = 30, stop = 37)))
  
  # Perform the sorting
  list_extracted_networks <- index_reviews[match(pmid_dataset_new, index_reviews$PMID), ]
  colnames(list_extracted_networks)[7:8] <- c("outcome_type", "interv_comp_type")
  list_extracted_networks$interv_comp_type <- 
    factor(plyr::revalue(list_extracted_networks$interv_comp_type,
                         c("non-pharmacological vs any" = "Non-pharma vs. Any",
                           "pharmacological vs pharmacological" = "Pharma vs. Pharma",
                           "pharmacological vs placebo" = "Pharma vs. Placebo")),
           levels = c("Pharma vs. Placebo", "Pharma vs. Pharma", "Non-pharma vs. Any"))
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
  list_extracted_networks$threshold_50 <- inner_join(data.frame(list_extracted_networks[, 2:3], 
                                                                average_size = size_net_label), 
                                                     threshold_set[, -5])[, 4]
  list_extracted_networks$threshold_75 <- inner_join(data.frame(list_extracted_networks[, 2:3], 
                                                                average_size = size_net_label), 
                                                     threshold_set[, -4])[, 4]
  
  return(list_extracted_networks)
  
}

## Function to capture warning
#' Original source: https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
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
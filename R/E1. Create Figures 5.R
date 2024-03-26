#*******************************************************************************
#*
#*
#*                       Creating Figure 5 of Manuscript                                                                                                                                                                                                                                    
#*              (Statistical testing for transitivity assessment)                                                                                                                                                                                                                                       
#*                
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("readxl", "plyr", "dplyr", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load functions ----
source("./R/function.collection_function.R")



## Load datasets ----
# TRACE-NMA dataset
load("./data/TRACE-NMA Dataset.RData")

# Overall dissimilarity results 
load("./data/Overall Dissimilarities_Results.RData")



## Dataset preparation ----
# Get 'dataset_new'
dataset_new00 <- get_dataset_new(read_all_excels)

# Remove datasets with less than four characteristics (after removing dose-related characteristics)
remove <- which(unname(unlist(lapply(dataset_new00, function(x) dim(x)[2] - 3))) < 4)
dataset_new0 <- dataset_new00[-remove] 

# Remove also datasets with less than four characteristics after removing dropped characteristics
#' (based on the 'comp_clustering' function of rnmamod)
excluded_datasets <- dataset_tests(dataset_new0)$exclude_datasets
dataset_new <- dataset_new0[-excluded_datasets] 

# Reduce the list of 'comp_clustering' results to those from the analysed 209 datasets
dissimilarities <- dissimilarities[-excluded_datasets]

#' Remove characteristics with missing data that 'comp_clustering' function
#' would also remove so that I use the same data as for the novel approach
dataset_test <- dataset_tests(dataset_new0)$dataset_new_final
names(dataset_test) <- names(dataset_new)

#' Next, remove datasets with at least one single-study comparison as 'oneway.test' cannot run 
#' (Error: 'not enough observations') and 'chisq.test' warns 'Chi-squared approximation may be incorrect' 
dataset_tests_final0 <- Filter(function(x) {length(which(table(x[, "Comparison"]) == 1)) == 0}, dataset_test)

# Number of datasets removed from previous step (they have at least one single-study comparison)
length(dataset_test) - length(dataset_tests_final0) # 178 datasets!!

#' Lastly, remove categorical characteristics with only one value for all trials, as 'chisq.test' is undefined (X-squared = NaN)
dataset_tests_final <- 
  lapply(dataset_tests_final0, function(x) {x <- x[,!(names(x) %in% names(which(apply(x[-c(1:3)], 2, function(y) length(unique(y))) == 1)))]; x})

# Vector of removed characteristics per analysed dataset
chars_removed_per_dataset0 <- 
  unname(unlist(lapply(dataset_tests_final0, function(x) dim(x)[2])) - 
           unlist(lapply(dataset_tests_final, function(x) dim(x)[2])))

# SOS: Exclude the 16th dataset that had one numeric variable with same values across all studies
chars_removed_per_dataset <- chars_removed_per_dataset0[-16]

# Descriptive statistics on removed categorical characteristics with same valua in all trials
sum(chars_removed_per_dataset)
summary(chars_removed_per_dataset[chars_removed_per_dataset > 0]) # 1 to 8 characteristics were removed ...
length(chars_removed_per_dataset[chars_removed_per_dataset > 0])  # ... from 12 datasets



## Perform one-way ANOVA and chi-squared test, where appropriate ----
# Get p-values - Warning: ANOVA yields p-value = NA when at least one comparison has the same values
test_results <-
  lapply(dataset_tests_final, 
         function(y) lapply(4:(dim(y)[2] - 1), 
                            function(x) if (typeof(y[, x]) == "double") 
                              oneway.test(y[, x] ~ y[, "Comparison"], var.equal = FALSE)$p.value else 
                                chisq.test(y[, x], y[, "Comparison"])$p.value))

# Define the conducted test
test_type <-
  lapply(dataset_tests_final, 
         function(y) lapply(4:(dim(y)[2] - 1), 
                            function(x) if (typeof(y[, x]) == "double") "One-way ANOVA" else "Chi-squared test"))

# Combine 'test_results' and 'test_type' by analysed dataset
test_type_results <- Map(cbind, test_results, test_type)

# Capture warnings from each test
capture_warning <- 
  lapply(dataset_tests_final, 
         function(y) lapply(4:(dim(y)[2] - 1), 
                            function(x) if (typeof(y[, x]) == "double") 
                              myTryCatch(oneway.test(y[, x] ~ y[, "Comparison"], var.equal = FALSE))$warning else 
                                myTryCatch(chisq.test(y[, x], y[, "Comparison"]))$warning))



## Datasets with warnings about chi-squared approximation being questionable 
# Vector with number of such warnings per analysed dataset
num_warn_chisquared_test <- unname(unlist(lapply(capture_warning, function(x) length(which(!is.na(x))))))

# Number of datasets with such warnings
length(num_warn_chisquared_test[num_warn_chisquared_test > 0])



## % Characteristics with NaN p-value (one-way ANOVA) per network ----
# Number of characteristics per network (-1 for to the 'Comparison' column at the end)
num_chars_initial <- unname(unlist(lapply(dataset_tests_final, function(x) dim(x[, -c(1:3)])[2] - 1)))

# Number of undefined F-tests (one-way ANOVA) per network
num_undefined_ftests_net <- unname(unlist(lapply(test_type_results, function(x) length(which(is.na(x[, 1]) == TRUE)))))

# Total number of such characteristics
sum(num_undefined_ftests_net)

# Descriptive statistics
summary(num_undefined_ftests_net[num_undefined_ftests_net > 0])

# Number of networks with at least one undefined F-test
length(num_undefined_ftests_net[num_undefined_ftests_net > 0])



## Investigate the number of analysed characteristics in the 31 'eligible' datasets ----
# % analysed characteristics per network 
perc_chars_analysed <- (num_chars_initial - num_undefined_ftests_net) / num_chars_initial

# Descriptives
round(summary(perc_chars_analysed[perc_chars_analysed < 1]) * 100, 1)

# Number of networks with less characteristics than originally
length(perc_chars_analysed[perc_chars_analysed < 1]) # 21!


## % of statistically significant characteristics per analysed network ----
# % of statistically significant tests per network 
stat_sign_char_net <- unname(unlist(lapply(test_type_results, function(x) length(which(x[, 1] < 0.05)) / dim(x)[1])))

# Summarise across networks
summary(stat_sign_char_net[stat_sign_char_net > 0]) * 100 # range: 4.17% to 50%

# Number of networks with conclusive results
#' At least one characteristic was associated with p-value < 5%
num_net_conclusive <- length(stat_sign_char_net[stat_sign_char_net > 0])   # 19 networks (out of 31) - The remaining 12 had *only* inconclusive results!



## Restrict both dataset to common PMIDs ----
# Get PMIDs from 'dataset_new'
pmid_dataset <- unlist(lapply(1:length(dataset_new), 
                              function(x) substr(names(dataset_new)[x], start = 30, stop = 37)))

# Get PMIDs from 'dataset_tests_final'
pmid_dataset_tests <- unlist(lapply(1:length(dataset_tests_final), 
                                    function(x) substr(names(dataset_tests_final)[x], start = 30, stop = 37)))

# Find the position of 'pmid_dataset_tests' in 'pmid_dataset'
pmid_position <- match(pmid_dataset_tests, pmid_dataset)

# Restrict 'dissimilarity' results to 'pmid_dataset_tests' (Keep 'Between-comparisons' only)
diss_dataset_restr <- lapply(pmid_position, function(x) subset(dissimilarities[[x]]$Total_dissimilarity, index_type == "Between-comparison"))

# Restrict datasets with their thresholds to 'pmid_dataset_tests'
dataset_threshold_restr <- subset(dataset_threshold(dataset_new), is.element(PMID, pmid_dataset_tests))

# Keep 'between-comparison dissimilarities' per analysed network
diss_dataset_restr_fin <- lapply(diss_dataset_restr, function(x) x[, 2])

# Define the extent of between-comparison dissimilarity per network (Median as threshold)
extent_diss_restr_50 <- 
  lapply(1:length(diss_dataset_restr_fin), 
         function(x) ifelse(diss_dataset_restr_fin[[x]] < dataset_threshold_restr$threshold_50[x], "low", "likely concerning"))

# Define the extent of between-comparison dissimilarity per network (3rd quartile as threshold)
extent_diss_restr_75 <- 
  lapply(1:length(diss_dataset_restr_fin), 
         function(x) ifelse(diss_dataset_restr_fin[[x]] < dataset_threshold_restr$threshold_75[x], "low", "likely concerning"))

# Include the p-valuzes from statistical testing, repeating them by the size of elements of 'diss_dataset_restr_fin'
sign_concl_per_net <- lapply(1:length(stat_sign_char_net), 
                             function(x) rep(stat_sign_char_net[x], length(diss_dataset_restr_fin[[x]])))

# Bring together
trans_concl_thresh <- Map(cbind, sign_concl_per_net, diss_dataset_restr_fin, extent_diss_restr_50, extent_diss_restr_75)
lapply(trans_concl_thresh, function(x) {colnames(x) <- c("Prec_sign_tests", "D_B", "median", "third quartile"); x})

# Number of network with transitivity conclusions based on 'second quartile'
sign_low_sec <- 
  length(Filter(function(x) all(as.numeric(x[, 1]) > 0 & x[, 3] == "low"), trans_concl_thresh)) 
nonsign_low_sec <- 
  length(Filter(function(x) all(as.numeric(x[, 1]) == 0 & x[, 3] == "low"), trans_concl_thresh))
sign_high_sec <- 
  length(Filter(function(x) any(as.numeric(x[, 1]) > 0 & x[, 3] == "likely concerning"), trans_concl_thresh))
nonsign_high_sec <- 
  length(Filter(function(x) any(as.numeric(x[, 1]) == 0 & x[, 3] == "likely concerning"), trans_concl_thresh))

# Number of network with transitivity conclusions based on 'third quartile'
sign_low_third <-
  length(Filter(function(x) all(as.numeric(x[, 1]) > 0 & x[, 4] == "low"), trans_concl_thresh))
nonsign_low_third <- 
  length(Filter(function(x) all(as.numeric(x[, 1]) == 0 & x[, 4] == "low"), trans_concl_thresh))
sign_high_third <- 
  length(Filter(function(x) any(as.numeric(x[, 1]) > 0 & x[, 4] == "likely concerning"), trans_concl_thresh))
nonsign_high_third <- 
  length(Filter(function(x) any(as.numeric(x[, 1]) == 0 & x[, 4] == "likely concerning"), trans_concl_thresh))



## Stacked barplots on transitivity conclusion from testing versus our approach (Figure 5) ----
# Prepare datasets
data_trans_concl <- 
  data.frame(value = c(sign_low_sec, nonsign_low_sec, sign_high_sec, nonsign_high_sec, 
                       sign_low_third, nonsign_low_third, sign_high_third, nonsign_high_third),
             pvalue = rep(c("Conclusive", "Inconclusive"), 4),
             dissimilarity = rep(c("Transitivity is likely", "Transitivity is questionable"), each = 2),
             threshold = rep(c("Second quartile", "Third quartile"), each = 4))

# Calculate % conditionally on p-value decision
data_trans_concl_new <- data_trans_concl %>%
  group_by(pvalue, threshold) %>%
  mutate(perc = value / sum(value))

# Create stacked barplot (second quartile for threshold)
barplot_second <-
  ggplot(subset(data_trans_concl_new, threshold == "Second quartile"),
       aes(x = pvalue,
           y = perc,
           fill = dissimilarity)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = pvalue,
                y = perc,
                group = dissimilarity,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", value,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.5,
            position = "stack",
            colour = "white") +
  labs(x = "Statistical test of transitivity was",
       y = "Percentage datasets (%)",
       fill = "Novel approach decision") +
  ggtitle("Dissimilarity threshold: second quartile") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))

# Create stacked barplot (third quartile for threshold)
barplot_third <-
  ggplot(subset(data_trans_concl_new, threshold == "Third quartile"),
       aes(x = pvalue,
           y = perc,
           fill = dissimilarity)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = pvalue,
                y = perc,
                group = dissimilarity,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", value,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.5,
            position = "stack",
            colour = "white") +
  labs(x = "Statistical testing of transitivity was",
       y = " ",
       fill = "Novel approach decision") +
  ggtitle("Dissimilarity threshold: third quartile") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))

# Bring together and save Figure 5
tiff("./Figures/Figure 5.tiff", 
     height = 18, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(barplot_second, barplot_third, 
          labels = c("a)", "b)"),
          common.legend = TRUE,
          legend = "bottom")
dev.off()

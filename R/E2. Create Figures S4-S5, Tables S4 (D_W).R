#*******************************************************************************
#*
#*
#*               Creating Supplementary Figures S4-S5 & Table S4                                                                      
#*                      (Within-comparison dissimilarities)                                                                                                                                     
#*                
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("tracenma", "dplyr", "ggplot2", "ggdist")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load functions ----
source("./R/function.collection_function.R")



## Load datasets ----
# Obtain the PMID number of the datasets (tracenma)
pmid_index <- index$PMID

# Load all 217 datasets as data-frames (tracenma)
read_all_excels <- lapply(pmid_index, function(x) get.dataset(pmid = x)$Dataset)

# Overall dissimilarity results 
load("./data/Overall Dissimilarities_Results.RData")



## Dataset preparation ----
# Get 'dataset_new'
dataset_new00 <- get_dataset_new(read_all_excels)

# Remove datasets with less than four characteristics (after removing dose-related characteristics)
remove <- which(unname(unlist(lapply(dataset_new00, 
                                     function(x) dim(subset(x, select = -c(trial, treat1, treat2)))[2]))) < 4)
dataset_new0 <- dataset_new00[-remove] 

# Remove indices referring to datasets with less than four characteristics (after removing dose-related characteristics)
index_new0 <- index[-remove, ]

# Remove also datasets with less than four characteristics after removing dropped characteristics
#' (based on the 'comp_clustering' function of rnmamod)
excluded_datasets <- dataset_tests(dataset_new0)$exclude_datasets
dataset_new <- dataset_new0[-excluded_datasets]
index_new <- index_new0[-excluded_datasets, ]

# Include proper threshold to each dataset based on their design factors
database_thresh <- dataset_threshold(dataset_new, index_new)[, c("PMID", "outcome_type", "interv_comp_type", "threshold_50", "threshold_75")]

# Reduce the list of 'comp_clustering' results to those from the analysed 209 datasets
dissimilarities <- dissimilarities[-excluded_datasets]



## Prepare dataset by outcome and intervention-comparator type ----
# Insert 'Comparison' in the dataset (control appears second in the compar.)
insert_comp <- lapply(dataset_new, function(x) {x$Comparison <- as.character(paste0(x$treat2, "-", x$treat1)); x})

# Number of total comparisons per network
total_study_comp <- unname(unlist(lapply(insert_comp, function(x) length(table(x$Comparison)))))

# Single-study comparisons
single_study_comp <- lapply(insert_comp, function(x) names(which(table(x$Comparison) == 1)))

# Find the single-study comparisons and set diagonal with 'NA'
comp_diss_mat <- lapply(1:length(dissimilarities), function(x) {if (length(unlist(single_study_comp[x])) > 0) diag(dissimilarities[[x]]$Comparisons_diss_table)[unname(unlist(single_study_comp[x]))] <- NA else dissimilarities[[x]]$Comparisons_diss_table; dissimilarities[[x]]$Comparisons_diss_table})

# Number of non-single-study comparisons per network
num_comp_net <- unlist(lapply(comp_diss_mat, function(x) length(na.omit(diag(x)))))

# Total number of non-single-study comparisons
total_comp <- sum(num_comp_net)

# Within-comparison dissimilarities (only non-single-study comparisons)
within_data <- unname(unlist(lapply(comp_diss_mat, function(x) na.omit(diag(x)))))

# Include the network ID 
network_id <- rep(1:length(dataset_new), num_comp_net)

# Repeat the outcome type based on 'num_comp_net'
outcome <- rep(database_thresh$outcome_type, num_comp_net)

# Repeat the intervention-comparator type based on 'num_comp_net'
interv_comp <- rep(database_thresh$interv_comp_type, num_comp_net)

# Number of networks per outcome and intervention-comparator type
number_networks <- database_thresh %>% 
  select(outcome_type, interv_comp_type) %>% 
  group_by(outcome_type, interv_comp_type) %>% 
  dplyr::summarize(value = n()) 
colnames(number_networks)[1:2] <- c("outcome", "interv_comp")



## Within-comparison dissimilarity distribution by outcome and intervention-comparator type (Figure S4) ----
# Prepare dataset for ggplot2
data_plot <- data.frame(within_data, outcome, interv_comp, network_id)
data_plot$interv_comp <- factor(data_plot$interv_comp,
                                levels = c("Pharma vs. Placebo", "Pharma vs. Pharma", "Non-pharma vs. Any"))

# Violin plot of within-comparison dissimilarity
tiff("./Figures/Figure S4.tiff", 
     height = 20, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(data_plot,
       aes(x = " ", 
           y = round(within_data, 2))) + 
  stat_halfeye(fill = scales::hue_pal()(5)[-c(1:3, 5)],
               .width = c(0.50, 0.95)) + 
  stat_summary(aes(label = sprintf("%.2f", after_stat(y))),
               fun = "median", 
               colour = "black", 
               fontface = "bold",
               size = 5,
               geom = "text", 
               position = position_nudge(x = -0.10)) +
  facet_grid(interv_comp ~ outcome) +
  geom_text(data = number_networks,
            aes(x = " ",
                y = 0,
                label = paste0("(n =", value, ")")),
            size = 4.3,
            col = "grey40", 
            hjust = 0.5, 
            vjust = 1,
            check_overlap = TRUE) +
  expand_limits(y = -0.05) +
  labs(y = "Within-comparison dissimilarities",
       x = " ") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        axis.ticks = element_blank())
dev.off()



## Within-comparison dissimilarity distribution *across* the networks ----
# Summary of within-comparison dissimilarity across networks  (manuscript)
summary(within_data)



## Within-comparison dissimilarity distribution outcome and intervention-comparator (Table S4) ----
# Summary of within-comparison dissimilarity
data_plot %>%
  group_by(outcome, interv_comp, .drop = FALSE) %>% 
  summarise(median = round(median(within_data), 2),
            q25 = round(quantile(within_data, 0.25), 2),
            q75 = round(quantile(within_data, 0.75), 2),
            min = round(min(within_data), 2),
            max = round(max(within_data), 2))

# % of non-single-study comparisons by outcome and intervention-comparator group (Table S4)
data.frame(perc_ns_comp = (num_comp_net / total_study_comp) * 100, database_thresh) %>%
  group_by(outcome_type, interv_comp_type, .drop = FALSE) %>% 
  summarise(min = round(min(perc_ns_comp), 2),
            max = round(max(perc_ns_comp), 2))



## Prepare dataset for grouped barplot per outcome and intervention-comparator type (Figure S5) ----
# Repeat the 'threshold' based on 'num_comp_net'
threshold_median <- rep(database_thresh$threshold_50, num_comp_net)
threshold_75qrt <- rep(database_thresh$threshold_75, num_comp_net)

# 50% threshold: % of comparisons with 'low' and 'probably concerning' D_W by outcome and intervention-comparator group
thres_within_50 <- 
  data.frame(outcome, interv_comp, within_data, threshold_median) %>%
  mutate(decision = factor(if_else(within_data < threshold_median, "low", "likely concerning"), 
                           levels = c("low", "likely concerning"))) %>%
  group_by(outcome, interv_comp) %>%
  dplyr::count(outcome, interv_comp, decision, .drop = FALSE) %>%
  mutate(freq = (n / sum(n)) * 100)

# 75% threshold: % of comparisons with 'low' and 'probably concerning' D_W by outcome and intervention-comparator group
thres_within_75 <- 
  data.frame(outcome, interv_comp, within_data, threshold_75qrt) %>%
  mutate(decision = factor(if_else(within_data < threshold_75qrt, "low", "likely concerning"), 
                           levels = c("low", "likely concerning"))) %>%
  group_by(outcome, interv_comp) %>%
  dplyr::count(outcome, interv_comp, decision, .drop = FALSE) %>%
  mutate(freq = (n / sum(n)) * 100)

# Merge '50% threshold' and '75% threshold'
thres_within_plot <- data.frame(rbind(thres_within_50, thres_within_75),
                                rep(c("Second quartile", "Third quartile"), each = dim(thres_within_50)[1]))
colnames(thres_within_plot)[6] <- "threshold"

# Create grouped barplot 
tiff("./Figures/Figure S5.tiff", 
     height = 25, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(thres_within_plot, 
       aes(x = threshold,
           y = freq, 
           fill = decision)) + 
  geom_bar(position = "dodge", 
           stat = "identity",
           alpha = 0.8) +
  geom_text(aes(label = paste0(round(freq, 1), "%", " ",  "(", n, ")")),
            position = position_dodge(width = .9),
            vjust = -0.2,
            size = 4.0) +
  facet_grid(interv_comp ~ outcome) +
  scale_fill_manual(values = c("#009E73", "#D55E00"),
                    labels = c("low" = "Low",
                               "likely concerning" = "Likely concerning")) +
  labs(x = "Threshold",
       y = "Percentage of comparisons (%)",
       fill = "Within-comparison dissimilarity") +
  scale_y_continuous(limits = c(0, 100), expand = c(0.1, 0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.margin = margin(0, 0, 0, 0),
        strip.text = element_text(size = 14, face = "bold"))
dev.off()

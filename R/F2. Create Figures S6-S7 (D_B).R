#*******************************************************************************
#*
#*
#*                     Creating Supplementary Figures S6-S7                                                                           
#*                     (Between-comparison dissimilarities)                                                                                                                                                         
#*                
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("dplyr", "ggplot2", "gghalves", "ggdist")
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

# Include proper threshold to each dataset based on their design factors
database_thresh <- dataset_threshold(dataset_new)[, c(2, 7:8, 11:12)]

# Reduce the list of 'comp_clustering' results to those from the analysed 209 datasets
dissimilarities <- dissimilarities[-excluded_datasets]



## Prepare dataset by outcome and intervention-comparator type ----
# Number of comparisons per network
num_comp <- lapply(dissimilarities, function(x) dim(x$Comparisons_diss_table)[1])

# Number of pairs of comparisons per network
num_comp_net <- unlist(lapply(num_comp, function(x) dim(combn(x, 2))[2]))

# Total number of pairs of comparisons
total_comp <- sum(num_comp_net)

# Between-comparison dissimilarities
between_data <- unname(unlist(lapply(dissimilarities, function(x) x$Comparisons_diss_table[lower.tri(x$Comparisons_diss_table)])))

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
  summarise(value = n()) 
colnames(number_networks)[1:2] <- c("outcome", "interv_comp")



## Between-comparison dissimilarity distribution by outcome and intervention-comparator type (Figure S6) ----
# Prepare dataset for ggplot2
data_plot <- data.frame(between_data, outcome, interv_comp, network_id)
data_plot$interv_comp <- factor(data_plot$interv_comp,
                                levels = c("Pharma vs. Placebo", "Pharma vs. Pharma", "Non-pharma vs. Any"))

# Violin plot of between-comparison dissimilarity
tiff("./Figures/Figure S6.tiff", 
     height = 20, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(data_plot,
       aes(x = " ", 
           y = round(between_data, 2))) + 
  stat_halfeye(fill = scales::hue_pal()(5)[-c(1:4)],
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
  labs(y = "Between-comparison dissimilarities",
       x = " ") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        axis.ticks = element_blank())
dev.off()



## Commonness of comparison transitivity per outcome and intervention-comparator type (Figure S7) ----
# Repeat the 'threshold' based on 'num_comp_net'
threshold_median <- rep(database_thresh$threshold_50, num_comp_net)
threshold_75qrt <- rep(database_thresh$threshold_75, num_comp_net)

# 50% threshold: % of comparisons with 'low' and 'probably concerning' D_B by outcome and intervention-comparator
thres_between_50 <- 
  data.frame(outcome, interv_comp, between_data, threshold_median) %>%
  mutate(decision = factor(if_else(between_data < threshold_median, "low", "probably concerning"), 
                           levels = c("low", "probably concerning"))) %>%
  group_by(outcome, interv_comp) %>%
  count(outcome, interv_comp, decision, .drop = FALSE) %>%
  mutate(freq = (n / sum(n)) * 100)

# 75% threshold: % of comparisons with 'low' and 'probably concerning' D_B by outcome and intervention-comparator
thres_between_75 <- 
  data.frame(outcome, interv_comp, between_data, threshold_75qrt) %>%
  mutate(decision = factor(if_else(between_data < threshold_75qrt, "low", "probably concerning"), 
                           levels = c("low", "probably concerning"))) %>%
  group_by(outcome, interv_comp) %>%
  count(outcome, interv_comp, decision, .drop = FALSE) %>%
  mutate(freq = (n / sum(n)) * 100)

# Merge '50% threshold' and '75% threshold'
thres_between_plot <- data.frame(rbind(thres_between_50, thres_between_75),
                                 rep(c("Second quartile", "Third quartile"), each = dim(thres_between_75)[1]))
colnames(thres_between_plot)[6] <- "threshold"

# Create grouped barplot 
tiff("./Figures/Figure S7.tiff", 
     height = 24, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(thres_between_plot, 
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
                               "probably concerning" = "Likely concerning")) +
  labs(x = "Threshold",
       y = "Percentage of pairs of comparisons (%)",
       fill = "Between-comparison dissimilarity") +
  ylim(c(0, 100)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.margin = margin(0, 0, 0, 0),
        strip.text = element_text(size = 14, face = "bold"))
dev.off()


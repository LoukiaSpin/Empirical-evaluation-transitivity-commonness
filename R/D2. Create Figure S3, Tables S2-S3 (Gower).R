#*******************************************************************************
#*
#*
#*               Creating Supplementary Figure S3 & Tables S2-S3                                       
#*                      (Gower's dissimilarity coefficient)                                                                                                               
#*                
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("dplyr", "ggplot2", "gghalves", "ggdist")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load functions ----
source("R/function.collection_function.R")



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
# Get GD distribution: Remove diagonals that are zero
gower_res0 <- lapply(dissimilarities, function(x) {diag(x$Trials_diss_table) <- NA; x$Trials_diss_table})

# Get GD distribution: Turn into vector and remove 'NA'
gower_res <- unname(unlist(lapply(gower_res0, function(x) na.omit(as.vector(x)))))

# Possible pairs of trials per network
poss_trial_pairs <- unname(unlist(lapply(dataset_new, function(x) dim(combn(dim(x)[1], 2))[2])))

# Include the network ID as many times as the size of the corresponding GD matrix
network_id <- rep(1:length(dataset_new), poss_trial_pairs)

# Repeat the outcome type based on 'poss_trial_pairs'
outcome_gower <- rep(database_thresh$outcome_type, poss_trial_pairs)

# Repeat the intervention-comparator type based on 'poss_trial_pairs'
interv_gower <- rep(database_thresh$interv_comp_type, poss_trial_pairs)

# Number of networks per outcome and intervention-comparator type 
number_networks <- database_thresh %>% 
  select(outcome_type, interv_comp_type) %>% 
  group_by(outcome_type, interv_comp_type) %>% 
  dplyr::summarise(value = n()) 
colnames(number_networks)[1:2] <- c("outcome_gower", "interv_gower")



## Gower's distribution by outcome and intervention-comparator type (Figure S3) ----
# Prepare dataset for ggplot2
data_gower_plot <- data.frame(gower_res, outcome_gower, interv_gower, network_id)
data_gower_plot$interv_gower <- factor(data_gower_plot$interv_gower,
                                       levels = c("Pharma vs. Placebo", "Pharma vs. Pharma", "Non-pharma vs. Any"))

# Violin plot 
tiff("./Figures/Figure S3.tiff", 
     height = 20, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(data_gower_plot,
       aes(x = " ", 
           y = round(gower_res, 2))) + 
  stat_halfeye(fill = scales::hue_pal()(5)[-c(1, 3:5)],
               .width = c(0.50, 0.95)) + 
  stat_summary(aes(label = sprintf("%.2f", after_stat(y))),
               fun = "median", 
               colour = "black", 
               fontface = "bold",
               size = 5,
               geom = "text", 
               position = position_nudge(x = -0.10)) +
  facet_grid(interv_gower ~ outcome_gower) +
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
  labs(y = "Gower's dissimilarity coefficient",
       x = " ") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        axis.ticks = element_blank())
dev.off()



## Gower's distribution *across* the datasets (manuscript) ----
# Summary of GD values
summary(gower_res)



## Gower's distribution by outcome and intervention-comparator (Table S2) ----
# Summary of GD values
data_gower_plot %>%
  group_by(outcome_gower, interv_gower, .drop = FALSE) %>% 
  summarise(median = round(median(gower_res), 2),
            q25 = round(quantile(gower_res, 0.25), 2),
            q75 = round(quantile(gower_res, 0.75), 2),
            min = round(min(gower_res), 2),
            max = round(max(gower_res), 2))



## Percentage of zero Gower *across* the networks  ----
# Number of zero Gower for each network (conditioning on networks with zero Gower)
count_zero_GD_net <- subset(data_gower_plot, gower_res == 0) %>% 
  group_by(network_id) %>% 
  summarise(n = n())  

# Number of Gower values for each network 
total_GD_net <- data_gower_plot %>% 
  group_by(network_id) %>% 
  summarise(n = n())  

# Distribution of % of zero Gower across the networks (manuscript)
left_join(count_zero_GD_net, total_GD_net, by = "network_id") %>%
  mutate(prop = (n.x / n.y) * 100) %>% 
  summarise(median = round(median(prop), 2),
            q25 = round(quantile(prop, 0.25), 2),
            q75 = round(quantile(prop, 0.75), 2),
            min = round(quantile(prop, 0.0), 2),
            max = round(quantile(prop, 1.0), 2))

# Number of networks with at least one zero Gower (manuscript)
networks_zero_GD_total <- 
  count_zero_GD_net %>%
  summarise(n = sum(n > 1),
            prop = (n / length(dataset_new) * 100)) # n=75 (35%)



## Percentage of zero Gower by outcome and intervention-comparator (Table S3) ----
# Number of zero GD values per network, outcome and intervention-comparator 
count_zero_GD <- subset(data_gower_plot, gower_res == 0) %>% 
  group_by(network_id, outcome_gower, interv_gower, .drop = FALSE) %>% 
  summarise(n = n()) 

# Number of Gower values by network, outcome and intervention-comparator group
total_GD <- data_gower_plot %>% 
  group_by(network_id, outcome_gower, interv_gower) %>% 
  summarise(n = n())

# Distribution of % of zero Gower by outcome and intervention-comparator (Table S3)
prop_zero_GD <-
  left_join(count_zero_GD, total_GD, by = c("network_id", "outcome_gower", "interv_gower")) %>%
  mutate(prop = (n.x / n.y) * 100) %>%
  na.omit() %>%
  group_by(outcome_gower, interv_gower) %>% 
  summarise(median = round(median(prop), 2),
            q25 = round(quantile(prop, 0.25), 2),
            q75 = round(quantile(prop, 0.75), 2),
            min = round(quantile(prop, 0.0), 2),
            max = round(quantile(prop, 1.0), 2)) 

# Number of networks with at least one zero GD 
networks_zero_GD <- count_zero_GD %>%
  group_by(outcome_gower, interv_gower) %>% 
  summarise(n = sum(n > 0))

# % of networks with at least one zero GD per outcome and intervention-comparator (Table S3)
left_join(networks_zero_GD, number_networks, by = c("outcome_gower", "interv_gower")) %>%
  mutate(prop = (n / value) * 100)

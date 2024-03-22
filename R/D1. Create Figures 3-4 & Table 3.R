#*******************************************************************************
#*
#*
#*                 Creating Figures 3-4 & Table 3 of Manuscript                                                                                                                                                               
#*        (Between-comparison dissimilarities & Transitivity commonness)                                                                                                                                                                   
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
dataset_new0 <- get_dataset_new(read_all_excels)

# Remove datasets with less than four characteristics (after removing dose-related characteristics)
remove <- which(unname(unlist(lapply(dataset_new0, function(x) dim(x)[2] - 3))) < 4)
dataset_new <- dataset_new0[-remove] 

# Include proper threshold to each dataset based on their design factors
database_thresh <- dataset_threshold(dataset_new)[, c(2, 7:8, 11:12)]



## Prepare dataset by outcome and intervention-comparator type ----
# Insert 'Comparison' in the dataset (control appears second in the compar.)
insert_comp <- lapply(dataset_new, function(x) {x$Comparison <- as.character(paste0(x$treat2, "-", x$treat1)); x})

# Single-study comparisons
single_study_comp <- lapply(insert_comp, function(x) names(which(table(x$Comparison) == 1)))

# Find the single-study comparisons and set diagonal with 'NA'
comp_diss_mat <- lapply(1:length(dissimilarities), function(x) {if (length(unlist(single_study_comp[x])) > 0) diag(dissimilarities[[x]]$Comparisons_diss_table)[unname(unlist(single_study_comp[x]))] <- NA else dissimilarities[[x]]$Comparisons_diss_table; dissimilarities[[x]]$Comparisons_diss_table})

# Number of pairs of comparisons per network
num_comp_net <- unlist(lapply(comp_diss_mat, function(x) length(x[lower.tri(x)])))

# Total number of pairs of comparisons
total_comp <- sum(num_comp_net)

# Between-comparison dissimilarities
between_data <- unname(unlist(lapply(comp_diss_mat, function(x) x[lower.tri(x)])))

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



## Between-comparison dissimilarity distribution *across* the networks ----
# Summary of between-comparison dissimilarity across networks
summary(between_data)



## Between-comparison dissimilarity distribution outcome and intervention-comparator (Table 3) ----
# Prepare dataset 
data_plot <- data.frame(between_data, outcome, interv_comp, network_id)
data_plot$interv_comp <- factor(data_plot$interv_comp,
                                levels = c("Pharma vs. Placebo", "Pharma vs. Pharma", "Non-pharma vs. Any"))

# Summary of between-comparison dissimilarity
data_plot %>%
  group_by(outcome, interv_comp, .drop = FALSE) %>% 
  summarise(median = round(median(between_data), 2),
            q25 = round(quantile(between_data, 0.25), 2),
            q75 = round(quantile(between_data, 0.75), 2),
            min = round(min(between_data), 2),
            max = round(max(between_data), 2))

# Number of pairs of comparisons by outcome and intervention-comparator group 
data.frame(num_comp_net, database_thresh) %>%
  group_by(outcome_type, interv_comp_type, .drop = FALSE) %>% 
  summarise(min = round(min(num_comp_net), 2),
            max = round(max(num_comp_net), 2))



## Commonness of *transitivity* by outcome and intervention-comparator (Figure 3) ----
# Repeat the 'threshold' based on 'num_comp_net'
threshold_median <- rep(database_thresh$threshold_50, num_comp_net)
threshold_75qrt <- rep(database_thresh$threshold_75, num_comp_net)

# 50% threshold: % *networks* with 'transitivity' and 'probably intransitivity' 
transit_50 <-
  data.frame(network_id, outcome, interv_comp, between_data, threshold_median) %>%
  mutate(decision = if_else(between_data < threshold_median, "low", "probably concerning")) %>%
  group_by(network_id, outcome, interv_comp) %>%
  count(network_id, outcome, interv_comp, decision, .drop = FALSE) %>%
  mutate(freq = round((n / sum(n)) * 100, 2)) %>%  
  filter(!is.na(decision)) %>%
  mutate(transitivity = factor(if_else(decision == "low" & freq == 100, "transitivity", "probably intransitivity"), levels = c("transitivity", "probably intransitivity"))) %>%
  distinct(network_id, .keep_all = TRUE) %>%
  group_by(outcome, interv_comp) %>%
  count(transitivity, .drop = FALSE) %>%
  mutate(freq = round((n / sum(n)) * 100, 2))

# 75% threshold: % *networks* with 'transitivity' and 'probably intransitivity'
transit_75 <-
  data.frame(network_id, outcome, interv_comp, between_data, threshold_75qrt) %>%
  mutate(decision = if_else(between_data < threshold_75qrt, "low", "probably concerning")) %>%
  group_by(network_id, outcome, interv_comp) %>%
  count(network_id, outcome, interv_comp, decision, .drop = FALSE) %>%
  mutate(freq = round((n / sum(n)) * 100, 2)) %>%  
  filter(!is.na(decision)) %>%
  mutate(transitivity = factor(if_else(decision == "low" & freq == 100, "transitivity", "probably intransitivity"), levels = c("transitivity", "probably intransitivity"))) %>%
  distinct(network_id, .keep_all = TRUE) %>%
  group_by(outcome, interv_comp) %>%
  count(transitivity, .drop = FALSE) %>%
  mutate(freq = round((n / sum(n)) * 100, 2))

# Merge '50% threshold' and '75% threshold'
transit_plot <- data.frame(rbind(transit_50, transit_75),
                           rep(c("Second quartile", "Third quartile"), c(dim(transit_50)[1], dim(transit_75)[1])))
colnames(transit_plot)[6] <- "threshold"

# Create grouped barplot 
tiff("./Figures/Figure 3.tiff", 
     height = 24, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(transit_plot, 
       aes(x = threshold,
           y = freq, 
           fill = transitivity)) + 
  geom_bar(position = "dodge", 
           stat = "identity",
           alpha = 0.8) +
  geom_text(aes(label = paste0(round(freq, 1), "%", " ",  "(", n, ")")),
            position = position_dodge(width = .9),
            vjust = -0.2,
            size = 4.0) +
  facet_grid(interv_comp ~ outcome) +
  scale_fill_manual(values = c("#009E73", "#D55E00"),
                    labels = c("transitivity" = "Likely present",
                               "probably intransitivity" = "Likely absent")) +
  scale_y_continuous(limits = c(0, 100), expand = c(0.1, 0)) +
  labs(x = "Threshold",
       y = "Percentage of datasets (%)",
       fill = "Evidence of transitivity") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.margin = margin(0, 0, 0, 0),
        strip.text = element_text(size = 14, face = "bold"))
dev.off()



## Commonness of 'low' D_B among networks with 'intransitivity' by outcome and intervention-comparator (Figure 4) ----
# 50% threshold: % *networks* with 'transitivity' and 'probably intransitivity' 
intransit_comp_50 <-
  data.frame(network_id, outcome, interv_comp, between_data, threshold_median) %>%
  mutate(decision = if_else(between_data < threshold_median, "low", "probably concerning")) %>%
  group_by(network_id, outcome, interv_comp) %>%
  count(network_id, outcome, interv_comp, decision, .drop = FALSE) %>%
  mutate(freq = round((n / sum(n)) * 100, 2)) %>%  
  filter(!is.na(decision)) %>% 
  mutate(transitivity = factor(if_else(decision == "low" & freq < 100 | decision == "probably concerning", "probably intransitivity", "transitivity"), levels = c("transitivity", "probably intransitivity"))) %>%
  filter(transitivity == "probably intransitivity")

# Number of networks with '50% threshold' by decision, outcome and intervention-comparator
num_intrans_net_50 <- 
  intransit_comp_50 %>%
  group_by(outcome, interv_comp) %>%
  count(decision)

# Number of intransitive network with '50% threshold' (ALL!!)
length(table(intransit_comp_50$network_id))  # 214

# 75% threshold: % *networks* with 'transitivity' and 'probably intransitivity' 
intransit_comp_75 <-
  data.frame(network_id, outcome, interv_comp, between_data, threshold_75qrt) %>%
  mutate(decision = if_else(between_data < threshold_75qrt, "low", "probably concerning")) %>%
  group_by(network_id, outcome, interv_comp) %>%
  count(network_id, outcome, interv_comp, decision, .drop = FALSE) %>%
  mutate(freq = round((n / sum(n)) * 100, 2)) %>%  
  filter(!is.na(decision)) %>% 
  mutate(transitivity = factor(if_else(decision == "low" & freq < 100 | decision == "probably concerning", "probably intransitivity", "transitivity"), levels = c("transitivity", "probably intransitivity"))) %>%
  filter(transitivity == "probably intransitivity")

# Number of networks with '75% threshold' by decision, outcome and intervention-comparator
num_intrans_net_75 <- 
  intransit_comp_75 %>%
  group_by(outcome, interv_comp) %>%
  count(decision)

# Number of intransitive network with '75% threshold'
length(table(intransit_comp_75$network_id))  # 188 (87.8%)

# Merge '50% threshold' and '75% threshold'
intransit_comp_plot <- data.frame(rbind(intransit_comp_50, intransit_comp_75),
                                  rep(c("Second quartile", "Third quartile"), 
                                      c(dim(intransit_comp_50)[1], dim(intransit_comp_75)[1])))
colnames(intransit_comp_plot)[8] <- "threshold"

# Merge number of networks under '50% threshold' and '75% threshold'
num_intrans_net <- data.frame(rbind(num_intrans_net_50, num_intrans_net_75),
                              rep(c("Second quartile", "Third quartile"), 
                                  c(dim(num_intrans_net_50)[1], dim(num_intrans_net_75)[1])))
colnames(num_intrans_net)[5] <- "threshold"

# Restrict to combinations of 'probably intransitivity' to get the background bars
intransit_bars <- transit_plot %>%
  filter(transitivity == "probably intransitivity")

# Get dot plot with background bars
tiff("./Figures/Figure 4.tiff", 
     height = 24, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(intransit_comp_plot,
       aes(x = threshold,
           y = freq,
           group = decision,
           colour = decision)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2),
             alpha = 0.5) +
  stat_summary(geom = "point",
               fun = "mean",
               col = "black",
               size = 3,
               shape = 18,
               position = position_dodge(width = 0.8)) +
  stat_summary(aes(label = sprintf("%.1f", after_stat(y))),
               geom = "text",
               fun = "mean",
               col = "black",
               position = position_dodge(width = 0.8),
               hjust = 1.4) +
  geom_text(data = num_intrans_net,
            aes(x = threshold,
                y = 5,
                label = paste0("(n=", n, ")")),
            position = position_dodge(width = 0.8),
            parse = FALSE,
            check_overlap = TRUE,
            size = 4.3,
            col = "grey40", 
            hjust = 0.5, 
            vjust = 2.3) +
  geom_bar(data = intransit_bars,
           aes(x = threshold,
               y = freq),
           fill = "#D55E00",
           alpha = 0.13,
           position = "dodge", 
           stat = "identity",
           inherit.aes = FALSE) +
  facet_grid(interv_comp ~ outcome) +
  scale_colour_manual(values = c("#009E73", "#D55E00"),
                      labels = c("low" = "Low",
                                 "probably concerning" = "Likely concerning")) +
  expand_limits(y = c(-5, 100)) +
  labs(x = "Threshold",
       y = "Percentage of pairs of comparisons per dataset (%)",
       colour = "Between-comparison dissimilarity") +
  guides(colour = guide_legend(override.aes = list(size = 4,
                                                   alpha = 1))) +
  theme_bw() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.margin = margin(0, 0, 0, 0),
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "white"))
dev.off()
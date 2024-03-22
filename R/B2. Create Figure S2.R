#*******************************************************************************
#*
#*
#*                       Creating Supplementary Figure S2                                                                                                                                                                                                                                                 
#*              (Distribution of characteristics across datasets)                                                                                                                                                                                                                                     
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



## Obtain the within-comparison and between comparison dissimilarities ----
# Insert 'Comparison' in the dataset (control appears second in the compar.)
insert_comp <- lapply(dataset_new, function(x) {x$Comparison <- as.character(paste0(x$treat2, "-", x$treat1)); x})

# Single-study comparisons
single_study_comp <- lapply(insert_comp, function(x) names(which(table(x$Comparison) == 1)))

# Find the single-study comparisons and set diagonal with 'NA'
comp_diss_mat <- lapply(1:length(dissimilarities), function(x) {if (length(unlist(single_study_comp[x])) > 0) diag(dissimilarities[[x]]$Comparisons_diss_table)[unname(unlist(single_study_comp[x]))] <- NA else dissimilarities[[x]]$Comparisons_diss_table; dissimilarities[[x]]$Comparisons_diss_table})

# Number of non-single-study comparisons per network
num_comp_net <- unlist(lapply(comp_diss_mat, function(x) length(na.omit(diag(x)))))

# Number of pairs of comparisons per network
num_pair_comp_net <- unlist(lapply(comp_diss_mat, function(x) length(x[lower.tri(x)])))

# Within-comparison dissimilarities
within_data <- unname(unlist(lapply(comp_diss_mat, function(x) na.omit(diag(x)))))

# Between-comparison dissimilarities
between_data <- unname(unlist(lapply(comp_diss_mat, function(x) x[lower.tri(x)])))



## Obtain the number of characteristics per network
num_chars <- unname(unlist(lapply(dataset_new, function(x) dim(x[, -c(1:3)])[2])))

# Repeat 'num_chars' based on 'num_comp_net' (Within-comparison dissimilarities)
num_chars_within <- rep(num_chars, num_comp_net)

# Repeat 'num_chars' based on 'num_pair_comp_net' (Between-comparison dissimilarities)
num_chars_between <- rep(num_chars, num_pair_comp_net)



## Create histogram with dot and density ----
#' The boxplot shows the median and IQR
tiff("./40_Analysis & Results/Figure S2.tiff", 
     height = 20, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(data.frame(num_chars),
       aes(x = num_chars, 
           y = 0)) + 
  #stat_halfeye(fill = scales::hue_pal()(5)[-c(1:4)]) + 
  stat_histinterval(slab_color = "gray70", 
                    interval_colour = "black",
                    interval_alpha = 1,
                    point_fill = "black",
                    point_alpha= 1,
                    point_size = 4,
                    outline_bars = TRUE, 
                    .width = c(0.50, 0.95),  # IQR and 95% interval
                    alpha = 0.2) +
  stat_slab(colour = "gray70",
            fill = NA) +
  stat_dots(aes(x = num_chars, 
                y = 0.004),
            position = "dodgejust",
            dotsize = 0.5,
            alpha = 0.7) +
  scale_x_continuous(breaks = seq(4, 41, 6)) + 
  labs(y = " ",
       x = "Number of extracted characteristics") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))
dev.off()

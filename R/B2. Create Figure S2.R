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



## Obtain the number of characteristics per network ----
num_chars <- unname(unlist(lapply(dataset_new, function(x) dim(x[, -c(1:3)])[2])))



## Create histogram with dot and density ----
#' The boxplot shows the median and IQR
tiff("./Figures/Figure S2.tiff", 
     height = 20, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(data.frame(num_chars),
       aes(x = num_chars, 
           y = 0)) + 
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

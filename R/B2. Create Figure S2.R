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
list.of.packages <- c("dplyr", "ggplot2", "gghalves", "ggdist", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load functions ----
source("./R/function.collection_function.R")



## Load datasets ----
# TRACE-NMA dataset
load("./data/TRACE-NMA Dataset.RData")



## Dataset preparation ----
# Get 'dataset_new'
dataset_new0 <- get_dataset_new(read_all_excels)

# Remove datasets with less than four characteristics (after removing dose-related characteristics)
remove <- which(unname(unlist(lapply(dataset_new0, function(x) dim(subset(x, select = -c(trial, treat1, treat2)))[2]))) < 4)
dataset_new <- dataset_new0[-remove] 

# Drop further datasets with less than four characteristics (after dropping characteristics with too many missing data)
dataset_new_red <- lapply(dataset_tests(dataset_new)$dataset_new_final, function(x) subset(x, select = -Comparison))



## Obtain the number of characteristics per dataset ----
# Original database (214 datasets)
num_chars <- unname(unlist(lapply(dataset_new, 
                                  function(x) dim(subset(x, select = -c(trial, treat1, treat2)))[2])))

# Reduced database (209 datasets)
num_chars_red <- unname(unlist(lapply(dataset_new_red, 
                                      function(x) dim(subset(x, select = -c(trial, treat1, treat2)))[2])))



## Create dot plots (The interval shows the median, IQR and 95% interval) ----
#' All characteristics
all_chars <-
  ggplot(data.frame(num_chars),
       aes(x = num_chars, 
           y = 0)) + 
  stat_dotsinterval(slab_color = "gray50", 
                    interval_colour = "black",
                    interval_alpha = 1,
                    point_fill = "black",
                    point_alpha= 1,
                    point_size = 4,
                    outline_bars = TRUE, 
                    .width = c(0.50, 0.95),  # IQR and 95% interval
                    alpha = 0.5) +
  scale_x_continuous(breaks = seq(4, 41, 3), limits = c(4, 41)) + 
  labs(y = " ",
       x = "") +
  ggtitle(paste0("All characteristics (n=", sum(num_chars),") and datasets (n=", length(num_chars), ")")) +
  theme_classic() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

#' After removing 'dropped' characteristics and datasets with less than 4 characteristics
reduced_chars <-
  ggplot(data.frame(num_chars_red),
         aes(x = num_chars_red, 
             y = 0)) + 
  stat_dotsinterval(slab_color = "gray50", 
                    interval_colour = "black",
                    interval_alpha = 1,
                    point_fill = "black",
                    point_alpha= 1,
                    point_size = 4,
                    outline_bars = TRUE, 
                    .width = c(0.50, 0.95),  # IQR and 95% interval
                    alpha = 0.5) +
  scale_x_continuous(breaks = seq(4, 41, 3), limits = c(4, 41)) + 
  labs(y = " ",
       x = "Number of extracted characteristics") +
  ggtitle(paste0("Reduced characteristics (n=", sum(num_chars_red),") and datasets (n=", length(num_chars_red), ")")) +
  theme_classic() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

# Bring together
tiff("./Figures/Figure S2.tiff", 
     height = 30, 
     width = 33, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(all_chars, reduced_chars,
          nrow = 2,
          labels = c("a)", "b)"))
dev.off()

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
remove <- which(unname(unlist(lapply(dataset_new0, function(x) dim(x)[2] - 3))) < 4)
dataset_new <- dataset_new0[-remove] 



## Obtain the number of characteristics per dataset ----
num_chars <- unname(unlist(lapply(dataset_new, function(x) dim(x[, -c(1:3)])[2])))
sum(num_chars)


## Networks with less than 4 characteristic after dropping (Table 2)
#' Spot dropped characteristics per comparison and dataset
dropped_char_list <- 
  lapply(dataset_new, function(x) {
    lapply(split(x, f = as.character(paste(x$treat2, "vs", x$treat1))), function(y) {
      if (dim(y)[1] > 1) as.vector(which(colSums(is.na(y)) == nrow(y) | 
                                           colSums(is.na(y)) == nrow(y) - 1))
    }) 
  })

#' Vector of the number of unique dropped characteristics per dataset
dropped_char <- as.numeric(lapply(dropped_char_list, function(x) if (length(unique(unlist(x))) == 0) {
  0
} else {
  length(unique(unlist(x)))
}))

#' Get the dataset(s) with less than 4 characteristics
exclude_datasets <- which(num_chars - dropped_char < 4) # 64 102 107 122 168

#' Re-calculate the analysed characteristics after excluding the dropped ones
num_chars_fin <- (num_chars - dropped_char)[-exclude_datasets]



## Create histogram with dots (The boxplot shows the median, IQR and 95% interval) ----
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
  #stat_slab(colour = "gray70",
  #          fill = NA) +
  #stat_dots(aes(x = num_chars, 
  #              y = 0.004),
  #          position = "dodgejust",
  #          dotsize = 0.5,
  #          alpha = 0.7) +
  scale_x_continuous(breaks = seq(4, 41, 3), limits = c(4, 41)) + 
  labs(y = " ",
       x = "") +
  ggtitle(paste0("All characteristics (n=", sum(num_chars),") and datasets (n=", length(num_chars), ")")) +
  theme_classic() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

#' After removing characteristics and datasets with less than 4 characteristics
reduced_chars <-
  ggplot(data.frame(num_chars_fin),
         aes(x = num_chars_fin, 
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
  #stat_slab(colour = "gray70",
  #          fill = NA) +
  #stat_dots(aes(x = num_chars_fin, 
  #              y = 0.004),
  #          position = "dodgejust",
  #          dotsize = 0.5,
  #          alpha = 0.7) +
  scale_x_continuous(breaks = seq(4, 41, 3), limits = c(4, 41)) + 
  labs(y = " ",
       x = "Number of extracted characteristics") +
  ggtitle(paste0("Reduced characteristics (n=", sum(num_chars_fin),") and datasets (n=", length(num_chars_fin), ")")) +
  theme_classic() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

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

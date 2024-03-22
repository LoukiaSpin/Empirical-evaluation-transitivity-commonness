#*******************************************************************************
#*
#*
#*                       Creating Supplementary Figure S1                          
#*           (Distribution of outcome and treatment-comparator types)                                                                            
#*                
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("reshape2", "ggplot2")
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

# Include proper threshold to each dataset based on their design factors
database_thresh <- dataset_threshold(dataset_new)[, c(2, 7:8)]



## Visualise distribution of 'Intervention-comparator type' and 'Outcome type' ----
# Get the summary table
data_bar_net <- table(database_thresh$outcome_type, database_thresh$interv_comp_type)

# Prepare dataset for ggplot2
data_bar_net_prop <- melt(prop.table(data_bar_net))
colnames(data_bar_net_prop)[1:2] <- c("outcome", "intervention")
data_bar_net_prop$intervention <- factor(data_bar_net_prop$intervention,
                                         levels = c("Pharma vs. Placebo", "Pharma vs. Pharma", "Non-pharma vs. Any"))

# Get stacked barplot
tiff("./Figures/Figure S1.tiff", 
     height = 20, 
     width = 37, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(data_bar_net_prop, 
       aes(x = outcome,
           y = value * 100, 
           fill = intervention)) + 
  geom_bar(position = "dodge", 
           stat = "identity",
           alpha = 0.8) +
  geom_text(aes(label = paste0(round(value * 100, 1), "%", " ",  "(", melt(data_bar_net)[, 3], ")")),
            position = position_dodge(width = .9),
            vjust = -0.2,
            size = 4.5) +
  scale_fill_manual(values = scales::hue_pal()(3),
                    labels = c("Pharma vs. Placebo" = "Pharmacological vs. Placebo",
                               "Pharma vs. Pharma" = "Pharmacological vs. Pharmacological",
                               "Non-pharma vs. Any" = "Non-pharmacological vs. Any")) +
  labs(x = "Outcome type",
       y = "Percentage of datasets (%)",
       fill = "Treatment-comparator type") +
  ylim(c(0, 100)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))
dev.off()

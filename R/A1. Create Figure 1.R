#*******************************************************************************
#*
#*
#*                         Create Figure 1 of Manuscript                                               
#*           (Schematic illustration of the proposed novel approach)                                  
#*
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************


## Load rnmamod developmental version
#remotes::install_github("https://github.com/LoukiaSpin/rnmamod.git", force = TRUE)


## Load libraries
list.of.packages <- "rnmamod"


## Construct dataset
data_set <- data.frame(trial = as.character(1:6),
                       treat1 = c("1", "1", "1", "1", "1", "2"),
                       treat1 = c("2", "2", "3", "3", "3", "3"),
                       sample = c(180, 185, 60, 55, 50, 85),
                       perc_female = c(80, 80, 20, 20, 20, 50),
                       blinding = factor(c("yes", "yes", "no", "no", "no", "yes")),
                       withdrawal = factor(c("no", "no", "yes", "yes", "yes", "no")))


# Check if typeof is correct
lapply(data_set, typeof)


## Gower dissimilarity
# Prepare dataset for the 'gower_distance' function
data_set_gower <- data.frame(trial = as.character(1:dim(data_set)[1]),
                             comp = as.character(paste(data_set$treat2, "vs", data_set$treat1)),
                             data_set[, 4:7])

# Check if typeof has been corrected
lapply(data_set_gower, typeof)


## Dissimilarities
comp_clustering(input = data_set,
                drug_names = c("A", "B", "C"),
                threshold = 0.13,  # General research setting
                informative = TRUE,
                get_plots = TRUE,
                label_size = 6,
                axis_text_size = 18)

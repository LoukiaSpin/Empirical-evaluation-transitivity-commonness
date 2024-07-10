#*******************************************************************************
#*
#*
#*                     Creating Figure 2 of the Manuscript                                                                                                                                                                                                                                                                                                               
#*           (Density plots of predictive distribution for I-squared)                                                                                                                                                                                            
#*                
#* Author: Loukia M. Spineli 
#* Date: March 2024
#*******************************************************************************



## Load libraries ----
list.of.packages <- c("ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Predictive distribution hyper-parameters: placebo-comparison, moderately sized trials, all outcomes ----
#' (distributions differ more substantially across the outcomes than intervention-comparators) 
dataset_dist <- data.frame(outcome = c("Objective", "Semi-objective", "Subjective", "General"),
                           thresh_mean = c(-7.36, -2.83, -1.14, -1.85), 
                           thresh_sd = c(4.81, 3.23, 2.03, 3.43))
df <- 5


## Probability density per outcome (logit scale)
# I2 values range
values_i2 <- seq(0.00001, 0.99999, 0.01)

# Function for logit (I2 is the input)
logit_fun <- function (x) {
  log(x / (1 - x))
}

# Get logit for selected I2 values
values_i2_logit <- logit_fun(values_i2)

# Obtain the pdf
prob_dens <- 
  lapply(1:dim(dataset_dist)[1],
         function(x) (1/dataset_dist[x, 3]) * dt((values_i2_logit - dataset_dist[x, 2]) / dataset_dist[x, 3], df))

# Bring together in a data-frame
dataset_pdf <- data.frame(value_x = values_i2_logit, 
                          prob_dens_y = unlist(prob_dens),
                          outcome = rep(dataset_dist$outcome, length(values_i2)))

# Define breaks (for ggplot2)
breaks_i2 <- c(0.00005, 0.0007, 0.01, 0.05, 0.25, 0.52, 0.75, 0.95, 0.99)

# Get plot for 'Objective' outcomes
objective_out <-
  ggplot(subset(dataset_pdf, outcome == "Objective"),
         aes(x = value_x,
             y = prob_dens_y)) +
  stat_function(fun = function(z) {(1 / dataset_dist[1, 3]) * dt((z - dataset_dist[1, 2]) / dataset_dist[1, 3], df = 5)}, 
                xlim = c(log(5e-05 / (1 - 5e-05)), log(0.01 / (1 - 0.01))),
                geom = "area",
                fill = "#D55E00",
                alpha = 0.5) +
  stat_function(fun = function(z) {(1 / dataset_dist[1, 3]) * dt((z - dataset_dist[1, 2]) / dataset_dist[1, 3], df = 5)}, 
                xlim = c(log(0.0007 / (1 - 0.0007)), log(0.00074 / (1 - 0.00074))),
                geom = "area",
                fill = "white") +
  stat_function(fun = function(z) {(1 / dataset_dist[1, 3]) * dt((z - dataset_dist[1, 2]) / dataset_dist[1, 3], df = 5)}, 
                col = "black", 
                linewidth = 1.3) +
  ylim(0, 0.20) +
  ggtitle("Objective outcomes") +
  labs(y = " ",
       x = " ") +
  scale_x_continuous(breaks = logit_fun(breaks_i2),
                     labels = breaks_i2 * 100,
                     limits = c(-15, 15)) +
  theme_classic() + 
  theme(title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"))

# Get plot for 'Semi-Objective' outcomes
semi_objective_out <-
  ggplot(subset(dataset_pdf, outcome == "Semi-objective"),
         aes(x = value_x,
             y = prob_dens_y)) +
  stat_function(fun = function(z) {(1 / dataset_dist[2, 3]) * dt((z - dataset_dist[2, 2]) / dataset_dist[2, 3], df = 5)}, 
                xlim = c(log(0.01 / (1 - 0.01)), log(0.27 / (1 - 0.27))),
                geom = "area",
                fill = "#D55E00",
                alpha = 0.5) +
  stat_function(fun = function(z) {(1 / dataset_dist[2, 3]) * dt((z - dataset_dist[2, 2]) / dataset_dist[2, 3], df = 5)}, 
                xlim = c(log(0.06 / (1 - 0.06)), log(0.063 / (1 - 0.063))),
                geom = "area",
                fill = "white") +
  stat_function(fun = function(z) {(1 / dataset_dist[2, 3]) * dt((z - dataset_dist[2, 2]) / dataset_dist[2, 3], df = 5)}, 
                col = "black", 
                linewidth = 1.3) +
  ylim(0, 0.20) +
  ggtitle("Semi-objective outcomes") +
  labs(y = "Density",
       x = "I-squared (%)") +
  scale_x_continuous(breaks = logit_fun(breaks_i2),
                     labels = breaks_i2 * 100,
                     limits = c(-15, 15)) +
  theme_classic() + 
  theme(title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"))

# Get plot for 'Subjective' outcomes
subjective_out <-
  ggplot(subset(dataset_pdf, outcome == "Subjective"),
         aes(x = value_x,
             y = prob_dens_y)) +
  stat_function(fun = function(z) {(1 / dataset_dist[3, 3]) * dt((z - dataset_dist[3, 2]) / dataset_dist[3, 3], df = 5)}, 
                xlim = c(log(0.09 / (1 - 0.09)), log(0.52 / (1 - 0.52))),
                geom = "area",
                fill = "#D55E00",
                alpha = 0.5) +
  stat_function(fun = function(z) {(1 / dataset_dist[3, 3]) * dt((z - dataset_dist[3, 2]) / dataset_dist[3, 3], df = 5)}, 
                xlim = c(log(0.25 / (1 - 0.25)), log(0.255 / (1 - 0.255))),
                geom = "area",
                fill = "white") +
  stat_function(fun = function(z) {(1 / dataset_dist[3, 3]) * dt((z - dataset_dist[3, 2]) / dataset_dist[3, 3], df = 5)}, 
                col = "black", 
                linewidth = 1.3) +
  ylim(0, 0.20) +
  ggtitle("Subjective outcomes") +
  labs(y = " ",
       x = "I-squared (%)") +
  scale_x_continuous(breaks = logit_fun(breaks_i2),
                     labels = breaks_i2 * 100,
                     limits = c(-15, 15)) +
  theme_classic() + 
  theme(title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"))

# Get plot for 'General health setting'
general_out <-
  ggplot(subset(dataset_pdf, outcome == "General"),
         aes(x = value_x,
             y = prob_dens_y)) +
  stat_function(fun = function(z) {(1 / dataset_dist[4, 3]) * dt((z - dataset_dist[4, 2]) / dataset_dist[4, 3], df = 5)}, 
                xlim = c(log(0.02 / (1 - 0.02)), log(0.53 / (1 - 0.53))),
                geom = "area",
                fill = "#D55E00",
                alpha = 0.5) +
  stat_function(fun = function(z) {(1 / dataset_dist[4, 3]) * dt((z - dataset_dist[4, 2]) / dataset_dist[4, 3], df = 5)}, 
                xlim = c(log(0.13 / (1 - 0.13)), log(0.135 / (1 - 0.135))),
                geom = "area",
                fill = "white") +
  stat_function(fun = function(z) {(1 / dataset_dist[4, 3]) * dt((z - dataset_dist[4, 2]) / dataset_dist[4, 3], df = 5)}, 
                col = "black", 
                linewidth = 1.3) +
  ylim(0, 0.20) +
  ggtitle("General healthcare setting") +
  labs(y = "Density",
       x = " ") +  
  scale_x_continuous(breaks = logit_fun(breaks_i2),
                     labels = breaks_i2 * 100,
                     limits = c(-15, 15)) +
  theme_classic() + 
  theme(title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"))

# Bring all together
tiff("./Figures/Figure 2.tiff", 
     height = 18, 
     width = 38, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(general_out, objective_out, semi_objective_out, subjective_out,
          labels = c("a)", "b)", "c)", "d)"))
dev.off()

#### Script to calculate the mean effect sizes (R²) and create the graphs ####

# Loading packages

library(dplyr)
library(ggplot2)
library(boot)
library(viridis)
library(lattice)
library(gridExtra)
library(tidyverse)

# Loading data

data_R_square <- read.csv("Results/all_results.csv", h=T, row.names = 1)

biodiversity_variables <- c("ant_rich", "bee_rich", "bird_rich", "db_rich", "liana_rich", "sap_rich", "tree_rich")
carbon_variables <- c("Aboveground_pool", "Litter_pool", "Dead_pool", "Soil_pool_new")
soil_variables <- c("pH", "Nperc", "P", "K", "Na", "CaMg", "Al")

data_R_square[data_R_square$variable %in% biodiversity_variables,"Component"] <- "biodiversity"
data_R_square[data_R_square$variable %in% carbon_variables,"Component"] <- "carbon" 
data_R_square[data_R_square$variable %in% soil_variables,"Component"] <- "soil"


#### Bootstrapping the Confidence interval of the mean for R-squared ####

# Function to obtain the mean
Bmean <- function(data, indices) {
  d <- data[indices]
  return(mean(d))
} 

# Function to obtain the CI of the mean

boot_ci_nunes <- function(data, n_random, type){
  results <- boot(data = data, statistic = Bmean, R = n_random)
  ci <- boot.ci(results, type = type)
  tibble(ci_lb = ci$bca[4], ci_ub = ci$bca[5])
}

average_R2 <- data_R_square %>% 
  group_by(Component, Transition) %>% 
  summarise(mean_R2 = mean(R2m), boot_ci_nunes(data = R2m, n_random = 10000, type = "bca"),
            n = n())

write.csv(file = "Results/average_R2.csv", x = average_R2)


#### Graphs ####

## Mean effect sizes ##

#My APA-format theme
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.position='none')

# Plot Data Frame
average_R2_ordered <- average_R2 %>%
  ungroup() %>% 
  # 2. Arrange by
  #   i.  facet group
  #   ii. bar height
  arrange(Component, mean_R2) %>%
  # 3. Add order column of row numbers
  mutate(order = row_number())


ggplot(average_R2_ordered, aes(y=order, x=mean_R2, xmin=ci_lb, xmax=ci_ub, colour = factor(Component)))+
  geom_point(size=3)+
  geom_errorbarh(aes(colour = factor(Component)), size = 1, height=1)+
  scale_colour_manual(values = c("#006D2C", "#F57D15FF", "#781C6D"))+
  scale_x_continuous(limits = c(0,1), name='R²')+
  facet_grid(Component~., scales = 'free', space='free')+
  geom_vline(xintercept=0)+
  scale_y_continuous(
    breaks = average_R2_ordered$order,
    labels = average_R2_ordered$Transition,
    expand = c(0,0)
  )+
  #Apply my APA theme
  apatheme


## Heat map

data_all_joined <- read.csv("Data/data_all_joined.csv", h=T, row.names = 1)

data_R_square <- data_R_square[order(match(data_R_square$variable,
                                           c("tree_rich", "sap_rich", "liana_rich", "bird_rich", "ant_rich", "db_rich",
                                             "bee_rich", "Aboveground_pool", "Litter_pool", "Dead_pool", "Soil_pool_new",
                                             "pH", "Al", "CaMg", "K", "Nperc", "Na", "P"))),]

data_R_square <- data_R_square[order(match(data_R_square$Transition,
                                           c("UF_LF", "UF_LBF", "UF_PA", "UF_MA", "LF_LBF", "LF_PA", "LF_MA", 
                                             "LBF_PA", "LBF_MA", "PA_MA", "MA_SFyoung", "PA_SFyoung",
                                             "SFyoung_SFold", "SFold_PA", "SFold_MA"))),]


matrix <- as.data.frame(pivot_wider(data_R_square, id_cols = Transition, names_from = variable, values_from = R2m))
row.names(matrix) <- matrix$Transition

levelplot(t(as.matrix(matrix[,2:19])), col.regions = colorRampPalette(c("#FCFDBFFF" ,"#FEBA80FF", "#F8765CFF", "#D3436EFF", "#982D80FF", "#5F187FFF", "#180F3EFF"))(200),
          colorkey = list(TRUE, height = 1), at = seq(0,1,0.01), scales = list(y = (list(cex=1, font=2)), tck = c(1,0), x=list(cex=1, rot=45, font=2)),
          aspect = 15/19, xlab = NULL, ylab = "Transitions")



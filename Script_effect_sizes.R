#### Script to calculate the mean effect sizes and create the graphs ####

# Loading packages

library(dplyr)
library(ggplot2)
library(lattice)
library(tidyverse)

# Loading data

efsize_data <- read.csv("efsize_data.csv", h=T)

biodiversity_variables <- c("ant_rich", "bee_rich", "bird_rich", "db_rich", "liana_rich", "sap_rich", "tree_rich")
carbon_variables <- c("Aboveground_pool", "Litter_pool", "Dead_pool", "Soil_pool_new")
soil_variables <- c("pH", "Nperc", "P", "K", "Na", "CaMg", "Al")

efsize_data[efsize_data$variable %in% biodiversity_variables,"Component"] <- "biodiversity"
efsize_data[efsize_data$variable %in% carbon_variables,"Component"] <- "carbon" 
efsize_data[efsize_data$variable %in% soil_variables,"Component"] <- "soil"


#### Graphs ####

## Effect sizes ##

#My APA-format theme
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.position='none')

efsize_data_ordered <- efsize_data %>%
  group_by(Component, transition) %>% 
  summarise(median_es = median(abs(Estimate)))

efsize_data_ordered <- efsize_data_ordered %>% 
  ungroup() %>% 
  arrange(Component, median_es) %>% 
  mutate(order = row_number())

efsize_data2 <- efsize_data %>% 
  group_by(Component) %>% 
  left_join(efsize_data_ordered, by = c("Component", "transition"))

#write.csv(x = efsize_data_ordered, file = "efsize_median.csv", row.names = F)


# Plot

ggplot(efsize_data2)+
  geom_boxplot(aes(y=as.factor(order), x=abs(Estimate)), fill=NA, outlier.shape = NA)+
  geom_text(aes(y=as.factor(order), x=2.8, label = round(median_es, digits = 2)))+
  geom_point(aes(y=as.factor(order), x=abs(Estimate), colour = factor(Component)),
             size=3, alpha = 0.65)+
  scale_colour_manual(values = c("#006D2C", "#F57D15FF", "#781C6D"))+
  facet_grid(Component~., scales= 'free', space='free')+
  scale_y_discrete(
    breaks = as.numeric(efsize_data_ordered$order),
    labels = as.factor(efsize_data_ordered$transition),
    expand = c(0.05,0)
  )+
  apatheme




## Heat map

efsize_data <- read.csv("efsize_data.csv", h=T)

efsize_data <- efsize_data[order(match(efsize_data$variable,
                                       c("tree_rich", "sap_rich", "liana_rich", "bird_rich", "ant_rich", "db_rich",
                                         "bee_rich", "Aboveground_pool", "Litter_pool", "Dead_pool", "Soil_pool_new",
                                         "pH", "Al", "CaMg", "K", "Nperc", "Na", "P"))),]

efsize_data <- efsize_data[order(match(efsize_data$transition,
                                       c("UF_LF", "UF_LBF", "UF_PA", "UF_MA", "LF_LBF", "LF_PA", "LF_MA", 
                                         "LBF_PA", "LBF_MA", "PA_MA", "MA_SFyoung", "PA_SFyoung",
                                         "SFyoung_SFold", "SFold_PA", "SFold_MA"))),]


matrix <- as.data.frame(pivot_wider(efsize_data, id_cols = transition, names_from = variable, values_from = Estimate))
row.names(matrix) <- matrix$transition

levelplot(t(as.matrix(matrix[,2:19])), col.regions = colorRampPalette(c("#FCFDBFFF" ,"#FEBA80FF", "#F8765CFF", "#D3436EFF", "#982D80FF", "#5F187FFF", "#180F3EFF"))(200),
          colorkey = list(TRUE, height = 1), at = seq(-1.8,2.7,0.05), scales = list(y = (list(cex=1, font=2)), tck = c(1,0), x=list(cex=1, rot=45, font=2)),
          aspect = 15/19, xlab = NULL, ylab = "Transitions")

levelplot(t(as.matrix(matrix[,2:19]))*-1, col.regions = colorRampPalette(c("#01665e", "#35978f", "#5ab4ac", "#f5f5f5", "#dfc27d", "#d8b365", "#8c510a"))(200),
          colorkey = list(TRUE, height = 1), at = seq(-2.7,2.7,0.05), scales = list(y = (list(cex=1, font=2)), tck = c(1,0), x=list(cex=1, rot=45, font=2)),
          aspect = 15/19, xlab = NULL, ylab = "Transitions")

#d8b365
#f5f5f5
#5ab4ac

range(matrix[,2:19])
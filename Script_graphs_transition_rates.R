#### Script to plot the graphs of transition rates along the year ####

library(dplyr)
library(ggplot2)
library(gridExtra)

# Data from MapBiomas Souza, C. M. et al. Reconstructing Three Decades of Land Use and Land Cover Changes
# in Brazilian Biomes with Landsat Archive and Earth Engine. Remote Sens. 12, 2735 (2020).

data_mapbiomas <- read.csv("Data/transitions_rates_MapBiomas.csv", h=T)

list_data <- list()
data_filtered <- NULL
data_filtered_2 <- NULL

for (i in unique(data_mapbiomas$from)) {
  data_filtered <- filter(data_mapbiomas, data_mapbiomas$from == i)
  
  for (j in unique(data_filtered$to)) {
    data_filtered_2 <- filter(data_filtered, data_filtered$to == j)
    
    list_data[[paste(i, "_", j, sep = "")]] <- data_filtered_2
    data_filtered_2
  }
  
  data_filtered <- NULL
  
}

list_plots <- list()

for (i in c(1:length(list_data))) {
  
  list_plots[[names(list_data[i])]] <-
    ggplot(list_data[[i]], aes(y=km2, x=year_2))+
    #Add data points and color them black
    geom_line(size = 1.5)+
    #Specify the limits of the x-axis and relabel it to something more meaningful
    scale_x_continuous(name='Years', n.breaks = 10)+
    #Give y-axis a meaningful label
    ylab('Rate (km² yr-1)')+
    ggtitle(names(list_data[i]))+
    theme_classic()
  
}

list_plots[["young_sf_old_sf"]] <- ggplot(list_data$young_sf_old_sf, aes(y=km2, x=year_2))+
  #Add data points and color them black
  geom_line(size = 1.5)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(name='Years', limits = c(2006,2020), n.breaks = 10)+
  #xlim(c(2006, 2020))+
  #Give y-axis a meaningful label
  ylab('Rate (km² yr-1)')+
  ggtitle("young_sf_old_sf")+
  theme_classic()

grid.arrange(grobs = list_plots, ncol = 4, nrow = 4)


# Data from MapBiomas Matricardi, E. A. T. et al. Long-term forest degradation surpasses deforestation in the Brazilian Amazon.
# Science (80). 369, 1378–1382 (2020).

data_matricardi <- read.csv("Data/transitions_rates_Matricardi.csv", h=T, sep = ";")

list_data <- list()
data_filtered <- NULL
data_filtered_2 <- NULL

for (i in unique(data_matricardi$from)) {
  data_filtered <- filter(data_matricardi, data_matricardi$from == i)
  
  for (j in unique(data_filtered$to)) {
    data_filtered_2 <- filter(data_filtered, data_filtered$to == j)
    
    list_data[[paste(i, "_", j, sep = "")]] <- data_filtered_2
    data_filtered_2
  }
  
  data_filtered <- NULL
  
}

list_plots <- list()

for (i in c(1:length(list_data))) {
  
  list_plots[[names(list_data[i])]] <-
    ggplot(list_data[[i]], aes(y=km2, x=year_2))+
    #Add data points and color them black
    geom_line(size = 1.5)+
    #Specify the limits of the x-axis and relabel it to something more meaningful
    scale_x_continuous(name='Years', n.breaks = 10)+
    #Give y-axis a meaningful label
    ylab('Rate (km² yr-1)')+
    ggtitle(names(list_data[i]))+
    theme_classic()
  
}

grid.arrange(grobs = list_plots, ncol = 3, nrow = 2)

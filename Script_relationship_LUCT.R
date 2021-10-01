#### Script to test the link between land-use and land-cover transition rates to their ecological impacts ####

# Loading packages 

library(dplyr)
library(ggplot2)
library(viridis)

# Loading data

data_R2 <- read.csv("Results/average_R2.csv", h=T, row.names = 1)
data_transitions <- read.table("Data/Transition_rates.txt", h=T)

data_all <- left_join(x = data_R2, y = data_transitions, by = "Transition")


biodiversity <- droplevels(data_all[data_all$Component=="biodiversity",])
carbon <- droplevels(data_all[data_all$Component=="carbon",])
soil <- droplevels(data_all[data_all$Component=="soil",])

#### Testing the regression models ####

anova(lm(mean_R2~log(km2), data = biodiversity), test = "F")
anova(lm(mean_R2~log(km2), data = carbon), test = "F")
anova(lm(mean_R2~log(km2), data = soil))
  
# None is statistically significant!

## Creating the graph

# My APA-format theme
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.position='none')


  # Calculating the medians - Separating the quadrants: i) low impact, low rate, 
      # ii) low impact, high rate, iii) high impact, low-rate, iv) high impact, high rate.

  vline.data <- data.frame(data_all %>% 
                             group_by(Component) %>% 
                             summarise(median(log(km2))))
  names(vline.data)[2] <- "median_km2"

  hline.data <- data.frame(data_all %>% 
                             group_by(Component) %>% 
                             summarise(median(mean_R2)))
  names(hline.data)[2] <- "median_R2"


# Plotting the graph
  
ggplot(data_all, aes(y=mean_R2, x=log(km2), colour = factor(Component)))+
  #Add data points and color them black
  geom_point(size = 4)+
  geom_vline(aes(xintercept = median_km2), vline.data)+
  geom_hline(aes(yintercept = median_R2), hline.data)+
  scale_x_continuous(name='log (km²/year)')+
  scale_colour_manual(values = c("#006D2C", "#F57D15", "#781C6D"))+
  ylim(c(0,1))+
  #labels of points
  geom_text(label = data_all$Transition, 
            nudge_x = 0.030,
            nudge_y = 0.045,
            check_overlap = F, colour = "black")+
  #Give y-axis a meaningful label
  ylab('R²')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  facet_grid(cols = vars(Component), scales= 'fixed', space='free')+
  #Apply my APA theme
  apatheme

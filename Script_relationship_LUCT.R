#### Script to test the relationship between land-use and land-cover transition rates to their ecological impacts ####

# Loading packages 

library(dplyr)
library(ggplot2)
library(viridis)
library(DHARMa)

# Loading data

data_ef <- read.csv("Results/efsize_median.csv", h=T)
data_transitions <- read.table("Data/Transition_rates.txt", h=T)

data_transitions[!data_transitions$transition %in% data_ef$transition, "transition"]

#Repeating data for bidirectional transitions: "SFyoung_MA", "MA_PA" and "SFyoung_PA"

data_ef <- rbind(data_ef, data_ef[data_ef$transition %in% c("MA_SFyoung", "PA_MA", "PA_SFyoung"),])

data_ef[(54-8):54,"transition"] <- rep(c("MA_PA","SFyoung_PA", "SFyoung_MA"), 3)

data_all <- left_join(x = data_ef, y = data_transitions, by = "transition")


biodiversity <- droplevels(data_all[data_all$Component=="biodiversity",])
carbon <- droplevels(data_all[data_all$Component=="carbon",])
soil <- droplevels(data_all[data_all$Component=="soil",])

#### Testing correlation ####

# Checking normality of effect sizes and log of transition's rates

shapiro.test(log(biodiversity$km2))
shapiro.test(biodiversity$median_es) # OK
shapiro.test(carbon$median_es) # No, Spearman instead of Pearson
shapiro.test(soil$median_es) # OK

cor.test(y=biodiversity$median_es, x=log(biodiversity$km2))
cor.test(y=carbon$median_es, x=log(carbon$km2), method = "spearman")
cor.test(y=soil$median_es, x=log(soil$km2))

# There is a negative correlation between transition rate and effect size for
# biodiversity and soil, but not for carbon


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
                           summarise(median(median_es)))
names(hline.data)[2] <- "median_es"


# Plotting the graph

ggplot(data_all, aes(y=median_es, x=log(km2), colour = factor(Component)))+
  #Add data points and color them black
  geom_point(size = 4)+
  geom_vline(aes(xintercept = median_km2), vline.data)+
  geom_hline(aes(yintercept = median_es), hline.data)+
  scale_x_continuous(name='log (km?/year)')+
  scale_colour_manual(values = c("#006D2C", "#F57D15", "#781C6D"))+
  ylim(c(0,2.8))+
  #labels of points
  geom_text(label = data_all$transition, 
            nudge_x = 0.030,
            nudge_y = 0.045,
            check_overlap = F, colour = "black")+
  #Give y-axis a meaningful label
  ylab('Effect size')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  facet_grid(cols = vars(Component), scales= 'fixed', space='free')+
  #Apply my APA theme
  apatheme

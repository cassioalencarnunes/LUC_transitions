#### Script to run  the analyses of land use and land cover transition effects on ecosystem variables ####
## Only for biodiversity variables considering only the richness of forest species! ##

# Loading packages

library(dplyr)
library(tidyr)
library(vegan)
library(lme4)
library(DHARMa)


# Loading data

ants_STM<-read.table("Data/ants_STM.txt",h=T)
ants_PGM<-read.table("Data/ants_PGM.txt",h=T)
bees_PGM<-read.table("Data/Bees_PGM.txt",h=T)
birds_matrix<-read.table("Data/Birds.txt",h=T)
birds_STM<-droplevels(birds_matrix[birds_matrix$Region=="STM",])
birds_PGM<-droplevels(birds_matrix[birds_matrix$Region=="PGM",])
db_matrix<-read.table("Data/Dung_beetles.txt",h=T)
db_STM<-droplevels(db_matrix[db_matrix$Region=="STM",])
db_PGM<-droplevels(db_matrix[db_matrix$Region=="PGM",])
lianas_STM<-read.table("Data/lianas_STM.txt",h=T)
lianas_PGM<-read.table("Data/lianas_PGM.txt",h=T)
sap_STM<-read.table("Data/saplings_STM.txt",h=T)
sap_PGM<-read.table("Data/saplings_PGM.txt",h=T)
trees_STM<-read.table("Data/trees_STM.txt",h=T)
trees_PGM<-read.table("Data/trees_PGM.txt",h=T)

# Loading transect classification
transect<-read.csv("Data/Transect_classification.csv", h=T, row.names = 1)
t_STM<-droplevels(transect[transect$Region=="STM",])
t_PGM<-droplevels(transect[transect$Region=="PGM",])
t_STM <- t_STM %>% 
  select(Transect_code, LU_UF)
t_PGM <- t_PGM %>% 
  select(Transect_code, LU_UF)

# Loading environmental data
environment <- read.csv("Data/environment.csv", h=T)
environment_PGM <- environment[environment$Region=="PGM",]
environment_STM <- environment[environment$Region=="STM",]


# Keeping only the valid transects

ants_STM<-droplevels(ants_STM[ants_STM$Transect_code %in% t_STM$Transect_code,])
ants_PGM<-droplevels(ants_PGM[ants_PGM$Transect_code %in% t_PGM$Transect_code,])
bees_PGM<-droplevels(bees_PGM[bees_PGM$Transect_code %in% t_PGM$Transect_code,])
birds_STM<-droplevels(birds_STM[birds_STM$Transect_code %in% t_STM$Transect_code,])
birds_PGM<-droplevels(birds_PGM[birds_PGM$Transect_code %in% t_PGM$Transect_code,])
db_STM<-droplevels(db_STM[db_STM$Transect_code %in% t_STM$Transect_code,])
db_PGM<-droplevels(db_PGM[db_PGM$Transect_code %in% t_PGM$Transect_code,])
lianas_STM<-droplevels(lianas_STM[lianas_STM$Transect_code %in% t_STM$Transect_code,])
lianas_PGM<-droplevels(lianas_PGM[lianas_PGM$Transect_code %in% t_PGM$Transect_code,])
sap_STM<-droplevels(sap_STM[sap_STM$Transect_code %in% t_STM$Transect_code,])
sap_PGM<-droplevels(sap_PGM[sap_PGM$Transect_code %in% t_PGM$Transect_code,])
trees_STM<-droplevels(trees_STM[trees_STM$Transect_code %in% t_STM$Transect_code,])
trees_PGM<-droplevels(trees_PGM[trees_PGM$Transect_code %in% t_PGM$Transect_code,])

# Environment
environment_STM<-droplevels(environment_STM[environment_STM$Transect_code %in% t_STM$Transect_code,])
environment_PGM<-droplevels(environment_PGM[environment_PGM$Transect_code %in% t_PGM$Transect_code,])


#### Only forest species ####

# At least one time in primary forest plots (irrespective of forest condition)

t_forest_STM <- t_STM[t_STM$LU_UF %in% c("UF", "LF", "LBF"),]
t_forest_PGM <- t_PGM[t_PGM$LU_UF %in% c("UF", "LF", "LBF"),]


#### Organising data frames to run the analyses ####

  # Dung Beetles
  db_STM <- left_join(db_STM, t_STM, by = "Transect_code")
  db_PGM <- left_join(db_PGM, t_PGM, by = "Transect_code")
  
  db_Non_Forest_STM <- db_STM %>% 
    filter(Transect_code %in% t_forest_STM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  db_STM_forest <- db_STM %>% 
    select(!db_Non_Forest_STM)
  
  db_Non_Forest_PGM <- db_PGM %>% 
    filter(Transect_code %in% t_forest_PGM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  db_PGM_forest <- db_PGM %>% 
    select(!db_Non_Forest_PGM)
  
    # Calculating species richness per transect
    db_STM_forest$rich<-specnumber(db_STM[,6:(ncol(db_STM)-1)])
    db_PGM_forest$rich<-specnumber(db_PGM[,6:(ncol(db_PGM)-1)])

  
  # Birds
  birds_STM <- left_join(birds_STM, t_STM, by = "Transect_code")
  birds_PGM <- left_join(birds_PGM, t_PGM, by = "Transect_code")
  
  birds_Non_Forest_STM <- birds_STM %>% 
    filter(Transect_code %in% t_forest_STM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  birds_STM_forest <- birds_STM %>% 
    select(!birds_Non_Forest_STM)
  
  birds_Non_Forest_PGM <- birds_PGM %>% 
    filter(Transect_code %in% t_forest_PGM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  birds_PGM_forest <- birds_PGM %>% 
    select(!birds_Non_Forest_PGM)
  
    # Calculating richness
    birds_STM_forest$rich<-specnumber(birds_STM[,5:(ncol(birds_STM)-1)])
    birds_PGM_forest$rich<-specnumber(birds_PGM[,5:(ncol(birds_PGM)-1)])

    
  # Ants
  ants_STM <- left_join(ants_STM, t_STM, by = "Transect_code")
  ants_PGM <- left_join(ants_PGM, t_PGM, by = "Transect_code")
  
  ants_Non_Forest_STM <- ants_STM %>% 
    filter(Transect_code %in% t_forest_STM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  ants_STM_forest <- ants_STM %>% 
    select(!ants_Non_Forest_STM)
  
  ants_Non_Forest_PGM <- ants_PGM %>% 
    filter(Transect_code %in% t_forest_PGM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  ants_PGM_forest <- ants_PGM %>% 
    select(!ants_Non_Forest_PGM)
  
    # Calculating richness
    ants_STM_forest$rich<-specnumber(ants_STM[,5:(ncol(ants_STM)-1)])
    ants_PGM_forest$rich<-specnumber(ants_PGM[,5:(ncol(ants_PGM)-1)])

    #Excluding NA's
    ants_STM<-droplevels(na.omit(ants_STM))
    

  # Trees
  trees_STM <- left_join(trees_STM, t_STM, by = "Transect_code")
  trees_PGM <- left_join(trees_PGM, t_PGM, by = "Transect_code")
  
  trees_Non_Forest_STM <- trees_STM %>% 
    filter(Transect_code %in% t_forest_STM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  trees_STM_forest <- trees_STM %>% 
    select(!trees_Non_Forest_STM)
  
  trees_Non_Forest_PGM <- trees_PGM %>% 
    filter(Transect_code %in% t_forest_PGM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  trees_PGM_forest <- trees_PGM %>% 
    select(!trees_Non_Forest_PGM)
  
    # Calculating richness
    trees_STM_forest$rich<-specnumber(trees_STM[,5:(ncol(trees_STM)-1)])
    trees_PGM_forest$rich<-specnumber(trees_PGM[,5:(ncol(trees_PGM)-1)])


  # Saplings
  sap_STM <- left_join(sap_STM, t_STM, by = "Transect_code")
  sap_PGM <- left_join(sap_PGM, t_PGM, by = "Transect_code")
  
  sap_Non_Forest_STM <- sap_STM %>% 
    filter(Transect_code %in% t_forest_STM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  sap_STM_forest <- sap_STM %>% 
    select(!sap_Non_Forest_STM)
  
  sap_Non_Forest_PGM <- sap_PGM %>% 
    filter(Transect_code %in% t_forest_PGM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  sap_PGM_forest <- sap_PGM %>% 
    select(!sap_Non_Forest_PGM)

    # Calculating richness
    sap_STM_forest$rich<-specnumber(sap_STM[,5:(ncol(sap_STM)-1)])
    sap_PGM_forest$rich<-specnumber(sap_PGM[,5:(ncol(sap_PGM)-1)])

  # Lianas
  lianas_STM <- left_join(lianas_STM, t_STM, by = "Transect_code")
  lianas_PGM <- left_join(lianas_PGM, t_PGM, by = "Transect_code")
  
  lianas_STM$LU<-t_STM$LU_FT_Code
  lianas_STM$LU_UF<-t_STM$LU_UF
  lianas_PGM$LU<-t_PGM$LU_FT_Code
  lianas_PGM$LU_UF<-t_PGM$LU_UF
  
  lianas_Non_Forest_STM <- lianas_STM %>% 
    filter(Transect_code %in% t_forest_STM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  lianas_STM_forest <- lianas_STM %>% 
    select(!lianas_Non_Forest_STM)
  
  lianas_Non_Forest_PGM <- lianas_PGM %>% 
    filter(Transect_code %in% t_forest_PGM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  lianas_PGM_forest <- lianas_PGM %>% 
    select(!lianas_Non_Forest_PGM)

    # Calculating richness
    lianas_STM_forest$rich<-specnumber(lianas_STM[,5:(ncol(lianas_STM)-1)])
    lianas_PGM_forest$rich<-specnumber(lianas_PGM[,5:(ncol(lianas_PGM)-1)])
    

  # Orchid bees
  bees_PGM <- left_join(bees_PGM, t_PGM, by = "Transect_code")
  
  bees_PGM <- na.omit(bees_PGM) 
  
  bees_Non_Forest_PGM <- bees_PGM %>% 
    filter(Transect_code %in% t_forest_PGM$Transect_code) %>% 
    select(where(is.numeric)) %>% 
    select(where(~sum(.) == 0)) %>% 
    names()
  
  bees_PGM_forest <- bees_PGM %>% 
    select(!bees_Non_Forest_PGM)

    # Calculating richness
    bees_PGM_forest$rich<-specnumber(bees_PGM[,5:(ncol(bees_PGM)-1)])



#### Creating the data frames with species richness of all groups and the other ecosystem variables per transect ####

# PGM region
    
data_joined_PGM <- ants_PGM_forest %>% select(Region, Catchment, Transect, Transect_code, LU_UF, rich) %>% 
  rename(ant_rich = rich) %>% 
  left_join(bees_PGM_forest %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(bee_rich = rich) %>%
  left_join(birds_PGM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(bird_rich = rich) %>%
  left_join(db_PGM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(db_rich = rich) %>%
  left_join(lianas_PGM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(liana_rich = rich) %>%
  left_join(sap_PGM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(sap_rich = rich) %>%
  left_join(trees_PGM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(tree_rich = rich) %>% 
  left_join(environment_PGM %>% select(-c(1:3,5)), by = "Transect_code")

# STM region
names(ants_STM_forest)
data_joined_STM <- ants_STM_forest %>% select(Region, Catchment, Transect, Transect_code, LU_UF, rich) %>% 
  rename(ant_rich = rich) %>% 
  left_join(birds_STM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(bird_rich = rich) %>%
  left_join(db_STM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(db_rich = rich) %>%
  left_join(lianas_STM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(liana_rich = rich) %>%
  left_join(sap_STM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(sap_rich = rich) %>%
  left_join(trees_STM_forest %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(tree_rich = rich) %>% 
  left_join(environment_STM %>% select(-c(1:3,5)), by = "Transect_code")

data_joined_PGM$Catchment <- as.numeric(data_joined_PGM$Catchment)

data_all_joined <- bind_rows(data_joined_STM, data_joined_PGM)

data_all_joined <- data_all_joined[data_all_joined$LU_UF %in% c("UF", "LF", "LBF", 
                                                                "MA", "PA", "SFyoung",
                                                                "SFold"),]


data_all_joined_scaled <- data_all_joined

data_all_joined_scaled[,6:ncol(data_all_joined_scaled)] <- scale(data_all_joined[,6:ncol(data_all_joined_scaled)], center = T, scale = T)

summary(data_all_joined_scaled)


write.csv(data_all_joined, "Data/data_rich_forest_sp.csv")


# Creating an object with all transitions, i.e. contrasts to be run

transitions <- c("UF - LF = 0",
                 "UF - LBF = 0",
                 "UF - PA = 0",
                 "UF - MA = 0",
                 "LF - LBF = 0",
                 "LF - PA = 0",
                 "LF - MA = 0",
                 "LBF - PA = 0",
                 "LBF - MA = 0",
                 "PA - MA = 0",
                 "PA - SFyoung = 0",
                 "MA - SFyoung = 0",
                 "SFold - PA = 0",
                 "SFold - MA = 0",
                 "SFyoung - SFold = 0")


#### LMMs - Species richness ####

# Each model has four explanatory variable, being Land use the variable of interest and
# Clay, elevation and slope co-variables. It also has catchment and region as random factors


biodiversity_variables <- c("ant_rich", "bird_rich", "db_rich", "liana_rich", "sap_rich", "tree_rich")

efsize_final_bio <- NULL
model_data_final_bio <- NULL

for (i in biodiversity_variables){
  
  m1_bio <- lmer(data_all_joined_scaled[,i] ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN + (1|Catchment) + (1|Region),
                 data = data_all_joined_scaled)
  
  model_checking_bio <- simulateResiduals(m1_bio)
  disp_param_bio <- testDispersion(model_checking_bio)
  hetero_param_bio <- testCategorical(model_checking_bio, catPred = data_all_joined_scaled[!is.na(data_all_joined_scaled[i]), "LU_UF"])
  
  anova_test_bio <- car::Anova(m1_bio)
  
  contrast_LU_bio <- multcomp::glht(m1_bio, linfct = multcomp::mcp(LU_UF = transitions))
  contrast_LU_test_bio <- summary(contrast_LU_bio)
  contrast_LU_ci_bio <- confint(contrast_LU_bio, level = 0.95)
  
  efsize_bio <- as.data.frame(contrast_LU_ci_bio$confint)
  efsize_bio$p_value <- contrast_LU_test_bio$test$pvalues
  efsize_bio$std_error <- contrast_LU_test_bio$test$sigma
  efsize_bio$variable <- i
  
  model_data_bio <- as.data.frame(anova_test_bio)
  model_data_bio$disp_param <- disp_param_bio$p.value
  model_data_bio$hetero_param <- hetero_param_bio$homogeneity$`Pr(>F)`[1]
  model_data_bio$variable <- i
  
  efsize_final_bio <- rbind(efsize_final_bio, efsize_bio)
  model_data_final_bio <- rbind(model_data_final_bio, model_data_bio)
  
  efsize_bio <- NULL
  model_data_bio <- NULL
  
}


#### LMM - Orchid Bee species richness ####

bee_PGM_data <- data_all_joined_scaled[data_all_joined_scaled$Region=="PGM",]


m1_bee <- lmer(bee_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN + (1|Catchment),
               data = data_all_joined_scaled)

model_checking_bee <- simulateResiduals(m1_bee)
disp_param_bee <- testDispersion(model_checking_bee)
hetero_param_bee <- testCategorical(model_checking_bee, catPred = data_all_joined_scaled[!is.na(data_all_joined_scaled$bee_rich), "LU_UF"])

anova_test_bee <- car::Anova(m1_bee)

contrast_LU_bee <- multcomp::glht(m1_bee, linfct = multcomp::mcp(LU_UF = transitions))
contrast_LU_test_bee <- summary(contrast_LU_bee)
contrast_LU_ci_bee <- confint(contrast_LU_bee, level = 0.95)

efsize_bee <- as.data.frame(contrast_LU_ci_bee$confint)
efsize_bee$p_value <- contrast_LU_test_bee$test$pvalues
efsize_bee$std_error <- contrast_LU_test_bee$test$sigma
efsize_bee$variable <- "bee_rich"

model_data_bee <- as.data.frame(anova_test_bee)
model_data_bee$disp_param <- disp_param_bee$p.value
model_data_bee$hetero_param <- hetero_param_bee$homogeneity$`Pr(>F)`[1]
model_data_bee$variable <- "bee_rich"

efsize_final_bio <- rbind(efsize_final_bio, efsize_bee)
model_data_final_bio <- rbind(model_data_final_bio, model_data_bee)



#### Saving biodiversity data in csv files ####

model_data_final_bio$explanatory <- rep(c("LU_UF", "CLAY_ALL", "ELEV_MEAN", "SLOPE_MEAN"), 7)
efsize_final_bio$transition <- rep(c("UF_LF", "UF_LBF", "UF_PA", "UF_MA", "LF_LBF",
                                     "LF_PA", "LF_MA" , "LBF_PA", "LBF_MA", "PA_MA", 
                                     "PA_SFyoung", "MA_SFyoung", "SFold_PA", "SFold_MA",
                                     "SFyoung_SFold"), 7)
write.csv(x = model_data_final_bio, file = "Results/model_data_bio_forest.csv", row.names = F)
write.csv(x = efsize_final_bio, file = "Results/efsize_bio_forest.csv", row.names = F)

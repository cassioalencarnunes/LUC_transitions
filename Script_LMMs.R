#### Script to run  the analyses of land use and land cover transition effects on ecosystem variables ####
      ## The script runs LMMs for each response variable ##
# Each model has four explanatory variable, being Land use the variable of interest and
# Clay, elevation and slope co-variables. It also has catchment and region as random factors


# Loading packages

library(dplyr)
library(tidyr)
library(lme4)
library(DHARMa)

# Loading data

data_all_joined <- read.csv("Data/data_all_joined.csv", h=T)

# Centring and Scaling data

data_all_joined_scaled <- data_all_joined

data_all_joined_scaled[,6:ncol(data_all_joined_scaled)] <- scale(data_all_joined[,6:ncol(data_all_joined_scaled)], center = T, scale = T)

summary(data_all_joined_scaled)

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


#### LMMs - Carbon pools and Soil properties ####

CS_variables <- c("Aboveground_pool", "Litter_pool", "Dead_pool", "Soil_pool_new", "pH", "Nperc",
                        "P", "K", "Na", "CaMg", "Al")

efsize_final <- NULL
model_data_final <- NULL

for (i in CS_variables){
    
  m1 <- lmer(data_all_joined_scaled[,i] ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN + (1|Catchment) + (1|Region), data = data_all_joined_scaled)

  model_checking <- simulateResiduals(m1)
  disp_param <- testDispersion(model_checking)
  hetero_param <- testCategorical(model_checking, catPred = data_all_joined_scaled[!is.na(data_all_joined_scaled[i]), "LU_UF"])
  plotQQunif(model_checking)
  
  anova_test <- car::Anova(m1)
    
  contrast_LU <- multcomp::glht(m1, linfct = multcomp::mcp(LU_UF = transitions))
  contrast_LU_test <- summary(contrast_LU)
  contrast_LU_ci <- confint(contrast_LU, level = 0.95)
  
  efsize <- as.data.frame(contrast_LU_ci$confint)
  efsize$p_value <- contrast_LU_test$test$pvalues
  efsize$std_error <- contrast_LU_test$test$sigma
  efsize$variable <- i
  
  model_data <- as.data.frame(anova_test)
  model_data$disp_param <- disp_param$p.value
  model_data$hetero_param <- hetero_param$homogeneity$`Pr(>F)`[1]
  model_data$variable <- i
    
  efsize_final <- rbind(efsize_final, efsize)
  model_data_final <- rbind(model_data_final, model_data)
  
  efsize <- NULL
  model_data<- NULL

}

# Saving the data in csv files

model_data_final$explanatory <- rep(c("LU_UF", "CLAY_ALL", "ELEV_MEAN", "SLOPE_MEAN"), 11)
efsize_final$transition <- rep(c("UF_LF", "UF_LBF", "UF_PA", "UF_MA", "LF_LBF",
                                 "LF_PA", "LF_MA" , "LBF_PA", "LBF_MA", "PA_MA", 
                                 "PA_SFyoung", "MA_SFyoung", "SFold_PA", "SFold_MA",
                                 "SFyoung_SFold"), 11)
write.csv(x = model_data_final, file = "Results/model_data_carbon_soil.csv", row.names = F)
write.csv(x = efsize_final, file = "Results/efsize_carbon_soil.csv", row.names = F)



#### LMMs - Species richness ####

biodiversity_variables <- c("ant_rich", "bird_rich", "db_rich", "liana_rich", "sap_rich", "tree_rich")
  
efsize_final_bio <- NULL
model_data_final_bio <- NULL

for (i in biodiversity_variables){
  
  m1_bio <- lmer(data_all_joined_scaled[,i] ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN + (1|Catchment) + (1|Region),
                  data = data_all_joined_scaled)
  
  model_checking_bio <- simulateResiduals(m1_bio)
  disp_param_bio <- testDispersion(model_checking_bio)
  hetero_param_bio <- testCategorical(model_checking_bio, catPred = data_all_joined_scaled[!is.na(data_all_joined_scaled[i]), "LU_UF"])
  plotQQunif(model_checking)
  
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


#### LM - Orchid Bee species richness  - Separated because it was not sampled in STM region #### 

bee_PGM_data <- data_all_joined_scaled[data_all_joined_scaled$Region=="PGM",]


m1_bee <- lmer(bee_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN + (1|Catchment),
               data = bee_PGM_data)

model_checking_bee <- simulateResiduals(m1_bee)
disp_param_bee <- testDispersion(model_checking_bee)
hetero_param_bee <- testCategorical(model_checking_bee, catPred = bee_PGM_data[!is.na(bee_PGM_data$bee_rich), "LU_UF"])
plotQQunif(model_checking)

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
write.csv(x = model_data_final_bio, file = "Results/model_data_bio.csv", row.names = F)
write.csv(x = efsize_final_bio, file = "Results/efsize_bio.csv", row.names = F)



#### Joining all results in one data frame ####

model_data <- rbind(model_data_final_bio, model_data_final)
efsize_data <- rbind(efsize_final_bio, efsize_final)

write.csv(x = model_data, file = "Results/model_data.csv", row.names = F)
write.csv(x = efsize_data, file = "results/efsize_data.csv", row.names = F)


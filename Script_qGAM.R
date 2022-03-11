#### Script to run validation analysis ####
      ## The script runs qGAMs for each response variable ##
# Each model has four explanatory variable, being Land use the variable of interest and
# Clay, elevation and slope co-variates. It also has catchment and region as random factors


# Loading packages

library(dplyr)
library(tidyr)
library(qgam)
library(mgcViz)

# Loading data

data_all_joined <- read.csv("Data/data_all_joined.csv", h=T)

# Centring and Scaling data

data_all_joined_scaled <- data_all_joined

data_all_joined_scaled[,6:ncol(data_all_joined_scaled)] <- scale(data_all_joined[,6:ncol(data_all_joined_scaled)], center = T, scale = T)

summary(data_all_joined_scaled)

data_all_joined_scaled$Region <- as.factor(data_all_joined_scaled$Region)
data_all_joined_scaled$Catchment <- as.factor(data_all_joined_scaled$Catchment)
data_all_joined_scaled$Transect_code <- as.factor(data_all_joined_scaled$Transect_code)
data_all_joined_scaled$LU_UF <- as.factor(data_all_joined_scaled$LU_UF)

# Creating an object with all transitions, i.e. contrasts to be run

transitions <- c("LU_UFUF - LU_UFLF = 0",
                 "LU_UFUF - (Intercept) = 0",
                 "LU_UFUF - LU_UFPA = 0",
                 "LU_UFUF - LU_UFMA = 0",
                 "LU_UFLF - (Intercept) = 0",
                 "LU_UFLF - LU_UFPA = 0",
                 "LU_UFLF - LU_UFMA = 0",
                 "(Intercept) - LU_UFPA = 0",
                 "(Intercept) - LU_UFMA = 0",
                 "LU_UFPA - LU_UFMA = 0",
                 "LU_UFPA - LU_UFSFyoung = 0",
                 "LU_UFMA - LU_UFSFyoung = 0",
                 "LU_UFSFold - LU_UFPA = 0",
                 "LU_UFSFold - LU_UFMA = 0",
                 "LU_UFSFyoung - LU_UFSFold = 0")

#### GAMs - Carbon pools and Soil properties ####


Aboveground_pool <- qgam(Aboveground_pool ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                         + s(Region, bs = "re") + s(Catchment, bs = "re"),
                         data = data_all_joined_scaled, qu = 0.5)
Litter_pool <- qgam(Litter_pool ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                         + s(Region, bs = "re") + s(Catchment, bs = "re"),
                         data = data_all_joined_scaled, qu = 0.5)
Dead_pool <- qgam(Dead_pool ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                         + s(Region, bs = "re") + s(Catchment, bs = "re"),
                         data = data_all_joined_scaled, qu = 0.5)
Soil_pool_new <- qgam(Soil_pool_new ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                         + s(Region, bs = "re") + s(Catchment, bs = "re"),
                         data = data_all_joined_scaled, qu = 0.5)

pH <- qgam(pH ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                      + s(Region, bs = "re") + s(Catchment, bs = "re"),
                      data = data_all_joined_scaled, qu = 0.5)
Nperc <- qgam(Nperc ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                      + s(Region, bs = "re") + s(Catchment, bs = "re"),
                      data = data_all_joined_scaled, qu = 0.5)
P <- qgam(P ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                      + s(Region, bs = "re") + s(Catchment, bs = "re"),
                      data = data_all_joined_scaled, qu = 0.5)
K <- qgam(K ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                      + s(Region, bs = "re") + s(Catchment, bs = "re"),
                      data = data_all_joined_scaled, qu = 0.5)
Na <- qgam(Na ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                      + s(Region, bs = "re") + s(Catchment, bs = "re"),
                      data = data_all_joined_scaled, qu = 0.5)
CaMg <- qgam(CaMg ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
           + s(Region, bs = "re") + s(Catchment, bs = "re"),
           data = data_all_joined_scaled, qu = 0.5)
CaMg3 <- qgam(CaMg ~ LU_UF + CLAY_ALL + ELEV_MEAN + s(SLOPE_MEAN) 
             + s(Region, bs = "re") + s(Catchment, bs = "re"),
             data = data_all_joined_scaled, qu = 0.5)
Al <- qgam(Al ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
             + s(Region, bs = "re") + s(Catchment, bs = "re"),
             data = data_all_joined_scaled, qu = 0.5)

CS_variables <- c("Aboveground_pool", "Litter_pool", "Dead_pool", "Soil_pool_new", "pH", "Nperc",
                  "P", "K", "Na", "CaMg", "Al")

ef_size_qgam <- NULL
anova_test_pvalue_final <- NULL
anova_test_random_final <- NULL


for (i in CS_variables){
    
  anova_test <- mgcv::anova.gam(get(i))
  anova_test_pvalue <- as.data.frame(anova_test$pTerms.table)
  anova_test_random <- as.data.frame(anova_test$s.table)
  anova_test_pvalue$variable <- i
  anova_test_random$variable <- i

  contrast_LU <- multcomp::glht(get(i), linfct = transitions)
  contrast_LU_test <- summary(contrast_LU)
  contrast_LU_ci <- confint(contrast_LU, level = 0.95)
  
  efsize <- as.data.frame(contrast_LU_ci$confint)
  efsize$p_value <- contrast_LU_test$test$pvalues
  efsize$std_error <- contrast_LU_test$test$sigma
  efsize$variable <- i

  ef_size_qgam <- rbind(ef_size_qgam, efsize)
  anova_test_pvalue_final <- rbind(anova_test_pvalue_final, anova_test_pvalue)
  anova_test_random_final <- rbind(anova_test_random_final, anova_test_random)
  
  efsize <- NULL
  anova_test <- NULL
  anova_test_pvalue <- NULL
  anova_test_random <- NULL

}

# Saving the data in csv files

anova_test_pvalue_final$explanatory <- rep(c("LU_UF", "CLAY_ALL", "ELEV_MEAN", "SLOPE_MEAN"), 11)
anova_test_random_final$explanatory <- rep(c("Region", "Catchment"), 11)
ef_size_qgam$transition <- rep(c("UF_LF", "UF_LBF", "UF_PA", "UF_MA", "LF_LBF",
                                 "LF_PA", "LF_MA" , "LBF_PA", "LBF_MA", "PA_MA", 
                                 "PA_SFyoung", "MA_SFyoung", "SFold_PA", "SFold_MA",
                                 "SFyoung_SFold"), 11)

#### GAMs - Species richness ####

ant_rich <- qgam(ant_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                         + s(Region, bs = "re") + s(Catchment, bs = "re"),
                         data = data_all_joined_scaled, qu = 0.5)
bird_rich <- qgam(bird_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                    + s(Region, bs = "re") + s(Catchment, bs = "re"),
                    data = data_all_joined_scaled, qu = 0.5)
db_rich <- qgam(db_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                  + s(Region, bs = "re") + s(Catchment, bs = "re"),
                  data = data_all_joined_scaled, qu = 0.5)
liana_rich <- qgam(liana_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                      + s(Region, bs = "re") + s(Catchment, bs = "re"),
                      data = data_all_joined_scaled, qu = 0.5)

sap_rich <- qgam(sap_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
           + s(Region, bs = "re") + s(Catchment, bs = "re"),
           data = data_all_joined_scaled, qu = 0.5)
tree_rich <- qgam(tree_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
              + s(Region, bs = "re") + s(Catchment, bs = "re"),
              data = data_all_joined_scaled, qu = 0.5)

biodiversity_variables <- c("ant_rich", "bird_rich", "db_rich", "liana_rich", "sap_rich", "tree_rich")

ef_size_qgam_bio <- NULL
anova_test_pvalue_final_bio <- NULL
anova_test_random_final_bio <- NULL

for (i in biodiversity_variables){
  
  anova_test <- mgcv::anova.gam(get(i))
  anova_test_pvalue <- as.data.frame(anova_test$pTerms.table)
  anova_test_random <- as.data.frame(anova_test$s.table)
  anova_test_pvalue$variable <- i
  anova_test_random$variable <- i
  
  contrast_LU <- multcomp::glht(get(i), linfct = transitions)
  contrast_LU_test <- summary(contrast_LU)
  contrast_LU_ci <- confint(contrast_LU, level = 0.95)
  
  efsize <- as.data.frame(contrast_LU_ci$confint)
  efsize$p_value <- contrast_LU_test$test$pvalues
  efsize$std_error <- contrast_LU_test$test$sigma
  efsize$variable <- i
  
  ef_size_qgam_bio <- rbind(ef_size_qgam_bio, efsize)
  anova_test_pvalue_final_bio <- rbind(anova_test_pvalue_final_bio, anova_test_pvalue)
  anova_test_random_final_bio <- rbind(anova_test_random_final_bio, anova_test_random)
  
  efsize <- NULL
  anova_test <- NULL
  anova_test_pvalue <- NULL
  anova_test_random <- NULL
  
}

#### GAM - Orchid Bee species richness  - Separated because it was not sampled in STM region #### 

bee_PGM_data <- droplevels(data_all_joined_scaled[data_all_joined_scaled$Region=="PGM",])


bee_rich <- qgam(bee_rich ~ LU_UF + CLAY_ALL + ELEV_MEAN + SLOPE_MEAN 
                 + s(Catchment, bs = "re"),
                 data = bee_PGM_data, qu = 0.5)

anova_test <- mgcv::anova.gam(bee_rich)
anova_test_pvalue <- as.data.frame(anova_test$pTerms.table)
anova_test_random <- as.data.frame(anova_test$s.table)
anova_test_pvalue$variable <- "bee_rich"
anova_test_random$variable <- "bee_rich"

contrast_LU <- multcomp::glht(bee_rich, linfct = transitions)
contrast_LU_test <- summary(contrast_LU)
contrast_LU_ci <- confint(contrast_LU, level = 0.95)

efsize <- as.data.frame(contrast_LU_ci$confint)
efsize$p_value <- contrast_LU_test$test$pvalues
efsize$std_error <- contrast_LU_test$test$sigma
efsize$variable <- "bee_rich"

ef_size_qgam_bio <- rbind(ef_size_qgam_bio, efsize)
anova_test_pvalue_final_bio <- rbind(anova_test_pvalue_final_bio, anova_test_pvalue)
anova_test_random_final_bio <- rbind(anova_test_random_final_bio, anova_test_random)


#### Saving biodiversity data in csv files ####

anova_test_pvalue_final_bio$explanatory <- rep(c("LU_UF", "CLAY_ALL", "ELEV_MEAN", "SLOPE_MEAN"), 7)
anova_test_random_final_bio$explanatory <- c(rep(c("Region", "Catchment"), 6), "Catchment")
ef_size_qgam_bio$transition <- rep(c("UF_LF", "UF_LBF", "UF_PA", "UF_MA", "LF_LBF",
                                     "LF_PA", "LF_MA" , "LBF_PA", "LBF_MA", "PA_MA", 
                                     "PA_SFyoung", "MA_SFyoung", "SFold_PA", "SFold_MA",
                                     "SFyoung_SFold"), 7)



#### Joining all results in one data frame ####

model_data <- rbind(anova_test_pvalue_final_bio, anova_test_pvalue_final)
efsize_data <- rbind(ef_size_qgam_bio, ef_size_qgam)

write.csv(x = model_data, file = "Results/qGAM_model_data.csv", row.names = F)
write.csv(x = efsize_data, file = "results/qGAM_efsize_data.csv", row.names = F)


########################################################################################


#### Validation analysis - Correlation between effect sizes ####

ef_size_lmm<- read.csv("Results/efsize_data.csv")
ef_size_qgam <- read.csv("Results/qGAM_efsize_data.csv")


ef_size_qgam$Estimate_lmm <- ef_size_lmm$Estimate

ef_size_qgam$Estimate_lmm <- ef_size_qgam$Estimate_lmm*-1
ef_size_qgam$Estimate <- ef_size_qgam$Estimate*-1

ef_size_qgam$variable <- as.factor(ef_size_qgam$variable)


# Correlation Analysis

z_final <- NULL

for (i in levels(ef_size_qgam$variable)){
  x <- cor(x = ef_size_qgam[ef_size_qgam$variable== i, "Estimate"], y = ef_size_qgam[ef_size_qgam$variable== i, "Estimate_lmm"])
  z <- cbind(x, i)
  z_final <- rbind(z_final, z)
}

z_final

## All r coefficients considerably high (>0.95)

# Graph

library(ggplot2)

ggplot(ef_size_qgam, aes(y=Estimate, x=Estimate_lmm))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = F)+
  labs(x = "LMM effect size", y = "qGAM effect size")+
  facet_wrap(variable~., scale = "free")+
  theme_classic()

#### Script to run  the analyses of land use and land cover transition effects on ecosystem variables ####
## The script runs GLMMs for each variable and each transition ##
## Only for biodiversity variables considering only the richness of forest species! ##

# Loading packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(lme4)
library(performance)
library(EnvStats)
library(MuMIn)

source(file = "Over_function.txt")

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
  rename(tree_rich = rich)

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
  rename(tree_rich = rich)

data_joined_PGM$Catchment <- as.numeric(data_joined_PGM$Catchment)
data_all_joined <- bind_rows(data_joined_STM, data_joined_PGM)

write.csv(data_all_joined, "Data/data_rich_forest_sp.csv")


#### Creating the tables for each transition ####

transitions <- cbind(c("UF","UF","UF", "UF","LF","LF","LF","LBF","LBF","PA","PA","MA","SFyoung","SFold","SFold"),
                     c("LF", "LBF","PA","MA","LBF","PA","MA","PA","MA","MA","SFyoung","SFyoung","SFold","PA","MA"))

row.names(transitions) <- paste(transitions[,1], "_", transitions[,2], sep = "")

data_all_joined$LU_UF <- as.factor(data_all_joined$LU_UF)
list_transitions <- list()

for (i in 1:15){
  
  list_transitions[[i]] <- data_all_joined %>%
    filter(LU_UF == transitions[i,1] | LU_UF == transitions[i,2]) %>%
    mutate(LU_UF = droplevels(LU_UF)) %>% 
    mutate(LU_UF = factor(LU_UF, levels = transitions[i,]))
  
}

names(list_transitions) <- row.names(transitions)

#### GLMMs - species richness ####

biodiversity_variables <- c("ant_rich", "bird_rich", "db_rich", "liana_rich", "sap_rich", "tree_rich")

coef_LU_final_bio <- NULL
coef_ranef_final_bio <- NULL
anova_test_final_bio <- NULL

list_results_LU_bio <- list()
list_results_final_bio <- list()


for (i in names(list_transitions)){
  
  data_for_GLMM_bio <- list_transitions[[i]]
  
  for (j in biodiversity_variables){
    
    m1_bio <- glmer(data_for_GLMM_bio[,j] ~ LU_UF + (1|Region), family = "poisson", control = glmerControl(optimizer="bobyqa", tolPwrss=1e-3, optCtrl = list(maxfun = 100000)),
                    data = data_for_GLMM_bio)
    over_test <- check_overdispersion(m1_bio)
    
    if (over_test$p_value >= 0.05){
      
      m0_bio <- glmer(data_for_GLMM_bio[,j] ~ (1|Region), family = "poisson", control = glmerControl(optimizer="bobyqa", tolPwrss=1e-3, optCtrl = list(maxfun = 100000)),
                      data = data_for_GLMM_bio)
      
      anova_test_bio <- anova(m0_bio, m1_bio)
      
      coef_LU_bio <- as.data.frame(t(m1_bio@beta))
      colnames(coef_LU_bio) <- levels(data_for_GLMM_bio$LU_UF)
      coef_LU_bio$variable <- j
      coef_LU_bio$family <- "Poisson"
      coef_LU_bio$p_value <- anova_test_bio$`Pr(>Chisq)`[2]
      coef_LU_bio$R2m <- r.squaredGLMM(m1_bio)[3,1]
      coef_LU_bio$R2c <- r.squaredGLMM(m1_bio)[3,2]
      coef_LU_bio$AIC <- anova_test_bio$AIC[2]
      coef_ranef_bio <- as.data.frame(t(ranef(m1_bio)$Region))
      coef_ranef_bio$variable <- j
      
      
      anova_test_final_bio <- rbind(anova_test_final_bio,anova_test_bio)
      coef_LU_final_bio <- rbind(coef_LU_final_bio, coef_LU_bio)
      coef_ranef_final_bio <- rbind(coef_ranef_final_bio, coef_ranef_bio)
      
    }
    
    else {
      
      print(j)
      m2_bio <- glmer.nb(data_for_GLMM_bio[,j] ~ LU_UF + (1|Region), control = glmerControl(optimizer="bobyqa", tolPwrss=1e-3, optCtrl = list(maxfun = 100000)),
                         data = data_for_GLMM_bio)
      m0.2_bio <- glmer.nb(data_for_GLMM_bio[,j] ~ (1|Region), control = glmerControl(optimizer="bobyqa", tolPwrss=1e-3, optCtrl = list(maxfun = 100000)),
                           data = data_for_GLMM_bio)
      
      anova_test_bio <- anova(m0.2_bio, m2_bio)
      
      coef_LU_bio <- as.data.frame(t(m2_bio@beta))
      colnames(coef_LU_bio) <- levels(data_for_GLMM_bio$LU_UF)
      coef_LU_bio$variable <- j 
      coef_LU_bio$family <- "Negative Binomial"
      coef_LU_bio$p_value <- anova_test_bio$`Pr(>Chisq)`[2]
      coef_LU_bio$R2m <- r.squaredGLMM(m2_bio)[3,1]
      coef_LU_bio$R2c <- r.squaredGLMM(m2_bio)[3,2]
      coef_LU_bio$AIC <- anova_test_bio$AIC[2]
      coef_ranef_bio <- as.data.frame(t(ranef(m2_bio)$Region))
      coef_ranef_bio$variable <- j
      
      anova_test_final_bio <- rbind(anova_test_final_bio,anova_test_bio)
      coef_LU_final_bio <- rbind(coef_LU_final_bio, coef_LU_bio)
      coef_ranef_final_bio <- rbind(coef_ranef_final_bio, coef_ranef_bio)
    }
    
  }
  
  list_results_LU_bio[[1]] <- anova_test_final_bio
  list_results_LU_bio[[2]] <- coef_LU_final_bio
  list_results_LU_bio[[3]] <- coef_ranef_final_bio
  
  list_results_final_bio[[i]] <- list_results_LU_bio
  
  coef_LU_final_bio <- NULL
  coef_ranef_final_bio <- NULL
  anova_test_final_bio <- NULL
  
  list_results_LU_bio <- NULL
  
}



for (i in names(list_results_final_bio)){
  
  data_frame_to_save <- list_results_final_bio[[i]][[2]]
  name_file <- paste("bio", "_", i, "_", "forest", ".csv", sep="")
  write.csv(x = data_frame_to_save, file = paste("Results/", name_file, sep = ""))
  
}


#### GLMMs - Orchid Bee species richness ####


coef_LU_final_bee <- NULL
anova_test_final_bee <- NULL

list_results_LU_bee <- list()
list_results_final_bee <- list()


for (i in names(list_transitions)){
  
  data_for_GLM_bee <- list_transitions[[i]]
  
  data_for_GLM_bee <- droplevels(data_for_GLM_bee[data_for_GLM_bee$Region == "PGM",])
  
  m1_bee <- glm(bee_rich ~ LU_UF, family = "poisson", data = data_for_GLM_bee)
  over_test <- check_overdispersion(m1_bee)
  
  if (over_test$p >= 0.05){
    
    m0_bee <- glm(bee_rich ~ 1, family = "poisson", data = data_for_GLM_bee)
    
    anova_test_bee <- anova(m0_bee, m1_bee, test = "Chisq")
    
    coef_LU_bee <- as.data.frame(t(m1_bee$coefficients))
    colnames(coef_LU_bee) <- levels(data_for_GLM_bee$LU_UF)
    coef_LU_bee$variable <- "bee_rich"
    coef_LU_bee$family <- "Poisson"
    coef_LU_bee$p_value <- anova_test_bee$`Pr(>Chi)`[2]
    coef_LU_bee$R2 <- r.squaredGLMM(m1_bee)[3]
    coef_LU_bee$AIC <- m1_bee$aic
    
    
    anova_test_final_bee <- rbind(anova_test_final_bee,anova_test_bee)
    coef_LU_final_bee <- rbind(coef_LU_final_bee, coef_LU_bee)
    
  }
  
  else {
    
    m2_bee <- glm(bee_rich ~ LU_UF, family = "quasipoisson", data = data_for_GLM_bee)
    m0.2_bee <- glm(bee_rich ~ 1, family = "quasipoisson", data = data_for_GLM_bee)
    
    anova_test_bee <- anova(m0.2_bee, m2_bee, test = "Chisq")
    
    coef_LU_bee <- as.data.frame(t(m2_bee$coefficients))
    colnames(coef_LU_bee) <- levels(data_for_GLM_bee$LU_UF)
    coef_LU_bee$variable <- "bee_rich"
    coef_LU_bee$family <- "Quasipoisson"
    coef_LU_bee$p_value <- anova_test_bee$`Pr(>Chi)`[2]
    coef_LU_bee$R2 <- r.squaredGLMM(m2_bee)[3,1]
    coef_LU_bee$AIC <- QAIC(object = m1_bee,chat =  deviance(m1_bee) / df.residual(m1_bee))
    
    anova_test_final_bee <- rbind(anova_test_final_bee,anova_test_bee)
    coef_LU_final_bee <- rbind(coef_LU_final_bee, coef_LU_bee)
  }
  
  list_results_LU_bee[[1]] <- anova_test_final_bee
  list_results_LU_bee[[2]] <- coef_LU_final_bee
  
  list_results_final_bee[[i]] <- list_results_LU_bee
  
  coef_LU_final_bee <- NULL
  coef_ranef_final_bee <- NULL
  anova_test_final_bee <- NULL
  
  list_results_LU_bee <- NULL
  
}


for (i in names(list_results_final_bee)){
  
  data_frame_to_save <- list_results_final_bee[[i]][[2]]
  data_frame_to_save <- bind_rows(data_frame_to_save, list_results_final_bee[[i]][[2]], list_results_final_bee[[i]][[2]])
  name_file <- paste("bee", "_", i, "_", "forest", ".csv", sep="")
  write.csv(x = data_frame_to_save, file = paste("Results/", name_file, sep = ""))
  
}

#### Joining all results in one dataframe ####

list_results_final_bio2 <- list_results_final_bio
binded_results_bio <- NULL
parameters_list <- NULL

for (i in names(list_results_final_bio2)){
  parameters_list <- as.data.frame(list_results_final_bio2[[i]][2])
  parameters_list$Transition <- i
  names(parameters_list)[1:2] <- c("LU1","LU2")
  
  binded_results_bio <- bind_rows(binded_results_bio, parameters_list)
}


list_results_final_bee2 <- list_results_final_bee
binded_results_bee <- NULL
parameters_list <- NULL

for (i in names(list_results_final_bee2)){
  parameters_list <- as.data.frame(list_results_final_bee2[[i]][2])
  parameters_list$Transition <- i
  names(parameters_list)[1:2] <- c("LU1","LU2")
  
  binded_results_bee <- bind_rows(binded_results_bee, parameters_list)
}

names(binded_results_bee)
names(binded_results_bio)

names(binded_results_bee)[6] <- "R2m"

binded_results <- bind_rows(binded_results_bio, binded_results_bee)

binded_results[binded_results$LU2<0,"Effect"] <- "decrease"
binded_results[binded_results$LU2>0,"Effect"] <- "increase"
binded_results[is.nan(binded_results$p_value),"p_value"] <- 0
binded_results[binded_results$p_value>0.05,"Effect"] <- "no_effect"

write.csv(x = binded_results, file = "Results/all_results_forest.csv")

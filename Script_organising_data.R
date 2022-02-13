#### Script to calculate species richness of the seven biological groups and organise the tables for analyses ####


# Loading packages

library(dplyr)
library(vegan)


# Loading data

# Biodiversity
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

#Carbon pools
carbon_STM<-read.table("Data/Carbon_all_STM.txt",h=T)
carbon_PGM<-read.table("Data/Carbon_all_PGM.txt",h=T)

# Soil properties
soil_STM<-read.csv("Data/Soil_STM.csv", h=T, row.names = 1)
soil_PGM<-read.csv("Data/Soil_PGM.csv", h=T, row.names = 1)

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

# Biodiversity
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

#Carbon structure
carbon_STM<-droplevels(carbon_STM[carbon_STM$Transect_code %in% t_STM$Transect_code,])
carbon_PGM<-droplevels(carbon_PGM[carbon_PGM$Transect_code %in% t_PGM$Transect_code,])

# Soil
soil_STM<-droplevels(soil_STM[soil_STM$Transect_code %in% t_STM$Transect_code,])
soil_PGM<-droplevels(soil_PGM[soil_PGM$Transect_code %in% t_PGM$Transect_code,])

# Environment
environment_STM<-droplevels(environment_STM[environment_STM$Transect_code %in% t_STM$Transect_code,])
environment_PGM<-droplevels(environment_PGM[environment_PGM$Transect_code %in% t_PGM$Transect_code,])


#### Organising data frames to run the analyses ####

# Biodiversity

# Dung Beetles
db_STM <- left_join(db_STM, t_STM, by = "Transect_code")
db_PGM <- left_join(db_PGM, t_PGM, by = "Transect_code")

# Calculating species richness per transect
db_STM$rich<-specnumber(db_STM[,6:(ncol(db_STM)-1)])
db_PGM$rich<-specnumber(db_PGM[,6:(ncol(db_PGM)-1)])

# Birds
birds_STM <- left_join(birds_STM, t_STM, by = "Transect_code")
birds_PGM <- left_join(birds_PGM, t_PGM, by = "Transect_code")

# Calculating richness
birds_STM$rich<-specnumber(birds_STM[,5:(ncol(birds_STM)-1)])
birds_PGM$rich<-specnumber(birds_PGM[,5:(ncol(birds_PGM)-1)])

# Ants
ants_STM <- left_join(ants_STM, t_STM, by = "Transect_code")
ants_PGM <- left_join(ants_PGM, t_PGM, by = "Transect_code")

# Calculating richness
ants_STM$rich<-specnumber(ants_STM[,5:(ncol(ants_STM)-1)])
ants_PGM$rich<-specnumber(ants_PGM[,5:(ncol(ants_PGM)-1)])

#Excluding NA's
ants_STM<-droplevels(na.omit(ants_STM))

# Trees
trees_STM <- left_join(trees_STM, t_STM, by = "Transect_code")
trees_PGM <- left_join(trees_PGM, t_PGM, by = "Transect_code")

# Calculating richness
trees_STM$rich<-specnumber(trees_STM[,5:(ncol(trees_STM)-1)])
trees_PGM$rich<-specnumber(trees_PGM[,5:(ncol(trees_PGM)-1)])


# Saplings
sap_STM <- left_join(sap_STM, t_STM, by = "Transect_code")
sap_PGM <- left_join(sap_PGM, t_PGM, by = "Transect_code")

# Calculating richness
sap_STM$rich<-specnumber(sap_STM[,5:(ncol(sap_STM)-1)])
sap_PGM$rich<-specnumber(sap_PGM[,5:(ncol(sap_PGM)-1)])

# Lianas
lianas_STM <- left_join(lianas_STM, t_STM, by = "Transect_code")
lianas_PGM <- left_join(lianas_PGM, t_PGM, by = "Transect_code")

# Calculating richness
lianas_STM$rich<-specnumber(lianas_STM[,5:(ncol(lianas_STM)-1)])
lianas_PGM$rich<-specnumber(lianas_PGM[,5:(ncol(lianas_PGM)-1)])

# Orchid bees
bees_PGM <- left_join(bees_PGM, t_PGM, by = "Transect_code")

# Calculating richness
bees_PGM$rich<-specnumber(bees_PGM[,5:(ncol(bees_PGM)-1)])

# Carbon pools
carbon_STM <- left_join(carbon_STM, t_STM, by = "Transect_code")
carbon_PGM <- left_join(carbon_PGM, t_PGM, by = "Transect_code")

# Soil properties
soil_STM <- left_join(soil_STM, t_STM, by = "Transect_code")
soil_PGM <- left_join(soil_PGM, t_PGM, by = "Transect_code")


#### Creating the data frames with species richness of all groups and the other ecosystem variables per transect ####

# PGM region

data_joined_PGM <- soil_PGM %>%
  select(c(1:4,12)) %>% 
  left_join(ants_PGM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(ant_rich = rich) %>%
  left_join(bees_PGM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(bee_rich = rich) %>%
  left_join(birds_PGM %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(bird_rich = rich) %>%
  left_join(db_PGM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(db_rich = rich) %>%
  left_join(lianas_PGM %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(liana_rich = rich) %>%
  left_join(sap_PGM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(sap_rich = rich) %>%
  left_join(trees_PGM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(tree_rich = rich)

data_joined_PGM <- data_joined_PGM %>%
  left_join(carbon_PGM %>% select(-LU_UF), by = "Transect_code") %>%
  left_join(soil_PGM %>% select(-c(1:3,12)), by = "Transect_code") %>% 
  left_join(environment_PGM %>% select(-c(1:3,5)), by = "Transect_code")

# STM region

data_joined_STM <- soil_STM %>%
  select(c(1:4,12)) %>% 
  left_join(ants_STM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(ant_rich = rich) %>%
  left_join(birds_STM %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(bird_rich = rich) %>%
  left_join(db_STM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(db_rich = rich) %>%
  left_join(lianas_STM %>% select(Transect_code, rich), by = "Transect_code") %>% 
  rename(liana_rich = rich) %>%
  left_join(sap_STM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(sap_rich = rich) %>%
  left_join(trees_STM %>% select(Transect_code, rich), by = "Transect_code") %>%
  rename(tree_rich = rich)

data_joined_STM <- data_joined_STM %>% 
  left_join(carbon_STM %>% select(-LU_UF), by = "Transect_code") %>%
  left_join(soil_STM %>% select(-c(1:3,12)), by = "Transect_code") %>% 
  left_join(environment_STM %>% select(-c(1:3,5)), by = "Transect_code")


data_all_joined <- bind_rows(data_joined_STM, data_joined_PGM)

data_all_joined <- data_all_joined[data_all_joined$LU_UF %in% c("UF", "LF", "LBF", 
                                                                "MA", "PA", "SFyoung",
                                                                "SFold"),]


write.csv(data_all_joined, "Data/data_all_joined.csv", row.names = F)

#### Script create the graphs ####

library(dplyr)
library(diagram)

data_all_joined <- read.csv("Data/data_all_joined.csv", h=T)

land_uses <- c("UF", "LF", "LBF", "PA", "MA", "SFyoung", "SFold")

variables_names <- colnames(data_all_joined[,c(6:22,28)])

data_all_joined2 <- data_all_joined %>% 
  filter(LU_UF == "UF" | LU_UF =="LF" | LU_UF =="LBF" | LU_UF =="PA" | LU_UF =="MA" | LU_UF =="SFyoung" | LU_UF =="SFold") %>% 
  droplevels()

mean_final<-NULL

for (i in variables_names){
  data_mean <- data_all_joined2 %>% 
    select(LU_UF, i)
  
  data_mean2 <- tapply(data_mean[,2], data_mean$LU_UF, mean, na.rm = T)
  
  mean_final <- cbind(mean_final, data_mean2)
}

mean_final <- as.data.frame(mean_final)
colnames(mean_final) <- variables_names

row.names(mean_final)[6]<-"SFy"
row.names(mean_final)[5]<-"SFo"

mean_final <- mean_final[order(match(row.names(mean_final),c("UF","LF","LBF","PA","MA","SFy","SFo"))),]

mean_table <- t(mean_final)
write.csv(file = "Results/mean_values_table.csv", x = mean_table)

# Order of the arrows:
# UF_LF, UF_PA, UF_MA, LF_LBF, LF_PA, LF_MA, LBF_PA, LFB_MA, PA_MA, MA_PA, MA_SFy, PA_SFy, SFy_PA, SFy_MA, SFy_SFo, SFo_PA, SFo_MA 

x0=c(0.25, 0.25, 0.25, 0.50, 0.50, 0.50, 0.75, 0.75, 0.35, 0.65, 0.65, 0.35, 0.35, 0.35, 0.35, 0.65, 0.65)
y0=c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.50, 0.50, 0.50, 0.50, 0.05, 0.05, 0.05, 0.05, 0.05)

x1=c(0.50, 0.35, 0.65, 0.75, 0.35, 0.65, 0.35, 0.65, 0.65, 0.35, 0.35, 0.35, 0.65, 0.35, 0.65, 0.35, 0.65)
y1=c(0.95, 0.50, 0.50, 0.95, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.05, 0.05, 0.50, 0.50, 0.05, 0.50, 0.50)

x00=x0+(x1-x0)*.15
x11=x1-(x1-x0)*.15
y00=y0+(y1-y0)*.15
y11=y1-(y1-y0)*.15

plot(NA, xlim=c(0,1), ylim=c(0,1),xaxt="n",yaxt="n", xlab=NA, ylab=NA,bty="n")
text(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05)-0.04, c("UF","LF","LBF","PA","MA","SFy","SFo"))
arrows(x0=x00,y0=y00,x1=x11,y1=y11,lwd=1,length = .2)
points(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05), pch = 16, cex = mean_final$bird_rich/max(mean_final$bird_rich)*4)

#### Panel with biodiversity groups ####

biodiversity_variables <- c("ant_rich", "bee_rich", "bird_rich", "db_rich", "liana_rich", "sap_rich", "tree_rich")

par(mfrow=c(3,3), mai=c(0.001,0.001,0.001,0.001))

for (i in biodiversity_variables){
  plot(NA, xlim=c(0,1), ylim=c(-0.1,1.1),xaxt="n",yaxt="n", xlab=NA, ylab=NA,bty="n")
  text(x = c(0.175,0.50,0.825,0.3,0.70,0.35,0.65), y = c(0.95,0.85,0.95,0.45,0.45,-0.05,-0.05), c("UF","LF","LBF","PA","MA","SFy","SFo"))
  text(x = 0.5, y = 1.1, i)
  arrows(x0=x00, y0=y00, x1=x11, y1=y11, lwd = 1, length = 0.07)
  curvedarrow(from = c(0.25,0.95), to = c(0.75,0.95), curve = -0.20, arr.pos = 0.86, lwd = 1, arr.type = "simple",
              arr.length = 0.18, segment = c(0.18,0.86))
  points(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05), pch = 16, 
         cex = (mean_final[,i]/max(mean_final[,i]))*4, col = "#006D2C")
  legend(x = 0.8, y = 0.9, round(sort(mean_final[,i], decreasing = T), digits = 1), pch=16, 
         pt.cex = sort((mean_final[,i]/max(mean_final[,i]))*4, decreasing = T),
         bty = "n", y.intersp = 1.6, x.intersp = 1.5, col = "#006D2C")
  
}


#### Panel with carbon groups ####

carbon_variables <- c("Aboveground_pool", "Litter_pool", "Dead_pool", "Soil_pool_new")

par(mfrow=c(2,2), mai=c(0.001,0.001,0.001,0.001))

for (i in carbon_variables){
  plot(NA, xlim=c(0,1), ylim=c(-0.1,1.1),xaxt="n",yaxt="n", xlab=NA, ylab=NA,bty="n")
  text(x = c(0.175,0.50,0.825,0.3,0.70,0.35,0.65), y = c(0.95,0.85,0.95,0.45,0.45,-0.05,-0.05), c("UF","LF","LBF","PA","MA","SFy","SFo"))
  text(x = 0.5, y = 1.1, i)
  arrows(x0=x00, y0=y00, x1=x11, y1=y11, lwd = 1, length = 0.07)
  curvedarrow(from = c(0.25,0.95), to = c(0.75,0.95), curve = -0.20, arr.pos = 0.86, lwd = 1, arr.type = "simple",
              arr.length = 0.18, segment = c(0.18,0.86))
  points(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05), pch = 16, 
         cex = (mean_final[,i]/max(mean_final[,i]))*4, col = "#F57D15FF")
  legend(x = 0.8, y = 0.9, round(sort(mean_final[,i], decreasing = T), digits = 1), pch=16, 
         pt.cex = sort((mean_final[,i]/max(mean_final[,i]))*4, decreasing = T),
         bty = "n", y.intersp = 1.6, x.intersp = 1.5, col = "#F57D15FF")
  
}

#### Pannel with soil groups ####

soil_variables <- c("pH", "Nperc", "P", "K", "Na", "CaMg", "Al")

par(mfrow=c(3,3), mai=c(0.001,0.001,0.001,0.001))

# Seprating pH because it is log scale (pH is the log of the concentration of H, so an increase of 1 is actually 10 times)

i<-"pH"
plot(NA, xlim=c(0,1), ylim=c(-0.1,1.1),xaxt="n",yaxt="n", xlab=NA, ylab=NA,bty="n")
text(x = c(0.175,0.50,0.825,0.3,0.70,0.35,0.65), y = c(0.95,0.85,0.95,0.45,0.45,-0.05,-0.05), c("UF","LF","LBF","PA","MA","SFy","SFo"))
text(x = 0.5, y = 1.1, i)
arrows(x0=x00, y0=y00, x1=x11, y1=y11, lwd = 1, length = 0.07)
curvedarrow(from = c(0.25,0.95), to = c(0.75,0.95), curve = -0.20, arr.pos = 0.86, lwd = 1, arr.type = "simple",
            arr.length = 0.18, segment = c(0.18,0.86))
points(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05), pch = 16, 
       cex = (0.09198603/(10^-mean_final[,i]/max(10^-mean_final[,i])))*4, col = "#781C6D")
legend(x = 0.8, y = 0.9, round(sort(mean_final[,i], decreasing = T), digits = 2), pch=16, 
       pt.cex = ((10^-(round(sort(mean_final[,i], decreasing = F), digits = 2)))*10^6/max((10^-(round(sort(mean_final[,i], decreasing = F), digits = 2)))*10^6))*4,
       bty = "n", y.intersp = 1.6, x.intersp = 1.5, col = "#781C6D")

# Other soil variables

for (i in soil_variables[-1]){
  plot(NA, xlim=c(0,1), ylim=c(-0.1,1.1),xaxt="n",yaxt="n", xlab=NA, ylab=NA,bty="n")
  text(x = c(0.175,0.50,0.825,0.3,0.70,0.35,0.65), y = c(0.95,0.85,0.95,0.45,0.45,-0.05,-0.05), c("UF","LF","LBF","PA","MA","SFy","SFo"))
  text(x = 0.5, y = 1.1, i)
  arrows(x0=x00, y0=y00, x1=x11, y1=y11, lwd = 1, length = 0.07)
  curvedarrow(from = c(0.25,0.95), to = c(0.75,0.95), curve = -0.20, arr.pos = 0.86, lwd = 1, arr.type = "simple",
              arr.length = 0.18, segment = c(0.18,0.86))
  points(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05), pch = 16, 
         cex = (mean_final[,i]/max(mean_final[,i]))*4, col = "#781C6D")
  legend(x = 0.8, y = 0.9, round(sort(mean_final[,i], decreasing = T), digits = 2), pch=16, 
         pt.cex = sort((mean_final[,i]/max(mean_final[,i]))*4, decreasing = T),
         bty = "n", y.intersp = 1.6, x.intersp = 1.5, col = "#781C6D")
  
}



#### Graphs of Forest species ####

rm(list = ls())

library(dplyr)
library(diagram)

data_all_joined <- read.csv("Data/data_rich_forest_sp.csv", h=T, row.names = 1)


land_uses <- c("UF", "LF", "LBF", "PA", "MA", "SFyoung", "SFold")

variables_names <- colnames(data_all_joined[,c(6:11,17)])

data_all_joined2 <- data_all_joined %>% 
  filter(LU_UF == "UF" | LU_UF =="LF" | LU_UF =="LBF" | LU_UF =="PA" | LU_UF =="MA" | LU_UF =="SFyoung" | LU_UF =="SFold") %>% 
  droplevels()

mean_final<-NULL

for (i in variables_names){
  data_mean <- data_all_joined2 %>% 
    select(LU_UF, i)
  
  data_mean2 <- tapply(data_mean[,2], data_mean$LU_UF, mean, na.rm = T)
  
  mean_final <- cbind(mean_final, data_mean2)
}

mean_final <- as.data.frame(mean_final)
colnames(mean_final) <- variables_names

row.names(mean_final)[6]<-"SFy"
row.names(mean_final)[5]<-"SFo"

mean_final <- mean_final[order(match(row.names(mean_final),c("UF","LF","LBF","PA","MA","SFy","SFo"))),]

# Order of the arrows:
# UF_LF, UF_PA, UF_MA, LF_LBF, LF_PA, LF_MA, LBF_PA, LFB_MA, PA_MA, MA_PA, MA_SFy, PA_SFy, SFy_PA, SFy_MA, SFy_SFo, SFo_PA, SFo_MA 

# Order of the arrows:
# UF_LF, UF_PA, UF_MA, LF_LBF, LF_PA, LF_MA, LBF_PA, LFB_MA, PA_MA, MA_PA, MA_SFy, PA_SFy, SFy_PA, SFy_MA, SFy_SFo, SFo_PA, SFo_MA 

x0=c(0.25, 0.25, 0.25, 0.50, 0.50, 0.50, 0.75, 0.75, 0.35, 0.65, 0.65, 0.35, 0.35, 0.35, 0.35, 0.65, 0.65)
y0=c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.50, 0.50, 0.50, 0.50, 0.05, 0.05, 0.05, 0.05, 0.05)

x1=c(0.50, 0.35, 0.65, 0.75, 0.35, 0.65, 0.35, 0.65, 0.65, 0.35, 0.35, 0.35, 0.65, 0.35, 0.65, 0.35, 0.65)
y1=c(0.95, 0.50, 0.50, 0.95, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.05, 0.05, 0.50, 0.50, 0.05, 0.50, 0.50)

x00=x0+(x1-x0)*.15
x11=x1-(x1-x0)*.15
y00=y0+(y1-y0)*.15
y11=y1-(y1-y0)*.15

plot(NA, xlim=c(0,1), ylim=c(0,1),xaxt="n",yaxt="n", xlab=NA, ylab=NA,bty="n")
text(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05)-0.04, c("UF","LF","LBF","PA","MA","SFy","SFo"))
arrows(x0=x00,y0=y00,x1=x11,y1=y11,lwd=1,length = .2)
points(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05), pch = 16, cex = mean_final$bird_rich/max(mean_final$bird_rich)*4)

#### Pannel with biodiversity groups ####

biodiversity_variables <- c("ant_rich", "bee_rich", "bird_rich", "db_rich", "liana_rich", "sap_rich", "tree_rich")

par(mfrow=c(3,3), mai=c(0.001,0.001,0.001,0.001))

for (i in biodiversity_variables){
  plot(NA, xlim=c(0,1), ylim=c(-0.1,1.1),xaxt="n",yaxt="n", xlab=NA, ylab=NA,bty="n")
  text(x = c(0.175,0.50,0.825,0.3,0.70,0.35,0.65), y = c(0.95,0.85,0.95,0.45,0.45,-0.05,-0.05), c("UF","LF","LBF","PA","MA","SFy","SFo"))
  text(x = 0.5, y = 1.1, i)
  arrows(x0=x00, y0=y00, x1=x11, y1=y11, lwd = 1, length = 0.07)
  curvedarrow(from = c(0.25,0.95), to = c(0.75,0.95), curve = -0.20, arr.pos = 0.86, lwd = 1, arr.type = "simple",
              arr.length = 0.18, segment = c(0.18,0.86))
  points(x = c(0.25,0.5,0.75,0.35,0.65,0.35,0.65), y = c(0.95,0.95,0.95,0.5,0.5,0.05,0.05), pch = 16, 
         cex = (mean_final[,i]/max(mean_final[,i]))*4, col = "#006D2C")
  legend(x = 0.8, y = 0.9, round(sort(mean_final[,i], decreasing = T), digits = 1), pch=16, 
         pt.cex = sort((mean_final[,i]/max(mean_final[,i]))*4, decreasing = T),
         bty = "n", y.intersp = 1.6, x.intersp = 1.5, col = "#006D2C")
  
}


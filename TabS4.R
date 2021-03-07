library("xlsx")
par<-read.xlsx("additional/from_gis/environ_variables.xlsx", sheetName = "data")
paro<-read.xlsx("additional/from_gis/PARenviron_pattern.xlsx", sheetName = "data")

      par  <- par[par$y_wgs<75,] # in case of Spitsbergen out
plot(par$y_wgs, log(par$total_concetration))#, ylim=c(0, 1000))
test.elev<-which(is.na((par$elevation))==TRUE)
par<-par[-test.elev,]
test.elev<-which(is.na((par$ForestBiomass10km))==TRUE)
par<-par[-test.elev,]


par.ad<-par[,2:16]
par.ad$Alnus<-par.ad$Alnus*0.25
par.ad$Betula<-par.ad$Betula*0.25
par.ad$Carpinus<-par.ad$Carpinus*0.33
par.ad$Corylus<-par.ad$Corylus*0.25
par.ad$Cyperaceae<-par.ad$Cyperaceae*1
par.ad$Fagus<-par.ad$Fagus*1
par.ad$Fraxinus<-par.ad$Fraxinus*2
par.ad$Juniperus<-par.ad$Juniperus*0.5
par.ad$Picea<-par.ad$Picea*0.5
par.ad$Pinus<-par.ad$Pinus*0.25
par.ad$Poaceae<-par.ad$Poaceae*1
par.ad$Quercus<-par.ad$Quercus*0.25
par.ad$Tilia<-par.ad$Tilia*2

par[,ncol(par)+1]<-rowSums(par.ad[,1:14])
colnames(par)[ncol(par)] <-"total_adjusted"

                      head(par.ad[,c(1:5,7:11,13:14)])
par[,ncol(par)+1]<-rowSums(par.ad[,c(1:5,7:11,13:14)])
colnames(par)[ncol(par)] <-"tree_adj"

par$total_concetration <- rowSums(par[,1+c(1:14)])
par[,ncol(par)+1]<- rowSums(par[,1+c(1:5,7:11,13:14)])
colnames(par)[ncol(par)] <-"tree"

vsl <- data.frame(
total_PAR=c(

summary(lm(log(total_concetration) ~ y_wgs , data=par))$adj.r.squared,
summary(lm(log(total_concetration) ~ bioclim1 , data=par))$adj.r.squared,
summary(lm(log(total_concetration) ~ ForestBiomass10km , data=par))$adj.r.squared,
summary(lm(log(total_concetration) ~ y_wgs +  bioclim1+ ForestBiomass10km, data=par))$adj.r.squared,
summary(lm(log(total_concetration) ~ y_wgs +  bioclim1+ ForestBiomass10km+elevation, data=par))$adj.r.squared),



adj_total_PAR=c(
summary(lm(log(total_adjusted) ~ y_wgs , data=par))$adj.r.squared,
summary(lm(log(total_adjusted) ~ bioclim1 , data=par))$adj.r.squared,
summary(lm(log(total_adjusted) ~ ForestBiomass10km , data=par))$adj.r.squared,
summary(lm(log(total_adjusted) ~ y_wgs +  bioclim1+ ForestBiomass10km, data=par))$adj.r.squared,
summary(lm(log(total_adjusted) ~ y_wgs +  bioclim1+ ForestBiomass10km+elevation, data=par))$adj.r.squared)

,
tree_PAR=c(
  
  summary(lm(log(tree) ~ y_wgs , data=par))$adj.r.squared,
  summary(lm(log(tree) ~ bioclim1 , data=par))$adj.r.squared,
  summary(lm(log(tree) ~ ForestBiomass10km , data=par))$adj.r.squared,
  summary(lm(log(tree) ~ y_wgs +  bioclim1+ ForestBiomass10km, data=par))$adj.r.squared,
  summary(lm(log(tree) ~ y_wgs +  bioclim1+ ForestBiomass10km+elevation, data=par))$adj.r.squared)

,



adj_tree_PAR=c(
  summary(lm(log(tree_adj) ~ y_wgs , data=par))$adj.r.squared,
  summary(lm(log(tree_adj) ~ bioclim1 , data=par))$adj.r.squared,
  summary(lm(log(tree_adj) ~ ForestBiomass10km , data=par))$adj.r.squared,
  summary(lm(log(tree_adj) ~ y_wgs +  bioclim1+ ForestBiomass10km, data=par))$adj.r.squared,
  summary(lm(log(tree_adj) ~ y_wgs +  bioclim1+ ForestBiomass10km+elevation, data=par))$adj.r.squared)



)

rownames(vsl) <- c("latitude", "MAT", "Forest cover 10 km", "latitude+MAT+Forest cover 10 km", "latitude+MAT+Forest cover 10 km+elevation")

write.xlsx(vsl, "TabS4_SpitsbergenOUT.xlsx")


plot(lm(log(total_adjusted) ~ y_wgs +  bioclim1+ ForestBiomass10km+elevation, data=par))


p_value_PAR=c(
  
  summary(lm(log(total_concetration) ~ y_wgs , data=par))$coefficients[2,4],
  summary(lm(log(total_concetration) ~ bioclim1 , data=par))$coefficients[2,4],
  summary(lm(log(total_concetration) ~ ForestBiomass10km , data=par))$coefficients[2,4],
  summary(lm(log(total_concetration) ~ y_wgs +  bioclim1+ ForestBiomass10km, data=par))$coefficients[2,4],
  summary(lm(log(total_concetration) ~ y_wgs +  bioclim1+ ForestBiomass10km+elevation, data=par))$coefficients[2,4]),
p_value_adjusted_PAR=c(
  summary(lm(log(total_adjusted) ~ y_wgs , data=par))$coefficients[2,4],
  summary(lm(log(total_adjusted) ~ bioclim1 , data=par))$coefficients[2,4],
  summary(lm(log(total_adjusted) ~ ForestBiomass10km , data=par))$coefficients[2,4],
  summary(lm(log(total_adjusted) ~ y_wgs +  bioclim1+ ForestBiomass10km, data=par))$coefficients[2,4],
  summary(lm(log(total_adjusted) ~ y_wgs +  bioclim1+ ForestBiomass10km+elevation, data=par))$coefficients[2,4])

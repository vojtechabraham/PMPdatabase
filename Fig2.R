obl <- c("nboreal",        "boreal",  "sboreal",         "lowertemp",        "middletemp","hightemp",   "highsouth")

s <- unique(read.table("recent/annual_trap_PARs.csv",stringsAsFactors=F, encoding = "UTF-8")[,c("trap_id", "year_id", "elevation", "position_trap_type")])
NROW(unique(s[,1]))

library(xlsx)
env <-  read.xlsx("additional/from_gis/environ_variables.xlsx", sheetIndex = 1, encoding = "UTF-8")[,c("Group.1","total_concetration", "bioclim1", "bioclim8", "bioclim12", "ForestBiomassCellValue","ForestBiomass10km", "y_wgs" )]

rgn <- read.table("additional/trap_areas_names.txt", sep="\t",stringsAsFactors=F, encoding = "UTF-8")
head(rgn)
rgn[1,1] <- "FIN/SPH/Z80"
rgn[rgn$V11 %in% c("EST", "LVA"),11] <- "EST, LVA"
rgn <- rgn[order(as.integer(rgn$V10)),]
rgn <- rgn[!(rgn$V1 %in% c("CH/WVDK/R5-a",
                           "CH/WVDK/R5-b",
                           "CH/WVDK/G12B",    
                           "CH/WVDK/G12A",
                           "CH/WVDK/G08-a", 
                           "CH/WVDK/G08-b", 
                           "CH/WVDK/G07-b",
                           "CH/WVDK/G07-a",
                           "CH/WVDK/SAG")),]
rgn[,"V10"] <- 1:nrow(rgn) 


sp <- merge(s,rgn[,c(1,6,8:12)], by=1) 
sp[sp$V8=="lowersouth","V8"] <- "highsouth"
sp <- merge(sp, env, by=1, all.x = T)
kk <- data.frame(st=sort(unique(sp$V10)),V10=1:NROW(unique(sp$V10)))
colnames(sp)[8] <-"out"
sp<-merge(sp, kk, by.x=8, by.y = 1)[,-1]
sp <- sp[order(sp$V10, sp$year_id),]
rownames(sp) <- 1:nrow(sp)

i=1
fn <- rbind( data.frame(row.names=obl[i],atmin=min(which(sp$V8==obl[i])),  atmax=max(which(sp$V8==obl[i]))))    
fn <- fn[-1,]
for(i in 1:NROW(obl)){
  fn <- rbind(fn, data.frame(row.names=obl[i],atmin=min(which(sp$V8==obl[i])),  atmax=max(which(sp$V8==obl[i]))))    
}

fn <- fn[obl,]

sp[sp[,"V12"] == "Finnmark" & sp$V11=="FIN","V11"] <- "NOR"
rgn[rgn$V11 %in% c("TR"),11] <- "TUR"

sp[,18] <- paste(sp[,"V12"], " (",sp[,"V11"],")", sep="")
sp <- sp[order(sp$V10),]
rg <- unique(sp$V18)

rg[6]

i=1
ff <- rbind( data.frame(row.names=rg[i],atmin=min(which(sp$V18==rg[i])),  atmax=max(which(sp$V18==rg[i]))))    
ff <- ff[-1,]
for(i in 1:NROW(rg)){
  ff <- rbind(ff, data.frame(row.names=rg[i],atmin=min(which(sp$V18==rg[i])),  atmax=max(which(sp$V18==rg[i]))))    
}
ff <- ff[rg,]

#dp <- data.frame(por=c(263:269),yr=c(1998,1998,1998,1998,1996,1996, 1996))




lat <- data.frame(
  row.names=aggregate(sp$y_wgs, by=list(sp$V8), FUN=min, na.rm=T )[,1],
  max=round(aggregate(sp$y_wgs, by=list(sp$V8), FUN=max, na.rm=T)[,2]),
  min=round(aggregate(sp$y_wgs, by=list(sp$V8), FUN=min, na.rm=T)[,2]))
lat <- lat[obl,]
lat
lat[2,] <- c(69,68)

lap <- c(paste(lat[1,"max"],"°N", sep=""),
         paste(lat[-7,"min"], "° ",lat[-1,"max"],"°", sep=""),
         paste(lat[7,"min"], "°N", sep=""))


oblp <- c("arctic/alpine",        "n. boreal",  "boreal",         "lower temperate",        "middle alt. temperate","alpine",   "temperate/Mediterranean")


ff[1,] <- c(1,2)
ylbd <- 3.8
ylbb <- 5.2
tik <- -0.12

par(mfrow=c(2,1),mar=c(0,7,7,2),  mgp=c(ylbd,0.7,0))
mx <- nrow(unique(sp[,c("V10","elevation")]))
n = 1/6
padse=seq(n, 1, n)
padse=seq(1, 50, 49/6)
padse <- c(padse[1]-(n/2),padse[2]-(n/3),padse[3]-(n/3), padse[4]-(n/3),padse[5]-(n/3),padse[6]-(n/3),padse[7]-(n/3))
padse<- padse*50
pzc <- c(44, 142, 243)

pdf("Fig2.pdf", height = 20, width = 17.3333333, pointsize = 25)
#emf("nadmor3.emf", height = 7, width = 16.3333333, pointsize = 12)


par(mfrow=c(7,1), las=1, mar=c(0,9,1,2), mgp=c(ylbd,0.7,0), oma=c(6,0,7,0))
#par(mfrow=c(2,1), las=1, mar=c(0,9,1,2), mgp=c(ylbd,0.7,0), oma=c(6,0,7,0))
#plot(1,1, "n", bty="n", axes = F, xlab = "", ylab = "")


## a)
ss <- barplot(unique(sp[,c("V10","elevation")])[,-1], xaxt="n",   ylab="", yaxt="n",xaxs="i", xlab="", col = "transparent", border=F)
      title(ylab="trap areas", line=ylbb, font.lab=2)
      abline(v=ss[sp[fn$atmax,"V10"],1]+0.6, col="gray")
      axis(3, line=4, at=c(0.5,ss[sp[fn$atmax,"V10"],1]+0.6), labels=lap, tck=-0.05, xpd=T)#rep("",9))
      axis(3, line=5, at=ss[pzc,1], labels=c("b o r e a l", "t e m p e r a t e", "M e d i t."), tick=F, xpd=T, cex.axis=(1.7),font.axis=2)
      par(mgp=c(6,0,0))
      axis(3, line=2.7, at= c(-20,ss[sp[fn$atmin,"V10"],1] + ((ss[sp[fn$atmax,"V10"],1]-ss[sp[fn$atmin,"V10"],1])/2)), labels=c("trap regions",oblp), tick=F, xpd=T, font = 2)
      axis(3, line=2.7, at= 82, labels="boreal", tick=F, xpd=T, font = 2)
      
      mtext(c("(a)"),  side=2,  line=8.5, outer=F, adj=0.01,  font = 2, cex=1.2)

##  b)
    par(mar=c(0,9,1,2),  mgp=c(ylbd,0.7,0))
ss <- barplot(unique(sp[,c("V10","elevation")])[,-1], xaxt="n",   ylab="(m a.s.l)", xaxs="i", xlab="", col = "black")
      box()
      title(ylab="altitude", line=ylbb, font.lab=2)
      abline(v=ss[sp[fn$atmax,"V10"],1]+0.6, col="gray")
      par(mgp=c(0,0.3,0))
      axis(3, at= ss[sp[floor(ff$atmin+ (ff$atmax-ff$atmin)/2),"V10"],1], labels=c("",rg[-1]), tick=F, las=2, xpd=T)#, cex.axis=1.2, font.axis=2)
      axis(3, at= c(4.3), labels="Utsjoki (FIN)" , tick=F, las=2, xpd=T)
      axis(3, at= c(-0.3), labels="Spitsbergen (SJM)", tick=F, las=2, xpd=T)
      axis(3, at=c(0,ss[sp[ff$atmax,"V10"],1]+0.6), labels=rep("",nrow(ff)+1), tck=-0.02)#rep("",9))
      mtext(c("(b)"), side=2,  line=8.5, outer=F, adj=0.01, font = 2, cex=1.2)

## c)
      par(mar=c(0,9,1,2),  mgp=c(ylbd,0.7,0))
ss <- barplot(unique(sp[,c("V10","total_concetration")])[,-1], xaxt="n",   ylab=expression("(grains cm"^-2~"year"^-1*")"), xaxs="i", xlab="", col = "black")
      abline(v=ss[sp[fn$atmax,"V10"],1]+0.6, col="gray")
      title(ylab="tree PAR", line=ylbb, font.lab=2)
      axis(3, at=c(0,ss[sp[ff$atmax,"V10"],1]+0.6), labels=rep("",nrow(ff)+1), tck=tik)#rep("",9))
      box()
      mtext(c("(c)"),  side=2,  line=8.5, outer=F, adj=0.01,  font = 2, cex=1.2)
      
## d)
      par(mar=c(0,9,1,2),  mgp=c(ylbd,0.7,0))
ss <- barplot(as.numeric(paste(unique(sp[,c("V10","ForestBiomass10km")])[,-1])), xaxt="n",   ylab="(%)", xaxs="i", xlab="", col = "black")
      abline(v=ss[sp[fn$atmax,"V10"],1]+0.6, col="gray")
      rect(xleft = ss[sp[ff["Lagodekhi (GEO)","atmin"],"V10"],1]-0.6, xright = ss[sp[ff["Lagodekhi (GEO)","atmax"],"V10"],1]+0.6, ybottom = 0, ytop=100, col="gray", border = "transparent")
      text(x=ss[sp[ff["Lagodekhi (GEO)","atmin"],"V10"],1]-0.6+((ss[sp[ff["Lagodekhi (GEO)","atmax"],"V10"],1]-ss[sp[ff["Lagodekhi (GEO)","atmin"],"V10"],1])/2),y=50, labels = "NO DATA", srt=90 )
      title(ylab="forest 10km", line=ylbb, font.lab=2)
      axis(3, at=c(0,ss[sp[ff$atmax,"V10"],1]+0.6), labels=rep("",nrow(ff)+1), tck=tik)#rep("",9))
      box()
      mtext(c("(d)"),  side=2,  line=8.5, outer=F, adj=0.01,  font = 2, cex=1.2)
      
## e)
      par(mar=c(0,9,1,2),  mgp=c(ylbd,0.7,0))
ss <- barplot(as.numeric(paste(unique(sp[,c("V10","bioclim12")])[,-1])), xaxt="n",   ylab="(mm)", xaxs="i", xlab="", col = "black")
      abline(v=ss[sp[fn$atmax,"V10"],1]+0.6, col="gray")
      title(ylab="APrecip", line=ylbb, font.lab=2)
      axis(3, at=c(0,ss[sp[ff$atmax,"V10"],1]+0.6), labels=rep("",nrow(ff)+1), tck=tik)#rep("",9))
      box()
      mtext(c("(e)"),  side=2,  line=8.5, outer=F, adj=0.01,  font = 2, cex=1.2)

## f) 
      par(mar=c(0,9,1,2),  mgp=c(ylbd,0.7,0))
ss <- barplot(as.numeric(paste(unique(sp[,c("V10","bioclim1")])[,-1])), xaxt="n",   ylab="(°C)", xaxs="i", xlab="", col = "black")
      #plot(sp[,"V10"],as.numeric(paste(sp[,"bioclim8"])), xaxt="n",  pch=15, cex=0.5, ylab="", xlim=c(0.5,mx+0.5),  xaxs="i", xlab="")
      abline(h=c(0), col="gray")
      abline(v=sp[fn$atmax,"V10"]+0.5, col="gray")
      title(ylab="MAT", line=ylbb, font.lab=2)
      box()
      #points(sp[,"V10"],as.numeric(paste(sp[,"bioclim1"])), xaxt="n",  pch=1, cex=0.5)
      axis(3, at=c(0,sp[ff$atmax,"V10"])+0.5, labels=rep("",nrow(ff)+1), tck=tik)#rep("",9))
      mtext(c("(f)"),  side=2,  line=8.5, outer=F, adj=0.01,  font = 2, cex=1.2)
      
      
## g) 
      par(mar=c(0,9,1,2),mgp=c(ylbd,1.5,0))
plot(sp[,"V10"],sp[,2] , pch=15, cex=0.5, col="transparent", xaxt="n", ylab="", xlim=c(0.5,mx+0.5), xaxs="i", xlab="")
      abline(h=c(1990,2000, 2010), col="gray")
      title(ylab="trapping year", line=ylbb, font.lab=2)
      box()
      abline(v=sp[fn$atmax,"V10"]+0.5, col="gray")
      axis(1, at=c(0.5,sp[fn$atmax,"V10"]+0.5), labels=lap, tck=-0.05, xpd=T)#rep("",9))
      axis(1, line=1.5, at=pzc, labels=c("b o r e a l", "t e m p e r a t e", "M e d i t."), tick=F, xpd=T, cex.axis=(1.7),font.axis=2)
      par(mgp=c(6,0,0))
      axis(1, at=sp[fn$atmin,"V10"]+((sp[fn$atmax,"V10"]-sp[fn$atmin,"V10"])/2), labels=oblp, tick=F, xpd=T)
      points(sp[,"V10"],sp[,2] , pch=15, cex=0.5)
      axis(3, at=c(0,sp[ff$atmax,"V10"])+0.5, labels=rep("",nrow(ff)+1), tck=tik)#rep("",9))
      mtext(c("(g)"),  side=2,  line=8.5, outer=F, adj=0.01,  font = 2, cex=1.2)
dev.off()


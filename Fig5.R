dt <- read.table("statistic/recent_fossil_stat2.txt", stringsAsFactors = F)

rgn <- read.table("additional/trap_areas_names.txt", sep="\t",stringsAsFactors=F, encoding = "UTF-8")
Encoding(rgn[,1]) <- "UTF-8"
rgn[rgn$V11 %in% c("EST", "LVA"),11] <- "Baltic countries"
rgn <- unique(rgn[order(as.integer(rgn$V10)),c(1,8)])
rgn[rgn$V8=="lowersouth",2] <- "highsouth"

mmg <- merge(dt, rgn, by=1)

ob <- c("arctic/alpine", "north boreal","boreal", "lowland temper.", "middle alt. temper.", "alpine", "temper./Mediterranean")
obl <- c("nboreal",        "boreal",  "sboreal",         "lowertemp",        "middletemp","hightemp",   "highsouth")
barf <- read.table("additional/colours_Brewer7web_Fig6.txt", sep = ",", stringsAsFactors = F)[,1]

fss <- as.character(unique(dt$sites)[1:18])
fosobl <-data.frame(site=c(
  "TOSACJQS","TSOULACC",  "BRUN",
  "AKUV","SUOVAL",
  "HOLT","KLOT","ABBO",
  "ROUGE",  "SUMINKO",
  "PRASILSKE","MNIVA",
  "SAEGIST","BACHALP",
  "SHABLA","ARKUT2",
  "VOULK", "RIBNO"),
  region=c( "nboreal","nboreal","nboreal",
            "boreal","boreal",
            "sboreal", "sboreal","sboreal",
            "lowertemp","lowertemp",
            "middletemp","middletemp",
            "hightemp","hightemp",
            
            "highsouth", "highsouth",
            "highsouth","highsouth"))

# SUBSET RECENT
fosr <- merge(dt, fosobl, by=1)

head(fosr)

nn <- merge(
aggregate(log(fosr$PAR), by=list(fosr$taxa, fosr$region), FUN=mean),
aggregate(log(mmg$PAR), by=list(mmg$taxa, mmg$V8), FUN=mean),
by=1:2, all=F)

i=1
for(i in c(1:21,23:81,83:nrow(nn)))
  {
nn[i,5] <- t.test(
  log(fosr[fosr$taxa==nn[i,1] & fosr$region== nn[i,2], "PAR"]),
  log(mmg[mmg$taxa==nn[i,1] & mmg$V8== nn[i,2], "PAR"]),
  
)$p.value
}

# #### optional
# for(i in c(1:21, 23:81,83:nrow(nn)))
# {
# 
# fos <- log(fosr[fosr$taxa==nn[i,1] & fosr$region== nn[i,2], "PAR"])
# rec <- log(mmg[mmg$taxa==nn[i,1] & mmg$V8== nn[i,2], "PAR"])
# fr <- data.frame(  data=c(fos,rec ), kod=c(rep("fos", NROW(fos)), rep("rec", NROW(rec))), stringsAsFactors = F)
# 
# nn[i,6] <-summary(aov(data~kod, data=fr))[[1]][1,5]
# }
# 
# nn[which(nn$V5>0.05),7] <- FALSE
# nn[which(nn$V5<=0.05),7] <- TRUE
# nn[which(nn$V6>0.05), 8] <- FALSE
# nn[which(nn$V6<=0.05),8] <- TRUE
# 
# colnames(nn)<-c("taxon", "reg", "foss_mean", "rec_mean", "p.value_t.test", "p.value_aov", "is.different_t.test0.05", "is.different_aov0.05")
# 
# #write.table(nn, "p_values.txt")
# 
# hist(nn$V5)
# hist(nn$V6)
# ####


nn2 <- merge(
  aggregate(log(fosr$PAR), by=list(fosr$taxa, fosr$region), FUN=mean),
  aggregate(log(mmg$PAR), by=list(mmg$taxa, mmg$V8), FUN=mean),
  by=1:2, all=T)

nn2 <- merge(nn2, nn[,c(1,2,5)], by=1:2, all=T)
nn2 <- merge(merge(unique(nn$Group.1), unique(nn$Group.2)), nn2,   by=1:2, all=T) 
nn2 <- merge(data.frame(rg=c("highsouth","hightemp", "lowertemp",  "middletemp", "boreal"  ,   "nboreal" ,   "sboreal"), 
                 kd=c(7,6,4,5,2,1,3)),
      nn2, by.x=1, by.y=2)

nn2<- nn2[order(nn2$x, nn2$kd),c(3,1,4:6)]

tabsx <- nn2
colnames(tabsx) <- c("taxon", "region", "mean log(fossil PAR)", "mean log(trap PAR)", "p-value")
head(tabsx)
library(xlsx)
write.xlsx(tabsx, "Tab.S5.xlsx", row.names = F)

ran<- read.table("statistic/ranges2.txt") 
ran[ran$Group.2<1.99, 2] <- 1
ran[ran$Group.2<1.99, 3] <- 0
rnn <- aggregate(ran[,c(3,6)], by=list(ran[,1],ran[,2]), FUN=max)
rnn[,5] <- rnn$hr+((rnn$hr2 -  rnn$hr)/2)
ramx<- aggregate(rnn$Group.2, by=list(rnn$Group.1), FUN=max)
ramx <- merge(ramx, rnn, by=1:2)
ramx[,4] <- 5

rnn <- merge(rnn, ramx, by.x=1:2, by.y=c(1,4), all.x=T)

rnn[rnn$Group.2==5,5] <- rnn[rnn$Group.2==5, "hr.x"] +((rnn[rnn$Group.2==5, "hr.y"] -  rnn[rnn$Group.2==5, "hr.x"])/2)
rnn <- rnn[!(rnn$Group.2>=6),] 
rnn[rnn$Group.2==5,2]<- paste(rnn[rnn$Group.2==5,2],rnn[rnn$Group.2==5,"x"], sep="-")

rnn[rnn$Group.2=="5-5",2] <- 5
rnn[rnn$Group.1=="Fraxinus" & rnn$Group.2==5,5] <-3000
rnn[rnn$Group.1=="total_concetration" & rnn$Group.2==5,5] <- 70000

require(xlsx)
v <- read.xlsx("statistic/TLDT.xlsx", sheetIndex = 1)

druhy <- sort(unique(dt$taxa))
druhl <- druhy
druhl[15] <- "tree PAR"
fontl <-c(3,3,3,3,3,1,3,3,3,3,3,1,3,3,1)
e=1
i=1
bre =30
pru <- 1
ax <- c(1,3,10,30,100,300,1000,3000,10000,30000,100000)
axl <- c("1","3","10","30","100","300","1000","3000","10000","30000","100000")
ob2 <- c("arc/al", "n.bor", "bor", "l.temp","m.temp", "alp", "t/Med")
barn <- c("white", barf, rev(barf)) 
library("MALDIquant")
#png(paste("hist_plot_LDT_new7.png", sep=""), height = 1450, width = 1000, units="mm", res=300, pointsize=45)#, compression="lzw")
cairo_pdf("Fig5pdf", height = 29, width = 20, pointsize=28)

nf <- layout(matrix(c(1, 2:7,
                      1, 8:13,
                      1, 14:19,
                      1, 20:25,
                      1, 26:31,
                      rep(32,7)),6,7,byrow=T), width=c(0.7,2.1,3, 2.1,3, 2.1,3), height=c(1,1,1,1,1, 0.4))
layout.show(nf)


par(mar=c(0,0,0,0))
plot( 1,1,pch=15, col="transparent",xlim = c(1,1) , bty="n", axes=F, cex=4,ylab = "", xlab = "")
text(  1,1, labels=expression("PAR (grains cm"^-2~"year"^-1*")"), srt=90 ,cex = 2, xpd=T)
par( mgp=c(4, 0.7, 0),   xaxs="i")
for(i in 1:NROW(druhy)) {  
  
  
  
  cele <- hist(log(
      dt[dt$taxa==druhy[i], "PAR"]
  ), breaks=bre, plot=F)
  
fktr <- NROW(cele$mids)/23
  
if(fktr>1){
  cele <- hist(log(
    dt[dt$taxa==druhy[i], "PAR"]
  ), breaks=bre/fktr, plot=F)
  fktr <- NROW(cele$mids)/23
  } 

  hh <- data.frame(cele$mids, cele$counts, 0)
  hh <-  hh[!(hh[,2]==0),] 
  hh[,1] <- exp(hh[,1])
  hf <- hh
  
  for(e in 1:NROW(obl)){
    zem <- hist( log(mmg[mmg$taxa==druhy[i] & mmg$V8 ==obl[e], 4]), breaks=cele$breaks, plot=F)
    zh <- data.frame(zem$mids, zem$counts)
    zh[,1] <- exp(zh[,1])
    pru[e] <- mean(mmg[mmg$taxa==druhy[i] & mmg$V8 ==obl[e], 4])
    pru[is.na(pru)] <- 0
    hh <- merge(hh, zh, by=1, all.x=T)

    colnames(hh)[e+3] <- obl[e]
    
  }
  
  
  recsum<- rowSums(hh[,4:ncol(hh)])
  for(e in  1:NROW(obl)) {hh[,e+3] <- hh[,e+3]+hh[,e+2]}
  
  for(e in 1:NROW(obl)){
    zem <- hist( log(fosr[fosr$taxa==druhy[i] & fosr$region== obl[e], 4]), breaks=cele$breaks, plot=F)
    zh <- data.frame(zem$mids, zem$counts)
    zh[,1] <- exp(zh[,1])
    hf <- merge(hf, zh, by=1, all.x=T)
    hf[,e+3] <- hf[,e+3]+hf[,e+2]
    colnames(hf)[ncol(hf)] <- obl[e]
  }

  hf[,3:10] <- hf[,3:10]*-1
  fossum <- hf[,2]-recsum
  
  #par(mfrow=c(1,2))
  par(mar=c(2,5,5,0.5))
  
  plot(sample(1:7,nrow(hh), replace=T), hh[,1],  col="transparent", log="y",xlab="", ylab="", main="",yaxt="n",  xaxt="n", xlim=c(0,8) )
# axis(1,at=1:7, labels=ob2, tick=F, las=2, mgp=c(0, 0.7, 0))
  axis(2, at=ax, labels = as.character(axl), las=2, cex.axis=1.3)
  mtext( text="differ.", side=3, outer=F, line=0, adj=0.5, cex=0.9)
  rzdl <- nn2[nn2$x==druhy[i],  ]
              stejny <- which(rzdl[, 5 ]>0.05)
              jiny <-   which(rzdl[, 5]<=0.05)
              
              if(NROW(stejny)>0){
                  points(x=stejny, y= exp((  rzdl[stejny,3]+ rzdl[stejny,4])/2), bg = barf[stejny], pch=21, cex=1, lwd=2)
                  points(x=stejny, y= exp((  rzdl[stejny,3]+ rzdl[stejny,4])/2), col = "black", pch="-", cex=1.5)
                  
              }
              if(NROW(jiny)>0){
                segments(x0=jiny, y0= exp(rzdl[jiny,3]), y1=exp(rzdl[jiny,4]), col = barf[jiny], lwd=8, lend=1)
                points(x=jiny, y=     exp(rzdl[jiny,4]), bg = barf[jiny], pch=21, cex=1, lwd=2)
                points(x=jiny, y=     exp(rzdl[jiny,3]), col = "black", pch="-", cex=1.5)
                
                }
              
              vsk<- exp(cele$breaks)
              cla <- match.closest( rnn[rnn$Group.1==druhy[i],3], vsk)
              abline(h=vsk[cla], lty=2)
              
              if(druhy[i] %in% v$taxon){
                ldt <- match.closest( v[v$taxon==druhy[i],4], vsk)
                abline(h=vsk[ldt], lwd=3)
              }
            
              
  par(mar=c(2,0,5,3), mgp=c(4, 0.7, 0))

  plot(hh[,ncol(hh)], hh[,1],  col="transparent", log="y",las=1, xlab="frequency", ylab="", main="",yaxt="n",  xlim=c(max(fossum)*-1, max(recsum)), cex.main=1.5, xaxt="n")
  # plot(hf[,ncol(hf)], hf[,1],  col="transparent",          las=1, xlab="frequency", ylab="", main=druhy[i], xlim=c(mx, 0), cex.main=1.5)
  pps <- seq(round(max(fossum)*-1, -1), round( max(recsum), -1), 10)
  axis(1,at=pps,labels = abs(pps)) 
 
  mtext( text=druhl[i], side=3, outer=F, line=1.8, adj=0, cex = 1.1, font = fontl[i]+1)
  mtext( text="fossil", side=3, outer=F, line=0, adj=0,cex = 0.9)
  mtext( text="trap", side=3, outer=F, line=0, adj=0.95, cex = 0.9)
  
  for(e in NROW(obl):1){
    if( sum(hf[,e+3])<0){
      segments(x0=0, x1=hf[hf[,e+3]<0,e+3], y0=hf[hf[,e+3]<0,1], y1=hf[hf[,e+3]<0,1], lwd=13/fktr, lend=1, col=barf[e] )
      #  axis(4,at=pru[e], labels=ob[e], col.axis=barf[e], las=1)
    }else {print(paste(druhy[i], obl[e], "chybi"))}
  }
  
  
    
  for(e in NROW(obl):1){
    if( sum(hh[,e+3])>0){
      segments(x0=0, x1=hh[hh[,e+3]>0,e+3], y0=hh[hh[,e+3]>0,1], y1=hh[hh[,e+3]>0,1], lwd=13/fktr, lend=1, col=barf[e] )
      #  axis(4,at=pru[e], labels=ob[e], col.axis=barf[e], las=1)
    }else {print(paste(druhy[i], obl[e], "chybi"))}
  }
  
  abline(v=0, lwd=3)
  
  
  vsk<- exp(cele$breaks)
  cla <- match.closest( rnn[rnn$Group.1==druhy[i],3], vsk)
  abline(h=vsk[cla], lty=2)
  axis(4, at=rnn[rnn$Group.1==druhy[i],5], labels=rnn[rnn$Group.1==druhy[i],2], col.ticks="transparent",col="transparent", line = -0.5, las=2, cex.axis=1.3)
  
 
  if(druhy[i] %in% v$taxon){
    ldt <- match.closest( v[v$taxon==druhy[i],4], vsk)
    abline(h=vsk[ldt], lwd=3)
    axis(4, at=vsk[ldt], labels="LDT", col.ticks="transparent",col="transparent", line = -0.5, las=2, cex.axis=1.3, font=2)
    
  }
}
par(mar=c(0,0,2,0))
plot( c(1,1,7),1:3,pch=15, col="transparent",xlim = c(0.4,7.6) , bty="n", axes=F, cex=4,ylab = "", xlab = "")



#plot(1,1, ylim=c(NROW(obl),0), "n", bty="n", axes = F, xlab = "", ylab = "")
text(  4.4,2.8, labels="regions | frequency" ,cex = 2, xpd=T)
points( c(1,2,3,3.8,5,5.8,6.8), rep(2,NROW(obl)),pch=15, col=barf, cex=3)
text(c(1,2,3,3.8,5,5.8,6.8), rep(1.5,NROW(obl)),labels=ob,cex = 1.2  )



#mtext( text="frequency of samples", side=1, outer=T, line=2, cex=1.8)
#mtext( text="PAR grains/cm2/year", side=2, outer=T, line=2, cex=1.8)

dev.off()



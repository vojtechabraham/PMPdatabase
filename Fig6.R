#### FUNCTIONS and LIBRARIES
library(mapplots)
library(plotrix)
library(maps)
library(geosphere)
library(gplots)  
library(foreign)
library(shapefiles)




colMax <- function (colData) { apply(colData, MARGIN=c(2), max)}
se <- function(x) sd(x)/sqrt(length(x))
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

sites <- c(
  "BRUN",
  "TOSACJQS", "TSOULACC",
    "SUOVAL",  "AKUV",
  "ABBO","KLOT",  "HOLT",
  "ROUGE",  "SUMINKO",
   "PRASILSKE", "MNIVA",
  "SAEGIST",  "BACHALP",
  "SHABLA",  "ARKUT2",
  "RIBNO",  "VOULK")

#########################################
# READ SHAPE FILES

require(raster)
setwd("C:/Users/vojta/ownCloud/documents/GIS_data")
forra <- raster("EuropeanForestMap/ALLforest_crs4326/forest.tif")
omance <- function(x){
  x[x==200] <- 0; x[x==300] <- 0
  return(x)
  }
forra_reclass <- calc(forra, fun = omance) 


colors <- c("transparent",add.alpha("darkgreen",0.1),add.alpha("darkgreen",0.2),add.alpha("darkgreen",0.3),
            add.alpha("darkgreen",0.4), add.alpha("darkgreen",0.5),add.alpha("darkgreen",0.6),
            add.alpha("darkgreen",0.7), add.alpha("darkgreen",0.8), add.alpha("darkgreen",0.9))

eu <- read.shapefile("World_Europe/world/continent")

vse <- read.table("EUFORGEN/vse.txt")
vse <- grep( ".prj", vse[,1], value = T)
cti <- substr(vse, 1, (nchar(vse)-4))
#cti <- cti[c(1:18, 21:30)]
cti[19] <- "Pinus pinea"
tax <- c(rep("Abies", 11),  "Alnus", "Alnus", "Betula", "Fagus", "Fagus", "Fraxinus", "Picea", rep("Pinus",6), rep("Quercus",4), "Tilia", "Tilia")
cti <- data.frame(tax, cti, stringsAsFactors = F)
for(i in 1:nrow(cti)){ assign(cti[i,2] ,shapefile(paste("EUFORGEN/",cti[i,2], sep = "")))}

cti[31:42,] <- cbind(c( "Alnus","Betula", "Quercus", "Fraxinus",
                        "Carpinus", "Carpinus", "Carpinus", 
                        "Corylus", "Juniperus", "Juniperus", "Juniperus", "Juniperus"),
                     c("alnus_incana2","betula_pubescens2", "quercus_coccifera2", "fraxinus_ornus2",
                       "carpinus_betulus", "carpinus_orientalis","ostrya_carpinifolia3",
                       "corylus", "ju_comm", "ju_oxy", "ju_pho", "ju_thur"))
for(i in 31:42){ assign(cti[i,2] ,shapefile(paste("ChorologyEuropeanSpec/change/",cti[i,2], sep="")))}


xlim <- c(-15, 47)
ylim <- c(34, 71)

############################
#Reading DATA
setwd("C:/Users/vojta/ownCloud/documents/PMP/__PMPpaper/_submission")
spsv <- read.table("additional/parameters_plot_Fig6.csv")
doBPmla <- -100
odBPsta <- 11000
cs <-rev(seq(250,10750, 500))

ran<- read.table("statistic/ranges2.txt") 
ran[ran$Group.2<1.99, 2] <- 1
ran[ran$Group.2<1.99, 3] <- 0
rnn <- aggregate(ran[,c(3,6)], by=list(ran[,1],ran[,2]), FUN=max)
rnn[rnn$hr<1000,5] <- round(rnn[rnn$hr<1000,"hr"],-1)
rnn[rnn$hr>1000,5] <- round(rnn[rnn$hr>1000,"hr"],-2)
rnn[rnn$hr2<1000,6] <- round(rnn[rnn$hr2<1000,"hr2"],-1)
rnn[rnn$hr2>1000,6] <- round(rnn[rnn$hr2>1000,"hr2"],-2)
rnn <- rnn[order(rnn$Group.1, rnn$Group.2),]
rnn[rnn$V6<rnn$V5,"V6"] <- "<"
i=1
druh=as.character(spsv[i,1])


titl=as.character(spsv$V5[i])
fon=as.integer(spsv$V6[i])
mn=as.integer(spsv$mnk[i])
ampl=as.integer(spsv$V4[i])


############################
#Reading DATA

dt <- read.table("statistic/recent_fossil_stat2.txt")
dt <- dt[dt$sites!="N/CJ/S2",]

############################

rgn <- read.table("additional/trap_areas_names.txt", sep="\t",stringsAsFactors=F, encoding = "UTF-8")
rgn[1,1] <- "FIN/SPH/Z80"
rgn[rgn$V11 %in% c("EST", "LVA"),11] <- "EST, LVA"
rgn[rgn$V11 %in% c("TR"),11] <- "TUR"
rgn[rgn[,"V12"] == "Finnmark" & rgn$V11=="FIN","V11"] <- "NOR"
rgn[,12] <- paste(rgn[,"V12"], " (",rgn[,"V11"],")", sep="")
rgn[(rgn$V12)=="North-Central Poland (POL)",12] <- ("N-C Poland (POL)")
rgn[(rgn$V12)=="Polistovo-Lovatskaya (RUS)",12] <- ("Polist.-Lovatsk. (RUS)")
rgn[310:315,12]
rgn[310:315,12] <- "Igneada/Kirk. (TUR)"
rgn[(rgn$V12)=="Riederhorn-Flesch (CHE)",12] <- "Riederhorn-Fl. (CHE)"
rgn[(rgn$V12)=="Capel Curig Wales (GBR)",12] <- "Capel Cur. Wales (GBR)"
rgn[38:49,12]
rgn[38:49,12] <- "Lofoten-Vester. (NOR)"
rgn <- rgn[order(as.integer(rgn$V10)),c(1,3,4,5,8,10,12)]


# SUBSET RECENT
let <- unique(dt[,c("sites", "age")])
druhy <-sort(as.character( unique(dt$taxa)))
vs <- merge(druhy,as.character( unique(dt[,1])) )
vsjo <- merge(let, vs, by.x=1, by.y=2, all=T)
dtba <- merge(vsjo, dt, by.x=1:3, by.y=c(1,3,2), all = T)
dtba[is.na(dtba)] <- 0
#dt[dt$V7<1.99 & dt$V7>0,"V7"] <- 1
dtba <-dtba[dtba$age==-100,]
dtbam <- merge( dtba, rgn[,c(1,6,7,3,4)], by=1, all=F, sort = F) 
dtbam <- dtbam[order(dtbam$V10, dtbam$x),]
colnames(dtbam)[c(3,7,8:11)] <- c("taxa", "cluster", "poradi", "obl", "lattr", "lontr")
pr <- aggregate(dtbam$poradi, list(dtbam$obl), FUN=mean)
pr$x <- (max(pr$x)-pr$x)
pr <- pr[order(pr$x),]
pr$x <- 1:nrow(pr)
dtbampr <- merge(dtbam, pr, by.x=9, by.y=1)
armm <- do.call(data.frame,aggregate(cbind(PAR)~obl+taxa, data=dtbam, function(dtbam) c(mean=mean(dtbam), se=se(dtbam), pc=length(dtbam))))
#colnames(armm) <- c("obl", "variable", "mean", "se", "pc")
mch <- data.frame(po=1:nrow(armm),ra=match(  armm$obl, unique(rgn$V12)))
mch <- mch[order(mch$ra), ]
armm <- armm[rev(mch$po),]
armm[armm$pc>10,"PAR.pc"] <- 10
armm[is.na(armm)]<-0


# prepare lines
obl <- as.character(unique(armm$obl))
rgn[rgn$V8=="lowersouth", "V8"] <- "highsouth"
reg <- unique(rgn[,c("V8","V12")])
reg <- reg[reg$V12 %in% obl,]
rownames(reg) <- 1:nrow(reg)
rr <- rev(unique(reg[,1])) 
i=1
fn <- rbind( data.frame(row.names=rr[i],atmin=min(which(reg$V8==rr[i])),  atmax=max(which(reg$V8==rr[i]))))    
fn <- fn[-1,]
for(i in 1:NROW(rr)){
  fn <- rbind(fn, data.frame(row.names=rr[i],atmin=min(which(reg$V8==rr[i])),  atmax=max(which(reg$V8==rr[i]))))    
}
# 

############################
# SUBSET FOSSIL
fosm <- read.table("fossil/mean_fosPAR.csv")
colnames(fosm) <- c("sites","taxa", "age", "PAR", "length", "se")
fosm[fosm$age==200,"age"] <- 250

rff <-dt[dt$age!=-100,]
#colnames(rff)[3] <- "taxa"
rff[rff$age==200,"age"] <- 250

renfe <- merge(fosm, rff[,c(1,2,3,7)],  by=c(1,2,3), all=T)
renfe[is.na(renfe)] <- 0

arff <- aggregate(renfe$V7, by=list(renfe$taxa, renfe$sites), FUN=max)
tecstra <- merge(arff, rff, by.x=c(2,1,3), by.y = c(1,2,7))
colnames(tecstra)[1:3] <- c("fossite","taxa", "cluster")

rff[rff$length>10,"length"] <- 10
   
library(RColorBrewer)
############################
##### SUBSET MAP
dtba_ <- merge(dtba, rgn[,c(1,6,7)], by=1)
bban <- aggregate(dtba_$V7, by=list(dtba_$x,dtba_$V12), FUN=min)
bbax <- aggregate(dtba_$V7, by=list(dtba_$x,dtba_$V12), FUN=max)
bba <- merge(bban, bbax, by=1:2)

swk <- sort(unique(dtba$V7))
bbb <- data.frame(taxon=NA, obl=NA, tridy=NA, stringsAsFactors = F)
for(i in 1:nrow(bba)){
bbb <- rbind(bbb, data.frame(taxon=bba[i,1],obl=bba[i,2],tridy=swk[match(bba[i,3], swk):match(bba[i,4], swk)]))
}
bbb <- bbb[-1,]
dtbb <- merge(bbb, dtba_, by.x=1:3, by.y=c(3,9,7), all=T)
dtbb <- dtbb[order(dtbb$taxon, dtbb$obl, dtbb$tridy),]


rd <- which(is.na(dtbb$PAR))
dtbb[rd,"age"] <- -200
i=1
for(i in 1:NROW(rd)){
#vrbr <- dtbb[dtbb$taxon==dtbb[rd[i],1]&dtbb$obl==dtbb[rd[i],2],]
dtbb[rd[i],c(4,6:9)] <- dtbb[rd[i]-1,c(4,6:9)]
}

colnames(dtbb)[c(1,3)] <- c("x", "V7")
dtbb<- dtbb[, colnames(dtba)]


    dist <- read.table("additional/from_gis/distances_fossil_traps.csv",sep=",", header=T, stringsAsFactors = F, encoding = "UTF-8")
    dist[dist$InputID=="RACHEL","InputID"] <- "PRASILSKE"
    dirfr <- merge(dist, dtbb, by.x=2, by.y=1, all.y = T) #############ZDE POZOR ZDA dtbb (realne a virtualni)anebo dtba (jen realne)
    
    frhot <- merge(dirfr, arff, by.x=c(2,5,9), by.y = c(2,1,3), all.y=T)
    frhot <- frhot[order(frhot$InputID,frhot$x, frhot$Distance),]
    vyb <- aggregate(frhot$Distance, by=list(frhot$InputID,frhot$x,frhot$V7), FUN=min)
    head(frhot)
    
    frrr <- merge(frhot, vyb, by.x=c(1,2,3,5), by.y=c(1,2,3,4))
    frrr <- frrr[order(frrr$x),]
    ###############
    foss <- read.dbf("additional/from_gis/foss.dbf")$dbf[,-2]
    i <- sapply(foss, is.factor)
    foss[i] <- lapply(foss[i], as.character)
    jmn <- foss[match(sites, foss[,4]),1]
    jmn <- c ("Bru", "Tos", "Tsu", "Suo", "Aku", "Abb", "Klo", "Hol", "Rou", "Sum", "Prá", "Mal", "Säg", "Bac",
    "Sha", "Ark", "Rib", "Vou")
    #jmn <- expression( bold("Bruvatnet"), bold("Toskaljavri"), bold("Tsuolbmajavri"), bold(
    #                     "Suovalampi"), bold("Akuvaara"), bold("Abborrtjärnen"), bold(
#                         "Klotjärnen"), bold("Holtjärnen"), bold("R" * tilde(o) * "uge T" * tilde(o) * "ugjärv"), 
#   bold("Suminko"), bold("Prášilské"), bold("Malá niva"), bold(
 #                        "Sägistalsee"), bold("Bachalpsee"), bold("Shabla"), bold(
  #                       "Arkutino 2"), bold("Ribno"), bold("Voulkaria"))
    
    frrr <- merge(frrr, foss, by.x=1, by.y=4)
    head(frrr)
    frg <- merge(frrr, rgn, by.x=5, by.y=1)
    x <- as.data.frame(x=rff$age)
    rff2 <- do.call(data.frame, aggregate(x, by=list(rff$sites, rff$taxa, rff$V7), function(x) c(min=min(x), pocet=length(x), max=max(x))))
    tail(rff2)
    fin <- merge(rff2, frg, by.x=c(1,2,3), by.y=c(2,3,4))
    
   i=1
   druh <- druhy[i] 
   
   podmm <- armm[armm$taxa==druh,] 
    kam <- barplot2( podmm$PAR.mean, horiz = T,   plot.ci=T,  ci.l=podmm$PAR.mean-podmm$PAR.se,ci.u=podmm$PAR.mean+podmm$PAR.se, col=podmm$PAR.pc,  main="zkouska", bty="n", las=2,cex.axis=1.5, axes=F )#, tcl=100, col.ticks="gray")
    dev.off()
    
    barkla <- c("grey",rainbow(9))#brewer.pal(max(max(plt$cluster), max(renfe$V7)), "Spectral")) 
      

  druh=  as.character(spsv[1,1])
  mn= spsv$mnk[1]
  ampl = spsv$V4[1]
  titl=spsv$V5[1]
  fon=spsv$V6[1]    
    qs=2
    qp=0.5
graf <- function(druh, mn, ampl, titl, fon){
#for (i in 1:NROW(sp)){
  # map data prep
  plt <- dtbam[dtbam$taxa==druh,c("lattr", "lontr", "cluster")]  
  plt <- plt[rev(order(plt$cluster)),]
  plt <- plt[plt$cluster!=0,]
  fos <-  fin[fin$Group.2==druh,c("x.y","y", "rff.age.min", "rff.age.max", "Group.3")]
  rec <-  fin[fin$Group.2==druh,c("V5","V4")]
  
  
# recent barplot data prep
#  podmm2 <- dtbampr[dtbampr$taxa==druh,]
podmm <- armm[armm$taxa==druh,]
sm <- aggregate(dtbam[dtbam$taxa==druh,"cluster"], by=list( dtbam[dtbam$taxa==druh,"obl"], dtbam[dtbam$taxa==druh,"cluster"]), FUN=length)
sm <- merge(sm, data.frame(obl, kam), by=1, all=T)
pchsm <- sm$Group.2
pchsm[pchsm>-1] <- 15
pchsm[sm$Group.2==0] <- 7
sm[sm$Group.2<2&sm$Group.2>1,"Group.2"] <-1
tr <- rnn[rnn$Group.1==druh,]

# strat plot data prep 
 drvy <- renfe[renfe$taxa== druh, ] 
 filtr <- aggregate(drvy$PAR, by=list(drvy$sites), FUN=sum)
  lk <- as.character(filtr[filtr$x>0,1])
  lk <- lk[order(match( lk, sites))]
  pw <- sort(match( lk, sites))
  jmnn <- jmn[pw]
  sloupcu <- NROW(lk)
  ha <- matrix(ncol=sloupcu+1, nrow=1)
  valmax <- c(drvy$PAR+drvy$se)
  
  for (hn in seq(1,sloupcu)) { ha[1,hn] <- max(valmax[which(drvy$age >= doBPmla & drvy$age < odBPsta & drvy$sites==lk[hn]) ]) }
  ha[1,ncol(ha)] <- max(podmm$PAR.mean+podmm$PAR.se)
  ha[ha==0]<- mn/2 #min(ha[ha>0])
  w <- as.integer(colMax(ha)/mn)+1
  ha4 <- w*mn
  #ha4[ha4==100+mn]<- 100
  ha5 <- ha4/1000
  sir <- c(sum(ha5)/2.5,ha5[sloupcu+1],sum(ha5)/4.5, min(ha5)*4, ha5[-(sloupcu+1)], min(ha5))#ha5[sloupcu+1]*1.5
 sire  <- sum(sir/(mn/1000))
  sire2  <- sum(sir/(mn/63.636363636363636363636363636364))
 #emf(paste(druh,"pks.emf",sep=""), height = 7, width = sire2*5)
#print(druh)
#pipa[i] <-(sire)
#}
  
# cairo_pdf(paste("Fig7.",druh,".pdf",sep=""), height = 10*ampl, width = 10*ampl, pointsize=10)
  
#  setwd("C:/Users/vojta/ownCloud/documents/PMP/__PMPpaper/NEW_GRAPHS")
png(paste("Fig6.",druh,".png",sep=""), height = 500*ampl, width = 500*ampl, units="mm", res=300, pointsize=16)
  nf <- layout(matrix(c(c(2:4, rep(1,(sloupcu+2))), c(2:4, seq(5,sloupcu+6))),2,sloupcu+5,byrow=T), width=sir, height=c(1,0.6))
  layout.show(nf)

  # drawing 1 map
  par(mar=c(5,10,5,10), cex.axis=1.5, cex.lab=2)
  if (druh == "total_concetration"){
      image(forra_reclass,col=colors, xlim=xlim, ylim=ylim,  xlab="Longitude", ylab="Latitude", cex.axis=qs, cex.lab=qs)
  
    }else{
      basemap(xlim, ylim,  cex.axis=qs, cex.lab=qs)
      draw.shape(eu, col="cornsilk", border="cornsilk")
      
      if (druh %in% cti[,1]){
          vbb <- cti[cti[,1]==(druh),]
       #   mgh <- get(vbb[1,2])
            
        #  if (nrow(vbb)==1 ){
         #     plot(mgh, col=add.alpha(gray(.4),alpha=0.4), border="transparent", add=T)
         # } else{
          #  mgh <- get(vbb[1,2])
              for(m in 1:(nrow(vbb))) {
            
              plot( get(vbb[m,2]), col=gray(.75), border="transparent", add=T)
          }
      }
  }
  
  
  
  symbols(jitter(plt[,2]),jitter(plt[,1] ), circles=plt[,3],  inches=1/5,  ann=F, bg=gray(0.2), fg=gray(0.2), xlim=xlim, ylim=ylim,add = T)
  e=1
  for(e in 1:nrow(fos)){
    gci <- gcIntermediate(fos[e,], rec[e,])
    lines(gci, lwd=1, col='red')
    arrows(x0= gci[nrow(gci)-1, "lon"], x1= gci[nrow(gci), "lon"], y0= gci[nrow(gci)-1, "lat"], y1= gci[nrow(gci),"lat"],lwd=1, col='red', length=0.1)
    symbols(x=fos$x, y=fos$y, squares = fos$Group.3, inches=1/5,  ann=F, bg="red", fg="black", xlim=xlim, ylim=ylim,add = T)
   # axis(4,at=fos$y,  labels=paste((fos$rff.age.max-50)/1000, (fos$rff.age.min-50)/1000, sep="-"), col.axis="red",tick=F, las=2)
  }
  title(main="(c)", cex.main=4, adj=0, line=1, font.main=2)
 legend("bottomleft", pch=c(16,22), col="black",pt.bg="red", bg="cornsilk", legend=c("trap","fossil"), pt.cex=5, cex = 2)
  # thigmophobe.labels(fos[,1:2], labels=paste(fos$rff.age.min,"-",fos$rff.age.max," BP", sep=""), cex=1.5, adj=1.3, font=2, col="red")
  
 #dev.off()  
 #png(paste("attempt1.",druh,".png",sep=""), height = 500*ampl, width = 400*ampl, units="mm", res=300, pointsize=16)
 
  #par( mfrow=c(1,4), mar=c(10,1,1,5))
  # drawing recent barplot - 3 windows 
  par(mar=c(14,0.5,14,0))
  plot(1:2,1:2,  col= 'transparent',  ylab="", xlab="", xlim=c(0, 20 ), ylim=c(doBPmla,odBPsta), yaxt="n",xaxt="n", main="", bty="n")
  title(main="(a)", cex.main=4, adj=0, line=1, font.main=2)
  #text(x=1, y=5000,labels="trap areas", cex=2.5, srt=90, adj=0.5)
  
  
  palette(rev(c(gray(0.05),gray(0.15),gray(0.25),gray(0.35),gray(0.45),gray(0.55),gray(0.65),gray(0.75),gray(0.85), gray(0.95))))
  kam <- barplot2( podmm$PAR.mean, horiz = T,   plot.ci=T,  ci.l=podmm$PAR.mean-podmm$PAR.se,ci.u=podmm$PAR.mean+podmm$PAR.se, col=gray(0.5),  main="", bty="n", las=2,cex.axis=qs, axes=F , yaxs="i")#, tcl=100, col.ticks="gray")
  nlb <- podmm$PAR.mean
  nlb[nlb<1000] <- round(nlb[nlb<1000],-1)
  nlb[nlb>1000] <- round(nlb[nlb>1000],-2)
  #text(x=podmm$PAR.mean+podmm$PAR.se, y=kam, labels=nlb, adj=-0.1)
  abline(h=kam[(42-fn$atmax)[-1]]+0.5, col="gray",xpd=T, lwd=2)
  axis(2, at=kam, labels=podmm$obl, las=1, cex.axis=qs+qp, font.axis=1)
  axis(side=1, at =  seq(0,ha4[NROW(ha4)], mn/2), labels =NA ,  tcl=par("tcl")*0.4)
  axis(side=1, at = seq(0,ha4[NROW(ha4)], mn), labels = c(seq(0,ha4[NROW(ha4)], mn)[-NROW(seq(0,ha4[NROW(ha4)], mn))],"") , cex.axis=qs, las=2)#0.75)
  
  title(main="(b)", cex.main=4, adj=1, line=1, font.main=2)
  
  par(mar=c(14,1,14,0))

  barplot2(rep(5,42), horiz = T, ylab = "", xlab="", xlim=c(-0.5,10), axes=F,col="transparent", border="transparent", yaxs="i")
  abline(h=kam[abs(42-(fn$atmax))]+0.5, col="gray",xpd=T, lwd=2)
  palette(c( barkla))
  abline(v=(0:10)[1:(NROW(tr$V5)+1)], col=(0:10), lwd=2, lty=2)
  palette(c("black", barkla))
  points(sm$Group.2,sm$kam, pch=pchsm, col=sm$Group.2+1, cex=3)
 kwd <- (0:10)[1:(NROW(tr$V5)+1)]
 kcd <- c(0,paste(tr$V5,"-",tr$V6))
  axis(1, at=kwd[seq(1,11,2)], kcd[seq(1,11,2)] , las=2, cex.axis=qs)
  axis(3, at=kwd[seq(2,10,2)], kcd[seq(2,10,2)] , las=2, cex.axis=qs)
  
 # plot(1:2,1:2,  col= 'transparent',  ylab="", xlab="", xlim=c(0, 20 ), ylim=c(doBPmla,odBPsta), yaxt="n",xaxt="n", main="", bty="n")
   #box()  
  mtext(titl,outer=T,side=3, adj=0.02, cex=4, font=fon, line=-4.3)  
  # boxplot(podmm2$PAR~podmm2$x, at=1:42, horizontal=T, yaxt="n", ylab="", border="transparent", las=2, cex.axis=2, xlab="")
  #points(x=podmm2$PAR, y=podmm2$x, col=podmm2$cluster+1, pch=15, cex=3)
  #boxplot(podmm2$PAR~podmm2$x, at=1:42, horizontal=T, yaxt="n", ylab="", add=T,  col="transparent", xlab="", xaxt="n")
  #dev.off()
  
  #drawing stratplot 
  par(mar=c(14,0.5,4,0))
   plot(1:2,1:2,  col= 'transparent',  ylab="", xlab="", xlim=c(0, 20 ), ylim=c(doBPmla,odBPsta), yaxt="n",xaxt="n", main="", bty="n")
  title(main="(d)", cex.main=4, line=0.5, adj=0.5, font.main=2)
 # palette(rev(c(gray(0.05),gray(0.15),gray(0.25),gray(0.35),gray(0.45),gray(0.55),gray(0.65),gray(0.75),gray(0.85), gray(0.95))))
  text(x=1, y=5000,labels="age (cal. BP)", cex=qs, srt=90, adj=0.5)
 i=1
  for (i in 1:(length(lk)))
  {
    
    print(lk[i])
    y <- drvy[which(drvy[,"sites"] == lk[i]),"age"]
    x0 <- match((cs),y)
    x0[which(is.na(x0))] <- 0
    x <- x0
    se <- x0
    pc <- x0
    x[which(x!=0)] <- rev(drvy[which(drvy[,"sites"] == lk[i]),"PAR"])
    se[which(se!=0)] <- rev(drvy[which(drvy[,"sites"] == lk[i]),"se"])
    pc[which(pc!=0)] <- rev(drvy[which(drvy[,"sites"] == lk[i]),"length"])
    mx<- ha4[i]
    fo <- tecstra[tecstra$fossite==lk[i] & tecstra$taxa==druh,]
    fo <- fo[order(fo$age),]
    
 
        if(nrow(fo)>=1){
          ktr <- match(fo$age, rev(cs))
          ic <- barplot2( x, horiz = T, plot.ci=F, col="transparent",border="transparent", ylab="", xlab="",  axes=F,xlim=c(0,mx),yaxt="n", main="", bty="n", las=2,cex.axis=1.5)#, tcl=100, col.ticks="gray")
          for(p in 1:NROW(ktr))  {
            yp <- rev(ic)[ktr[p]]
            polygon(x=c(0,mx, mx,0), y=c(yp-0.6,yp-0.6, yp+0.6, yp+0.6 ), col=barkla[fo[p,"cluster"]], border="transparent")
          }
          par(new=TRUE)
          }
        
        ic <- barplot2( x, horiz = T, plot.ci=T,  ci.l=x-se,ci.u=x+se, col="black", ylab="", xlab="", axes=F, xlim=c(0,mx),yaxt="n", main="", bty="n", las=2,cex.axis=1.5)#, tcl=100, col.ticks="gray")
        #polygon(c(0,x,0), c( min(y),  y,max(y)), col="black")
        text(cex=qs+qp, x=max(x)/10, y=NROW(x0)+5, jmnn[i], xpd=TRUE, font=1, pos=4, srt=90 )
        axis(side=1, at =  seq(0,ha4[i], mn/2), labels =NA ,  tcl=par("tcl")*0.4)
        axis(side=1, at = seq(0,ha4[i], mn), labels = c(seq(0,ha4[i], mn)[-NROW(seq(0,ha4[i], mn))],"") , cex.axis=qs, las=2)#0.75)
        
      if(i==1){
        axis(2, at=ic, labels=cs, las=1, cex.axis=qs)
      
        }   
        
    
    
      }
    
  
  
 
  mtext(expression("PAR (grains cm"^-2~"year"^-1*")") ,outer=T,side=1,cex=2, font=1.7, line=-2, adj=0.7)
  #mtext("age (years BP)",outer=T,side=2,cex=1.5, font=1.7, line=-2)
  
  dev.off()
  
  
}
i=2
  for(i in 1:nrow(spsv)){ 
    graf(as.character(spsv[i,1]), spsv$mnk[i], spsv$V4[i], spsv$V5[i], spsv$V6[i])
    
    }

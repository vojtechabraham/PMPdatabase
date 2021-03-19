
s <- unique(read.table("recent/annual_trap_PARs.csv",stringsAsFactors=F, encoding = "UTF-8")[,c("trap_id", "year_id", "elevation", "position_trap_type")])
NROW(unique(s[,1]))

library(xlsx)
env <-  read.xlsx("additional/from_gis/environ_variables.xlsx", sheetIndex = 1, encoding = "UTF-8")#[,c("Group.1","total_concetration", "bioclim1", "bioclim8", "bioclim12", "ForestBiomass10km", "y_wgs" )]
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
spe <- merge(sp, env, by=1, all.x = T)
kk <- data.frame(st=sort(unique(spe$V10)),V10=1:NROW(unique(spe$V10)))
colnames(sp)[8] <-"vyhodit"
spe <-merge(spe, kk, by.x=8, by.y = 1)[,-1]
spe <- spe[order(spe$V10.y, spe$year_id),]


sip <- unique(spe[,-2])
rownames(sip) <- 1:nrow(sip)
sip <- sip[!(sip$trap_id %in% c("UK/HP/CC7" )),]
sip[,55]<- (sip[,c( "total_concetration")]/rowSums(sip[,c("Cyperaceae", "Poaceae", "total_concetration")]))*100

uni <- unique(sip[,c("V10.y","total_concetration", "ForestBiomass10km", "V12", "V55")])[,-1]
i <- sapply(uni, is.factor)
uni[i] <- lapply(uni[i], as.character)
uni[,2] <- as.numeric(uni[,2])

str(uni)
cas <- cbind(29:1, cbind(seq( 0,  84, 3), seq(3,87,3)))
#cas <- cbind(9:1, cbind(seq( 0,  80, 10), seq(10,90,10)))
cs <- cas[,-1]
class_column <- function(co, sl, sitko, od, do)
{had <- rep(NA, nrow(co))
for (i in seq(1,nrow(sitko),1))
{had[which(co[,sl]>=sitko[i,od] & co[,sl]<sitko[i,do])] <- sitko[i,od]+((sitko[i,do]-sitko[i,od])/2)}
return(cbind(co,had))} 
proc <- class_column(uni, "ForestBiomass10km", cs, 1,2) 

cra  <- aggregate(proc$total_concetration, by=list(proc$had), FUN=min) 
elemko <- lm(cra$x~cra$Group.1)


crap  <- aggregate(proc$V55, by=list(proc$had), FUN=min) ###& zde proc$V55 nebo proc$ForestBiomass10km 
elemkop <- lm(crap$x~crap$Group.1)


tb <- read.table("additional/from_gis/dist_to_chorol.txt")
tb <- aggregate(tb$dist, by=list(tb$taxon, tb$trap), FUN=min)


## we include Alnus incana
tb[tb$Group.1=="Alnus" ,3] <- 0
tb[tb$Group.1=="Alnus" & tb$Group.2 %in% c("FIN/SPH/B65") ,3] <- 25000
tb[tb$Group.1=="Alnus" & tb$Group.2 %in% c("IS/MH/01","IS/MH/02", "IS/MH/03") ,3] <- 1200000
tb[tb$Group.1=="Alnus" & tb$Group.2 %in% c("N/CJ/S3","N/CJ/S2", "N/CJ/S1"),3] <- 800000

## we include Ostrya
tb[tb$Group.1=="Carpinus" & tb$Group.2 %in% c("CYP/TRO1", "CYP/TRO2", "CYP/TRO3", "CYP/TRO4") ,3] <- 130000

# Fagus
tb[tb$Group.1=="Fagus" & tb$Group.2 %in% c(
        "BUL/MF/10",
        "BUL/MF/16", 
        "BUL/MF/09", 
        "BUL/MF/08", 
        "BUL/MF/07", 
        "BUL/MF/05",
        "CH/WVDK/HAG"
        ),3] <- 0

tb[tb$Group.1=="Fagus" & substr(tb$Group.2,1,2) =="PL",3] <- 0

#Quercus
tb[tb$Group.1=="Quercus" & substr(tb$Group.2,1,2) %in% c("CZ", "RU", "BU", "GE", "CY", "GR", "TR") ,3] <- 0
tb[tb$Group.1=="Quercus" & substr(tb$Group.2,1,9) %in% c( "CH/WVDK/A", "CH/WVDK/F" ,"CH/WVDK/R", "CH/WVDK/S","CH/WVDK/Z","CH/WVDK/H", "CH/WVDK/M") ,3] <- 0

# Picea only Fennoscandia and Island
tb <- tb[!(tb$Group.1=="Picea" & !(substr(tb$Group.2,1,1) %in% c("S", "N", "F", "I"))),]

# Fraxinus
tb[tb$Group.1=="Fraxinus" & substr(tb$Group.2,1,2) %in% c( "BU", "GE", "CY", "GR", "TR") ,3] <- 0

# Carpinus
tb[tb$Group.1=="Carpinus" & substr(tb$Group.2,1,2) %in% c( "BU", "GE", "CY", "GR", "TR") ,3] <- 0

## UK out
tb <- tb[ !(substr(tb$Group.2,1,2) %in% c("UK")),]

dt <- read.table("statistic/recent_fossil_stat2.txt")
smi <- aggregate(dt[dt$taxa == "total_concetration", "PAR"], by=list(dt[dt$taxa == "total_concetration", "sites"], dt[dt$taxa == "total_concetration", "age"]), FUN=sum)
dt2 <- merge(dt, smi, by.x=c(1,3), by.y=1:2)
dt2[,9] <- (dt2$PAR/dt2$x)*100
colnames(dt2)[8:9]<-c("PARsum","proc")
dst <- merge(dt2, tb, by.x=c(3,1), by.y=c(1,2))
dst$x <- dst$x/1000
dr <- as.character(unique(dst$taxa)) 
dr <- dr[-c(3,8,10)]
i=4
dr <- dr[-2]
dr <- dr[-1]


## Andersen values
env <- env[!is.na(env$Group.1),]

par.ad<-env[,2:16]
par.ad$Alnus<-par.ad$Alnus*0.25
par.ad$Betula<-par.ad$Betula*0.25
par.ad$Carpinus<-par.ad$Carpinus*0.33
par.ad$Corylus<-par.ad$Corylus*0.25
par.ad$Cyperaceae<-par.ad$Cyperaceae*1
par.ad$Fraxinus<-par.ad$Fraxinus*2
par.ad$Juniperus<-par.ad$Juniperus*0.5
par.ad$Picea<-par.ad$Picea*0.5
par.ad$Pinus<-par.ad$Pinus*0.25
par.ad$Poaceae<-par.ad$Poaceae*1
par.ad$Quercus<-par.ad$Quercus*0.25
par.ad$Tilia<-par.ad$Tilia*2
par.ad$total_concetration<-rowSums(par.ad[,1:14])

####
for(i in 1:NROW(dr)){
        t  <- dst[ as.character(dst$taxa) == dr[i],]
        y  <- (log(t$PAR))
        x  <- (t$x)
        rg <- lm(y~x)
        vs <- exp(predict(rg,data.frame(x=200), interval='confidence'))
        vv <- data.frame(taxon=dr[i], obs=nrow(t), vs)
        if(i==1){ v<-vv} else {v<-rbind(v,vv) }
}
write.xlsx(v,"statistic/TLDT2.xlsx")
v <- read.xlsx("statistic/TLDT.xlsx", sheetIndex = 1)
require(gplots) 

#cairo_pdf("Fig5.pdf",  height=12, width=12, pointsize = 33 )

#tiff("Fig5.tiff",  height=600, width=600, pointsize = 23 )
#par(mar=c(6,5,4,1))
#ss <- barplot2(v$fit, plot.ci = T, ci.l = v$lwr, ci.u = v$upr, las=2, main="LDT at 200 km", ylab = expression("PAR (grains cm"^-2~"year"^-1*")")   )        
#axis(1, at=ss, font.axis=3,labels =  v$taxon , las=2, tck=F, col="transparent")
#dev.off()



library(plotrix)
cairo_pdf( "Fig3.pdf", height = 18, width = 20, pointsize = 33)

        #tiff("Fig3.tiff",  height=900, width=1000, pointsize = 23)
      
        
        par(mgp=c(4,1,0), mar=c(4,6,1,1), mfrow=c(1,2), oma=c(0,0,1,0))
        
        plot(env[,"y_wgs"], (par.ad$total_concetration), log="y", xlab = "", ylab ="", pch=16, las=1 , bty="n") # could you please turn this into a nice graph for the paper
        axis(2, at=c(10,100,1000,10000, 50000), labels=c("","","","",""))
        title(ylab=expression("total PAR (grains cm"^-2~"year"^-1*")"), line = 4.5)
        title(ylab="adjusted by Andersen values", line = 3.5)
        title(xlab="latitude", line = 2.5)
        
        
        #par(  mar=c(6,6,1,1) )#, oma=c(4,4,0,1))
        plot(uni$total_concetration~as.numeric(paste(uni$ForestBiomass10km)), main="",ylim=c(0,80000), bty="n",ylab = expression("tree PAR (grains cm"^-2~"year"^-1*")"), xlab="", las=1)
        title(xlab="forest cover within 10 km (%)", line = 2.5)
        axis(side=2, at =  seq(0,80000, 10000), labels =NA)
        axis(side=2, at =  seq(0,80000, 2500), labels =NA ,  tcl=par("tcl")*0.4)
        points(cra, pch=16)
        abline(elemko)
       legend(x=30, y=85000, pch=c(1,16), legend = c("1","2"), bty="n", horiz = T,xpd=T)
        # mtext( "forest cover within 10 km (%)", side=1,outer=F, cex=1, font=2, line=4, las=0)
        #mtext(expression("tree PAR (grains cm"^-2~"year"^-1*")"), outer=F,side=2, cex=1, font=2, line=5, las=0)
        mtext("(a)", outer=T,side=3, cex=2, font=2, line=-1, las=0, adj=0.01)
        mtext("(b)", outer=T,side=3, cex=2, font=2, line=-1, las=0, adj=0.5)
        dev.off()
        
        dr_rg <- list()
  cairo_pdf( "Fig4b_with_zero.pdf", height = 16, width = 32, pointsize = 48)
        
       # tiff("Fig4.tiff",  height=800, width=1600, pointsize = 32)
        #pdf("long_dist_PAR_novy.pdf")
        par( mfrow=c(2,4),mar=c(4,5,2,1),mgp=c(3,1,0), oma=c(3,2,0,0))
        i=2
        for(i in 1:NROW(dr)){
            
              tt <- dst[ as.character(dst$taxa) == dr[i],]
              plot(log(tt$PAR)~(tt$x), pch=16, main=dr[i], xlab="", ylab="" , yaxt="n", bty="n",  font.main=3,  cex.main=1.5,  col="transparent", xlim=c(-1*(max(tt$x)/20),max(tt$x)))#, ylim=c(-0.001, ))
              t <- dst[ as.character(dst$taxa) == dr[i],]
              
                             # text(thigmophobe(x=(t[t$PAR>100,"x"]),y=(t[t$PAR>100,"PAR"])), labels=(t[t$PAR>100,"sites"]), cex=.7, xpd=T)
#                text((t[t$PAR>100,"x"]),log(t[t$PAR>100,"PAR"]), labels=(t[t$PAR>100,"sites"]), cex=.7, xpd=T)
               #thigmophobe.labels((t[t$PAR>0,"x"]),log(t[t$PAR>0,"PAR"]), labels=substr(t[t$PAR>0,"sites"],1,10), cex=.5, xpd=T)
                
              y<-(log(t$PAR))
              x <- (t$x)
              rg <- lm(y~x)
              dr_rg[[i]] <- summary(rg)
            #-  title(main=paste("R2=",round(summary(rg)$r.squared,2), sep="" ), line=0.5, cex.main=0.7, font.main=1)
              abline(rg)
              
            #  rect(xleft = -1*(max(tt$x)/10),ybottom = 0,xright = 0,ytop = max(log(tt$PAR)), border = "transparent", col="white")
              axis(2, at=log(c(0.000001,1,3,10,30,100,300,1000,3000,10000,30000)), labels=c(c("",1,3,10,30,100,300,1000,3000,10000,30000)), las=2)
                    points(log(t$PAR)~(t$x), pch=16)
              
              nu <- dst[dst$x==0 & as.character(dst$taxa) == dr[i],]
              boxplot(log(nu$PAR),boxwex=max(tt$x)/10, xpd=T, at=-1*(max(tt$x)/20), add=T, bty="n", axes=F, range=4, col="gray", lty=1, lend=2)
             #v[v$taxon==dr[i], 7] <- mean(nu$PAR)
             #v[v$taxon==dr[i], 8] <- median(nu$PAR)
             
              if(i==1){
            mtext(side = 3, "(a)", cex=2, line=-0.3, font=2, adj=-1.2,  xpd=T)
          }
        }
        par( mar=c(2.5,5,2,1),mgp=c(3,1,0))
        ss <- barplot2(v$fit, plot.ci = T, ci.l = v$lwr, ci.u = v$upr, las=2, main="LDT at 200 km", ylab = expression("PAR (grains cm"^-2~"year"^-1*")")   )        
        mtext(side = 3, "(b)", cex=2, line=1, font=2, adj=-0.8,  xpd=T)
        axis(1, at=ss, font.axis=3,labels =  v$taxon , las=2, tck=F, col="transparent")
        
        mtext( "distance (km) from area of distribution",side=1,outer=T, cex=0.9, line=1, las=0)
        mtext(expression("PAR (grains cm"^-2~"year"^-1*")"), outer=T,side=2, cex=0.9, font=2, line=0, las=0)
        
        
        dev.off()

v[,9] <- v$fit/v$V8
colnames(v)[7:9] <- c("mean PAR within", "median PAR within", "ldt_to_median" )

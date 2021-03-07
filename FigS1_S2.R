
obl <- c("nboreal",        "boreal",  "sboreal",         "lowertemp",        "middletemp","hightemp",   "highsouth")
library(pgirmess)
library(xlsx)
env <-  read.xlsx("additional/from_gis/environ_variables.xlsx", sheetIndex = 1, encoding = "UTF-8")[,1:15 ]
env <- env[!is.na(env$Group.1),]
head(env)

ba <- read.table("additional/colours_taxa.txt", sep=";", stringsAsFactors=F)
rgn <- read.table("additional/trap_areas_names.txt", sep="\t",stringsAsFactors=F, encoding = "UTF-8")
head(rgn)
rgn[1,1] <- "FIN/SPH/Z80"
rgn[rgn$V11 %in% c("EST", "LVA"),11] <- "EST, LVA"

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

sp <- merge(env,rgn[,c(1,8,10:12)], by=1) # ve vyslednem sp je o jeden radek min nez v s protoze u HUN/EM/PCS-5 nenei koordinata a nevi se kde je
sp[sp$V8=="lowersouth","V8"] <- "highsouth"
sp <- sp[order(as.integer(sp$V10)),]
rownames(sp) <- 1:nrow(sp)

i=1
fn <- rbind( data.frame(row.names=obl[i],atmin=min(which(sp$V8==obl[i])),  atmax=max(which(sp$V8==obl[i]))))    
fn <- fn[-1,]
for(i in 1:NROW(obl)){
  fn <- rbind(fn, data.frame(row.names=obl[i],atmin=min(which(sp$V8==obl[i])),  atmax=max(which(sp$V8==obl[i]))))    
}

fn <- fn[obl,]


sp[sp[,"V12"] == "Finnmark" & sp$V11=="FIN","V11"] <- "NOR"
sp[,ncol(sp)+1] <- paste(sp[,"V12"], " (",sp[,"V11"],")", sep="")
sp <- sp[order(sp$V10),]
rg <- unique(sp[,ncol(sp)])

i=1
ff <- rbind( data.frame(row.names=rg[i],atmin=min(which(sp[,ncol(sp)]==rg[i])),  atmax=max(which(sp[,ncol(sp)]==rg[i]))))    
ff <- ff[-1,]
for(i in 1:NROW(rg)){
  ff <- rbind(ff, data.frame(row.names=rg[i],atmin=min(which(sp[,ncol(sp)]==rg[i])),  atmax=max(which(sp[,ncol(sp)]==rg[i]))))    
}
ff <- ff[rg,]


oblp <- c("arctic/alpine",        "n. boreal",  "boreal",         "lower temperate",        "middle alt. temperate","alpine",   "temperate/Mediterranean")




ylbd <- 4
ylbb <- 5
tik <- -0.12

#sp[sp==0]<-NA
sp[is.na(sp)]<-0
pdf("FigS1.pdf", height = 15, width = 20.3333333, pointsize = 25)
par(mar=c(0,8,1,3), mfrow=c(4,1))
par( mgp=c(6,0.5,0))
plot(1:14, rep(1,14),pch=15, col=ba$V3,ylim = c(1,10) , bty="n", axes=F, xlab="",cex=4,ylab = "")
text(1:14, rep(3,14), labels=ba$V1, srt=90, adj=0, cex = 2, font = ba$V4)
sp[is.na(sp)]<-0
ss <- barplot(t(as.matrix(pclig(sp[,ba$V1]))), beside=F,xaxt="n",  las=2, ylab="proportions", xaxs="i", xlab="", col = ba$V3, border="transparent" ,  cex.axis = 1.4 , xpd = T, cex.lab=1.4)
abline(v=ss[fn$atmax]+0.6, col="gray")
par(mar=c(0,8,1,3), mgp=c(6,0.5,0))
ss <- barplot(t(as.matrix(sp[,ba$V1])),beside=F,xaxt="n",  las=2, ylab=expression("PAR (grains cm"^-2~"year"^-1*")"), xaxs="i", xlab="", col = ba$V3, border="transparent", cex.axis = 1.4 , xpd = T,  cex.lab=1.4)
abline(v=ss[fn$atmax]+0.6, col="gray")
par(mgp=c(0,0.3,0))
axis(1, at= ss[floor(ff$atmin+ (ff$atmax-ff$atmin)/2)], labels=rg, tick=F, las=2, xpd=T)#, cex.axis=1.2, font.axis=2)
axis(1, at=  c(4.1,313.9), labels=c(  "Utsjoki (FIN)",  "Cyprus (CYP)") , tick=F, las=2, xpd=T)
axis(1, at=c(0,ss[ff$atmax]+0.6), labels=rep("",nrow(ff)+1), tck=-0.02)#rep("",9))
dev.off()

library(reshape)
fos <- read.table("fossil/mean_fosPAR.csv")[,1:4]
fos <- fos[fos$variable!="total_concetration",]
head(fos)
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
dt <- merge( unique(fos$variable), unique(fos$age))
dt2 <- merge(unique(fos$sites),  unique(fos$variable))
dtt <- merge(dt, dt2, by.x = 1, by.y=2)
fos <- merge( fos,dtt, by.y = c(3,1,2), by.x = 1:3, all=T)
fos[is.na(fos$mean),4] <- 0

jmn <- c( "Bruvatnet", "Toskaljavri", "Tsuolbmajavri",
                     "Suovalampi",           "Akuvaara" ,        
          "Abborrtjärnen",        "Klotjärnen",          "Holtjärnen"  ,      
          expression("R"*tilde(o)*"uge T"*tilde(o)*"ugjärv")    ,   "Suminko"      ,    
                  "Prášilské"  ,      "Malá niva"  ,    
          "Sägistalsee",          "Bachalpsee"  ,   
          "Shabla"       ,   "Arkutino 2"   ,   
          "Ribno"  ,        "Voulkaria")
i=1


pdf("FigS2.pdf", height = 20.3333333, width = 15, pointsize = 25)
par(mfrow=c(2,11), mar=c(3,0,14,0.5))
for(i in 1:18){
  if(i %in% c(1)){plot(1,1, col="transparent", axes = F, bty="n", xlab = "", ylab="")} 
  if(i %in% c(10)){plot(1,1, col="transparent", axes = F, bty="n", xlab = "", ylab="")
    plot(1,1, col="transparent", axes = F, bty="n", xlab = "", ylab="")
    
    par(mar=c(7,0,10,0.5))}
  
  
tbl <- cast(fos[fos$sites==sites[i],-1], formula=age~variable)
#tbl <- merge(dt, tbl, by=1, all.x=T)
ss <- barplot(t(as.matrix(pclig(tbl[order(-tbl$age),ba$V1]))), beside=F, names.arg = rep("",22), horiz = T,las=2, ylab="", xaxs="i", xlab="", col = ba$V3, border="transparent" )
text(0.4,ss[22]+1,jmn[i], srt=90, xpd=T , adj = 0, cex = 2, font = 2)
if(i %in% c(1,10)){
axis(2, at=ss, labels = rev(tbl$age), las=2, xpd=T)
}}
if(i==18){
plot( rep(1,14),1:14,pch=15, col=ba$V3,xlim = c(1,10) , bty="n", axes=F, cex=4,ylab = "", xlab = "")
text( rep(4,14),1:14, labels=ba$V1, adj=0, srt=45,cex = 1, xpd=T, font = ba$V4)
}
mtext(side = 1, outer = T, "proportions", line = -2)
dev.off()



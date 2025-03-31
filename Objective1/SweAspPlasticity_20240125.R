###Figure out which aspen clone has the most/least gxe
source("/Users/kelly.swarts/Code/treeringgenomics/Utils/TRGFunctions.R")
trans <- F
if (trans) {
  val <- "transVal"
  outDir <- "/Users/kelly.swarts/VBC\ Dropbox/Kelly\ Swarts/Kelly/Umea/Phenos/SwAsp_Heights_Diameters/Plasticity/backtransformed"
} else {
    val <- "rawVal"
    outDir <- "/Users/kelly.swarts/VBC\ Dropbox/Kelly\ Swarts/Kelly/Umea/Phenos/SwAsp_Heights_Diameters/Plasticity/untransformed"
    }
saevar <- read.table('/Users/kelly.swarts/VBC\ Dropbox/Kelly\ Swarts/Kelly/Umea/Phenos/SwAsp_Heights_Diameters/SwAsp_Saevar/SwAsp_Saevar_Heights_diameters.txt',header=T,as.is=T,sep="\t")
saevar$Location <- "Saevar"
#colnames(saevar)[2:10] <- paste("Saevar.",colnames(saevar)[2:10],sep="")
ekebo <- read.table('/Users/kelly.swarts/VBC\ Dropbox/Kelly\ Swarts/Kelly/Umea/Phenos/SwAsp_Heights_Diameters/SwAsp_Ekebo/SwAsp_Ekebo_Heights_diameters.txt',header=T,as.is=T,sep="\t")
ekebo$Location <- "Ekebo"
#colnames(ekebo)[2:5] <- paste("Ekebo.",colnames(ekebo)[2:5],sep="")

#get into shape for modeling height
seh <- saevar %>% filter(Clone>0) %>% select(!contains("Diameter"))
pcs <- colnames(seh)[which(grepl("^s",colnames(seh)))]
seh <- seh %>% pivot_longer(cols = any_of(pcs),values_to = "Height",names_to = c("Year","Type"),names_sep = "_") %>% filter(!is.na(Height)) %>% as.data.frame()
seh$Year <- as.numeric(str_remove(seh$Year,"s"))
seh <- seh %>% group_by(Clone,Block,Row,Plant) %>% mutate(!!paste("ar",1,sep="") := lag(x =Height,order_by = Year)) %>% as.data.frame()
write.table(seh %>% group_by(Clone,Year,Location) %>% summarise(cnt=dplyr::n()) %>% filter(cnt<2) %>% as.data.frame(),paste(outDir,"/LowRepSaevar.txt",sep=""),col.names = T,row.names = F,quote=F)
str(seh)
eeh <- ekebo %>% filter(Clone>0) %>% select(!contains(c("DBH","Diameter")))
pce <- colnames(eeh)[which(grepl("^e",colnames(eeh)))]
eeh <- eeh %>% pivot_longer(cols = any_of(pce),values_to = "Height",names_to = c("Year","Type"),names_sep = "_") %>% filter(!is.na(Height)) %>% as.data.frame()
eeh$Year <- as.numeric(str_remove(eeh$Year,"e"))
eeh <- eeh %>% group_by(Clone,Block,Row,Plant) %>% mutate(!!paste("ar",1,sep="") := lag(x =Height,order_by = Year)) %>% as.data.frame()
write.table(eeh %>% group_by(Clone,Year,Location) %>% summarise(cnt=dplyr::n()) %>% filter(cnt<2) %>% as.data.frame(),paste(outDir,"/LowRepEkebo.txt",sep=""),col.names = T,row.names = F,quote=F)
str(eeh)
df <- rbind(seh[,c("Location","Block","Row","Plant","Clone","Year","Height","ar1")],eeh[,c("Location","Block","Row","Plant","Clone","Year","Height","ar1")])

#check it out
type <- "Height"
pdf(paste(outDir,"/Plasticity",type,"_CheckNormality.pdf",sep=""),width = 7,height=5)
checkNorm(df[,type],main=paste("Check normality for raw",type))
checkNorm(log10(df[,type]),main=paste("Check normality for log10",type))
checkNorm(log(df[,type]),main=paste("Check normality for ln",type))
checkNorm(sqrt(df[,type]),main=paste("Check normality for sqrt",type))
#lme <- lm(as.formula(paste(type,"~Year",sep="")),na.action = na.omit,data=df)
#bc <- boxcox(lme)
#lambda <- bc$x[which(bc$y==max(bc$y))]
#print(paste("boxcox transformation value:",lambda))
dev.off()
##USE LOG10

#Setup and run model
df$LocBlk <- paste(df$Location,df$Block)
df$LocClone <- paste(df$Clone,df$Location)
df$LocYear <- paste(df$Location,df$Year)
df$LocCloneYear <- paste(df$Clone,df$Location,df$Year)
mod <- lmer(log10(Height)~(1|Location)+(1|LocYear)+(1|Clone)+(1|LocBlk)+(1|LocCloneYear),data=df,na.action = na.omit)
sink(paste(outDir,"/ModelFit.txt",sep=""))
summary(mod)
sink()


#plot residuals
pdf(paste(outDir,"/ResidualsVsFitted.pdf",sep=""),width = 7,height =5)
plot(resid(mod),fitted(mod))
dev.off()
pdf(paste(outDir,"/ResidualsQQ.pdf",sep=""),width = 7,height =5)
qqplot(resid(mod),rnorm(n = length(resid(mod)),mean = 0,sd = sd(resid(mod))))
dev.off()


#Check out location year effect
locYear <- data.frame(LocYear=rownames(ranef(mod)$LocYear),val=ranef(mod)$LocYear)
locYear$Loc <- unlist(lapply(locYear$LocYear,function(x){str_split(x," ")[[1]][1]}))
locYear$Year <- unlist(lapply(locYear$LocYear,function(x){str_split(x," ")[[1]][2]}))
locYear$LocCol <- "firebrick"
locYear$LocCol[which(locYear$Loc=="Saevar")] <- "navy"
locYear$rawVal <- locYear$X.Intercept. #
locYear$transVal <- 10^locYear$X.Intercept. #
pdf(paste(outDir,"/locYear.pdf",sep=""),width = 7,height = 5)
plot(locYear$Year,locYear[,val],col=locYear$LocCol,pch=19,cex=1.5,ylab="location-year BLUP", xlab="Year",main="Location+year variation for height")
legend("bottomright",legend = c("Ekebo","Saevar"),col=c("firebrick","navy"),pch=19)
dev.off()


#Check out location clone year effect
locCloneYear <- data.frame(LocCloneYear=rownames(ranef(mod)$LocCloneYear),val=ranef(mod)$LocCloneYear)
locCloneYear$Clone <- unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][1]}))
locCloneYear$Loc <- unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][2]}))
locCloneYear$Year <- as.numeric(unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][3]})))
locCloneYear$transVal <- 10^locCloneYear$X.Intercept. #
locCloneYear$rawVal <- locCloneYear$X.Intercept. #10^locCloneYear$X.Intercept. #
locCloneYear$LocCol <- "firebrick"
locCloneYear$LocCol[which(locCloneYear$Loc=="Saevar")] <- "navy"
plot(locCloneYear$Year,locCloneYear[,val],col=locCloneYear$LocCol,pch=19,cex=1.5)
plot(locCloneYear$Year[which(locCloneYear$Loc=="Saevar")],locCloneYear[,val][which(locCloneYear$Loc=="Saevar")],col=locCloneYear$LocCol[which(locCloneYear$Loc=="Saevar")],pch=19,cex=1.5)
locCloneYear <- locCloneYear[order(locCloneYear$Clone,locCloneYear$Loc,locCloneYear$Year),]
locCloneYear$yearLabels <- paste(locCloneYear$Loc,locCloneYear$Year,sep="-")
locCloneYear$Year[which(locCloneYear$Loc=="Saevar")] <- 10+locCloneYear$Year[which(locCloneYear$Loc=="Saevar")] #just for plotting purposes


#generate platicity scores
plast <- data.frame(clone=character(0),stderrEst=numeric(0))
for (clone in unique(locCloneYear$Clone)) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  plast <- rbind(plast,data.frame(clone=clone,stderrEst=sd(cur[,val])/sqrt(length(cur[,val]))))
}
plast <- plast[order(plast$stderrEst),]


#add genotypic estimates
plast$genoEst <- ranef(mod)$Clone$'(Intercept)'[match(plast$clone,rownames(ranef(mod)$Clone))] #10^(ranef(mod)$Clone$'(Intercept)'[match(plast$clone,rownames(ranef(mod)$Clone))]) #
genoHeat <- getHeatMapCol(plast$genoEst,color.range = c("navy","white","firebrick"))
plast$genoCol <- genoHeat$cols
plast$col <- "#00000080"
plast$col[1:10] <- "lightblue"
plast$col[(nrow(plast)-10):nrow(plast)] <- "pink"


#plot low
pdf(paste(outDir,"/locCloneYear_LowPlasticity.pdf",sep=""),width = 7,height = 4)
plot(0,xlim=c(min(locCloneYear$Year),(max(locCloneYear$Year)+2)),ylim=c(min(locCloneYear[,val]),max(locCloneYear[,val])),ylab="GxE BLUP",xlab="",xaxt="n")
title(main="Low plasticity",outer=F,line = 0.1)
title(main="Clone by location-year",outer=F)
for (clone in unique(locCloneYear$Clone)) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  lines(cur$Year,cur[,val],col="#00000080")
}
for (clone in plast$clone[1:10]) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  lines(cur$Year,cur[,val],col=plast$genoCol[which(plast$clone==clone)],lwd=2)
}
addColorScale(info = plast,heatmap = genoHeat, colorTerm = "genoEst", title="Clone BLUP")
axis(side = 1,las=2,at = unique(locCloneYear$Year),labels = unique(locCloneYear$yearLabels),main="Year")
dev.off()


#plot high
pdf(paste(outDir,"/locCloneYear_HighPlasticity.pdf",sep=""),width = 7,height = 4)
plot(0,xlim=c(min(locCloneYear$Year),(max(locCloneYear$Year)+2)),ylim=c(min(locCloneYear[,val]),max(locCloneYear[,val])),ylab="GxE BLUP",xlab="",xaxt="n")
title(main="High plasticity",outer=F,line = 0.1)
for (clone in unique(locCloneYear$Clone)) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  lines(cur$Year,cur[,val],col="#00000080")
}
for (clone in plast$clone[(nrow(plast)-10):nrow(plast)]) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  lines(cur$Year,cur[,val],col=plast$genoCol[which(plast$clone==clone)],lwd=2)
}
addColorScale(info = plast,heatmap = genoHeat,colorTerm = "genoEst",title="Clone BLUP")
axis(side = 1,las=2,at = unique(locCloneYear$Year),labels = unique(locCloneYear$yearLabels),main="Year")
dev.off()


#plot all, colored by G
pdf(paste(outDir,"/locCloneYear_AllGxebyGenotypeHighlightLow.pdf",sep=""),width = 7,height = 4)
plot(0,xlim=c(min(locCloneYear$Year),(max(locCloneYear$Year)+2)),ylim=c(min(locCloneYear[,val]),max(locCloneYear[,val])),ylab="GxE BLUP",xlab="",xaxt="n")
title(main="High plasticity",outer=F,line = 0.1)
for (clone in plast$clone[order(plast$genoEst,decreasing = T)]) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  lines(cur$Year,cur[,val],col=plast$genoCol[which(plast$clone==clone)],lwd=1)
}
addColorScale(info = plast,heatmap = genoHeat,colorTerm = "genoEst",title="Clone BLUP")
axis(side = 1,las=2,at = unique(locCloneYear$Year),labels = unique(locCloneYear$yearLabels),main="Year")
dev.off()


pdf(paste(outDir,"/locCloneYear_AllGxebyGenotypeHighlightHigh.pdf",sep=""),width = 7,height = 4)
plot(0,xlim=c(min(locCloneYear$Year),(max(locCloneYear$Year)+2)),ylim=c(min(locCloneYear[,val]),max(locCloneYear[,val])),ylab="GxE BLUP",xlab="",xaxt="n")
title(main="High plasticity",outer=F,line = 0.1)
for (clone in plast$clone[order(plast$genoEst,decreasing = F)]) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  lines(cur$Year,cur[,val],col=plast$genoCol[which(plast$clone==clone)],lwd=1)
}
addColorScale(info = plast,heatmap = genoHeat,colorTerm = "genoEst",title="Clone BLUP")
axis(side = 1,las=2,at = unique(locCloneYear$Year),labels = unique(locCloneYear$yearLabels),main="Year")
dev.off()


#plot genotype against plasticity
pdf(paste(outDir,"/PlasticityByGenoBLUP_highlight10.pdf",sep=""),width = 7,height = 5)
plot(plast$genoEst,plast$stderrEst,col=plast$col,xlab="Genotypic estimates",ylab="Plasticity score",pch=19,cex=2)
lines(abline(lm(plast$stderrEst~plast$genoEst)),col="firebrick")
text(plast$genoEst[c(1:10,((nrow(plast)-10):nrow(plast)))],plast$stderrEst[c(1:10,((nrow(plast)-10):nrow(plast)))],label=plast$clone[c(1:10,((nrow(plast)-10):nrow(plast)))])
dev.off()


#######
#within location
for (loc in c("Saevar","Ekebo")) {
  if (loc=="Saevar") {df <- seh[,c("Location","Block","Row","Plant","Clone","Year","Height","ar1")]
  }else {df <- eeh[,c("Location","Block","Row","Plant","Clone","Year","Height","ar1")]}
  locDir <- paste(outDir,"/",loc,sep="")
  mkdirs(locDir)
  
  
  #Setup and run model
  df$CloneYear <- paste(df$Clone,df$Year)
  df$Clone <- as.factor(df$Clone)
  df$Year <- as.factor(df$Year)
  
  mod <- lmer(log10(Height)~(1|Clone)+(1|Block)+(1|CloneYear)+(1|Year),data=df,na.action = na.omit)
  mod <- lmer(log10(Height)~(1|Block)+Clone*Year,data=df,na.action = na.omit)
  
  sink(paste(locDir,"/",loc,"Only_ModelFit.txt",sep=""))
  print(summary(mod))
  sink()
  
  
  cloneYear <- data.frame("cloneYear"=names(fixef(mod)[which(grepl(":",names(fixef(mod))))]),rawVal=fixef(mod)[which(grepl(":",names(fixef(mod))))],transVal=10^(fixef(mod)[which(grepl(":",names(fixef(mod))))]))
  cloneYear$Year <- as.numeric(unlist(lapply(cloneYear$cloneYear,function(x){strsplit(x,"Year")[[1]][2]})))
  cloneYear$Clone <- as.numeric(str_replace(unlist(lapply(cloneYear$cloneYear,function(x){strsplit(x,":")[[1]][1]})),"Clone",""))
  plast <- data.frame("clone"=as.numeric(str_replace(names(fixef(mod)[which(grepl(":",names(fixef(mod)))==F & grepl("Clone",names(fixef(mod)))==T)]),"Clone","")),genoEst=fixef(mod)[which(grepl(":",names(fixef(mod)))==F & grepl("Clone",names(fixef(mod)))==T)])
  plast$stderrEst <- NA
  genoHeat <- getHeatMapCol(plast$genoEst,color.range = c("navy","white","firebrick"))
  plast$genoCol <- genoHeat$cols
  
  
  #plot and generate plasticity scores
  pdf(paste(locDir,"/locCloneYear_AllPlasticity.pdf",sep=""),width = 7,height = 4)
  plot(0,xlim=c(min(cloneYear$Year),(max(cloneYear$Year)+2)),ylim=c(min(cloneYear[,val]),max(cloneYear[,val])),ylab="GxE BLUP",xlab="",xaxt="n")
  title(main=paste("All plasticity within",loc),outer=F,line = 0.1)
  title(main="Clone by year",outer=F)
  for (clone in unique(cloneYear$Clone)) {
    cur <- cloneYear[which(cloneYear$Clone==clone),]
    plast[which(plast$clone==clone),"stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
    lines(cur$Year,cur[,val],col=plast$genoCol[which(plast$clone==clone)],lwd=1)
  }
  plast <- plast[order(plast$stderrEst),]
  addColorScale(info = plast,heatmap = genoHeat,colorTerm = "genoEst",title="Clone BLUP")
  axis(side = 1,las=2,at = unique(locCloneYear$Year),labels = unique(locCloneYear$yearLabels),main="Year")
  dev.off()
  
  
  #plot high
  pdf(paste(locDir,"/locCloneYear_HighPlasticity.pdf",sep=""),width = 7,height = 4)
  plot(0,xlim=c(min(cloneYear$Year),(max(cloneYear$Year)+2)),ylim=c(min(cloneYear[,val]),max(cloneYear[,val])),ylab="GxE BLUP",xlab="",xaxt="n")
  title(main=paste("High plasticity within",loc),outer=F,line = 0.1)
  title(main="Clone by year",outer=F)
  for (clone in unique(cloneYear$Clone)) {
    cur <- cloneYear[which(cloneYear$Clone==clone),]
    plast[which(plast$clone==clone),"stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
    lines(cur$Year,cur[,val],col="#00000080",lwd=1)
  }
  for (clone in unique(plast$clone[order(plast$stderrEst,decreasing = T)])[1:10]) {
    cur <- cloneYear[which(cloneYear$Clone==clone),]
    plast[which(plast$clone==clone),"stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
    lines(cur$Year,cur[,val],col=plast$genoCol[which(plast$clone==clone)],lwd=3)
  }
  addColorScale(info = plast,heatmap = genoHeat,colorTerm = "genoEst",title="Clone BLUP")
  axis(side = 1,las=2,at = unique(locCloneYear$Year),labels = unique(locCloneYear$yearLabels),main="Year")
  dev.off()
  
  
  #plot low
  pdf(paste(locDir,"/locCloneYear_LowPlasticity.pdf",sep=""),width = 7,height = 4)
  plot(0,xlim=c(min(cloneYear$Year),(max(cloneYear$Year)+2)),ylim=c(min(cloneYear[,val]),max(cloneYear[,val])),ylab="GxE BLUP",xlab="",xaxt="n")
  title(main=paste("Low plasticity within",loc),outer=F,line = 0.1)
  title(main="Clone by year",outer=F)
  for (clone in unique(cloneYear$Clone)) {
    cur <- cloneYear[which(cloneYear$Clone==clone),]
    plast[which(plast$clone==clone),"stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
    lines(cur$Year,cur[,val],col="#00000080",lwd=1)
  }
  for (clone in unique(plast$clone[order(plast$stderrEst,decreasing = F)])[1:10]) {
    cur <- cloneYear[which(cloneYear$Clone==clone),]
    plast[which(plast$clone==clone),"stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
    lines(cur$Year,cur[,val],col=plast$genoCol[which(plast$clone==clone)],lwd=3)
  }
  addColorScale(info = plast,heatmap = genoHeat,colorTerm = "genoEst",title="Clone BLUP")
  axis(side = 1,las=2,at = unique(locCloneYear$Year),labels = unique(locCloneYear$yearLabels),main="Year")
  dev.off()
  
  
  #plot genotype against plasticity
  plast$col <- "#00000080"
  plast$col[1:10] <- "lightblue"
  plast$col[(nrow(plast)-10):nrow(plast)] <- "pink"
  pdf(paste(locDir,"/PlasticityByGenoBLUP_highlight10.pdf",sep=""),width = 7,height = 5)
  plot(plast$genoEst,plast$stderrEst,col=plast$col,xlab="Genotypic estimates",ylab="Plasticity score",main=paste(loc,"Adj. R2:",round(summary(lm(plast$stderrEst~plast$genoEst))$adj.r.squared,3)),pch=19,cex=2)
  lines(abline(lm(plast$stderrEst~plast$genoEst)),col="firebrick")
  text(plast$genoEst[c(1:10,((nrow(plast)-10):nrow(plast)))],plast$stderrEst[c(1:10,((nrow(plast)-10):nrow(plast)))],label=plast$clone[c(1:10,((nrow(plast)-10):nrow(plast)))])
  dev.off()
}

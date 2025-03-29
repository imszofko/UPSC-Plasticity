trans <- F
if (trans) {
  val <- "transVal"
  outDir <- "~/UPSC_ThesisWork/Data/Plasticity/backtransformed/"
} else {
  val <- "rawVal"
  outDir <- "~/UPSC_ThesisWork/Data/Plasticity/untransformed/"
}

#Loading packages
library(lme4)
library(stringr)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
library(broom.mixed)
library(RColorBrewer)
library(ggpubr)

#Look at the more specific Block effect within the two separate locations
#Defining South, Middle, and North Sweden and identify clones local/nonlocal to Ek/Sä
#Defining North, Middle and South of Sweden
#
###Functions used in the code

AspLocation <- function(Latitude){
  northSwe <- 63.00 #above 63
  southSwe <- 59.00 #Below 59
  
  if (is.na(Latitude)){
    return(NA)
  }
  if (Latitude <= southSwe){
    return("Southern")
  }
  if (Latitude > southSwe & Latitude < northSwe) {
    return("Middle")
  }
  else{
    return("Northern")
  }
}

QuantileVal <- function(residval, genoval, residVector, genoVector){
  resid25 <- round(quantile(residVector, 0.25, na.rm = T), 4)
  resid75 <- round(quantile(residVector, 0.75, na.rm = T), 4)
  geno25 <- round(quantile(genoVector, 0.25, na.rm = T), 4)
  geno75 <- round(quantile(genoVector, 0.75, na.rm = T), 4)
  
  #Assigning labels
  if (residval <= resid25 && genoval <= geno25){
    return("BP,BG")
  }
  
  if (residval >= resid75 && genoval <= geno25){
    return("GP, BG")
  }
  if (residval <= resid25 && genoval >= geno75) {
    return("BP,GG")
  }
  if (residval >= resid75 && genoval >= geno75) {
    return("GP,GG")
  }
  else{
    return("Center")
  }
}

QuantileDf <- function(dataframe, residCol, genotypeCol){
  dataframe$QLabels <- mapply(QuantileVal, 
                              dataframe[[residCol]], 
                              dataframe[[genotypeCol]],
                              MoreArgs = list(residVector = dataframe[[residCol]],
                                              genoVector = dataframe[[genotypeCol]]))
  return(dataframe)
}

SaevarData <- read.table("~/UPSC_ThesisWork/Data/SwAsp_Saevar/SwAsp_Saevar_Heights_diameters.txt",
                         header = T,
                         as.is = T,
                         sep = '\t') #4473 entries after tidying
SaevarData$Location <- "Saevar"

SaevarProvenanceData <- read.table("~/UPSC_ThesisWork/Data/SwAsp_Saevar/SwAsp.full.clone.descriptions.txt",
                                   header = T,
                                   as.is = T,
                                   sep = '\t')

SaeverGEData <- read.table("~/UPSC_ThesisWork/Data/Saevar_buds_GE/GE_matrix_samples.tsv",
                           header = T,
                           as.is = T,
                           sep = '\t')

#The names of the genotypes are funky in the GE data, so we need to make sure the names match up first.
#Importing the SwAsp name reassignment table
SaevarNameReassignment <- read.table("~/UPSC_ThesisWork/Data/Saevar_buds_GE/swasp_reassignments.txt",
                                     header = T,
                                     as.is = T,
                                     sep = '\t')

##Tidying up THE Saevar Data
#Check clone lifespan
SaevarData <- SaevarData %>% filter(Clone > 0) %>% dplyr::select(!contains("Diameter"))
saevarColNames <- colnames(SaevarData)[which(grepl("^s", colnames(SaevarData)))]
#Unites the columns needed to create the same sample name format of old.samples in SaevarNameReassignment
SaevarData <- unite(SaevarData, col = "GE_Sample_Name", c('Meta.col', 'Meta.row', 'Clone'), sep = '-')
SaevarData[c("Meta.col", "Meta.row", "Clone")] <- str_split_fixed(SaevarData$GE_Sample_Name, '-', 3)
SaevarData <- SaevarData %>% pivot_longer(cols = any_of(saevarColNames) , values_to = "Height", names_to = c("Year","Type"),names_sep = "_") %>% as.data.frame()
SaevarData$Year <- as.numeric(str_remove(SaevarData$Year, "s"))
SaevarData <- SaevarData %>% group_by(Clone, Block, Row, Plant) %>% mutate(!!paste("ar",1,sep="") := lag(x =Height,order_by = Year)) %>% as.data.frame()
#SaevarData <- SaevarData %>%  group_by(Clone) %>%   mutate(Status = ifelse(n_distinct(Year) == 9 & (!is.na(Height)), "Alive", "Dead")) %>%   ungroup()

write.table(SaevarData %>% group_by(Clone,Year,Location) %>% summarise(cnt = dplyr::n()) %>% filter(cnt < 2) %>% as.data.frame(),paste(outDir,"/LowRepSaevarData.txt",sep=""),col.names = T,row.names = F,quote=F)

#Filling in the remaining heights of the dead trees
#SaevarData <- SaevarData %>%   group_by(GE_Sample_Name) %>%   arrange(GE_Sample_Name, Year) %>%    mutate(Height = ifelse(Status == "Dead", NA, Height)) %>%   fill(Height, .direction = "downup")

#Creating the locations for Saevar
SaevarData$Latitude <- SaevarProvenanceData$Latitude[match(SaevarData$Clone, SaevarProvenanceData$Clone)]
SaevarData$Population <- SaevarProvenanceData$Population[match(SaevarData$Clone, SaevarProvenanceData$Clone)]
SaevarData$Province <- sapply(SaevarData$Latitude, AspLocation)
SaevarData$Year <- as.factor(SaevarData$Year)
SaevarData$CloneProvYear <- paste(SaevarData$Province, SaevarData$Clone, SaevarData$Year)
SaevarData$LocCloneYear <- paste(SaevarData$Clone, SaevarData$Location, SaevarData$Year)
SaevarData$Height <- log10(SaevarData$Height)
SaevarData$Clone <- as.factor(SaevarData$Clone)
SaevarData$Clone <- relevel(SaevarData$Clone, ref = "67")

type = "Height"
pdf(paste(outDir, type, "Saevar_NormalityCheck.pdf", sep = ""),width = 7,height=5)
hist(SaevarData$Height, col = 'red', main = "Sävar Data")
dev.off()

pdf(paste(outDir, type, "_Distribution_Saevar.pdf", sep = ""),width = 7,height=5)
hist(log10(SaevarData$Height), col = 'purple', main = paste("log10 Transformation for Sävar",type))
hist(log(SaevarData$Height), col = 'purple', main = paste("log Transformation for Sävar", type))
hist(sqrt(SaevarData$Height), col = 'purple', main = paste("Sqrt Transformation for Sävar", type))
dev.off()

##Ekebo Data
EkeboData <- read.table("~/UPSC_ThesisWork/Data/SwAsp_Ekebo/SwAsp_Ekebo_Heights_diameters.txt",
                        header = T,
                        as.is = T,
                        sep = '\t') # 504 entries
EkeboData$Location <- "Ekebo"

##create subset and then filter and tidy up the data

#Now I am going to create a subset of data, that will only include the genotype data of the GE data that we DO have
EkeboGEData <- read.table("~/UPSC_ThesisWork/Data/Ekebo_wood_GE/GE_matrix_genotype_vst.tsv",
                          header = T,
                          as.is = T,
                          sep = '\t',
                          row.names = 1)

#Tidying up the original data
EkeboData <- EkeboData %>% filter(Clone > 0) %>% dplyr::select(!contains(c("DBH","Diameter")))
ekeboColNames <- colnames(EkeboData)[which(grepl("^e",colnames(EkeboData)))]
EkeboData <- EkeboData %>% pivot_longer(cols = any_of(ekeboColNames),values_to = "Height",names_to = c("Year","Type"),names_sep = "_") %>% filter(!is.na(Height)) %>% as.data.frame()
EkeboData$Year <- as.numeric(str_remove(EkeboData$Year,"e"))
EkeboData <- EkeboData %>% group_by(Clone,Block,Row,Plant) %>% mutate(!!paste("ar",1,sep="") := lag(x =Height,order_by = Year)) %>% as.data.frame()
#EkeboData <- EkeboData %>%  group_by(Clone) %>%  mutate(Status = ifelse(n_distinct(Year) == 8 & (!is.na(Height)), "Alive", "Dead")) %>%  ungroup()

#Storing the column names of the GE data (this is what will be in the subset) and turning it into a dataframe
ekeboGEColNames <- colnames(EkeboGEData)
ekeboGEColNames <- data.frame(ekeboGEColNames)

#Extracting the clone number from the sample ID
ekeboGEColNames <- ekeboGEColNames %>% mutate(Clones = str_extract(ekeboGEColNames, "(?<=_)\\d+"))
ekeboGEColNames$Clones <- as.numeric(ekeboGEColNames$Clones)

#Removing duplicates of ekeboColNames
EkeboGEClones <- ekeboGEColNames[!duplicated(ekeboGEColNames$Clones),]

#Filling in the remaining heights of the dead trees
#EkeboData <- EkeboData %>%   group_by(Ekebo.Pos) %>%  arrange(Ekebo.Pos, Year) %>%    mutate(Height = ifelse(Status == "Dead", NA, Height)) %>%   fill(Height, .direction = "downup")

#Creating locations for Ekebo
EkeboData$Latitude <- SaevarProvenanceData$Latitude[match(EkeboData$Clone, SaevarProvenanceData$Clone)]
EkeboData$Population <- SaevarProvenanceData$Population[match(EkeboData$Clone, SaevarProvenanceData$Clone)]
EkeboData$Province <- sapply(EkeboData$Latitude, AspLocation)
EkeboData$Clone <- as.factor(EkeboData$Clone)
EkeboData$Year <- as.factor(EkeboData$Year)
EkeboData$CloneProvYear <- paste(EkeboData$Clone, EkeboData$Province, EkeboData$Year)
EkeboData$LocCloneYear <- paste(EkeboData$Clone, EkeboData$Location, EkeboData$Year)
EkeboData$Height <- log10(EkeboData$Height)
EkeboData$Clone <- relevel(EkeboData$Clone, ref = "67")


EkeboSubset <- subset(EkeboData, EkeboData$Clone %in% EkeboGEClones$Clones) #422 entries
SaevarSubSet <- subset(SaevarData, SaevarData$GE_Sample_Name %in% SaevarNameReassignment$old.sample) # 1962 entries

pdf(paste(outDir, type, "Ekebo_NormalityCheck.pdf", sep = ""),width = 7,height=5)
hist(EkeboData$Height, col = 'red', main = "Ekebo Data")
dev.off()

pdf(paste(outDir, type, "_Distribution_Ekebo.pdf", sep = ""),width = 7,height=5)
hist(log10(EkeboData$Height), col = 'purple', main = paste("log10 Transformation for Ekebo",type))
hist(log(EkeboData$Height), col = 'purple', main = paste("log Transformation for Ekebo", type))
hist(sqrt(EkeboData$Height), col = 'purple', main = paste("Sqrt Transformation for Ekebo", type))
dev.off()

#All Data
AllData <- rbind(SaevarData[, c("Location","Block","Row","Plant","Clone","Year", "Population", "Province","Height", "ar1")], EkeboData[, c("Location","Block","Row","Plant","Clone","Year", "Population", "Province","Height", "ar1")])
AllData$Location <- factor(AllData$Location)
AllData$Year <- factor(AllData$Year)
AllData$LocBlock <- paste(AllData$Location, AllData$Block)
AllData$LocClone <- paste(AllData$Clone, AllData$Location)
AllData$LocYear <- paste(AllData$Location, AllData$Year)
AllData$LocCloneYear <- paste(AllData$Clone, AllData$Location, AllData$Year)
AllData$CloneProvYear <- paste(AllData$Clone, AllData$Province, AllData$Year)
hist(AllData$Height, col = 'purple', main = paste("log10 Transformation for All Data",type))
AllData$Height <- log10(AllData$Height)

####Modelling
##All Data
mod <- lmer((Height) ~ (1 | Block) + (1|Clone) + (1 | LocCloneYear) + (1 | Year) + (1|LocBlock) + (1|LocYear), data = AllData, na.action = na.omit)

#including status just regresses it out 
sink(paste(outDir, "/ModelFit.txt", sep = ''))
summary(mod)
sink()

####Data Extraction for AllData
pdf(paste(outDir, "ResidualsVsFitted.pdf", sep=""), width = 10, height =8)
plot(fitted(mod), resid(mod), main = "ResidualsVsFitted for AllData")
abline(h = 0)
dev.off()

##Extracting the Clone:Location:Year interations for Alldata
locCloneYear <- data.frame(LocCloneYear = rownames(ranef(mod)$LocCloneYear), rawVal = ranef(mod)$LocCloneYear)
locCloneYear$Clone <- unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][1]}))
locCloneYear$Loc <- unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][2]}))
locCloneYear$Year <- as.numeric(unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][3]})))
locCloneYear$LocColor <- "#33cc33"
locCloneYear$LocColor[which(locCloneYear$Loc == "Saevar")] <- "#3300FF"
locCloneYear$rawVal <- locCloneYear$X.Intercept.
locCloneYear$transVal <- 10^locCloneYear$X.Intercept.
locCloneYear <- locCloneYear[order(locCloneYear$Clone,locCloneYear$Loc,locCloneYear$Year),]
locCloneYear$yearLabels <- paste(locCloneYear$Loc,locCloneYear$Year,sep="-")
locCloneYear$Year[which(locCloneYear$Loc=="Saevar")] <- 10+locCloneYear$Year[which(locCloneYear$Loc=="Saevar")] #just for plotting purposes

plasticity <- data.frame(clone = character(0),stderrEst = numeric(0))
for (clone in unique(locCloneYear$Clone)) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  plasticity <- rbind(plasticity, data.frame(clone = clone, stderrEst = sd(cur[,val])/sqrt(length(cur[,val]))))
}
plasticity$genoEst <- ranef(mod)$Clone$'(Intercept)'[match(plasticity$clone,rownames(ranef(mod)$Clone))] #10^(ranef(mod)$Clone$'(Intercept)'[match(plast$clone,rownames(ranef(mod)$Clone))]) #
plasticity <- plasticity[order(plasticity$stderrEst),]
colnames(plasticity)[which(names(plasticity) == "clone")] <- "Clone"
plasticity$col <- "#00000080"
plasticity$col[1:10] <- "lightblue"
plasticity$col[(nrow(plasticity)-10):nrow(plasticity)] <- "pink"
locCloneYear <- locCloneYear %>% left_join(plasticity, by = "Clone")



#Plotting for between the two locations
pdf(paste(outDir, "Clone-Location-Year_Effect.pdf",sep=""),width = 10,height = 8)
plot(locCloneYear$Year,locCloneYear[,"rawVal"],col = locCloneYear$LocColor, pch=19, cex=1.5,
     main = "Clone:Location:Year Variation for Height of AllData (Raw Values)", xlab =  "Year", ylab = "Clone/Location/Year BLUP")

plot(locCloneYear$Year,locCloneYear[,"transVal"],col = locCloneYear$LocColor, pch=19, cex=1.5,
     main = "Clone:Location:Year Variation for Height of AllData (Trans Values)", xlab =  "Year", ylab = "Clone/Location/Year BLUP")
plot(locCloneYear$Year[which(locCloneYear$Loc == "Saevar")], locCloneYear[,"rawVal"][which(locCloneYear$Loc == "Saevar")], 
     col = locCloneYear$LocColor[which(locCloneYear$Loc =="Saevar")], pch=19, cex=1.5, main = "Clone:Location:Year Variation for Height of AllData in Sävar (Raw Values)",
     xlab =  "Year (Sävar)", ylab = "Clone/Location/Year BLUP")
plot(locCloneYear$Year[which(locCloneYear$Loc == "Saevar")], locCloneYear[,"transVal"][which(locCloneYear$Loc == "Saevar")], 
     col = locCloneYear$LocColor[which(locCloneYear$Loc == "Saevar")], pch=19, cex=1.5, main = "Clone:Location:Year Variation for Height of AllData in Sävar (Trans Values)",
     xlab =  "Year (Sävar)", ylab = "Clone/Location/Year BLUP")

plot(locCloneYear$Year[which(locCloneYear$Loc == "Ekebo")], locCloneYear[,"rawVal"][which(locCloneYear$Loc == "Ekebo")], 
     col = locCloneYear$LocColor[which(locCloneYear$Loc == "Ekebo")], pch=19, cex=1.5, main = "Clone:Location:Year Variation for Height of AllData in Ekebo (Raw Values)",
     xlab =  "Year (Ekebo)", ylab = "Clone/Location/Year BLUP")
plot(locCloneYear$Year[which(locCloneYear$Loc == "Ekebo")], locCloneYear[,"transVal"][which(locCloneYear$Loc == "Ekebo")], 
     col = locCloneYear$LocColor[which(locCloneYear$Loc == "Ekebo")], pch=19, cex=1.5, main = "Clone:Location:Year Variation for Height of AllData in Ekebo (Trans Values)",
     xlab =  "Year (Sävar)", ylab = "Clone/Location/Year BLUP")
dev.off()

pdf(paste(outDir, "All_PlasticityIndividuals.pdf",sep=""),width = 10, height = 8)
ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(locCloneYear, Clone %in% plasticity$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity Clones \nClone by Location-Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
  geom_line(color = "black", alpha = 0.5) +
  geom_line(data = subset(locCloneYear, Clone %in% plasticity$Clone[(nrow(plasticity)-10):nrow(plasticity)]), linewidth = 1.5) +
  ggtitle("High Plasticity Clones \nClone by Location-Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

locCloneYear <- locCloneYear %>%
  mutate(Clone = reorder(Clone, genoEst, FUN = mean, decreasing = T))

lcyo <- ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
  geom_line(data = subset(locCloneYear, genoEst %in% plasticity$genoEst), linewidth = .75) +
  labs(y = "GxE BLUP", x = '') + 
  ggtitle("Low Plasticity") +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

locCloneYear <- locCloneYear %>%
  mutate(Clone = reorder(Clone, genoEst, FUN = mean, decreasing = F))

lcyno <- ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
  geom_line(data = subset(locCloneYear, genoEst %in% plasticity$genoEst), linewidth = .75) +
  labs(y = "GxE BLUP", x = '') + 
  ggtitle("High Plasticity") +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggarrange(lcyo, lcyno, labels = c("A", "B"), ncol = 2, nrow = 1)

ggplot(plasticity, aes(x = stderrEst, y = genoEst, colour = col)) + 
  ggtitle(paste("10 High and Low Plastic Clones from Data Set \n Adj. R2:",round(summary(lm(plasticity$genoEst~plasticity$stderrEst))$adj.r.squared,3))) +
  geom_point(size = 4, shape = 19, show.legend = F) +
  geom_smooth(method = 'lm', se = F, color = 'red') +
  labs(x = "Plasticity Score", y = 'Genotypic Estimates') + 
  geom_text(data = plasticity[c(1:10, (nrow(plasticity) - 10):nrow(plasticity)), ],
            aes(label = Clone),  # Label with clone names
            color = "black",  # Text color
            vjust = -0.5,  # Adjust vertical position of labels
            hjust = 0.5) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()





###Data Extraction for location specific
for (loc in c("Saevar","Ekebo")) {
  if (loc == "Saevar")  {df <- SaevarData[which(SaevarData$"Location" == loc),c("Location","Block","Row","Plant","Clone","Year", "Latitude", "Population", "Province","Height", "ar1")]
  }else {df <- EkeboData[which(EkeboData$"Location" == loc),c("Location","Block","Row","Plant","Clone","Year", "Latitude", "Population", "Province","Height", "ar1")]}
  locDir <- paste(outDir,"Location/",sep="")
  
  SubSet <- SaevarSubSet[which(SaevarSubSet$"Location" ==  "Saevar"),c("Location","Block","Row","Plant","Clone","Year", "Latitude", "Population", "Province","Height", "ar1")]
  SubSet <- EkeboSubset[which(EkeboSubset$"Location" ==  "Ekebo"),c("Location","Block","Row","Plant","Clone","Year", "Latitude", "Population", "Province","Height", "ar1")]
  
  
  #df <- df %>% filter(Year != 8)
  
  ##Modelling
  modLoc <- lmer((Height) ~ (1 | Block) + Clone*Year + log10(ar1), data = df, na.action = na.omit)
  #CLone year to keep the interaction 

  sink(paste(locDir,"/",loc,"_ModelFit.txt",sep=""))
  print(summary(modLoc))
  sink()
  
  
  cloneYear <- data.frame("cloneYear" = names(fixef(modLoc)[which(grepl(":",names(fixef(modLoc))))]),
                          "rawVal" = fixef(modLoc)[which(grepl(":",names(fixef(modLoc))))],
                          "transVal" = 10^fixef(modLoc)[which(grepl(":", names(fixef(modLoc))))])
  
  cloneYear$Year <- as.numeric(unlist(lapply(cloneYear$cloneYear,function(x){strsplit(x,"Year")[[1]][2]})))
  cloneYear$Clone <- as.numeric(str_replace(unlist(lapply(cloneYear$cloneYear,function(x){strsplit(x,":")[[1]][1]})),"Clone",""))
  

  plasticityLoc <- data.frame("Clone"=as.numeric(str_replace(names(fixef(modLoc)[which(grepl(":",names(fixef(modLoc)))==F & grepl("Clone",names(fixef(modLoc)))==T)]),"Clone","")),
                              genoEst=fixef(modLoc)[which(grepl(":",names(fixef(modLoc)))==F &  grepl("Clone",names(fixef(modLoc)))==T)])
  ##Make clone1 genotypc = 1
  #plasticityLoc$Clone[is.na(plasticityLoc$Clone)] <- 1
  plasticityLoc[which(is.na(plasticityLoc$genoEst)),"genoEst"] <- 0
  #rownames(plasticityLoc) <- plasticityLoc$Clone
  
  #plasticity scores
  for (clone in unique(cloneYear$Clone)) {
    cur <- cloneYear[which(cloneYear$Clone == clone),]
    plasticityLoc[which(plasticityLoc$Clone == clone), "stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
  }
  
  plasticityLoc <- plasticityLoc[order(plasticityLoc$stderrEst),]
  colnames(plasticityLoc)[which(names(plasticityLoc) == "clone")] <- "Clone"
  plasticityLoc$Population <- df$Population[match(plasticityLoc$Clone, df$Clone)]
  plasticityLoc$Latitude <- df$Latitude[match(plasticityLoc$Clone, df$Clone)]
  plasticityLoc$Prov <- as.factor(sapply(plasticityLoc$Latitude, AspLocation))
  plasticityLoc$Location <- df$Location[match(plasticityLoc$Clone, df$Clone)]
  
  plasticityLoc[which(is.na(plasticityLoc$stderrEst)), "stderrEst"] <- 0
  plasticityLoc$residVal <- resid(lm(plasticityLoc$stderrEst~plasticityLoc$genoEst))
  plasticityLoc$residValSwitch <- resid(lm(plasticityLoc$genoEst~plasticityLoc$stderrEst))
  
  #Adding quantile labels
  plasticityLoc <- QuantileDf(plasticityLoc, residCol = "residVal", genotypeCol = "genoEst")
  plasticityLoc$QLabels <- as.factor(plasticityLoc$QLabels)
  #plasticityLoc$resids <- plasticityLoc$resids[match(as.numeric(names(resids)), plasticityLoc$Clone)]
  
  plasticityLocSub <- plasticityLoc %>% filter(Clone %in% SubSet$Clone)
  plasticityLocSub$residVal <- resid(lm(plasticityLocSub$stderrEst~plasticityLocSub$genoEst))
  plasticityLocSub <- QuantileDf(plasticityLocSub, residCol = "residVal", genotypeCol = "genoEst")
  plasticityLocSub$QLabels <- as.factor(plasticityLocSub$QLabels)
  cloneYearSub <- cloneYear %>% filter(Clone %in% SubSet$Clone)
  
  plasticityLoc$Clone <- as.factor(plasticityLoc$Clone)
  cloneYear$Clone <- as.factor(cloneYear$Clone)
  cloneYear <- cloneYear %>% left_join(plasticityLoc, by = "Clone")
  
  plasticityLocSub$Clone <- as.factor(plasticityLocSub$Clone)
  cloneYearSub$Clone <- as.factor(cloneYearSub$Clone)
  cloneYearSub <- cloneYearSub %>% left_join(plasticityLocSub, by = "Clone")
  
  cloneYear$Year <- as.factor(cloneYear$Year)
  cloneYearSub$Year <- as.factor(cloneYearSub$Year)
  
  cloneYear <- cloneYear %>% inner_join(df %>% group_by(Location, Clone, Year) %>% reframe(cnt=dplyr::n()) %>% filter(cnt > 2))
  cloneYearSub <- cloneYearSub %>% inner_join(df %>% group_by(Location, Clone, Year) %>% reframe(cnt=dplyr::n()) %>% filter(cnt > 2))
  
  
}

  #df[,c("Location","Clone","Year")] %>% group_by(Location,Clone,Year) %>% reframe(cnt=dplyr::n()) %>% filter(cnt>2) %>% data.frame()
  pdf(paste(locDir, loc, "FittedVsResids.pdf",sep=""),width = 10, height = 7)
  plot(fitted(modLoc), resid(modLoc))
  dev.off()
  
  pdf(paste(locDir, loc, "PlasticityIndividuals.pdf", sep = ''), height = 7, width = 10)
  ggplot(cloneYear, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
    geom_line(color = 'black', alpha = 0.5) +
    geom_line(data = subset(cloneYear, Clone %in% plasticityLoc$Clone), linewidth = 1.5) +
    ggtitle(paste(loc, "All Plasticity")) +
    labs(y = "GxE BLUP", x = '') + 
    scale_x_discrete(cloneYear$Year, cloneYear$Year) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  #Subset
  ggplot(cloneYearSub, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
    geom_line(color = 'black', alpha = 0.5) +
    geom_line(data = subset(cloneYearSub, Clone %in% plasticityLocSub$Clone), linewidth = 1.5) +
    ggtitle(paste(loc, "All Plasticity Subset")) +
    labs(y = "GxE BLUP", x = '') + 
    scale_x_discrete(cloneYearSub$Year, cloneYearSub$Year) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  ggplot(cloneYear, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
    geom_line(color = 'black', alpha = 0.5) +
    geom_line(data = subset(cloneYear, Clone %in% plasticityLoc$Clone[1:10]), linewidth = 1.5) +
    ggtitle(paste(loc, "Low Plasticity Clones")) +
    labs(y = "GxE BLUP", x = '') + 
    scale_x_discrete(cloneYear$Year, cloneYear$Year) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  #Subset
  ggplot(cloneYearSub, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
    geom_line(color = 'black', alpha = 0.5) +
    geom_line(data = subset(cloneYearSub, Clone %in% plasticityLocSub$Clone[1:10]), linewidth = 1.5) +
    ggtitle(paste(loc, "Low Plasticity Clones Subset")) +
    labs(y = "GxE BLUP", x = '') + 
    scale_x_discrete(cloneYearSub$Year, cloneYearSub$Year) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  ggplot(cloneYear, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
    geom_line(color = "black", alpha = 0.5) +
    geom_line(data = subset(cloneYear, Clone %in% plasticityLoc$Clone[(nrow(plasticityLoc)-10):nrow(plasticityLoc)]), linewidth = 1.5) +
    ggtitle(paste(loc, "High Plasticity Clones")) +
    labs(y = "GxE BLUP", x = '') +     
    scale_x_discrete(cloneYear$Year, cloneYear$Year) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  #Subset
  ggplot(cloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
    geom_line(color = "black", alpha = 0.5) +
    geom_line(data = subset(cloneYearSub, Clone %in% plasticityLocSub$Clone[(nrow(plasticityLocSub)-10):nrow(plasticityLocSub)]), linewidth = 1.5) +
    ggtitle(paste(loc, "High Plasticity Clones Subset")) +
    labs(y = "GxE BLUP", x = '') +     
    scale_x_discrete(cloneYearSub$Year, cloneYearSub$Year) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  cloneYear <- cloneYear %>%
    mutate(Clone = reorder(Clone, genoEst, FUN = mean, decreasing = T))
  cloneYearSub <- cloneYearSub %>%
    mutate(Clone = reorder(Clone, genoEst, FUN = mean, decreasing = T))
  
  ggplot(cloneYear, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
    geom_line(data = subset(cloneYear, genoEst %in% plasticityLoc$genoEst), linewidth = .75) +
    labs(y = "GxE BLUP", x = 'Year') + 
    ggtitle(paste("Low Plasticity Clones in", loc)) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    scale_x_discrete(breaks = cloneYear$Year, labels = cloneYear$Year) +
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  #Subset
  CYSO <- ggplot(cloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
    geom_line(data = subset(cloneYearSub, genoEst %in% plasticityLocSub$genoEst), linewidth = .75) +
    labs(y = "GxE BLUP", x = 'Year') + 
    ggtitle(paste("Low Plasticity Clones in", loc)) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    scale_x_discrete(breaks = cloneYearSub$Year, labels = cloneYearSub$Year) +
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  cloneYear <- cloneYear %>%
    mutate(Clone = reorder(Clone, genoEst, FUN = mean, decreasing = F))
  cloneYearSub <- cloneYearSub %>%
    mutate(Clone = reorder(Clone, genoEst, FUN = mean, decreasing = F))
  
  ggplot(cloneYear, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
    geom_line(data = subset(cloneYear, genoEst %in% plasticityLoc$genoEst), linewidth = .75) +
    labs(y = "GxE BLUP", x = 'Year') + 
    ggtitle(paste("High Plasticity Clones in", loc)) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    scale_x_discrete(breaks = cloneYear$Year, labels = cloneYear$Year) +
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  #Subset
  CYs <- ggplot(cloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = genoEst)) + 
    geom_line(data = subset(cloneYearSub, genoEst %in% plasticityLocSub$genoEst), linewidth = .75) +
    labs(y = "GxE BLUP", x = 'Year') + 
    ggtitle(paste("High Plasticity Clones in", loc)) +
    scale_color_gradientn(name = 'Clone BLUP', 
                          colors = c("navy", "white", "firebrick")) +  
    scale_x_discrete(breaks = cloneYearSub$Year, labels = cloneYearSub$Year) +
    theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
  
  ggarrange(CYSO, CYs, labels = c("A", "B"), ncol = 2, nrow = 1)
  
  dev.off()
  
  
  ###GENOEST x STDERREST PLOTTING
  pdf(paste(locDir, loc, "GenoEstxStderrEst.pdf", sep = ""), height = 7, width = 10)
  ##Plotting genotype x plasticity 
  ggplot(plasticityLoc, aes(x = genoEst, y = stderrEst, group = Clone, color = Prov)) + 
    geom_point(size = 2) +
    ggtitle(paste(loc, "\nAdj. R2:",round(summary(lm(plasticityLoc$stderrEst~plasticityLoc$genoEst))$r.squared,3))) +
    labs(x = "Genotypic Estimates", y = 'Plasticity Score \n StderrEst of Clone') +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_text(data = plasticityLoc,
              aes(label = Clone),  # Label with clone names
              color = "black",  # Text color
              vjust = -0.5,  # Adjust vertical position of labels
              hjust = 0.5, 
              size = 2.5) +
    geom_vline(xintercept = quantile(plasticityLoc$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_vline(xintercept = quantile(plasticityLoc$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    geom_hline(yintercept = quantile(plasticityLoc$stderrEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLoc$stderrEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    theme(axis.line = element_line(colour = "grey10"))    # Rotates x-axis labels
  
  #Flipping the x and the Y
  ggplot(plasticityLoc, aes(x = stderrEst, y = genoEst, group = Clone, color = Prov)) + 
    geom_point(size = 2) +
    ggtitle(paste(loc, "\nAdj. R2:",round(summary(lm(plasticityLoc$genoEst~plasticityLoc$stderrEst))$r.squared,3))) +
    labs(x = "Plasticity Score \n StderrEst of Clones", y = 'Genotypic Estimates') +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_text(data = plasticityLoc,
              aes(label = Clone),  # Label with clone names
              color = "black",  # Text color
              vjust = -0.5,  # Adjust vertical position of labels
              hjust = 0.5, 
              size = 2.5) +    
    geom_vline(xintercept = quantile(plasticityLoc$stderrEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_vline(xintercept = quantile(plasticityLoc$stderrEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    geom_hline(yintercept = quantile(plasticityLoc$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLoc$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    theme(axis.line = element_line(colour = "grey10"))    # Rotates x-axis labels
  dev.off()
  
  
  ####Subsets
  pdf(paste(locDir, loc, "Subset_GenoEstxStderrEst.pdf", sep = ""), height = 7, width = 10)
  # Genotype x Stderrest    
  ggplot(plasticityLocSub, aes(x = genoEst, y = stderrEst, group = Clone, color = Prov)) + 
    geom_point(size = 2) +
    ggtitle(paste(loc, "Subset \nAdj. R2:",round(summary(lm(plasticityLocSub$stderrEst~plasticityLocSub$genoEst))$r.squared,3))) +
    labs(x = "Genotypic Estimates", y = 'Plasticity Score \n StderrEst of Clones') +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_text(data = plasticityLocSub,
              aes(label = Clone),  # Label with clone names
              color = "black",  # Text color
              vjust = -0.5,  # Adjust vertical position of labels
              hjust = 0.5, 
              size = 2.5) +
    geom_hline(yintercept = quantile(plasticityLocSub$stderrEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLocSub$stderrEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    geom_vline(xintercept = quantile(plasticityLocSub$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_vline(xintercept = quantile(plasticityLocSub$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    theme(axis.line = element_line(colour = "grey10"))    # Rotates x-axis labels
  
  #X and Y flipped
  CYF <- ggplot(plasticityLocSub, aes(x = stderrEst, y = genoEst, group = Clone, color = Prov)) + 
    geom_point(size = 2) +
    ggtitle(paste(loc, "Subsetted Clones"), subtitle = paste("Genotypic Estimates by Standard Error of the Mean; Adjusted R2:",round(summary(lm(plasticityLoc$stderrEst~plasticityLoc$genoEst))$r.squared,3))) +
    labs(x = "Plasticity Score \n (Standard Error Estimate)", y = 'Genotypic Estimates') +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_text(data = plasticityLocSub,
              aes(label = Clone),  # Label with clone names
              color = "black",  # Text color
              vjust = -0.5,  # Adjust vertical position of labels
              hjust = 0.5, 
              size = 2.5) +
    geom_hline(yintercept = quantile(plasticityLocSub$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLocSub$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    geom_vline(xintercept = quantile(plasticityLocSub$stderrEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_vline(xintercept = quantile(plasticityLocSub$stderrEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    theme(axis.line = element_line(colour = "grey10"))    # Rotates x-axis labels
  dev.off()
  
  
  ##RESIDUALS
  pdf(paste(locDir, loc, "All_ResidualsxGenoEst.pdf", sep = ""), height = 10, width = 15)
  #Genotype vs Residual for picking clone purposes
  ggplot(plasticityLoc, aes(x = genoEst, y = residVal, color = Prov)) + 
    geom_point(aes(group = Clone, shape = QLabels, stroke = 1.5), size = 2.5, fill = "red") +
    scale_shape_manual(values = c("BP,BG" = 25, "BP,GG" = 21, "GP, BG" = 23, "GP,GG" = 24, "Center" = 20)) +
    scale_color_brewer(palette = "Paired") +
    ggtitle(paste(loc, "Clones"), subtitle = paste("Genotypic Estimates by Residuals; Adjusted R2:",round(summary(lm(plasticityLoc$residVal~plasticityLoc$genoEst))$r.squared,3))) +
    labs(x = "Genotypic Estimates", y = 'Plasticity Score \n Residuals (StderrEst~Genotype)') +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_text(data = plasticityLoc,
              aes(label = Clone),  # Label with clone names
              color = "black",  # Text color
              vjust = -0.8,  # Adjust vertical position of labels
              hjust = 0.5, 
              size = 2.5) +
    geom_vline(xintercept = quantile(plasticityLoc$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_vline(xintercept = quantile(plasticityLoc$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    geom_hline(yintercept = quantile(plasticityLoc$residVal, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLoc$residVal, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    theme(axis.line = element_line(colour = "grey10"))    # Rotates x-axis labels
  
  #Flipped x and Y for residuals   
  ggplot(plasticityLoc, aes(x = residVal, y = genoEst, color = Prov)) + 
    geom_point(aes(group = Clone, shape = QLabels, stroke = 1.5), size = 2.5, fill = "red") +
    scale_shape_manual(values = c("BP,BG" = 25, "BP,GG" = 21, "GP, BG" = 23, "GP,GG" = 24, "Center" = 20)) +
    scale_color_brewer(palette = "Paired") +
    ggtitle(paste(loc, "Clones"), subtitle = paste("Genotypic Estimates by Residuals; Adjusted R2:",round(summary(lm(plasticityLoc$residVal~plasticityLoc$genoEst))$r.squared,3))) +
    labs(x = "Plasticity Score \n Residuals (StderrEst~Genotype)", y = 'Genotypic Estimates') +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_text(data = plasticityLoc,
              aes(label = Clone),  # Label with clone names
              color = "black",  # Text color
              vjust = -0.8,  # Adjust vertical position of labels
              hjust = 0.5, 
              size = 2.5) +
    geom_vline(xintercept = quantile(plasticityLoc$residVal, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_vline(xintercept = quantile(plasticityLoc$residVal, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    geom_hline(yintercept = quantile(plasticityLoc$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLoc$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    theme(axis.line = element_line(colour = "grey10"))    # Rotates x-axis labels
  
  ##Residuals for Subset
  ggplot(plasticityLocSub, aes(x = genoEst, y = residVal, color = Prov)) + 
    geom_point(aes(group = Clone, shape = QLabels, stroke = 1.5), size = 2.5, fill = "red") +
    scale_shape_manual(values = c("BP,BG" = 25, "BP,GG" = 21, "GP, BG" = 23, "GP,GG" = 24, "Center" = 20)) +
    scale_color_brewer(palette = "Paired") +
    ggtitle(paste(loc, "Subsetted Clones"), subtitle = paste("Genotypic Estimates by Residuals; Adjusted R2:",round(summary(lm(plasticityLoc$residVal~plasticityLoc$genoEst))$r.squared,3))) +
    labs(y = "Plasticity Score \n Residuals (StderrEst~Genotype)", x = 'Genotypic Estimates') +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_text(data = plasticityLocSub,
              aes(label = Clone),  # Label with clone names
              color = "black",  # Text color
              vjust = -0.8,  # Adjust vertical position of labels
              hjust = 0.5, 
              size = 2.5) +
    geom_hline(yintercept = quantile(plasticityLocSub$residVal, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLocSub$residVal, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    geom_vline(xintercept = quantile(plasticityLocSub$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_vline(xintercept = quantile(plasticityLocSub$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    theme(axis.line = element_line(colour = "grey10"))    # Rotates x-axis labels
  
  ##Residuals Switched
  CYFS <- ggplot(plasticityLocSub, aes(x = residVal, y = genoEst, color = Prov)) + 
    geom_point(aes(group = Clone, shape = QLabels, stroke = 1.5), size = 2.5, fill = "red") +
    scale_shape_manual(values = c("BP,BG" = 25, "BP,GG" = 21, "GP, BG" = 23, "GP,GG" = 24, "Center" = 20)) +
    scale_color_brewer(palette = "Paired") +
    ggtitle(paste(loc, "Subsetted Clones"), subtitle = paste("Genotypic Estimates by Residuals (StderrEst~Genotype); Adjusted R2:",round(summary(lm(plasticityLoc$residVal~plasticityLoc$genoEst))$r.squared,3))) +
    labs(x = "Plasticity Score \n Residuals (StderrEst~Genotype)", y = 'Genotypic Estimates') +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
    geom_text(data = plasticityLocSub,
              aes(label = Clone),  # Label with clone names
              color = "black",  # Text color
              vjust = -0.8,  # Adjust vertical position of labels
              hjust = 0.5, 
              size = 2.5) +
    geom_hline(yintercept = quantile(plasticityLocSub$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLocSub$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    geom_vline(xintercept = quantile(plasticityLocSub$residVal, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_vline(xintercept = quantile(plasticityLocSub$residVal, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    theme(axis.line = element_line(colour = "grey10"))    # Rotates x-axis labels
  
  ggarrange(CYF, CYFS, labels = c("A", "B"), ncol = 2, nrow = 1)
  dev.off()
  
##Writing to a file all the clones that are in one of the quadrants
plasticityLocSub$QCloneProv <- paste(plasticityLocSub$Clone, plasticityLocSub$Prov)
sink(paste(locDir, loc, "-ChosenClones.txt", sep = ''))


clones <- data.frame(Clones = plasticityLocSub$Clone[plasticityLocSub$QLabels %in% c("BP,GG", "GP,GG", "BP,BG", "GP, BG")])
addClones <- data.frame(Clones = c(56, 42))
if (loc == "Ekebo"){
  clones <- rbind(clones, addClones)
  
}
clones$Prov <- plasticityLocSub$Prov[match(clones$Clones, plasticityLocSub$Clone)]
clones$QLabel <- plasticityLocSub$QLabels[match(clones$Clones, plasticityLocSub$Clone)]
write.table(clones, file = paste(locDir, loc, "_ChosenClones.txt", sep = ''), sep = "\t", row.names = F)


#No SD as we dont have the data for it 
  #Run as one set of years to another set of years and compare them
  #Just run as leaving out more years
  ##Plotting the resids on the side to make the plot flat
  ##Adding SD h and v and choosing clones from those quadrants

#Old code
    #Working on the residuals 
  residYear <- augment(modLoc) %>% dplyr::select(Clone = Clone, Year = Year, Resid = .resid)
  residYear <- residYear %>% filter(Year %in% cloneYear$Year)
  residYear <- residYear %>%
    group_by(Clone) %>%
    summarize(residVal = sd(Resid, na.rm = T)) %>%
    ungroup()
  residYear$residVal[which(is.na(residYear$residVal))] <- 0
  residYear <- residYear[-1,]
  plasticityLoc$Clone <- as.factor(plasticityLoc$Clone)
  plasticityLoc <- plasticityLoc %>% left_join(residYear, by = "Clone")
  plasticityLoc[which(is.na(plasticityLoc$residVal)), "residVal"] <- 0
  
  #residYear <- residYear %>% mutate(cloneYear = paste0("Clone", Clone, ":Year", Year))
  #rownames(residYear) <- residYear$Clone
  
  geom_vline(xintercept = quantile(plasticityLoc$stderrEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    annotate("text", x = min(plasticityLoc$genoEst), y = quantile(plasticityLoc$stderrEst, probs = 0.25, na.rm = T), 
             label = "25% Quantile (Plasticity)", vjust = -1, hjust = 0, color = "red") +
    geom_vline(xintercept = quantile(plasticityLoc$stderrEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple") +
    annotate("text", x = min(plasticityLoc$genoEst), y = quantile(plasticityLoc$stderrEst, probs = 0.75, na.rm = T), 
             label = "75% Quantile (Plasticity)", vjust = -1, hjust = 0, color = "purple") +
    geom_hline(yintercept = quantile(plasticityLoc$genoEst, probs = 0.25, na.rm = T), linetype = "dashed", color = "red") +
    geom_hline(yintercept = quantile(plasticityLoc$genoEst, probs = 0.75, na.rm = T), linetype = "dashed", color = "purple")
    
  plasticityLoc <- plasticityLoc %>%
    mutate(
      QLabels = case_when(
        genoEst < quantile(plasticityLoc$genoEst, probs = 0.25, na.rm = TRUE) & residVal < quantile(plasticityLoc$residVal, probs = 0.25, na.rm = TRUE) ~ "BP:BG",
        genoEst > quantile(plasticityLoc$genoEst, probs = 0.75, na.rm = TRUE) & residVal < quantile(plasticityLoc$residVal, probs = 0.25, na.rm = TRUE) ~ "BP:GG", 
        genoEst < quantile(plasticityLoc$genoEst, probs = 0.25, na.rm = TRUE) & residVal > quantile(plasticityLoc$residVal, probs = 0.75, na.rm = TRUE)) ~ "GP:BG",
      genoEst > quantile(plasticityLoc$genoEst, probs = 0.75, na.rm = TRUE) & residVal > quantile(plasticityLoc$residVal, probs = 0.75, na.rm = TRUE) ~ "GP:GG",
      TRUE ~ "Center"
    )
  
  
  
  quantile(plasticityLoc$genoEst, probs = 0.25, na.rm = TRUE)
  quantile(plasticityLoc$genoEst, probs = 0.70, na.rm = TRUE)
  quantile(plasticityLoc$residVal, probs = 0.25, na.rm = TRUE)
  quantile(plasticityLoc$residVal, probs = 0.70, na.rm = TRUE)
  "BP,GG", "GP,GG", "BP,BG", "GP, BG"
  clones <-(data.frame("BP,GG" = plasticityLocSub$QCloneProv[plasticityLocSub$QLabels=="BP,GG"], 
                       "GP,GG" = plasticityLocSub$QCloneProv[plasticityLocSub$QLabels=="GP,GG"],
                       "BP,BG" = plasticityLocSub$QCloneProv[plasticityLocSub$QLabels=="BP,BG"],
                       "GP, BG" =plasticityLocSub$QCloneProv[plasticityLocSub$QLabels=="GP, BG"]))
  
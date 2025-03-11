trans <- F
if (trans) {
  val <- "transVal"
  outDir <- "~/UPSC_ThesisWork/Data/BCY_Plasticity/backtransformed/"
} else {
  val <- "rawVal"
  outDir <- "~/UPSC_ThesisWork/Data/BCY_Plasticity/untransformed/"
}
#Loading packages
library(lme4)
library(stringr)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
library(zoo)
#Look at the more specific Block effect within the two separate locations
#Defining South, Middle, and North Sweden and identify clones local/nonlocal to Ek/Sä
#Defining North, Middle and South of Sweden
#
###Functions used in the code
northSwe <- 63.00 #above 63
southSwe <- 59.00 #Below 59

AspLocation <- function(Latitude){
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
SaevarData <- SaevarData %>% pivot_longer(cols = any_of(saevarColNames) , values_to = "Height", names_to = c("Year","Type"),names_sep = "_")
SaevarData$Year <- as.numeric(str_remove(SaevarData$Year, "s"))
SaevarData <- SaevarData %>% group_by(Clone, Block, Row, Plant) %>% mutate(!!paste("ar",1,sep="") := lag(x =Height,order_by = Year)) %>% as.data.frame()
SaevarData <- SaevarData %>%
  group_by(Clone) %>%
  mutate(Status = ifelse(n_distinct(Year) == 9 & (!is.na(Height)), "Alive", "Dead")) %>%
  ungroup()

write.table(SaevarData %>% group_by(Clone,Year,Location) %>% summarise(cnt = dplyr::n()) %>% filter(cnt < 2) %>% as.data.frame(),paste("~/UPSC_ThesisWork/Data/Plasticity","/LowRepSaevarData.txt",sep=""),col.names = T,row.names = F,quote=F)

#Filling in the remaining heights of the dead trees
SaevarData <- SaevarData %>%
  group_by(GE_Sample_Name) %>%  # Group by individual tree ID
  arrange(GE_Sample_Name, Year) %>%  
  mutate(Height = ifelse(Status == "Dead", NA, Height)) %>% 
  fill(Height, .direction = "downup")

#Creating the locations for Saevar
SaevarData$Latitude <- SaevarProvenanceData$Latitude[match(SaevarData$Clone, SaevarProvenanceData$Clone)]
SaevarData$Population <- SaevarProvenanceData$Population[match(SaevarData$Clone, SaevarProvenanceData$Clone)]
SaevarData$Province <- sapply(SaevarData$Latitude, AspLocation)

##Setting up modeling
SaevarData$Clone <- as.factor(SaevarData$Clone)
SaevarData$Year <- as.factor(SaevarData$Year)
SaevarData$Status <- as.factor(SaevarData$Status)
SaevarData$CloneYear <- paste(SaevarData$Clone, SaevarData$Year)
SaevarData$BlkCloneYear <- paste(SaevarData$Clone, SaevarData$Block, SaevarData$Year)
SaevarData$CloneProvYear <- paste(SaevarData$Province, SaevarData$Clone, SaevarData$Year)

#Creating the Saevar subset
#This subset is already going to be tidy and does not need anything done to it
SaevarSubSet <- subset(SaevarData, SaevarData$GE_Sample_Name %in% SaevarNameReassignment$old.sample) # 1962 entries
SaevarSubSet[c("Meta.col", "Meta.row", "Clone")] <- str_split_fixed(SaevarSubSet$GE_Sample_Name, '-', 3)
write.table(SaevarSubSet %>% group_by(Clone,Year,Location) %>% summarise(cnt = dplyr::n()) %>% filter(cnt < 2) %>% as.data.frame(),paste("~/UPSC_ThesisWork/Data/Plasticity","/LowRepSaevarSubSet.txt",sep=""),col.names = T,row.names = F,quote=F)

#Modelling for Saevar
#RRMM for Block specific clone deviations
#modSaevar <- lmer(log10(Height) ~ CloneProv + (1 | Clone) + (1 | Block) + (1 | CloneProvYear) + (1 | BlkClone), data = SaevarData, na.action = na.omit)
modSaevar <- lmer(log10(Height) ~ (1 | Block) + Clone + (1 | Province) + CloneProvYear + Year, data = SaevarData, na.action = na.omit)
#including status just regresses it out 
sink(paste(outDir,"/Saevar_ModelFit.txt",sep=""))
summary(modSaevar)
sink()
#rank deficiency means that there are not replicates

#Extracting the interactions between the clone and non/local in the specific location
cloneBlkProvYear <- data.frame("Data" = names(coef(modSaevar)[which(grepl("CloneBlkProvYear",names(coef(modSaevar))))]),rawVal = coef(modSaevar)[which(grepl("CloneBlkProvYear",names(coef(modSaevar))))], transVal = 10^(coef(modSaevar)[which(grepl("CloneBlkProvYear", names(coef(modSaevar))))]))
cloneBlkProvYear$Clone <- as.numeric(str_replace(unlist(lapply(cloneBlkProvYear$Data,function(x){strsplit(x," ")[[1]][1]})),"CloneBlkProvYear",""))
cloneBlkProvYear$Block <- unlist(lapply(cloneBlkProvYear$Data,function(x){str_split(x," ")[[1]][2]}))
cloneBlkProvYear$Prov <- unlist(lapply(cloneBlkProvYear$Data,function(x){str_split(x," ")[[1]][3]}))
cloneBlkProvYear$Year <- as.numeric(unlist(lapply(cloneBlkProvYear$Data,function(x){str_split(x," ")[[1]][4]})))
cloneBlkProvYear$Status <- unlist(lapply(cloneBlkProvYear$Data,function(x){str_split(x," ")[[1]][5]}))
cloneBlkProvYear$rawVal[is.na(cloneBlkProvYear$rawVal)] <- 0
cloneBlkProvYear$transVal[is.na(cloneBlkProvYear$transVal)] <- 0

plasticityS <- data.frame(data.frame("Clone" = str_remove(names(coef(modSaevar)[which(grepl("Block",names(coef(modSaevar))) == F & grepl("Year", names(coef(modSaevar))) == F & grepl("Clone",names(coef(modSaevar)))==T)]), "Clone"), 
                                     genoEst = coef(modSaevar)[which(grepl("Block",names(coef(modSaevar)))==F & grepl("Year", names(coef(modSaevar)))==F & grepl("Clone",names(coef(modSaevar)))==T)]))
plasticityS[which(is.na(plasticityS$genoEst)),"genoEst"] <- 0
plasticityS$Prov <- SaevarData$Province[match(plasticityS$Clone,SaevarData$Clone)]
plasticityS$Status <- cloneBlkProvYear$Status[match(plasticityS$Clone, cloneBlkProvYear$Clone)]
for (clone in unique(cloneBlkProvYear$Clone)) {
  cur <- cloneBlkProvYear[which(cloneBlkProvYear$Clone == clone),]
  plasticityS[which(plasticityS$Clone == clone), "stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
}
plasticityS <- plasticityS[order(plasticityS$stderrEst),]
cloneBlkProvYear <- cbind(plasticityS, cloneBlkProvYear[match(plasticityS$Clone, cloneBlkProvYear$Clone), colnames(cloneBlkProvYear)[which(colnames(cloneBlkProvYear) != "Clone")]])
cloneBlkProvYear <- cloneBlkProvYear[, -9] #remove duplicate column

#Extracting the interactions between the clone and non/local in the specific location for Subset
cloneBlkProvYearSub <- data.frame("Data" = names(coef(modSSub)[which(grepl("CloneBlkProvYear",names(coef(modSSub))))]),rawVal = coef(modSSub)[which(grepl("CloneBlkProvYear",names(coef(modSSub))))], transVal = 10^(coef(modSSub)[which(grepl("CloneBlkProvYear", names(coef(modSSub))))]))
cloneBlkProvYearSub$Clone <- as.numeric(str_replace(unlist(lapply(cloneBlkProvYearSub$Data,function(x){strsplit(x," ")[[1]][1]})),"CloneBlkProvYear",""))
cloneBlkProvYearSub$Block <- unlist(lapply(cloneBlkProvYearSub$Data,function(x){str_split(x," ")[[1]][2]}))
cloneBlkProvYearSub$Prov <- unlist(lapply(cloneBlkProvYearSub$Data,function(x){str_split(x," ")[[1]][3]}))
cloneBlkProvYearSub$Year <- as.numeric(unlist(lapply(cloneBlkProvYearSub$Data,function(x){str_split(x," ")[[1]][4]})))
cloneBlkProvYearSub$Status <- unlist(lapply(cloneBlkProvYearSub$Data,function(x){str_split(x," ")[[1]][5]}))
cloneBlkProvYearSub$rawVal[is.na(cloneBlkProvYearSub$rawVal)] <- 0
cloneBlkProvYearSub$transVal[is.na(cloneBlkProvYearSub$transVal)] <- 0

plasticitySSub <- data.frame(data.frame("Clone" = str_remove(names(coef(modSSub)[which(grepl("Block",names(coef(modSSub))) == F & grepl("Year", names(coef(modSSub))) == F & grepl("Clone",names(coef(modSSub)))==T)]), "Clone"), 
                                     genoEst = coef(modSSub)[which(grepl("Block",names(coef(modSSub)))==F & grepl("Year", names(coef(modSSub)))==F & grepl("Clone",names(coef(modSSub)))==T)]))
plasticitySSub[which(is.na(plasticitySSub$genoEst)),"genoEst"] <- 0
plasticitySSub$Prov <- SaevarSubSet$Province[match(plasticitySSub$Clone,SaevarSubSet$Clone)]
plasticitySSub$Status <- cloneBlkProvYearSub$Status[match(plasticitySSub$Clone, cloneBlkProvYearSub$Clone)]
for (clone in unique(cloneBlkProvYearSub$Clone)) {
  cur <- cloneBlkProvYearSub[which(cloneBlkProvYearSub$Clone == clone),]
  plasticitySSub[which(plasticitySSub$Clone == clone), "stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
}
plasticitySSub <- plasticitySSub[order(plasticitySSub$stderrEst),]
cloneBlkProvYearSub <- cbind(plasticitySSub, cloneBlkProvYearSub[match(plasticitySSub$Clone, cloneBlkProvYearSub$Clone), colnames(cloneBlkProvYearSub)[which(colnames(cloneBlkProvYearSub) != "Clone")]])
cloneBlkProvYearSub <- cloneBlkProvYearSub[, -9] #remove duplicate column

##Plot All plasticity in Saevar
pdf(paste(outDir, "/Saevar_CBPY_Plasticity.pdf", sep = ''), width = 12, height = 8)
##We are not looking at it actoss years anymore, we are looking at it based on the province of the Clones

ggplot(cloneBlkProvYear, aes(x = Prov, y = stderrEst, fill =  Status)) + 
  geom_boxplot() +
  ggtitle("Sävar Block:Province Interaction Box Plot") +
  labs(y = "Plasticity Score", x = 'Province of Sweden') + 
  theme(axis.line = element_line(colour = "grey10"))   # Rotates x-axis labels

ggplot(cloneBlkProvYearSub, aes(x = Prov, y = stderrEst, fill =  Status)) + 
  geom_boxplot() +
  ggtitle("Sävar Subset Block:Province Interaction Box Plot") +
  labs(y = "Plasticity Score", x = 'Province of Sweden') + 
  theme(axis.line = element_line(colour = "grey10"))   # Rotates x-axis labels

for (pr in unique(plasticityS$Prov)) {
  cur <- plasticityS[which(plasticityS$Prov == pr),]
  
  print(ggplot(cur, aes(x = genoEst, y = stderrEst, group = Clone, color = genoEst, shape = Status)) + 
          geom_point(size = 4) +
          ggtitle(paste("Sävar", pr,"Adj. R2:",round(summary(lm(cur$stderrEst~cur$genoEst))$adj.r.squared,3))) +
          labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
          scale_color_gradientn(name = 'Clone BLUP', 
                                colors = c("#003300", "white", "#9900FF")) +  
          geom_text(data = cur,
                    aes(label = Clone),  # Label with clone names
                    color = "black",  # Text color
                    vjust = -0.5,  # Adjust vertical position of labels
                    hjust = 0.5) +
          theme(axis.line = element_line(colour = "grey10")))   # Rotates x-axis labels
}

for (pr in unique(plasticitySSub$Prov)) {
  cur <- plasticitySSub[which(plasticitySSub$Prov == pr),]
  
  print(ggplot(cur, aes(x = genoEst, y = stderrEst, group = Clone, color = genoEst, shape = Status)) + 
          geom_point(size = 4) +
          ggtitle(paste("Sävar Subset", pr,"Adj. R2:",round(summary(lm(cur$stderrEst~cur$genoEst))$adj.r.squared,3))) +
          labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
          scale_color_gradientn(name = 'Clone BLUP', 
                                colors = c("#003300", "white", "#9900FF")) +  
          geom_text(data = cur,
                    aes(label = Clone),  # Label with clone names
                    color = "black",  # Text color
                    vjust = -0.5,  # Adjust vertical position of labels
                    hjust = 0.5) +
          theme(axis.line = element_line(colour = "grey10")))  
}
dev.off()



#Ekebo Data
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
EkeboData <- EkeboData %>% pivot_longer(cols = any_of(ekeboColNames),values_to = "Height",names_to = c("Year","Type"),names_sep = "_") %>% as.data.frame()
EkeboData$Year <- as.numeric(str_remove(EkeboData$Year,"e"))
EkeboData <- EkeboData %>% group_by(Clone,Block,Row,Plant) %>% mutate(!!paste("ar",1,sep="") := lag(x =Height,order_by = Year)) %>% as.data.frame()
EkeboData <- EkeboData %>%
  group_by(Clone) %>%
  mutate(Status = ifelse(n_distinct(Year) == 8 & (!is.na(Height)), "Alive", "Dead")) %>%
  ungroup()

#Storing the column names of the GE data (this is what will be in the subset) and turning it into a dataframe
ekeboGEColNames <- colnames(EkeboGEData)
ekeboGEColNames <- data.frame(ekeboGEColNames)

#Extracting the clone number from the sample ID
ekeboGEColNames <- ekeboGEColNames %>% mutate(Clones = str_extract(ekeboGEColNames, "(?<=_)\\d+"))
ekeboGEColNames$Clones <- as.numeric(ekeboGEColNames$Clones)

#Removing duplicates of ekeboColNames
EkeboGEClones <- ekeboGEColNames[!duplicated(ekeboGEColNames$Clones),]

#Filling in the remaining heights of the dead trees
EkeboData <- EkeboData %>%
  group_by(Ekebo.Pos) %>%  # Group by individual tree ID
  arrange(Ekebo.Pos, Year) %>%  
  mutate(Height = ifelse(Status == "Dead", NA, Height)) %>% 
  fill(Height, .direction = "downup")

#Creating locations for Ekebo
EkeboData$Latitude <- SaevarProvenanceData$Latitude[match(EkeboData$Clone, SaevarProvenanceData$Clone)]
EkeboData$Population <- SaevarProvenanceData$Population[match(EkeboData$Clone, SaevarProvenanceData$Clone)]
EkeboData$Province <- sapply(EkeboData$Latitude, AspLocation)

#Ekebo
EkeboData$Clone <- as.factor(EkeboData$Clone)
EkeboData$Year <- as.factor(EkeboData$Year)
EkeboData$CloneYear <- paste(EkeboData$Clone, EkeboData$Year)
EkeboData$BlkCloneYear <- paste(EkeboData$Clone, EkeboData$Block, EkeboData$Year)
EkeboData$CloneBlkProvYear <- paste(EkeboData$Clone, EkeboData$Block, EkeboData$Province, EkeboData$Year, EkeboData$Status)

EkeboSubset <- subset(EkeboData, EkeboData$Clone %in% EkeboGEClones$Clones) #422 entries
write.table(EkeboSubset %>% group_by(Clone,Year,Location) %>% summarise(cnt = dplyr::n()) %>% filter(cnt < 2) %>% as.data.frame(),paste("~/UPSC_ThesisWork/Data/Plasticity","/LowRepEkeboSubset.txt",sep=""),col.names = T,row.names = F,quote=F)

#Modelling for Ekebo
#RRMM for Block specific clone deviations
sink(paste(outDir,"/Ekebo_ModelFit.txt",sep=""))
#modEkebo <- lmer(log10(Height) ~ CloneBlkProvYear + (1 | Clone) + (1 | Block) + (1 | BlkCloneYear) + (1 | CloneYear), data = EkeboData, na.action = na.omit)
modEkebo <- lm(log10(Height) ~ Clone + Block + Province + CloneBlkProvYear + BlkCloneYear + CloneYear + Year, data = EkeboData, na.action = na.exclude)
summary(modEkebo)
sink()

sink(paste(outDir,"/EkeboSubset_ModelFit.txt",sep=""))
#modEkebo <- lmer(log10(Height) ~ CloneBlkProvYear + (1 | Clone) + (1 | Block) + (1 | BlkCloneYear) + (1 | CloneYear), data = EkeboData, na.action = na.omit)
modESub <- lm(log10(Height) ~ Clone + Block + Province + CloneBlkProvYear + BlkCloneYear + CloneYear + Year, data = EkeboSubset, na.action = na.exclude)
summary(modESub)
sink()

##Ekebo plotting
cBPYEk <- data.frame("Data" = names(coef(modEkebo)[which(grepl("CloneBlkProvYear",names(coef(modEkebo))))]),rawVal = coef(modEkebo)[which(grepl("CloneBlkProvYear",names(coef(modEkebo))))], transVal = 10^(coef(modEkebo)[which(grepl("CloneBlkProvYear", names(coef(modEkebo))))]))
cBPYEk$Clone <- as.numeric(str_replace(unlist(lapply(cBPYEk$Data,function(x){strsplit(x," ")[[1]][1]})),"CloneBlkProvYear",""))
cBPYEk$Block <- unlist(lapply(cBPYEk$Data,function(x){str_split(x," ")[[1]][2]}))
cBPYEk$Prov <- unlist(lapply(cBPYEk$Data,function(x){str_split(x," ")[[1]][3]}))
cBPYEk$Year <- as.numeric(unlist(lapply(cBPYEk$Data,function(x){str_split(x," ")[[1]][4]})))
cBPYEk$Status <- unlist(lapply(cBPYEk$Data, function(x){str_split(x," ")[[1]][5]}))
cBPYEk$rawVal[is.na(cBPYEk$rawVal)] <- 0
cBPYEk$transVal[is.na(cBPYEk$transVal)] <- 0

plasticityE <- data.frame(data.frame("Clone" = str_remove(names(coef(modEkebo)[which(grepl("Block",names(coef(modEkebo))) == F & grepl("Year", names(coef(modEkebo))) == F & grepl("Clone",names(coef(modEkebo)))==T)]), "Clone"), 
                                     genoEst = coef(modEkebo)[which(grepl("Block",names(coef(modEkebo)))==F & grepl("Year", names(coef(modEkebo)))==F & grepl("Clone",names(coef(modEkebo)))==T)]))
plasticityE[which(is.na(plasticityE$genoEst)),"genoEst"] <- 0
plasticityE$Prov <- EkeboData$Province[match(plasticityE$Clone,EkeboData$Clone)]
plasticityE$Status <- cBPYEk$Status[match(plasticityE$Clone, cBPYEk$Clone)]
plasticityE$stderrEst <- NA
for (clone in unique(cBPYEk$Clone)) {
  cur <- cBPYEk[which(cBPYEk$Clone == clone),]
  plasticityE[which(plasticityE$Clone == clone), "stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
}
plasticityE <- plasticityE[order(plasticityE$stderrEst),]
cBPYEk <- cbind(plasticityE, cBPYEk[match(plasticityE$Clone, cBPYEk$Clone), colnames(cBPYEk)[which(colnames(cBPYEk) != "Clone")]])
cBPYEk <- cBPYEk[, -10] #remove duplicate column
cBPYEk <- cBPYEk[, -11] #remove duplicate column

#Subset
cBPYEkSub <- data.frame("Data" = names(coef(modESub)[which(grepl("CloneBlkProvYear",names(coef(modESub))))]),rawVal = coef(modESub)[which(grepl("CloneBlkProvYear",names(coef(modESub))))], transVal = 10^(coef(modESub)[which(grepl("CloneBlkProvYear", names(coef(modESub))))]))
cBPYEkSub$Clone <- as.numeric(str_replace(unlist(lapply(cBPYEkSub$Data,function(x){strsplit(x," ")[[1]][1]})),"CloneBlkProvYear",""))
cBPYEkSub$Block <- unlist(lapply(cBPYEkSub$Data,function(x){str_split(x," ")[[1]][2]}))
cBPYEkSub$Prov <- unlist(lapply(cBPYEkSub$Data,function(x){str_split(x," ")[[1]][3]}))
cBPYEkSub$Year <- as.numeric(unlist(lapply(cBPYEkSub$Data,function(x){str_split(x," ")[[1]][4]})))
cBPYEkSub$Status <- unlist(lapply(cBPYEkSub$Data,function(x){str_split(x," ")[[1]][5]}))
cBPYEkSub$rawVal[is.na(cBPYEkSub$rawVal)] <- 0
cBPYEkSub$transVal[is.na(cBPYEkSub$transVal)] <- 0

plasticityESub <- data.frame(data.frame("Clone" = str_remove(names(coef(modESub)[which(grepl("Block",names(coef(modESub))) == F & grepl("Year", names(coef(modESub))) == F & grepl("Clone",names(coef(modESub)))==T)]), "Clone"), 
                                     genoEst = coef(modESub)[which(grepl("Block",names(coef(modESub)))==F & grepl("Year", names(coef(modESub)))==F & grepl("Clone",names(coef(modESub)))==T)]))
plasticityESub[which(is.na(plasticityESub$genoEst)),"genoEst"] <- 0
plasticityESub$Prov <- EkeboSubset$Province[match(plasticityESub$Clone, EkeboSubset$Clone)]
plasticityESub$Status <- cBPYEkSub$Status[match(plasticityESub$Clone, cBPYEkSub$Clone)]
plasticityESub$stderrEst <- NA
for (clone in unique(cBPYEkSub$Clone)) {
  cur <- cBPYEkSub[which(cBPYEkSub$Clone == clone),]
  plasticityESub[which(plasticityESub$Clone == clone), "stderrEst"] <- sd(cur[,val])/sqrt(length(cur[,val]))
}
plasticityESub <- plasticityESub[order(plasticityESub$stderrEst),]
cBPYEkSub <- cbind(plasticityESub, cBPYEkSub[match(plasticityESub$Clone, cBPYEkSub$Clone), colnames(cBPYEkSub)[which(colnames(cBPYEkSub) != "Clone")]])
cBPYEkSub <- cBPYEkSub[, -10] #remove duplicate column
cBPYEkSub <- cBPYEkSub[, -11] #remove duplicate column

##Plot All plasticity in Saevar
pdf(paste(outDir, "/Ekebo_CBPY_Plasticity.pdf", sep = ''), width = 12, height = 8)
##We are not looking at it actoss years anymore, we are looking at it based on the province of the Clones

ggplot(cBPYEk, aes(x = Prov, y = stderrEst, fill = Status)) + 
  geom_boxplot() +
  ggtitle("Ekebo Block:Province Interaction Box Plot") +
  labs(y = "Plasticity Score", x = 'Province of Sweden') + 
  theme(axis.line = element_line(colour = "grey10"))   # Rotates x-axis labels

ggplot(cBPYEkSub, aes(x = Prov, y = stderrEst, fill = Status)) + 
  geom_boxplot() +
  ggtitle("Ekebo Block:Province Interaction Box Plot") +
  labs(y = "Plasticity Score", x = 'Province of Sweden') + 
  theme(axis.line = element_line(colour = "grey10"))   # Rotates x-axis labels



for (pr in unique(plasticityE$Prov)) {
  cur <- plasticityE[which(plasticityE$Prov == pr),]
  
  print(ggplot(cur, aes(x = genoEst, y = stderrEst, group = Clone, color = genoEst, shape = Status)) + 
          geom_point(size = 4) +
          ggtitle(paste("Ekebo", pr,"Adj. R2:",round(summary(lm(cur$stderrEst~cur$genoEst))$adj.r.squared,3))) +
          labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
          scale_color_gradientn(name = 'Clone BLUP', 
                                colors = c("#003300", "white", "#9900FF")) +  
          geom_text(data = cur,
                    aes(label = Clone),  # Label with clone names
                    color = "black",  # Text color
                    vjust = -0.5,  # Adjust vertical position of labels
                    hjust = 0.5) +
          theme(axis.line = element_line(colour = "grey10")))   # Rotates x-axis labels
  
}

#subset
for (pr in unique(plasticityESub$Prov)) {
  cur <- plasticityESub[which(plasticityESub$Prov == pr),]
  
  print(ggplot(cur, aes(x = genoEst, y = stderrEst, group = Clone, color = genoEst, shape = Status)) + 
          geom_point(size = 4) +
          ggtitle(paste("Ekebo Subset", pr,"Adj. R2:",round(summary(lm(cur$stderrEst~cur$genoEst))$adj.r.squared,3))) +
          labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
          scale_color_gradientn(name = 'Clone BLUP', 
                                colors = c("#003300", "white", "#9900FF")) +  
          geom_text(data = cur,
                    aes(label = Clone),  # Label with clone names
                    color = "black",  # Text color
                    vjust = -0.5,  # Adjust vertical position of labels
                    hjust = 0.5) +
          theme(axis.line = element_line(colour = "grey10")))   # Rotates x-axis labels
  
}

dev.off()



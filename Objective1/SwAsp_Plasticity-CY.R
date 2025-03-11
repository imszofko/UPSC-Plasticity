library(lme4)
library(stringr)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
#Starting Objective 1
#Figuring out the plasticity scores for the intersecting data, and both subsets of data
#Figuring out which aspen genotypes are the most and least plastic
#
#Reading in the data tables
#
#Saever Data
SaevarData <- read.table("~/UPSC_ThesisWork/Data/SwAsp_Saevar/SwAsp_Saevar_Heights_diameters.txt",
                         header = T,
                         as.is = T,
                         sep = '\t') #4473 entries after tidying
SaevarData$Location <- "Saevar"

SaevarProvenanceData <- read.table("~/UPSC_ThesisWork/Data/SwAsp_Saevar/SwAsp.full.clone.descriptions.txt",
                                   header = T,
                                   as.is = T,
                                   sep = '\t')
##Tidying up THE Saevar Data
SaevarData <- SaevarData %>% filter(Clone > 0) %>% dplyr::select(!contains("Diameter"))
saevarColNames <- colnames(SaevarData)[which(grepl("^s", colnames(SaevarData)))]
SaevarData <- SaevarData %>% pivot_longer(cols = any_of(saevarColNames) , values_to = "Height", names_to = c("Year","Type"),names_sep = "_")
SaevarData$Year <- as.numeric(str_remove(SaevarData$Year, "s"))
SaevarData <- SaevarData %>% group_by(Clone, Block, Row, Plant) %>% mutate(!!paste("ar",1,sep="") := lag(x =Height,order_by = Year)) %>% as.data.frame()

write.table(SaevarData %>% group_by(Clone,Year,Location) %>% summarise(cnt = dplyr::n()) %>% filter(cnt < 2) %>% as.data.frame(),paste("~/UPSC_ThesisWork/Data/Plasticity","/LowRepSaevarData.txt",sep=""),col.names = T,row.names = F,quote=F)

#Now i am going to create the subdata set, it will include only the genotype data of the GE data that we DO have
#
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


#Unites the columns needed to create the same sample name format of old.samples in SaevarNameReassignment
SaevarData <- unite(SaevarData, col = "GE_Sample_Name", c('Meta.col', 'Meta.row', 'Clone'), sep = '-')

#Creating the Saevar subset
#This subset is already going to be tidy and does not need anything done to it
SaevarSubSet <- subset(SaevarData, SaevarData$GE_Sample_Name %in% SaevarNameReassignment$old.sample) # 1962 entries
#separating the old sample names into Col, Row and genotype into new columns

SaevarSubSet[c("Meta.col", "Meta.row", "Clone")] <- str_split_fixed(SaevarSubSet$GE_Sample_Name, '-', 3)
SaevarData[c("Meta.col", "Meta.row", "Clone")] <- str_split_fixed(SaevarData$GE_Sample_Name, '-', 3)

#There are 1962 objects by the end of this
#The way that this data frame looks is that the each height for the clone is not stored in the same row,
#but instead continuously right after the other, so there are not actually this many clones, it is just not compiled anymroe into one row but split into different rows
write.table(SaevarSubSet %>% group_by(Clone,Year,Location) %>% summarise(cnt = dplyr::n()) %>% filter(cnt < 2) %>% as.data.frame(),paste("~/UPSC_ThesisWork/Data/Plasticity","/LowRepSaevarSubSet.txt",sep=""),col.names = T,row.names = F,quote=F)

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
EkeboData <- EkeboData %>% pivot_longer(cols = any_of(ekeboColNames),values_to = "Height",names_to = c("Year","Type"),names_sep = "_") %>% filter(!is.na(Height)) %>% as.data.frame()
EkeboData$Year <- as.numeric(str_remove(EkeboData$Year,"e"))
EkeboData <- EkeboData %>% group_by(Clone,Block,Row,Plant) %>% mutate(!!paste("ar",1,sep="") := lag(x =Height,order_by = Year)) %>% as.data.frame()

#Storing the column names of the GE data (this is what will be in the subset) and turning it into a dataframe
ekeboGEColNames <- colnames(EkeboGEData)
ekeboGEColNames <- data.frame(ekeboGEColNames)

#Extracting the clone number from the sample ID
ekeboGEColNames <- ekeboGEColNames %>% mutate(Clones = str_extract(ekeboGEColNames, "(?<=_)\\d+"))
ekeboGEColNames$Clones <- as.numeric(ekeboGEColNames$Clones)

#Removing duplicates of ekeboColNames
EkeboGEClones <- ekeboGEColNames[!duplicated(ekeboGEColNames$Clones),]

#Subsetting the data in EkeboData based on Clones EkeboGEClones
EkeboSubset <- subset(EkeboData, EkeboData$Clone %in% EkeboGEClones$Clones) #422 entries

#The way that this data frame looks is that the each height for the clone is not stored in the same row,
# but instead continuously right after the other, so there are not actually this many clones, it is just not compiled anymroe into one row but split into different rows
write.table(EkeboSubset %>% group_by(Clone,Year,Location) %>% summarise(cnt = dplyr::n()) %>% filter(cnt < 2) %>% as.data.frame(),paste("~/UPSC_ThesisWork/Data/Plasticity","/LowRepEkeboSubset.txt",sep=""),col.names = T,row.names = F,quote=F)

##Plotting for within location Saevar
SaevarData$CloneYear <- paste(SaevarData$Clone, SaevarData$Year)
SaevarData$Clone <- as.factor(SaevarData$Clone)
SaevarData$Year <- as.factor(SaevarData$Year)

SaevarSubSet$CloneYear <- paste(SaevarSubSet$Clone, SaevarSubSet$Year)
SaevarSubSet$Clone <- as.factor(SaevarSubSet$Clone)
SaevarSubSet$Year <- as.factor(SaevarSubSet$Year)

#RRMM for location specific clone deviations
sink(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Saevar/","SaevarLoc_ModelFit.txt",sep=""))
modSaevar <- lmer(log10(Height) ~ (1 | Clone) + (1 | Block) + (1 | CloneYear), data = SaevarData, na.action = na.omit)
summary(modSaevar)
modSaevar <- lmer(log10(Height) ~ (1 | Block) + Clone*Year, data = SaevarData, na.action = na.omit)
summary(modSaevar)
sink()

sink(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Saevar/","SaevarSubLoc_ModelFit.txt",sep=""))
modSaevarSub <- lmer(log10(Height) ~ (1 | Clone) + (1 | Block) + (1 | CloneYear), data = SaevarSubSet, na.action = na.omit)
summary(modSaevarSub)
modSaevarSub <- lmer(log10(Height) ~ (1 | Block) + Clone*Year, data = SaevarSubSet, na.action = na.omit)
summary(modSaevarSub)
sink()

##Modeling for within location ekebo
EkeboData$CloneYear <- paste(EkeboData$Clone, EkeboData$Year)
EkeboData$Clone <- as.factor(EkeboData$Clone)
EkeboData$Year <- as.factor(EkeboData$Year)

EkeboSubset$CloneYear <- paste(EkeboSubset$Clone, EkeboSubset$Year)
EkeboSubset$Clone <- as.factor(EkeboSubset$Clone)
EkeboSubset$Year <- as.factor(EkeboSubset$Year)

##RRMM for location specific to Ekebo
sink(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Ekebo/","EkeboLoc_ModelFit.txt",sep=""))
modEkebo <- lmer(log10(Height) ~ (1 | Clone) + (1 | Block) + (1 | CloneYear), data = EkeboData, na.action = na.omit)
summary(modEkebo)
modEkebo <- lmer(log10(Height) ~ (1 | Block) + Clone*Year, data = EkeboData, na.action = na.omit)
summary(modEkebo)
sink()

sink(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Ekebo/","EkeboSubLoc_ModelFit.txt",sep=""))
modEkeboSub <- lmer(log10(Height) ~ (1 | Clone) + (1 | Block) + (1 | CloneYear), data = EkeboSubset, na.action = na.omit)
summary(modEkeboSub)
modEkeboSub <- lmer(log10(Height) ~ (1 | Block) + Clone*Year, data = EkeboSubset, na.action = na.omit)
summary(modEkeboSub)
sink()

#Extracting the interactions between the clone and the year in the specific location
cloneYearS <- data.frame("cloneYearS"=names(fixef(modSaevar)[which(grepl(":",names(fixef(modSaevar))))]),rawVal=fixef(modSaevar)[which(grepl(":",names(fixef(modSaevar))))],transVal=10^(fixef(modSaevar)[which(grepl(":",names(fixef(modSaevar))))]))
cloneYearS$Year <- as.numeric(unlist(lapply(cloneYearS$cloneYearS,function(x){strsplit(x,"Year")[[1]][2]})))
cloneYearS$Clone <- as.numeric(str_replace(unlist(lapply(cloneYearS$cloneYearS,function(x){strsplit(x,":")[[1]][1]})),"Clone",""))
plasticityS <- data.frame("clone"=as.numeric(str_replace(names(fixef(modSaevar)[which(grepl(":",names(fixef(modSaevar)))==F & grepl("Clone",names(fixef(modSaevar)))==T)]),"Clone","")),genoEst=fixef(modSaevar)[which(grepl(":",names(fixef(modSaevar)))==F & grepl("Clone",names(fixef(modSaevar)))==T)])
for (clone in unique(cloneYearS$Clone)) {
  cur <- cloneYearS[which(cloneYearS$Clone==clone),]
  plasticityS[which(plasticityS$clone==clone),"stderrEst"] <- sd(cur[,"rawVal"])/sqrt(length(cur[,"rawVal"]))
}
plasticityS <- plasticityS[order(plasticityS$stderrEst),]
colnames(plasticityS)[which(names(plasticityS) == "clone")] <- "Clone"
cloneYearS <- cloneYearS %>% left_join(plasticityS, by = "Clone")

#Subset
cloneYearSS <- data.frame("cloneYearSS"=names(fixef(modSaevarSub)[which(grepl(":",names(fixef(modSaevarSub))))]),rawVal=fixef(modSaevarSub)[which(grepl(":",names(fixef(modSaevarSub))))],transVal=10^(fixef(modSaevarSub)[which(grepl(":",names(fixef(modSaevarSub))))]))
cloneYearSS$Year <- as.numeric(unlist(lapply(cloneYearSS$cloneYearSS,function(x){strsplit(x,"Year")[[1]][2]})))
cloneYearSS$Clone <- as.numeric(str_replace(unlist(lapply(cloneYearSS$cloneYearSS,function(x){strsplit(x,":")[[1]][1]})),"Clone",""))
plasticitySS <- data.frame("clone"=as.numeric(str_replace(names(fixef(modSaevarSub)[which(grepl(":",names(fixef(modSaevarSub)))==F & grepl("Clone",names(fixef(modSaevarSub)))==T)]),"Clone","")),genoEst=fixef(modSaevarSub)[which(grepl(":",names(fixef(modSaevarSub)))==F & grepl("Clone",names(fixef(modSaevarSub)))==T)])
for (clone in unique(cloneYearSS$Clone)) {
  cur <- cloneYearSS[which(cloneYearSS$Clone==clone),]
  plasticitySS[which(plasticitySS$clone==clone),"stderrEst"] <- sd(cur[,"rawVal"])/sqrt(length(cur[,"rawVal"]))
}
plasticitySS <- plasticitySS[order(plasticitySS$stderrEst),]
colnames(plasticitySS)[which(names(plasticitySS) == "clone")] <- "Clone"
cloneYearSS <- cloneYearSS %>% left_join(plasticitySS, by = "Clone")

##Ekebo
cloneYearE <- data.frame("cloneYearE"=names(fixef(modEkebo)[which(grepl(":",names(fixef(modEkebo))))]),rawVal=fixef(modEkebo)[which(grepl(":",names(fixef(modEkebo))))],transVal=10^(fixef(modEkebo)[which(grepl(":",names(fixef(modEkebo))))]))
cloneYearE$Year <- as.numeric(unlist(lapply(cloneYearE$cloneYearE,function(x){strsplit(x,"Year")[[1]][2]})))
cloneYearE$Clone <- as.numeric(str_replace(unlist(lapply(cloneYearE$cloneYearE,function(x){strsplit(x,":")[[1]][1]})),"Clone",""))
plasticityE <- data.frame("clone"=as.numeric(str_replace(names(fixef(modEkebo)[which(grepl(":",names(fixef(modEkebo)))==F & grepl("Clone",names(fixef(modEkebo)))==T)]),"Clone","")),genoEst=fixef(modEkebo)[which(grepl(":",names(fixef(modEkebo)))==F & grepl("Clone",names(fixef(modEkebo)))==T)])
plasticityE$stderrEst <- NA
for (clone in unique(cloneYearE$Clone)) {
  cur <- cloneYearE[which(cloneYearE$Clone==clone),]
  plasticityE[which(plasticityE$clone==clone),"stderrEst"] <- sd(cur[,"rawVal"])/sqrt(length(cur[,"rawVal"]))
}
plasticityE <- plasticityE[order(plasticityE$stderrEst),]
colnames(plasticityE)[which(names(plasticityE) == "clone")] <- "Clone"
cloneYearE <- cloneYearE %>% left_join(plasticityE, by = "Clone")

#Subset
cloneYearES <- data.frame("cloneYearES"=names(fixef(modEkeboSub)[which(grepl(":",names(fixef(modEkeboSub))))]),rawVal=fixef(modEkeboSub)[which(grepl(":",names(fixef(modEkeboSub))))],transVal=10^(fixef(modEkeboSub)[which(grepl(":",names(fixef(modEkeboSub))))]))
cloneYearES$Year <- as.numeric(unlist(lapply(cloneYearES$cloneYearES,function(x){strsplit(x,"Year")[[1]][2]})))
cloneYearES$Clone <- as.numeric(str_replace(unlist(lapply(cloneYearES$cloneYearES,function(x){strsplit(x,":")[[1]][1]})),"Clone",""))
plasticityES <- data.frame("clone"=as.numeric(str_replace(names(fixef(modEkeboSub)[which(grepl(":",names(fixef(modEkeboSub)))==F & grepl("Clone",names(fixef(modEkeboSub)))==T)]),"Clone","")),genoEst=fixef(modEkeboSub)[which(grepl(":",names(fixef(modEkeboSub)))==F & grepl("Clone",names(fixef(modEkeboSub)))==T)])
plasticityES$stderrEst <- NA
for (clone in unique(cloneYearES$Clone)) {
  cur <- cloneYearES[which(cloneYearES$Clone==clone),]
  plasticityES[which(plasticityES$clone==clone),"stderrEst"] <- sd(cur[,"rawVal"])/sqrt(length(cur[,"rawVal"]))
}
plasticityES <- plasticityES[order(plasticityES$stderrEst),]
colnames(plasticityES)[which(names(plasticityES) == "clone")] <- "Clone"
cloneYearES <- cloneYearES %>% left_join(plasticityES, by = "Clone")

##Plot All plasticity in Saevar
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Saevar/", "CloneYear_Plasticity.pdf", sep = ''), width = 7, height = 4)
ggplot(cloneYearS, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearS, Clone %in% plasticityS$Clone), linewidth = 1.5) +
  ggtitle("All Plasticity within Sävar \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearS$Year, labels = cloneYearS$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#Subset
ggplot(cloneYearSS, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearSS, Clone %in% plasticitySS$Clone), linewidth = 1.5) +
  ggtitle("All Plasticity within Sävar Subset \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearS$Year, labels = cloneYearS$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()

#Plot all plasticity in Ekebo
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Ekebo/", "CloneYear_Plasticity.pdf", sep = ''), width = 7, height = 4)
ggplot(cloneYearE, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearE, Clone %in% plasticityE$Clone), linewidth = 1.5) +
  ggtitle("All Plasticity within Ekebo \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearE$Year, labels = cloneYearE$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#Subset
ggplot(cloneYearES, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearES, Clone %in% plasticityES$Clone), linewidth = 1.5) +
  ggtitle("All Plasticity within Ekebo Subset \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearES$Year, labels = cloneYearES$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()


##Plotting low plasticity individuals in Saevar
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Saevar/", "CloneYear_LowPlasticity.pdf", sep = ''), width = 7, height = 4)
##Low plasticity in all saevar data
ggplot(cloneYearS, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearS, Clone %in% plasticityS$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity in Sävar \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearS$Year, labels = cloneYearS$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#low plasticity in saevar subset data
ggplot(cloneYearSS, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearSS, Clone %in% plasticitySS$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity in Sävar SubSet\nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearSS$Year, labels = cloneYearSS$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()


##Low plasticity in Ekebo datasets
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Ekebo/", "CloneYear_LowPlasticity.pdf", sep = ''), width = 7, height = 4)
##Low plasticity in all saevar data
ggplot(cloneYearE, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearE, Clone %in% plasticityE$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity in Ekebo \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearE$Year, labels = cloneYearE$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#low plasticity in saevar subset data
ggplot(cloneYearES, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearES, Clone %in% plasticityES$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity in Ekebo SubSet\nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearES$Year, labels = cloneYearES$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()


##Plotting the high plasticity individuals in Saevar dataframes
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Saevar/", "CloneYear_HighPlasticity.pdf", sep = ''), width = 7, height = 4)
##Plotting high within Saevar
ggplot(cloneYearS, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearS, Clone %in% plasticityS$Clone[(nrow(plasticityS)-10):nrow(plasticityS)]), linewidth = 1.5) +
  ggtitle("High Plasticity in Sävar \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearS$Year, labels = cloneYearS$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#plotting high within subset of saevar
ggplot(cloneYearSS, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearSS, Clone %in% plasticitySS$Clone[(nrow(plasticitySS)-10):nrow(plasticitySS)]), linewidth = 1.5) +
  ggtitle("High Plasticity in Sävar Subset \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearSS$Year, labels = cloneYearSS$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()


##Plotting high plasticity individuals in Ekebo dataframes
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Ekebo/", "CloneYear_HighPlasticity.pdf", sep = ''), width = 7, height = 4)
##Plotting high individuals in Ekebo plasticity
ggplot(cloneYearE, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearE, Clone %in% plasticityE$Clone[(nrow(plasticityE)-10):nrow(plasticityE)]), linewidth = 1.5) +
  ggtitle("High Plasticity in Ekebo \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearE$Year, labels = cloneYearE$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(cloneYearES, aes(x = Year, y = rawVal, group = Clone, color = genoEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(cloneYearES, Clone %in% plasticityES$Clone[(nrow(plasticityES)-10):nrow(plasticityES)]), linewidth = 2.0) +
  ggtitle("High Plasticity in Ekebo Subset \nClone by Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = cloneYearES$Year, labels = cloneYearES$Year) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()


##plotting genotype against plasticity (genoest vs stderrest) Saevar data sets
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Saevar/", "PlasticityByGenoBLUP_highlight10.pdf", sep = ''), width = 10, height = 7)

plasticityS$col <- "#00000080"
plasticityS$col[1:10] <- "lightblue"
plasticityS$col[(nrow(plasticityS)-10):nrow(plasticityS)] <- "pink"

plasticitySS$col <- "#00000080"
plasticitySS$col[1:10] <- "lightblue"
plasticitySS$col[(nrow(plasticitySS)-10):nrow(plasticitySS)] <- "pink"

#Alldata saevar
ggplot(plasticityS, aes(x = genoEst, y = stderrEst, colour = col)) + 
  ggtitle(paste("Saevar Adj. R2:",round(summary(lm(plasticityS$stderrEst~plasticityS$genoEst))$adj.r.squared,3))) +
  geom_point(size = 4, shape = 19, show.legend = F) +
  geom_smooth(method = 'lm', se = F, color = 'red') +
  labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
  geom_text(data = plasticityS[c(1:10, (nrow(plasticityS) - 10):nrow(plasticityS)), ],
            aes(label = Clone),  # Label with clone names
            color = "black",  # Text color
            vjust = -0.5,  # Adjust vertical position of labels
            hjust = 0.5) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#subset data saevar
ggplot(plasticitySS, aes(x = genoEst, y = stderrEst, colour = col)) + 
  ggtitle(paste("Saevar Subset Adj. R2:",round(summary(lm(plasticitySS$stderrEst~plasticitySS$genoEst))$adj.r.squared,3))) +
  geom_point(size = 4, shape = 19, show.legend = F) +
  geom_smooth(method = 'lm', se = F, color = 'red') +
  labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
  geom_text(data = plasticitySS[c(1:10, (nrow(plasticitySS) - 10):nrow(plasticitySS)), ],
            aes(label = Clone),  # Label with clone names
            color = "black",  # Text color
            vjust = -0.5,  # Adjust vertical position of labels
            hjust = 0.5) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()


##Plotting genotype against plasticity (genoest vs stderrest) for Ekebo data sets
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/LocationPlast/Ekebo/", "PlasticityByGenoBLUP_highlight10.pdf", sep = ''), width = 10, height = 7)

plasticityE$col <- "#00000080"
plasticityE$col[1:10] <- "lightblue"
plasticityE$col[(nrow(plasticityE)-10):nrow(plasticityE)] <- "pink"

plasticityES$col <- "#00000080"
plasticityES$col[1:10] <- "lightblue"
plasticityES$col[(nrow(plasticityES)-10):nrow(plasticityES)] <- "pink"

#Alldata Ekebo
ggplot(plasticityE, aes(x = genoEst, y = stderrEst, colour = col)) + 
  ggtitle(paste("Ekebo Adj. R2:",round(summary(lm(plasticityE$stderrEst~plasticityE$genoEst))$adj.r.squared,3))) +
  geom_point(size = 4, shape = 19, show.legend = F) +
  geom_smooth(method = 'lm', se = F, color = 'red') +
  labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
  geom_text(data = plasticityE[c(1:10, (nrow(plasticityE) - 10):nrow(plasticityE)), ],
            aes(label = Clone),  # Label with clone names
            color = "black",  # Text color
            vjust = -0.5,  # Adjust vertical position of labels
            hjust = 0.5) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#subset data Ekebo
ggplot(plasticityES, aes(x = genoEst, y = stderrEst, colour = col)) + 
  ggtitle(paste("Ekebo Subset Adj. R2:",round(summary(lm(plasticityES$stderrEst~plasticityES$genoEst))$adj.r.squared,3))) +
  geom_point(size = 4, shape = 19, show.legend = F) +
  geom_smooth(method = 'lm', se = F, color = 'red') +
  labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
  geom_text(data = plasticityES[c(1:10, (nrow(plasticityES) - 10):nrow(plasticityES)), ],
            aes(label = Clone),  # Label with clone names
            color = "black",  # Text color
            vjust = -0.5,  # Adjust vertical position of labels
            hjust = 0.5) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()








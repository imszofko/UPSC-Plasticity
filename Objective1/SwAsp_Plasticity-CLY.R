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

#Both location's data are combined into one dataframe for normal data and the subset
AllData <- rbind(SaevarData[, c("Location","Block","Row","Plant","Clone","Year","Height","ar1")] , EkeboData[, c("Location","Block","Row","Plant","Clone","Year","Height","ar1")])
SubSetData <- rbind(SaevarSubSet[, c("Location","Block","Row","Plant","Clone","Year","Height","ar1")] , EkeboSubset[, c("Location","Block","Row","Plant","Clone","Year","Height","ar1")])

##Starting the Stat testing
#Check the normality of both data sets
type = "Height"
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/", type, "NormalityCheck.pdf", sep = ""),width = 7,height=5)
hist(AllData[, type], col = 'red', main = "All Data")
hist(SubSetData[, type], col = 'red', main = "Subset Data")
dev.off()

pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/", type, "_Distribution_Transformation.pdf", sep = ""),width = 7,height=5)
hist(log10(AllData[, type]), col = 'purple', main = paste("log10 Transformation for All Data",type))
hist(log10(SubSetData[, type]), col = 'purple', main = paste("log10 Transformation for Subset Data",type))
hist(log(AllData[, type]), col = 'purple', main = paste("log Transformation for All Data", type))
hist(log(SubSetData[, type]), col = 'purple', main = paste("log Transformation for Subset Data",type))
hist(sqrt(AllData[, type]), col = 'purple', main = paste("Sqrt Transformation for All Data", type))
hist(sqrt(SubSetData[, type]), col = 'purple', main = paste("Sqrt Transformation for Subset Data", type))

normAllData <-data.frame(log10(AllData[, type]))

#log10 for All Data
qqnorm(SubSetData[, type])
qqline(SubSetData[, type])

qqnorm(log10(SubSetData[, type]))
qqline(log10(SubSetData[, type]))

qqnorm(log(SubSetData[, type]))
qqline(log(SubSetData[, type]))
#There is barely a difference so I will use log10 for both
dev.off()


##ANOVA Testing
###Modeling the height to be function of the year and the clone

#Factoring the year and the location
AllData$Height <- log10(AllData$Height)
SubSetData$Height <- log10(SubSetData$Height)

AllData$LocationF <- factor(AllData$Location,
                           levels = c("Saevar", "Ekebo"))
AllData$YearF <- factor(AllData$Year,
                       levels = c(5,6,7,8,9,10,11,12,13))


SubSetData$LocationF <- factor(SubSetData$Location,
                           levels = c("Saevar", "Ekebo"))
SubSetData$YearF <- factor(SubSetData$Year,
                       levels = c(5,6,7,8,9,10,11,12,13))

aovAll <- aov(Height ~ YearF + Clone*LocationF, data = AllData, na.action = na.omit)
aovSub <- aov(Height ~ YearF + Clone*LocationF, data = SubSetData)
summary(aovAll)
summary(aovSub)
# large F value means that there is a greater difference among the group means. It suggests that the variations between the groups are significant--larger f value means that variation is caused by independent variable is real and not due to chance
# If p-value is less thatn the chosen significance level it indiciates that there are statistically significant differences among the groups. You have evidence to reject the null hypothesis which assums no significance
# 

pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/ANOVA_Results.pdf", sep = ""), width = 7, height = 5)
plot(aovAll, main = "All Data ANOVA Results")
plot(aovSub, main = "Subset Data ANOVA Results")
dev.off()

#Post HOC testing
TukeyHSD(aovAll, which = "Location")
TukeyHSD(aovSub, which = "Location")
###results from ANOVA are not that good, RRMM is better anyway but I wanted to be able to compare it to something

##Example from document
#Looking at the regression and only the time indicate as a covariate
#heightLm <- lm(Height ~ Year, data = AllData)
#summary(heightLm)

##Runing the mixed regression model
#heightMM <- lmer(Height ~ Year + (1 | Clone) + (1 | Location), data = AllData)
#summary(heightMM)

##Setup Model
AllData$LocBlock <- paste(AllData$Location, AllData$Block)
AllData$LocClone <- paste(AllData$Clone, AllData$Location)
AllData$LocYear <- paste(AllData$Location, AllData$Year)
AllData$LocCloneYear <- paste(AllData$Clone, AllData$Location, AllData$Year)

SubSetData$LocBlock <- paste(SubSetData$Location, SubSetData$Block)
SubSetData$LocClone <- paste(SubSetData$Clone, SubSetData$Location)
SubSetData$LocYear <- paste(SubSetData$Location, SubSetData$Year)
SubSetData$LocCloneYear <- paste(SubSetData$Clone, SubSetData$Location, SubSetData$Year)
##This setup essentially means Location:Block instead of writing it out in the model, it is done before hand for clarity and simplicity

#AllModel <- lmer(Height ~ Location + (1 | Year) + (1 | Clone), data = AllData, na.action = na.omit)
#summary(AllModel)

##Allowing each sample to have their own intercept
#mod <- lmer(Height ~ (1 | Location) + (1 | Year) + (1 | Location:Year) + (1 | Clone) + (1 | Location:Clone) + (1 | LocBlock), data = AllData, na.action = na.omit)

mod <- lmer(Height ~ (1 | Clone) + (1 | Location) + (1 | Year) + (1 | LocBlock) + (1 | LocCloneYear) + (1 | LocYear), data = AllData, na.action = na.omit)
summary(mod)

#mod1 <- lmer(Height ~ (1 | LocBlock) + (1 | Location:Clone) + (1 | Location:Year), data = AllData, na.action = na.omit)
#mod2 <- lmer(Height ~ (1 | LocCloneYear), data = AllData, na.action = na.omit)

modsub <- lmer(Height ~ (1 | Clone) + (1 | Location) + (1 | Year) + (1 | LocBlock) + (1 | LocCloneYear) + (1 | LocYear), data = SubSetData, na.action = na.omit)
summary(modsub) 


sink(paste("~/UPSC_ThesisWork/Data/Plasticity/ModelFit.txt",sep=""))
summary(mod)
summary(modsub)
sink()

##Visualization of the modeling results
#Plotting the residuals vs the fitted

pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/ResidualsVsFitted.pdf", sep=""), width = 10, height =8)
plot(fitted(mod), resid(mod), main = "ResidualsVsFitted for AllData")
abline(h = 0)
plot(fitted(modsub), resid(modsub), main = "ResidualsVsFitted for SubData")
abline(h = 0)
dev.off()


#Why was this done this way with rnorm?
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/ResidualsQQPlot.pdf",sep=""),width = 10, height =8)
qqplot(resid(mod), rnorm(n = length(resid(mod)), mean = 0, sd = sd(resid(mod))))

qqnorm(resid(mod))
qqline(resid(mod))

plot(density(resid(mod)))

qqplot(resid(modsub), rnorm(n = length(resid(modsub)), mean = 0, sd = sd(resid(modsub))))

qqnorm(resid(modsub))
qqline(resid(modsub))

plot(density(resid(modsub)))
dev.off()


##Check out the Location:Year effect for Alldata
locYear <- data.frame(LocYear = rownames(ranef(mod)$LocYear), val = ranef(mod)$LocYear)
locYear$Year <- unlist(lapply(locYear$LocYear,function(x){str_split(x," ")[[1]][2]}))
locYear$LocYear <- unlist(lapply(locYear$LocYear,function(x){str_split(x," ")[[1]][1]}))
locYear$LocCol <- "#993300"
locYear$LocCol[which(locYear$LocYear=="Saevar")] <- "#006666"
locYear$rawVal <- locYear$X.Intercept. #
locYear$transVal <- 10^locYear$X.Intercept. #

##Check out location year effect for the Subset
locYearSub <- data.frame(LocYear = rownames(ranef(modsub)$LocYear), val = ranef(modsub)$LocYear)
locYearSub$Year <- unlist(lapply(locYearSub$LocYear,function(x){str_split(x," ")[[1]][2]}))
locYearSub$LocYear <- unlist(lapply(locYearSub$LocYear,function(x){str_split(x," ")[[1]][1]}))
locYearSub$LocCol <- "#666600"
locYearSub$LocCol[which(locYearSub$LocYear=="Saevar")] <- "#660099"
locYearSub$rawVal <- locYearSub$X.Intercept. #
locYearSub$transVal <- 10^locYearSub$X.Intercept. #

pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/Location-Year_Effect.pdf",sep=""),width = 10,height = 8)
##Intercepting data
plot(locYear$Year, locYear[, "rawVal"], col = locYear$LocCol, 
     pch=19, cex=1.5, ylab = "Location-year BLUP", xlab = "Year", main="Location:Year Variation for Height of AllData (Raw Values)")

legend("bottomright", legend = c("Ekebo","Saevar"),col = c('#993300', '#006666'), pch=19)

plot(locYear$Year, locYear[, "transVal"], col = locYear$LocCol, 
     pch=19, cex=1.5, ylab = "Location-year BLUP", xlab = "Year", main="Location:Year Variation for Height of AllData (Trans Values)")

legend("bottomright", legend = c("Ekebo","Saevar"),col = c('#993300', '#006666'), pch=19)

##Subset of data
plot(locYearSub$Year, locYearSub[, "rawVal"], col = locYearSub$LocCol, 
     pch=19, cex=1.5, ylab = "Location-year BLUP", xlab = "Year", main="Location:Year Variation for Height of SubsetData (Raw Values)")

legend("bottomright", legend = c("Ekebo","Saevar"),col = c('#666600', '#660099'), pch=19)

plot(locYearSub$Year, locYearSub[, "transVal"], col = locYearSub$LocCol, 
     pch=19, cex=1.5, ylab = "Location-year BLUP", xlab = "Year", main="Location:Year Variation for Height of SubsetData (Trans Values)")

legend("bottomright", legend = c("Ekebo","Saevar"),col = c('#666600', '#660099'), pch=19)
dev.off()

##Extracting the Clone:Location:Year interations for Alldata
locCloneYear <- data.frame(LocCloneYear = rownames(ranef(mod)$LocCloneYear), val = ranef(mod)$LocCloneYear)
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

##For subdata
locCloneYearSub <- data.frame(LocCloneYear = rownames(ranef(modsub)$LocCloneYear), val = ranef(modsub)$LocCloneYear)
locCloneYearSub$Clone <- unlist(lapply(locCloneYearSub$LocCloneYear,function(x){str_split(x," ")[[1]][1]}))
locCloneYearSub$Loc <- unlist(lapply(locCloneYearSub$LocCloneYear,function(x){str_split(x," ")[[1]][2]}))
locCloneYearSub$Year <- as.numeric(unlist(lapply(locCloneYearSub$LocCloneYear,function(x){str_split(x," ")[[1]][3]})))
locCloneYearSub$LocColor <- "#CC6600"
locCloneYearSub$LocColor[which(locCloneYearSub$Loc == "Saevar")] <- "#CC99FF"
locCloneYearSub$rawVal <- locCloneYearSub$X.Intercept.
locCloneYearSub$transVal <- 10^locCloneYearSub$X.Intercept.
locCloneYearSub <- locCloneYearSub[order(locCloneYearSub$Clone,locCloneYearSub$Loc,locCloneYearSub$Year),]
locCloneYearSub$yearLabels <- paste(locCloneYearSub$Loc,locCloneYearSub$Year,sep="-")
locCloneYearSub$Year[which(locCloneYearSub$Loc=="Saevar")] <- 10+locCloneYearSub$Year[which(locCloneYearSub$Loc=="Saevar")] #just for plotting purposes

##Plotting the random effect values
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/Clone-Location-Year_Effect.pdf",sep=""),width = 10,height = 8)

##The plots for not specific location
plot(locCloneYear$Year,locCloneYear[,"rawVal"],col = locCloneYear$LocColor, pch=19, cex=1.5,
     main = "Clone:Location:Year Variation for Height of AllData (Raw Values)", xlab =  "Year", ylab = "Clone/Location/Year BLUP")

plot(locCloneYear$Year,locCloneYear[,"transVal"],col = locCloneYear$LocColor, pch=19, cex=1.5,
     main = "Clone:Location:Year Variation for Height of AllData (Trans Values)", xlab =  "Year", ylab = "Clone/Location/Year BLUP")

plot(locCloneYearSub$Year, locCloneYearSub[, "rawVal"], col = locCloneYearSub$LocColor, 
     pch = 19, cex = 1.5, main = "Clone:Location:Year Variation for Height of SubsetData (Raw Values)", xlab =  "Year", ylab = "Clone/Location/Year BLUP")

plot(locCloneYearSub$Year, locCloneYearSub[, "transVal"], col = locCloneYearSub$LocColor, 
     pch = 19, cex = 1.5, main = "Clone:Location:Year Variation for Height of subsetData (Trans Values)", xlab =  "Year", ylab = "Clone/Location/Year BLUP")

##Plots specific to location for Alldata
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

##Plots specific to location for SubsetData
plot(locCloneYearSub$Year[which(locCloneYearSub$Loc == "Saevar")], locCloneYearSub[, "rawVal"][which(locCloneYearSub$Loc == "Saevar")],
     col = locCloneYearSub$LocColor[which(locCloneYearSub$Loc == "Saevar")], pch = 19, cex = 1.5, main = "Clone:Location:Year Variation for Height of SubsetData in Sävar (Raw Values)", 
     xlab = "Year (Sävar)", ylab = "Clone/Location/Year BLUP")
plot(locCloneYearSub$Year[which(locCloneYearSub$Loc == "Saevar")], locCloneYearSub[, "transVal"][which(locCloneYearSub$Loc == "Saevar")],
     col = locCloneYearSub$LocColor[which(locCloneYearSub$Loc == "Saevar")], pch = 19, cex = 1.5, main = "Clone:Location:Year Variation for Height of SubsetData in Sävar (Trans Values)", 
     xlab = "Year (Sävar)", ylab = "Clone/Location/Year BLUP")

plot(locCloneYearSub$Year[which(locCloneYearSub$Loc == "Ekebo")], locCloneYearSub[, "rawVal"][which(locCloneYearSub$Loc == "Ekebo")],
     col = locCloneYearSub$LocColor[which(locCloneYearSub$Loc == "Ekebo")], pch = 19, cex = 1.5, main = "Clone:Location:Year Variation for Height of SubsetData in Ekebo (Raw Values)", 
     xlab = "Year (Ekebo)", ylab = "Clone/Location/Year BLUP")

plot(locCloneYearSub$Year[which(locCloneYearSub$Loc == "Ekebo")], locCloneYearSub[, "transVal"][which(locCloneYearSub$Loc == "Ekebo")],
     col = locCloneYearSub$LocColor[which(locCloneYearSub$Loc == "Ekebo")], pch = 19, cex = 1.5, main = "Clone:Location:Year Variation for Height of SubsetData in Ekebo (Trans Values)", 
     xlab = "Year (Ekebo)", ylab = "Clone/Location/Year BLUP")
dev.off()


###Generating Plasticity Scores
plasticity <- data.frame(Clone = character(0), stderrEst = numeric(0))
for (clone in unique(locCloneYear$Clone)) {
  cur <- locCloneYear[which(locCloneYear$Clone==clone),]
  plasticity <- rbind(plasticity, data.frame(clone = clone, stderrEst = sd(cur[,"rawVal"])/sqrt(length(cur[,"rawVal"]))))
}

#add genotypic estimates
plasticity$GenotypicEst <- ranef(mod)$Clone$'(Intercept)'[match(plasticity$clone,rownames(ranef(mod)$Clone))] #10^(ranef(mod)$Clone$'(Intercept)'[match(plast$clone,rownames(ranef(mod)$Clone))]) #
plasticity <- plasticity[order(plasticity$stderrEst),]
colnames(plasticity)[which(names(plasticity) == "clone")] <- "Clone"
plasticity$col <- "#00000080"
plasticity$col[1:10] <- "lightblue"
plasticity$col[(nrow(plasticity)-10):nrow(plasticity)] <- "pink"
locCloneYear <- locCloneYear %>% left_join(plasticity, by = "Clone")

#Generating Plasticity for subset
plasticitySub <- data.frame(Clone = character(0), stderrEst = numeric(0))
for (clone in unique(locCloneYearSub$Clone)) {
  cur <- locCloneYearSub[which(locCloneYearSub$Clone==clone),]
  plasticitySub <- rbind(plasticitySub, data.frame(clone = clone, stderrEst = sd(cur[,"rawVal"])/sqrt(length(cur[,"rawVal"]))))
}

#add genotypic estimates
plasticitySub$GenotypicEst <- ranef(modsub)$Clone$'(Intercept)'[match(plasticitySub$clone,rownames(ranef(modsub)$Clone))] #10^(ranef(mod)$Clone$'(Intercept)'[match(plast$clone,rownames(ranef(mod)$Clone))]) #
plasticitySub <- plasticitySub[order(plasticitySub$stderrEst),]
colnames(plasticitySub)[which(names(plasticitySub) == "clone")] <- "Clone"
plasticitySub$col <- "#00000080"
plasticitySub$col[1:10] <- "lightblue"
plasticitySub$col[(nrow(plasticitySub)-10):nrow(plasticitySub)] <- "pink"
locCloneYearSub <- locCloneYearSub %>% left_join(plasticitySub, by = "Clone")


##Plotting
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/locCloneYear_LowPlasticityIndividuals.pdf",sep=""),width = 10, height = 8)
ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, color = GenotypicEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(locCloneYear, Clone %in% plasticity$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity Clones \nClone by Location-Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, color = Clone)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(locCloneYear, Clone %in% plasticity$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity Clones \nClone by Location-Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#Subset
ggplot(locCloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = GenotypicEst)) + 
  geom_line(color = "black", alpha = 0.5) +
  geom_line(data = subset(locCloneYearSub, Clone %in% plasticitySub$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity Clones \nClone by Location-Year of Subset Data") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYearSub$Year, labels = locCloneYearSub$yearLabels) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(locCloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = Clone)) + 
  geom_line(color = "black", alpha = 0.5) +
  geom_line(data = subset(locCloneYearSub, Clone %in% plasticitySub$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity Clones \nClone by Location-Year of Subset Data") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYearSub$Year, labels = locCloneYearSub$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()

##All Data
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/locCloneYear_HighPlasticityIndividuals.pdf",sep=""),width = 10, height = 8)
ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, colour = GenotypicEst)) + 
  geom_line(color = "black", alpha = 0.5) +
  geom_line(data = subset(locCloneYear, Clone %in% plasticity$Clone[(nrow(plasticity)-10):nrow(plasticity)]), linewidth = 1.5) +
  ggtitle("High Plasticity Clones \nClone by Location-Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, colour = Clone)) + 
  geom_line(color = "black", alpha = 0.5) +
  geom_line(data = subset(locCloneYear, Clone %in% plasticity$Clone[(nrow(plasticity)-10):nrow(plasticity)]), linewidth = 1.5) +
  ggtitle("High Plasticity Clones \nClone by Location-Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

#Subdata
ggplot(locCloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = GenotypicEst)) + 
  geom_line(color = "black", alpha = 0.5) +
  geom_line(data = subset(locCloneYearSub, Clone %in% plasticitySub$Clone[(nrow(plasticitySub)-10):nrow(plasticitySub)]), linewidth = 1.5) +
  ggtitle("High Plasticity Clones \nClone by Location-Year Subset") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYearSub$Year, labels = locCloneYearSub$yearLabels) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(locCloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = Clone)) + 
  geom_line(color = "black", alpha = 0.5) +
  geom_line(data = subset(locCloneYearSub, Clone %in% plasticitySub$Clone[(nrow(plasticitySub)-10):nrow(plasticitySub)]), linewidth = 1.5) +
  ggtitle("High Plasticity Clones \nClone by Location-Year Subset") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYearSub$Year, labels = locCloneYearSub$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()


##Highlight all low
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/locCloneYear_AllGxebyGenotypeHighlightLow.pdf",sep=""),width = 10, height = 8)
locCloneYear <- locCloneYear %>%
  mutate(Clone = reorder(Clone, GenotypicEst, FUN = mean, decreasing = T))
locCloneYearSub <- locCloneYearSub %>%
  mutate(Clone = reorder(Clone, GenotypicEst, FUN = mean, decreasing = T))


ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, colour = GenotypicEst)) + 
  geom_line(data = subset(locCloneYear, GenotypicEst %in% plasticity$GenotypicEst), linewidth = .75) +
  labs(y = "GxE BLUP", x = '') + 
  ggtitle("Low Plasticity") +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(locCloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = GenotypicEst)) + 
  geom_line(data = subset(locCloneYearSub, GenotypicEst %in% plasticitySub$GenotypicEst), linewidth = .75) +
  labs(y = "GxE BLUP", x = '') + 
  ggtitle("Low Plasticity of Subset") +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  scale_x_continuous(breaks = locCloneYearSub$Year, labels = locCloneYearSub$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()


##Highlight all high gxE By Genotypic Score
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/locCloneYear_AllGxebyGenotypeHighlightHigh.pdf",sep=""),width = 10, height = 8)
locCloneYear <- locCloneYear %>%
  mutate(Clone = reorder(Clone, GenotypicEst, FUN = mean, decreasing = F))
locCloneYearSub <- locCloneYearSub %>%
  mutate(Clone = reorder(Clone, GenotypicEst, FUN = mean, decreasing = F))

ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, colour = GenotypicEst)) + 
  geom_line(data = subset(locCloneYear, GenotypicEst %in% plasticity$GenotypicEst), linewidth = .75) +
  labs(y = "GxE BLUP", x = '') + 
  ggtitle("High Plasticity") +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(locCloneYearSub, aes(x = Year, y = rawVal, group = Clone, colour = GenotypicEst)) + 
  geom_line(data = subset(locCloneYearSub, GenotypicEst %in% plasticitySub$GenotypicEst), linewidth = .75) +
  labs(y = "GxE BLUP", x = '') + 
  ggtitle("High Plasticity of Subset") +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  scale_x_continuous(breaks = locCloneYearSub$Year, labels = locCloneYearSub$yearLabels) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()

#plot genotype against plasticity
pdf(paste("~/UPSC_ThesisWork/Data/Plasticity/PlasticityByGenoBLUP_highlight10.pdf",sep=""),width = 10,height = 7)
ggplot(plasticity, aes(x = GenotypicEst, y = stderrEst, colour = col)) + 
  ggtitle(paste("10 High and Low Plastic Clones from Data Set \n Adj. R2:",round(summary(lm(plasticity$stderrEst~plasticity$GenotypicEst))$adj.r.squared,3))) +
  geom_point(size = 4, shape = 19, show.legend = F) +
  geom_smooth(method = 'lm', se = F, color = 'red') +
  labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
  geom_text(data = plasticity[c(1:10, (nrow(plasticity) - 10):nrow(plasticity)), ],
    aes(label = Clone),  # Label with clone names
    color = "black",  # Text color
    vjust = -0.5,  # Adjust vertical position of labels
    hjust = 0.5) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

ggplot(plasticitySub, aes(x = GenotypicEst, y = stderrEst, colour = col)) + 
  ggtitle(paste("10 High and Low Plastic Clones from SubSet Data \n Adj. R2:",round(summary(lm(plasticity$stderrEst~plasticity$GenotypicEst))$adj.r.squared,3))) +
  geom_point(size = 4, shape = 19, show.legend = F) +
  geom_smooth(method = 'lm', se = F, color = 'red') +
  labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
  geom_text(data = plasticitySub[c(1:10, (nrow(plasticitySub) - 10):nrow(plasticitySub)), ],
            aes(label = Clone),  # Label with clone names
            color = "black",  # Text color
            vjust = -0.5,  # Adjust vertical position of labels
            hjust = 0.5) +
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels
dev.off()















##Plot against latitude or location from provenance file
##

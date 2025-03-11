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
SaevarData$CloneBlkProvYear <- paste(SaevarData$Clone, SaevarData$Block, SaevarData$Province, SaevarData$Year, SaevarData$Status)

#Creating the Saevar subset
#This subset is already going to be tidy and does not need anything done to it
SaevarSubSet <- subset(SaevarData, SaevarData$GE_Sample_Name %in% SaevarNameReassignment$old.sample) # 1962 entries
SaevarSubSet[c("Meta.col", "Meta.row", "Clone")] <- str_split_fixed(SaevarSubSet$GE_Sample_Name, '-', 3)
write.table(SaevarSubSet %>% group_by(Clone,Year,Location) %>% summarise(cnt = dplyr::n()) %>% filter(cnt < 2) %>% as.data.frame(),paste("~/UPSC_ThesisWork/Data/Plasticity","/LowRepSaevarSubSet.txt",sep=""),col.names = T,row.names = F,quote=F)

AllData <- rbind(SaevarData[, c("Location","Block","Row","Plant","Clone","Year","Height","ar1")] , EkeboData[, c("Location","Block","Row","Plant","Clone","Year","Height","ar1")])
AllDataStat <- rbind(SaevarData[, c("Location","Block","Row","Plant","Clone","Year", "Population", "Province","Height","Status", "ar1")] , EkeboData[, c("Location","Block","Row","Plant","Clone","Year", "Population", "Province","Height", "Status", "ar1")])
type = "Height"
AllData$Height <- log10(AllData$Height)
AllData$LocationF <- factor(AllData$Location,
                            levels = c("Saevar", "Ekebo"))
AllData$YearF <- factor(AllData$Year,
                        levels = c(5,6,7,8,9,10,11,12,13))
AllData$LocBlock <- paste(AllData$Location, AllData$Block)
AllData$LocClone <- paste(AllData$Clone, AllData$Location)
AllData$LocYear <- paste(AllData$Location, AllData$Year)
AllData$LocCloneYear <- paste(AllData$Clone, AllData$Location, AllData$Year)

AllDataStat$Height <- log10(AllDataStat$Height)
AllDataStat$LocationF <- factor(AllDataStat$Location,
                                levels = c("Saevar", "Ekebo"))
AllDataStat$YearF <- factor(AllDataStat$Year,
                            levels = c(5,6,7,8,9,10,11,12,13))
AllDataStat$Latitude <- SaevarProvenanceData$Latitude[match(AllDataStat$Clone, SaevarProvenanceData$Clone)]
AllDataStat$Population <- SaevarProvenanceData$Population[match(AllDataStat$Clone, SaevarProvenanceData$Clone)]
AllDataStat$Province <- sapply(AllDataStat$Latitude, AspLocation)

AllDataStat$LocBlock <- paste(AllDataStat$Location, AllDataStat$Block)
AllDataStat$LocClone <- paste(AllDataStat$Clone, AllDataStat$Location)
AllDataStat$LocYear <- paste(AllDataStat$Location, AllDataStat$Year)
AllDataStat$LocCloneYear <- paste(AllDataStat$Clone, AllDataStat$Location, AllDataStat$Province, AllDataStat$Year, AllDataStat$Status)

mod <- lmer(Height ~ (1 | Clone) + (1 | Location) + (1 | Year) + (1 | LocBlock) + (1 | LocCloneYear) + (1 | LocYear) + Status, data = AllDataStat, na.action = na.omit)
summary(mod)

qqplot(resid(mod), rnorm(n = length(resid(mod)), mean = 0, sd = sd(resid(mod))))

qqnorm(resid(mod))
qqline(resid(mod))

plot(density(resid(mod)))

locCloneYear <- data.frame(LocCloneYear = rownames(ranef(mod)$LocCloneYear), val = ranef(mod)$LocCloneYear)
locCloneYear$Clone <- unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][1]}))
locCloneYear$Loc <- unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][2]}))
locCloneYear$Year <- as.numeric(unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][3]})))
locCloneYear$Status <- unlist(lapply(locCloneYear$LocCloneYear,function(x){str_split(x," ")[[1]][4]}))
locCloneYear$LocColor <- "#33cc33"
locCloneYear$LocColor[which(locCloneYear$Loc == "Saevar")] <- "#3300FF"
locCloneYear$rawVal <- locCloneYear$X.Intercept.
locCloneYear$transVal <- 10^locCloneYear$X.Intercept.
locCloneYear <- locCloneYear[order(locCloneYear$Clone,locCloneYear$Loc,locCloneYear$Year),]
locCloneYear$yearLabels <- paste(locCloneYear$Loc,locCloneYear$Year,sep="-")
locCloneYear$Year[which(locCloneYear$Loc=="Saevar")] <- 10+locCloneYear$Year[which(locCloneYear$Loc=="Saevar")] #just for plotting purposes

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

ggplot(locCloneYear, aes(x = Year, y = rawVal, group = Clone, color = GenotypicEst)) + 
  geom_line(color = 'black', alpha = 0.5) +
  geom_line(data = subset(locCloneYear, Clone %in% plasticity$Clone[1:10]), linewidth = 1.5) +
  ggtitle("Low Plasticity Clones \nClone by Location-Year") +
  labs(y = "GxE BLUP", x = '') + 
  scale_x_continuous(breaks = locCloneYear$Year, labels = locCloneYear$yearLabels) +
  scale_color_gradientn(name = 'Clone BLUP', 
                        colors = c("navy", "white", "firebrick")) +  
  theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5))   # Rotates x-axis labels

for (pr in unique(plasticitySSub$Prov)) {
  cur <- plasticitySSub[which(plasticitySSub$Prov == pr),]
  
  print(ggplot(plasticity, aes(x = GenotypicEst, y = stderrEst, colour = col)) + 
          ggtitle(paste(pr," \n Adj. R2:",round(summary(lm(plasticity$stderrEst~plasticity$GenotypicEst))$adj.r.squared,3))) +
          geom_point(size = 4, shape = 19, show.legend = F) +
          geom_smooth(method = 'lm', se = F, color = 'red') +
          labs(y = "Plasticity Score", x = 'Genotypic Estimates') + 
          geom_text(data = plasticity[c(1:10, (nrow(plasticity) - 10):nrow(plasticity)), ],
                    aes(label = Clone),  # Label with clone names
                    color = "black",  # Text color
                    vjust = -0.5,  # Adjust vertical position of labels
                    hjust = 0.5) +
          theme(axis.line = element_line(colour = "grey50"), axis.text.x = element_text(angle = 90, vjust = 0.5)))   # Rotates x-axis labels
}


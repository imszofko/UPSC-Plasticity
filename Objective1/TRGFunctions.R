#####These are functions for interacting with data from the Swarts Lab, especially designed to connect to the lab azure database (trg-srv.postgres.database.azure.com)
###Kelly Swarts, 2019

Sys.setenv(lang="EN")
library(Hmisc)
library(RColorBrewer)
library(bio3d)
library(RPostgres)
library(jsonlite)
library(R.utils)
library(stringr)
library(DBI)
library(rworldmap)
library(elevatr)
library(dplR)
library(raster)
library(tiler)
library(qrcode)
library(plotly)
library(breedR)
library(sommer)
library(sfc)
library(sf)
library(spatialEco)
library(mapboxer)
library(lme4)
library(vioplot)
library(hues)
#Download and/or load packages####
if(require("ncdf4")==F){
  install.packages("ncdf4")
  library(ncdf4)
}
if(require("ncdf4.helpers")==F){
  install.packages("ncdf4.helpers")
  library(ncdf4.helpers)
}
if(require("lubridate")==F){
  install.packages("lubridate")
  library(lubridate)
}

#source(paste(getwd(),"/src/functions/shinyFunctions.R",sep=""))


options(digits=6)

###Global variables for mapping 
col.status.grey <- c("#000000","#2F4F4F","#778899","#A9A9A9","#DCDCDC")
col.status <- c("#43613a","#9b4825","#dfa77d","#df7f32","#c2514c")
names(col.status) <- c("Live","Standing Dead","Fallen Dead","Uprooted","Stump")
names(col.status.grey) <- c("Live","Standing Dead","Fallen Dead","Uprooted","Stump")
col.species <- c("#5ea45e","#73c6e1","#c3d84a","#b1a4dc","#3b3f5a","#304a96","#497a78","#622f86","#b330a6","#5c3c73","#eb5d2b","#ae367f","#695271","#565130","#354e14","#9ee240","#7b9f54","#a53ad4","#ea8c59","#396344","#c2e580","#405752","#a38878","#626d28","#624243","#de6e90","#1f4f51","#e7957e","#59a5a4","#882d37","#d797a0","#e35a61","#637ce3","#b3c1e2","#922719","#dec6af","#c05f51","#e545d7","#967c24","#415f76","#acb36b","#98e499","#6b705b","#e6ec3b","#876943","#98af94","#9e53ab","#e4332a","#7a5d1e","#655fe5","#c891e3","#975484","#5eba92","#63e3d8","#e38b24","#e470c7","#854019","#718c1e","#5d30c5","#ead97a","#57ac2e","#5f42ae","#7d2f52","#e63a98","#928297","#c2ac31","#eac12d","#4f8caf","#d6a248","#b66d2f","#304623","#67e371","#b9e4d6","#9eb03e","#c170e7","#46b463","#4d4815","#be3361","#8372b6","#eca3d5","#bc521e","#4b8a62","#7b1d67","#29403b","#3c7f2d","#454837","#dcbed3","#663d20","#d1ac72","#235820","#c53a34","#609ade","#e92b60","#a66751","#d5e1aa","#3b5d92","#c17aac","#64e540","#58ebb0","#9b6069","#8e885d")
names(col.species) <- seq(1,101,1)
names.species <- c("Picea abies","Pinus sylvestris","Fagus sylvatica","Carpinus betulus","Fraxinus spp.","Ilex aquifolium","Larix spp.","Populus spp.","Betula spp.","Quercus spp.","Pseudotsuga menziesii","Abies spp.","Juniperus spp.","Cupressus spp.","Arabidopsis thaliana","Zea mays","Zea mays","Zea mays","Acer psudoplatanus","Sorbus acuparia","Petasites spp","Athyrium filix-femina","Abies alba","Acer spp.","Ips typographus","Abies concolor","Pinus ponderosa","Gossypium spp.","Cedrus spp.","Cannabis sativa","Salix spp.","Pinus mugo","Lemna spp.","Thanasimus spp.","Hylastes spp.","Dryocoetes autographus","Picea omorika","Picea pungens","Picea orientalis","Picea breweriana","Picea pungens","Picea schrenkiana","Picea bicolor","Picea asperata","Picea spp.","Picea engelmanii","Picea glehnii","Picea purpurea","Corylus avellana","Prunus spp.","UNK spp.","UNK Hardwood","Laburnum anagroides","Cretagus spp.","Rosa spp.")


####Columns for different db tables
plotCols <- c("plotid","collected","slope","aspect","altitude","centerpoint","ne","se","nw","sw","country","recorder","person_soilmoisture","person_dna","photolocation","comment","altname")

##Connect by odbc DEPRECATED
#' Get a connection through a DSN to a database. Check out the following for generating a functional DSN on mac: https://www.boriel.com/postgresql-odbc-connection-from-mac-os-x.html
#' @param DSN the DSN connection name. The default is "azure-trg" is none supplied. This will obviously only work if there is a DSN called azure-trg set up.
#' @return A functional connection or NULL if connection does not work
#' @export
getCon <- function(DSN) {
  con <- tryCatch({
    DBI::dbConnect(odbc::odbc(), DSN)
  }, error = function(err) {
    message(paste("Problem with connection. Please check DSN:",DSN))
    return(NULL)
  })
  return(con)
}

##Query trgdb
#' Return a query using a DSN connection string for the database of choice
#' @param DSN the DSN connection name. The default is "azure-trg" is none supplied. This will obviously only work if there is a DSN called azure-trg set up.
#' @param query A appropriate *sql language query for the database. Includes the ';' at the end automatically
#' @return A result (via dbFetch()) or NULL if connection does not work
#' @export
getDataFromDBOLD <- function(DSN="azure-trg",query) {
  con <- getCon(DSN)
  if (is.null(con)) return(NULL)
  res <- tryCatch({
    dbSendQuery(conn = con, statement = paste(query,";",sep=""))
  }, error = function(err) {
    message(paste("Problem with query:",query))
    return(NULL)
  })
  if (is.null(res)) return(res)
  mo <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  return(mo)
}

##Query trgdb
#' Return a query using a DSN connection string for the database of choice
#' @param DSN the DSN connection name. The default is "azure-trg" is none supplied. This will obviously only work if there is a DSN called azure-trg set up. MEANINGLESS NOW, ALWAYS CALLS AZURE-TRG
#' @param query A appropriate *sql language query for the database. Includes the ';' at the end automatically
#' @return A result (via dbFetch()) or NULL if connection does not work
#' @export
getDataFromDB <- function(DSN="azure=trg",query) {
  con <- RPostgres::dbConnect( RPostgres::Postgres(), user = 'picea@trg-srv',
                               password = 'piceaabies',
                               dbname = 'trgdb',
                               host = 'trg-srv.postgres.database.azure.com',
                               port = 5432,
                               sslmode = 'require' )
  if (is.null(con)) return(NULL)
  res <- tryCatch({
    RPostgres::dbSendQuery(conn = con, statement = paste(query,";",sep=""))
  }, error = function(err) {
    message(paste("Problem with query:",query))
    return(NULL)
  })
  if (is.null(res)) return(res)
  mo <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  return(mo)
}

#' Make a CSV file for core labels that can input into a label printer
#' @param plotid the plotid that you want to make labels for. Will pull from the database through 'getDataFromDB'
#' @param directory A direcrory to write to. Defaults to current directory
#' @return NULL
#' @export
CSVForCoreLabels <- function(plotid,directory="./") {
  res <- getDataFromDB(query = paste("Select * from core where plotid =",plotid," order by coreid;"))
  json <- paste("{ \"data\": \"",res$coreid,"\" }",sep="")
  tree <- as.numeric(substr(res$treeid,6,8))
  short <- paste(res$plotid,"-",tree,res$core,sep="")
  out <- data.frame("json"=json,"coreid"=res$coreid,"shortname"=short,"plot"=res$plotid,"tree"=tree,"core"=res$core)
  write.csv(out,file =str_replace_all(paste(directory,"/",str_pad(plotid,5,pad="0"),".csv",sep=""),"//","/"),quote = F,row.names = F)
}
##like for (plotid in 82:86) {CSVForCoreLabels(plotid=plotid,directory="/Users/kelly.swarts/Dropbox (VBC)/FieldData/QRcodes")}

makePlateMap <- function(plateids,table="dnaextraction",pdfFile="./plateMap.pdf",textsize=.75) {
  sam <- getDataFromDB(query = paste("Select * from",table,"where plateid in (",paste(plateids,collapse = ","),") order by plateid,position;"))
  sam$altname <- str_replace(sam$altname,pattern = "BLANK_",replacement = "BL")
  sam$altname <- str_replace(sam$altname,pattern = "-",replacement = "\n")
  pdf(pdfFile,width = (4.65*2),height = (3.1*2))
  par(mar=c(0,0,0,0.2),oma=c(0,0,0,0),mfrow=c(2,2))
  for (plate in plateids) {
    types=unique(sam[which(sam$plateid==plate),"tissue"])
    types <- types[order(types)]
    types <- types[-which(types=="BLANK")]
    plot <- paste(unique(sam$plotid[which(sam$plateid==plate)])[which(is.na(unique(sam$plotid[which(sam$plateid==plate)]))==F)],collapse = ",")
    for (cur in c("","BACKUP")) {
      plot(NULL,yaxt='n',xaxt='n',xlim=c(0,12.5),ylim=c(0,9.2),bty="n",xaxs="i",yaxs="i")
      rect(xleft = c(.5,2.5,4.5,6.5,8.5,10.5),ybottom = 0,xright = c(1.5,3.5,5.5,7.5,9.5,11.5),ytop = 8,col = "lightgrey")
      abline(h=c(0,1,2,3,4,5,6,7,8,8.5),v = c(0,.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5))
      rect(xleft = 0,ybottom = c(0,8,8),xright = c(.5,12.5,12.5),ytop = c(9.2,9.2,8.5),col = "white")
      title(main = paste(" PlateID ",plate," - Plot ",plot," (",paste(types,collapse=","),") ",cur,sep=""),line =-1.1,adj = 0,cex.main=.85)
      text(x=.4,y=c(7.5,6.5,5.5,4.5,3.5,2.5,1.5,.5),labels=c("A","B","C","D","E","F","G","H"),adj=1,font=2,cex=1.2)
      text(x=seq(1,12),y=8.25,labels=seq(1,12),font=2,cex=1.2)
      for (col in 1:12) {
        for (row in 1:8) {
          if (length(sam[which(sam$plateid==plate & sam$position==(((col-1)*8)+row)),"altname"])>0) {
            text(x=col,y=9-row-.5,labels=sam[which(sam$plateid==plate & sam$position==(((col-1)*8)+row)),"altname"],font=2,cex=textsize)
            print(paste(col,(9-row-.5),(((col-1)*8)+row),sam[which(sam$plateid==plate & sam$position==(((col-1)*8)+row)),"altname"]))
          }
        }
      }
    }
    if (which(plateids==plate)%%2==0) {
      for (i in 1:4) {plot(NULL,yaxt='n',xaxt='n',xlim=c(0,12.5),ylim=c(0,9.2),bty="n",xaxs="i",yaxs="i")}
    }
  }
  dev.off()
}
#makePlateMap(plateids = c(138,139),pdfFile = "/Users/kelly.swarts/Dropbox (VBC)/FieldData/PlateLabels/MultiplePlates_138_139")

###Functions to process tree-ring files
#' Turns tab delimited year/width file to rwl. Multiplies by 10 before turning into rwl
#' @param chronFile tab-delimited file with year in first column and ring-width in subsequent
#' @export
chronToRWL <- function(chronFile,seriesname="chron") {
  df <- read.table(chronFile,as.is=T)
  years <- df[,1]
  df <- as.matrix(df[,2:ncol(df)])
  colnames(df) <- seriesname
  df <- apply(df,2,function(x) {if (min(x)<0) x-(min(x)) else x})
  df <- as.data.frame(df*10)
  row.names(df) <- years
  write.rwl(df,fname = paste(tools::file_path_sans_ext(chronFile),".rwl",sep=""), prec = 0.01)
}

splitNameTRG <- function(string,repRows=1) {
  name <- str_pad(strsplit(string,"_")[[1]][1], 9, pad = "0")
  appendName <- strsplit(string,"_")[[1]][2]
  sampleName <- name
  #if (is.na(appendName)==F) sampleName <- paste(name,appendName,sep = "_")
  name <- strsplit(name, "")[[1]]
  return(as.data.frame(cbind(sample=rep(sampleName,repRows),plot=rep(paste(name[3],name[4],name[5],sep = ""),repRows),tree=rep(paste(name[7],name[8],sep = ""),repRows),core=rep(name[9],repRows),segment=rep(appendName,repRows))))
}

readTucsonPhen <- function(rwl,header=F) {
  df <- read.tucson(rwl,"tucson",header=header)
  osms <- list()
  osm <- data.frame(c(sample=character(0),plot=character(0),tree=character(0),core=character(0),width=numeric(0)),stringsAsFactors = F)
  for (core in 1:ncol(df)) {
    curr <- as.matrix(df[,core])
    names(curr) <- as.numeric(rownames(df))
    curr <- curr[which(is.na(curr)==F)]
    name <- paste(str_pad(str_sub(colnames(df)[core],1,3), 5, pad = "0"),str_pad(str_sub(colnames(df)[core],4,6), 3, pad = "0"),str_sub(colnames(df)[core],7,7),"_",str_sub(colnames(df)[core],8,8),sep="")
    currOSM <- data.frame(splitNameTRG(name,length(curr)),year=as.numeric(names(curr)),width=as.numeric(curr),stringsAsFactors=F)
    if (name%in%names(osms)) {
      osms[[name]] <- rbind(osms[[name]],currOSM)
      allD <- seq(min(osms[[name]]$year),max(osms[[name]]$year),1)
      missing <- allD[which(allD%in%osms[[name]]$year==F)]
      print(paste("Core",name,"is missing years",paste(missing,collapse = ", ")))
      osms[[name]] <- rbind(osms[[name]],data.frame(splitNameTRG(name,length(missing)),year=missing,width=rep(NA,length(missing)),stringsAsFactors=F))
      osms[[name]] <- osms[[name]][order(osms[[name]]$year,decreasing = F),]
    } else {osms[[name]] <- currOSM}
  }
  finalOsm <- data.frame(c(sample=character(0),plot=character(0),tree=character(0),core=character(0),year=numeric(0),width=numeric(0),AR1=numeric(0),AR2=numeric(0),AR3=numeric(0),AR4=numeric(0),AR5=numeric(0),AR6=numeric(0),AR7=numeric(0),AR8=numeric(0),AR9=numeric(0),AR10=numeric(0)),stringsAsFactors = F)
  for (curr in osms) {
    for (ar in 1:10) {
      curr[,paste("AR",ar,sep="")]=c(rep(NA,ar),curr$width)[1:nrow(curr)]
    }
    finalOsm <- rbind(finalOsm,curr)
  }
  return(finalOsm)
}

readTucsonPhenGeneric <- function(rwl,header=F) {
  df <- read.tucson(rwl,"tucson",header=header)
  osms <- list()
  pop <- str_split(string = fs::path_file(rwl),pattern = ".rwl")[[1]][1]
  osm <- data.frame(c(population=character(0),sample=character(0),core=character(0),width=numeric(0),detrendwidth=numeric(0),stringsAsFactors = F))
  #osm <- data.frame(c(population=character(0),sample=character(0),core=character(0),width=numeric(0),detrendLG=numeric(0),detrendSG=numeric(0)),stringsAsFactors = F)
  for (core in 1:ncol(df)) {
    curr <- as.matrix(df[,core])
    names(curr) <- as.numeric(rownames(df))
    curr <- curr[which(is.na(curr)==F)]
    AgeAtYear <- 1:length(curr)
    #detrendshift <- lm(sqrt(curr)~detrendArea(data.frame(AgeAtYear=1:(length(curr)+10)))[11:(length(curr)+10)]+detrendArea(data.frame(AgeAtYear)))
    #detrendshiftonly <- lm(sqrt(curr)~detrendArea(data.frame(AgeAtYear=1:(length(curr)+10)))[11:(length(curr)+10)])
    detrend <- lm(sqrt(curr)~AgeAtYear+detrendArea(data.frame(AgeAtYear)))
    name <- paste(pop,"_",colnames(df)[core],sep="")
    #currOSM <- data.frame(population=rep(pop,length(curr)),sample=rep(name,length(curr)),core=rep(colnames(df)[core],length(curr)),year=as.numeric(names(curr)),width=as.numeric(curr),detrendLG=coef(detrend)[1]+resid(detrend),detrendSG=coef(detrendshift)[1]+resid(detrendshift),detrendSGO=coef(detrendshiftonly)[1]+resid(detrendshiftonly),stringsAsFactors=F)
    currOSM <- data.frame(population=rep(pop,length(curr)),sample=rep(name,length(curr)),core=rep(colnames(df)[core],length(curr)),year=as.numeric(names(curr)),width=as.numeric(curr),detrendwidth=coef(detrend)[1]+resid(detrend),orderRings=AgeAtYear,stringsAsFactors=F)
    osms[[name]] <- currOSM
  }
  finalOsm <- osms[[1]][0,]
  for (curr in osms) {
    for (ar in 1:10) {
      curr[,paste("AR",ar,sep="")]=c(rep(NA,ar),curr$detrendwidth)[1:nrow(curr)]
    }
    finalOsm <- rbind(finalOsm,curr)
  }
  return(finalOsm)
}

getVarianceComponentsLMER <- function(mod) {
  varcomp <- as.data.frame(summary(mod)$varcor)[,c(1,4,5)]
  print(paste("total variance in model:",(sum(varcomp[,2]))))
  print(paste("total variance explained:",(sum(varcomp[1:(nrow(varcomp)-1),2])/sum(varcomp[,2]))))
  print(paste("prop explainable variance:",paste(varcomp[1:(nrow(varcomp)-1),1]," (",round(varcomp[1:(nrow(varcomp)-1),2]/sum(varcomp[1:(nrow(varcomp)-1),2]),3),")",sep="",collapse=", ")))
  return(varcomp)
}

significantDetrend <- function(lmmod,p) {
  summ <- summary(lmmod)$coef
  sig <- which(summ[2:nrow(summ),4]<p)
  if (is.null(nrow(sig))) return(lmmod$model[,1])
  else return(coef(lmmod)[1]+(lmmod$model[,sig]*coef(lmmod)[sig])+resid(lmmod))
  
}

rwlDirToOSM <- function(dirRWL,side="L",generic=F) {
  rwls <- list.files(dirRWL,pattern = "*.rwl",full.names = T)
  osm <- rwlsToOSM(paste(rwls,collapse=","),generic=generic)
  osm$side <- side
  print(paste("read in files from",paste(rwls,collapse = "\n")))
  return(osm)
}

rwlsToOSM <- function(rwls=params$rwls,generic=F) {
  rwls <- strsplit(rwls,",")[[1]]
  if (generic) {osm <- readTucsonPhenGeneric(rwls[1])
  } else {osm <- readTucsonPhen(rwls[1])}
  if (length(rwls)>1) {
    for (curr in 2:length(rwls)) {
      if (generic) {
        add <- readTucsonPhenGeneric(rwls[curr])
      } else {
        add <- readTucsonPhen(rwls[curr])
      }
      osm <- rbind(osm,add)
    }
  }
  i <- sapply(osm, is.factor)
  osm[i] <- lapply(osm[i], as.character)
  osm$coreid <- osm$sample
  osm$treeid <- substring(osm$coreid,1,8)
  return(osm)
}

###missing rings based on distance to center
###rings a numeric vector used to estimate the scaling parameter and pithDist is the distance to the pith
#helper function that is also used elsewhere
repgeo <- function(index,scaling) {return(sort(rgeom(index,scaling),decreasing = T))}
repexp <- function(index,scaling) {return(sort(rexp(index,scaling),decreasing = T))}
##Check scalings: plot(round(apply(matrix(unlist(lapply(rep(index,1000),scaling=.5,FUN = repgeo)),nrow = 1000,byrow = T),2,median)))
estMissingRingsFromPith <- function(rings,pithDist, treeid=NULL) {
  if (pithDist<=0) {
    printf("\nCHECK!!!! CANNOT ESTIMATE PITH FROM %s (returning 0): pithDist (mm) = %s, meanCore = %s",treeid,pithDist,mean(rings$width))
    return(0)
  }
  rings <- rings[which(rings$side%in%c("L","left")),c("year","width")]
  rings <- rings[order(rings$year),]
  #scaling <-  fitdistr(round(rings$width,0),"geometric")$estimate ##This doesn't make sense. We already know that the pith is far away
  scaling <- .5 #this is approximately the empirical mean for cores with inner piths
  index <- 0
  ests <- numeric(0)
  while((pithDist-sum(ests))>0) {
    index <- index+1
    medgeo <- round(apply(matrix(unlist(lapply(rep(index,1000),scaling=scaling,FUN = repgeo)),nrow = 1000,byrow = T),2,median))
    ests <- mean(rings$width)+((medgeo))*mean(rings$width[(nrow(rings)-2):nrow(rings)])
  }
  estR <- rbind(data.frame("year"=(min(rings$year)-index+1):(min(rings$year)-1),"width"=ests[2:(length(ests))]),rings[,c("year","width")])
  plot(estR$year,estR$width,type="l",main=paste(treeid," Est. years from pith by DBH as ",index-1,"years\nnegative geometric scaling term fit to core",sep=""))
  points(rings$year,rings$width,pch=19,col="red",cex=.5)
  legend("topleft",pch=19,cex=.5,col="red",legend = "Measured rings")
  #radius = sqrt(area/pi)
  printf("\nEstimate missing rings by DBH for %s: Nmissing = %s, pithDist (mm) = %s, meanCore = %s, scaling = %s",treeid,index-1,pithDist,mean(rings$width),scaling)
  
  return(index-1)
}

shinyWidthForPlot <- function(plots=NULL,shinyBaseDir,type="shortDist") {
  coremeasure <- c("uniqueid","coreid","segment","side","year","annualwidthmm","earlywoodmm","latewoodmm","ncells","bluechannel","break","firescar","frostring","lightring","bluering","sapwood","unkscar","iacrack","rxnwood","resinducttrauma","iadf","comment")
  widths <- list()
  for (p in list.dirs(path = paste(shinyBaseDir,"shinyMod",sep="/"),full.names = F)) {
    if (p=="") next
    if (is.null(plots) || as.numeric(p)%in%plots) {
      for (w in list.files(path = paste(shinyBaseDir,"shinyMod",p,sep="/"),pattern=type,full.names = F)) {
        cur <- read.table(paste(shinyBaseDir,"shinyMod",p,w,sep="/"),header=T,as.is=T)
        js <- read_json(list.files(paste(shinyBaseDir,"shinyMod",p,sep="/"),pattern=glob2rx(paste(str_sub(w,1,15),"*.json",sep="")),full.names = T))
        mo <- getDataFromDB(query=paste("select * from microscopyobject where objectid =",unique(str_sub(cur$name,13))))
        cur$ageatyear <- js$pith$est_rings_to_pith$dist[[1]]+(cur$year-min(cur$year))
        cur$innerpith <- js$pith$present
        cur$estimatedpith <- min(cur$year)-js$pith$est_rings_to_pith$dist[[1]]+1
        cur$firstmeasuredyear <- min(cur$year)
        cur$lastmeasuredyear <- max(cur$year)
        cur$cuttingdate <- js$cuttingdate
        if (is.null(js$pith$bark)) {cur$outerbark <- F
        } else {cur$outerbark <- js$pith$bark}
        cur$coreid <- mo$coreid
        cur$plot <-mo$plotid
        cur$tree <- as.numeric(str_sub(cur$name,6,8))
        cur$core <- str_sub(cur$name,8,9)
        cur$segment <- mo$segmentnum
        cur <- cur %>% group_by(side) %>% mutate(!!paste("ar",i,sep="") := lag(x =width,order_by = year)) %>% as.data.frame()
        cur <- cur[,c("coreid","plot", "tree", "core", "innerpith","estimatedpith","cuttingdate","firstmeasuredyear","lastmeasuredyear","outerbark","segment","side", "year", "width","ageatyear","ar1")]
        widths[[w]] <- cur
      }
    }
    
  }
  widths <- do.call(rbind,widths)
  return(widths)
}

dbToOSM <- function(plots=NULL,estMissingPith="dbh",coreFile=NULL,pithFile=NULL,excludeTrees=NULL,ar=NULL,ard=NULL,confidence=c(NULL,1:4)) {
  if (estMissingPith%in%c("normal","dbh")==F) {
    throw.Exception(this,"estMissingPith value needs to be either 'normal' or 'dbh'")
  }
  if (is.null(plots)) {osm <- getDataFromDB(DSN = "azure-trg",paste("SELECT m.coreid, substring(m.coreid,3,3)::int as plot, substring(m.coreid,7,2) as tree, substring(m.coreid,9,1) as core, m.segment, m.side,m.year, m.annualwidthmm as width, m.year-i.estimatedpith+1 as ageatyear, lag(m.annualwidthmm,1,NULL) over (order by m.coreid,m.year) as ar1,i.* FROM coremeasure m left join (select p.plotid,t.treeid,c.coreid as sampleid,c.samplingheightcm,c.cuttingdate,c.coreaspect,c.outerbark,c.dateconfidence,c.estimatedpith,c.firstmeasuredyear,c.lastmeasuredyear,c.innerpith,p.slope,p.aspect,p.altitude,t.collected,p.country,p.recorder,p.person_dna,p.altname,m.mappointid,m.mapper,m.sampletime,m.status,m.dbhcm,t.heightmeters,t.canopybaseheight,m.speciesid,m.compasssd,m.compasshd,m.compassh,m.compassdeg,m.compassaz,c.personid,mp.median_wgs84e,mp.median_wgs84n,mp.median_alt from plot p INNER JOIN tree t ON p.plotid = t.plotid RIGHT JOIN mappedobject m ON t.treeid = m.treeid LEFT JOIN mappoint mp on mp.mappointid=m.mappointid INNER JOIN core c ON t.treeid=c.treeid where m.ignore is not true) i on m.coreid=i.sampleid where i.dateconfidence in (",paste('NULL,',paste(confidence,collapse=","),sep=""),") order by m.coreid, m.year",sep=""))
  } else {osm <- getDataFromDB(DSN = "azure-trg",paste("SELECT m.coreid, substring(m.coreid,3,3)::int as plot, substring(m.coreid,7,2) as tree, substring(m.coreid,9,1) as core, m.segment, m.side, m.year, m.annualwidthmm as width, m.year-i.estimatedpith+1 as ageatyear, lag(m.annualwidthmm,1,NULL) over (order by m.coreid,m.year) as ar1,i.* FROM coremeasure m left join (select p.plotid,t.treeid,c.coreid as sampleid,c.samplingheightcm,c.cuttingdate,c.coreaspect,c.outerbark,c.dateconfidence,c.estimatedpith,c.firstmeasuredyear,c.lastmeasuredyear,c.innerpith,p.slope,p.aspect,p.altitude,t.collected,p.country,p.recorder,p.person_dna,p.altname,m.mappointid,m.mapper,m.sampletime,m.status,m.dbhcm,t.heightmeters,t.canopybaseheight,m.speciesid,m.compasssd,m.compasshd,m.compassh,m.compassdeg,m.compassaz,c.personid,mp.median_wgs84e,mp.median_wgs84n,mp.median_alt from plot p INNER JOIN tree t ON p.plotid = t.plotid RIGHT JOIN mappedobject m ON t.treeid = m.treeid LEFT JOIN mappoint mp on mp.mappointid=m.mappointid INNER JOIN core c ON t.treeid=c.treeid where m.ignore is not true) i on m.coreid=i.sampleid where substring(m.coreid,3,3)::int in (",paste(plots,collapse=","),") and i.dateconfidence in (",paste('NULL,',paste(confidence,collapse=","),sep=""),") order by m.coreid, m.year",sep="")) }
  return(expandOSM(osm,plots,estMissingPith,coreFile,pithFile,excludeTrees,ar,ard))
}

shinyToOSM <- function(plots=NULL,shinyBaseDir,measureType="shortDist",estMissingPith="dbh",coreFile=NULL,pithFile=NULL,excludeTrees=NULL,ar=NULL,ard=NULL) {
  if (estMissingPith%in%c("normal","dbh")==F) {
    throw.Exception(this,"estMissingPith value needs to be either 'normal' or 'dbh'")
  }
  if (is.null(plots)) {osm <- getDataFromDB(DSN = "azure-trg","select p.plotid,t.treeid,c.coreid as sampleid,c.samplingheightcm,c.coreaspect,c.dateconfidence,p.slope,p.aspect,p.altitude,t.collected,p.country,p.recorder,p.person_dna,p.altname,m.mappointid,m.mapper,m.sampletime,m.status,m.dbhcm,t.heightmeters,t.canopybaseheight,m.speciesid,m.compasssd,m.compasshd,m.compassh,m.compassdeg,m.compassaz,c.personid,mp.median_wgs84e,mp.median_wgs84n,mp.median_alt from plot p INNER JOIN tree t ON p.plotid = t.plotid RIGHT JOIN mappedobject m ON t.treeid = m.treeid LEFT JOIN mappoint mp on mp.mappointid=m.mappointid INNER JOIN core c ON t.treeid=c.treeid")
  } else {osm <- getDataFromDB(DSN = "azure-trg",paste("select p.plotid,t.treeid,c.coreid as sampleid,c.samplingheightcm,c.coreaspect,c.dateconfidence,p.slope,p.aspect,p.altitude,t.collected,p.country,p.recorder,p.person_dna,p.altname,m.mappointid,m.mapper,m.sampletime,m.status,m.dbhcm,t.heightmeters,t.canopybaseheight,m.speciesid,m.compasssd,m.compasshd,m.compassh,m.compassdeg,m.compassaz,c.personid,mp.median_wgs84e,mp.median_wgs84n,mp.median_alt from plot p INNER JOIN tree t ON p.plotid = t.plotid RIGHT JOIN mappedobject m ON t.treeid = m.treeid LEFT JOIN mappoint mp on mp.mappointid=m.mappointid INNER JOIN core c ON t.treeid=c.treeid where substring(c.coreid,3,3)::int in (",paste(plots,collapse=","),")",sep="")) }
  widths <- shinyWidthForPlot(plots = plots,shinyBaseDir = "/Users/kelly.swarts/Code/Rstudio/trg-shiny",type = measureType)
  osm <- cbind(widths,osm[match(widths$coreid,osm$sampleid),])
  osm <- osm[,c("coreid","plot","tree","core","segment","side","year","width", "ageatyear", "ar1","plotid","treeid","sampleid","samplingheightcm","cuttingdate","coreaspect","outerbark","dateconfidence","estimatedpith", "firstmeasuredyear", "lastmeasuredyear" ,"innerpith", "slope", "aspect","altitude","collected","country", "recorder","person_dna","altname", "mappointid","mapper","sampletime","status","dbhcm", "heightmeters","canopybaseheight","speciesid", "compasssd", "compasshd", "compassh","compassdeg","compassaz", "personid","median_wgs84e", "median_wgs84n","median_alt")]
  return(expandOSM(osm,plots,estMissingPith,coreFile,pithFile,excludeTrees,ar,ard))
}

expandOSM <- function(osm,plots=NULL,estMissingPith="dbh",coreFile=NULL,pithFile=NULL,excludeTrees=NULL,ar=NULL,ard=NULL) {
  if (!is.null(excludeTrees)) osm <- osm[which(osm$treeid%in%excludeTrees==F)]
  miss <- osm[which(is.na(osm$width)),]
  if (nrow(miss)>0) {
    printf("Missing ring widths for cores %s!!!! CHECK!!!! Will delete before proceeding.",paste(miss$coreid,collapse=", "))
  }
  osm <- osm[which(is.na(osm$width)==F),]
  osm$coreid <- gsub("[[:space:]]", "", osm$coreid)
  osm$size <- NA
  coreinfo <- getCoreInfoFromOSM(osm,filename=coreFile)
  if (length(which(is.na(osm$coreaspect)==T))>0) {
    printf("Missing core aspect for cores %s!!!! Will impute with coreid.",paste(unique(osm[which(is.na(osm$coreaspect)),"coreid"]),collapse=", "))
    osm[which(is.na(osm$coreaspect)),"coreaspect"] <- osm[which(is.na(osm$coreaspect)),"coreid"]
  }
  #impute missing first measured year by first present width if there are NAs or mismatches between firstmeasuredyear and dated widths
  if (length(which(coreinfo$firstmeasuredyear!=coreinfo$firstpresentyear))>0) {
    newfirst <- coreinfo[match(osm$coreid,coreinfo$coreid),c("firstpresentyear")]
    shift <- newfirst-osm$firstmeasuredyear
    print("Changed firstmeasured year for following based on early measured date:")
    print(unique(cbind(coreid=osm$coreid,firstmeasuredyear_old=osm$firstmeasuredyear,firstmeasuredyear_new=newfirst)[which(osm$firstmeasuredyear!=newfirst),]))
    osm$firstmeasuredyear <- newfirst
    osm$estimatedpith <- osm$estimatedpith+shift
  }
  
  for (tree in unique(osm$treeid)) {
    cur <- osm[which(osm$treeid == tree & osm$side%in%c("left","L")),]
    if (length(unique(cur$dbhcm))>1) throw.Exception(printf("Two DBH values found for tree %s! Check in DB!",tree))
    ncores <- length(unique(cur$coreid)) 
    if (ncores>3) printf("More than three cores found for tree %s! Check in DB!",tree)
    curdbh <- (unique(cur$dbhcm)*20)/ncores
    ss <- data.frame(year=min(cur$year):max(cur$year),size=rep(NA,length(min(cur$year):max(cur$year))))
    for (n in max(cur$year):min(cur$year)) {ss[which(ss$year==n),"size"] <- curdbh-sum(cur$width[which(cur$year %in% n:max(cur$year))])}
    osm[which(osm$treeid==tree),"size"] <- ss$size[match(osm[which(osm$treeid==tree),"year"],ss$year)]
  }
  osm$estpithtree <- NULL
  osm$ageatyeartree <- NULL
  osm$estpithcore <- osm$estimatedpith
  osm$ageatyearcore <- NULL
  ##Fill in estimated pith by dbh for those that are null. For the rest, take the estimated pith of the longer series
  uniqpiths <- unique(as.data.frame(osm %>% group_by(coreid,side) %>% reframe(treeid,coreid,n=dplyr::n(),estimatedpith,mindated=min(year),samplingheight=unique(samplingheightcm),coreaspect=unique(coreaspect))))
  if (is.null(pithFile)==F) pdf(pithFile,width = 7,height=5)
  for (tree in unique(osm$treeid)) {
    #cur <- uniqpiths[which(uniqpiths$treeid==tree & uniqpiths$side%in%c("L","left")),]
    cur <- uniqpiths %>% filter(treeid==tree) %>% group_by(coreid) %>% summarise(treeid = unique(treeid),n=max(n),estimatedpith=min(estimatedpith),mindated=min(mindated)) %>% as.data.frame()
    pithtree <- NA
    notna <- cur[which(is.na(cur$estimatedpith)==F),]
    if (nrow(cur[which(is.na(cur$estimatedpith)==F),])>0 ) { #& nrow(cur[which(is.na(cur$estimatedpith)==F),])!= nrow(cur)
      pithtree <- min(notna[which(notna$n==max(notna$n)),"estimatedpith"])
      if (pithtree<min(cur$mindated)) printf("\nEstimated pith date (tree) for %s based on longest series (early date breaks ties) set as %s",tree,pithtree)
      if (length(which(is.na(cur$estimatedpith)==T))>0) {
        for (curCore in cur[which(is.na(cur$estimatedpith)==T),"coreid"]) {
          if (pithtree>cur[which(cur$coreid==curCore),"mindated"]) {
            core <- osm[which(osm$coreid==curCore),]
            if (estMissingPith=="dbh") {
              miss <- estMissingRingsFromPith(rings = core,pithDist = (((unique(core$dbhcm)*10)/2)-sum(core$width)),treeid=tree)
              printf("\nOne core is dated but the minimum dated year is less than the estimated pith! Estimated pith date (tree and core) for %s based on DBH=%s from early date %s on core %s as %s (%s years)",unique(core$treeid),unique(core$dbhcm),min(core$year),unique(core$coreid),min(core$year)-miss,miss)
            } else {
              miss <- round(rnorm(1,20,4),0)
              printf("\nOne core is dated but the minimum dated year is less than the estimated pith! Estimated pith date (tree and core) for %s based on normal distribution(mean=20,sd) from early date %s on core %s as %s (%s years)",unique(core$treeid),min(core$year),unique(core$coreid),min(core$year)-miss,miss)
            }
            pithtree <- min(core$year)-miss
            osm[which(osm$coreid == curCore),"estpithcore"] <- pithtree
          } else {
            osm[which(osm$coreid == curCore),"estpithcore"] <- pithtree
            printf("\nEstimated pith date (core) for missing core %s based on longest series %s set as %s",curCore,notna[which(notna$n==max(notna$n)),"coreid"],pithtree)
          }
        }
      }
    } else { #if (nrow(cur[which(is.na(cur$estimatedpith)==F),])!= nrow(cur)) {
      core <- osm[which(osm$coreid==cur[which(cur$n==max(cur$n)),"coreid"]),]
      if (estMissingPith=="dbh") {
        miss <- estMissingRingsFromPith(rings = core,pithDist = (((unique(core$dbhcm)*10)/2)-sum(core$width)),treeid=tree)
        printf("\nEstimated pith date (tree and core) for %s based on DBH=%s from early date %s on core %s as %s (%s years)",unique(core$treeid),unique(core$dbhcm),min(core$year),unique(core$coreid),min(core$year)-miss,miss)
      } else {
        miss <- round(rnorm(1,20,4),0)
        printf("\nEstimated pith date (tree and core) for %s based on normal distribution(mean=20,sd) from early date %s on core %s as %s (%s years)",unique(core$treeid),min(core$year),unique(core$coreid),min(core$year)-miss,miss)
      }
      pithtree <- min(core$year)-miss
      osm[which(osm$treeid == tree),"estpithcore"] <- pithtree
    }
    osm[which(osm$treeid == tree),"estpithtree"] <- pithtree
  }
  if (is.null(pithFile)==F) dev.off()
  osm$coreside <- paste(osm$coreid,osm$side,sep="_")
  osm$ageatyeartree <- osm$year-osm$estpithtree
  osm$ageatyearcore <- osm$year-osm$estpithcore
  geolookup <- data.frame(year=0:max(max(osm$ageatyearcore),max(osm$ageatyeartree)),val=round(apply(matrix(unlist(lapply(rep(max(max(osm$ageatyearcore),max(osm$ageatyeartree))+1,1000),scaling=.5,FUN = repgeo)),nrow = 1000,byrow = T),2,median)))
  osm$ageatyearcoregeo <- geolookup$val[match(osm$ageatyearcore,geolookup$year)]
  osm$ageatyeartreegeo <- geolookup$val[match(osm$ageatyeartree,geolookup$year)]
  explookup <- data.frame(year=0:max(max(osm$ageatyearcore),max(osm$ageatyeartree)),val=apply(matrix(unlist(lapply(rep(max(max(osm$ageatyearcore),max(osm$ageatyeartree))+1,1000),scaling=.5,FUN = repexp)),nrow = 1000,byrow = T),2,median))
  osm$ageatyearcoreexp <- explookup$val[match(osm$ageatyearcore,explookup$year)]
  osm$ageatyeartreeexp <- explookup$val[match(osm$ageatyeartree,explookup$year)]
  osm$detrendCD <- NULL
  osm$detrendCoreExpLin <- NULL
  osm$detrendCoreGeoLin <- NULL
  osm$detrendCoreExp <- NULL
  osm$detrendCoreGeo <- NULL
  osm$coremean <- NULL
  ##detrend for crossdate and mean width
  for (core in unique(osm$coreside)) {
    curr <- osm[which(osm$coreside==core),]
    curr$detrendCoreExpLin <- lm(sqrt(curr$width)~explookup[(curr$ageatyearcore-min(curr$ageatyearcore)+1),"val"]+curr$year)$coefficients[1]+resid(lm(sqrt(curr$width)~explookup[(curr$ageatyearcore-min(curr$ageatyearcore)+1),"val"]+curr$year))
    curr$detrendCoreGeoLin <- lm(sqrt(curr$width)~geolookup[(curr$ageatyearcore-min(curr$ageatyearcore)+1),"val"]+curr$year)$coefficients[1]+resid(lm(sqrt(curr$width)~geolookup[(curr$ageatyearcore-min(curr$ageatyearcore)+1),"val"]+curr$year))
    curr$detrendCoreExp <- lm(sqrt(curr$width)~explookup[(curr$ageatyearcore-min(curr$ageatyearcore)+1),"val"])$coefficients[1]+resid(lm(sqrt(curr$width)~explookup[(curr$ageatyearcore-min(curr$ageatyearcore)+1),"val"]))
    curr$detrendCoreGeo <- lm(sqrt(curr$width)~geolookup[(curr$ageatyearcore-min(curr$ageatyearcore)+1),"val"])$coefficients[1]+resid(lm(sqrt(curr$width)~geolookup[(curr$ageatyearcore-min(curr$ageatyearcore)+1),"val"]))
    if (unique(curr$side)%in%c("left","L")) {
      detrendCDL <- resid(lm(sqrt(curr[which(curr$side%in%c("L","left")),]$width)~smooth.spline(x = curr[which(curr$side%in%c("L","left")),]$year, y = sqrt(curr[which(curr$side%in%c("L","left")),]$width), spar = 0.5)$y))
      curr[which(curr$side%in%c("L","left")),"detrendCD"] <- detrendCDL
    } else if (unique(curr$side)%in%c("right","R")) {
      detrendCDR <- resid(lm(sqrt(curr[which(curr$side%in%c("R","right")),]$width)~smooth.spline(x = curr[which(curr$side%in%c("R","right")),]$year, y = sqrt(curr[which(curr$side%in%c("R","right")),]$width), spar = 0.5)$y))
      curr[which(curr$side%in%c("R","right")),"detrendCD"] <- detrendCDR
    }
    #plot(curr$year,sqrt(curr$width),type="l")
    #lines(curr$year,curr$detrendCD+1,col="red")
    osm[which(osm$coreside==core),"detrendCD"] <- curr$detrendCD
    osm[which(osm$coreside==core),"detrendCoreExpLin"] <- curr$detrendCoreExpLin
    osm[which(osm$coreside==core),"detrendCoreGeoLin"] <- curr$detrendCoreGeoLin
    osm[which(osm$coreside==core),"detrendCoreExp"] <- curr$detrendCoreExp
    osm[which(osm$coreside==core),"detrendCoreGeo"] <- curr$detrendCoreGeo
    osm[which(osm$coreside==core),"coremean"] <- mean(curr$width)
  }
  if (is.null(ard)==F) {
    osm <- osm %>% group_by(coreside) %>% mutate(ard1 = lag(x = detrendCD,order_by = year)) %>% as.data.frame()
    for (i in 2:ard) {
      osm <- osm %>% group_by(coreside) %>% mutate(!!paste("ard",i,sep="") := lag(x = get(paste("ard",i-1,sep="")),order_by = year)) %>% as.data.frame()
    }
  }
  if (is.null(ar)==F) {
    osm <- osm %>% group_by(coreside) %>% mutate(ar1 = lag(x = sqrt(width),order_by = year)) %>% as.data.frame()
    if (ar>1) {
      for (i in 2:ar) {
        osm <- osm %>% group_by(coreside) %>% mutate(!!paste("ar",i,sep="") := lag(x = get(paste("ar",i-1,sep="")),order_by = year)) %>% as.data.frame()
      }
    }
  }
  return(osm)
}

getCoreInfoFromOSM <- function(osm,filename=NULL) {
  if ("estpithcore"%in%colnames(osm)) {
    coreinfo <- as.data.frame(osm %>% group_by(coreid) %>% reframe(Nrings=n(),coreaspect=unique(coreaspect),samplingheightcm=unique(samplingheightcm),Nsegments=unique(segment),confidence=unique(dateconfidence),samplingheightcm=unique(samplingheightcm),coreaspect=unique(coreaspect),"estpith"=unique(estimatedpith),"firstmeasuredyear"=unique(firstmeasuredyear),"firstpresentyear"=min(year),"innerpith"=unique(innerpith),"estpithcore"=unique(estpithcore),"estpithtree"=unique(estpithtree),min(ageatyeartree),min(ageatyearcore),"nonunique"=if_else((length(unique(paste(year,side,sep="_")))==dplyr::n()),F,T),"maxjumpsqrt"=max(diff(sqrt(width))),"5sdjump"=5*sd(diff(sqrt(width))),"gt5sdjump"=if_else(max(diff(sqrt(width)))>5*sd(diff(sqrt(width))),T,F),"maxwidthsqrt"=max(sqrt(width)),"10sdwidth"=10*sd(sqrt(width)),"gt10sd"=if_else(max(sqrt(width))>10*sd(sqrt(width)),T,F)))
  } else {
    coreinfo <- as.data.frame(osm %>% group_by(coreid) %>% reframe(Nrings=n(),coreaspect=unique(coreaspect),samplingheightcm=unique(samplingheightcm),Nsegments=unique(segment),confidence=unique(dateconfidence),samplingheightcm=unique(samplingheightcm),coreaspect=unique(coreaspect),"estpith"=unique(estimatedpith),"firstmeasuredyear"=unique(firstmeasuredyear),"firstpresentyear"=min(year),"innerpith"=unique(innerpith),"nonunique"=if_else((length(unique(paste(year,side,sep="_")))==dplyr::n()),F,T),"maxjumpsqrt"=max(diff(sqrt(width))),"5sdjump"=5*sd(diff(sqrt(width))),"gt5sdjump"=if_else(max(diff(sqrt(width)))>5*sd(diff(sqrt(width))),T,F),"maxwidthsqrt"=max(sqrt(width)),"10sdwidth"=10*sd(sqrt(width)),"gt10sd"=if_else(max(sqrt(width))>10*sd(sqrt(width)),T,F)))
  }
  print(paste("Check cores containing NA in firstmeasuredyear:",paste(coreinfo$coreid[which(is.na(coreinfo[,"firstmeasuredyear"]))],collapse=",")))
  print(paste("Check cores containing ages less than 0:",paste(unique(osm[which(osm$ageatyear<0),c("coreid")]),collapse=",")))
  print(paste("Check cores containing firstpresentyear != firstmeasuredyear:",paste(coreinfo[which(coreinfo$firstpresentyear!=coreinfo$firstmeasuredyear),"coreid"],collapse=",")))
  print(paste("Check cores containing jumps greater than 5 std dev:",paste(coreinfo$coreid[coreinfo$gt5sd],collapse=",")))
  print(paste("Check cores containing rings greater than 10 std dev:",paste(coreinfo$coreid[coreinfo$gt10sd],collapse=",")))
  print(paste("Check cores with non-unique year/side values:",paste(coreinfo$coreid[coreinfo$nonunique],collapse=",")))
  print(paste("Missing core aspect:",paste(coreinfo$coreid[which(is.na(coreinfo$coreaspect))],collapse=",")))
  print(paste("Missing core height:",paste(coreinfo$coreid[which(is.na(coreinfo$samplingheightcm))],collapse=",")))
  if (is.null(filename)==F) write.table(coreinfo,filename,quote = F,row.names = F,col.names = T)
  return(coreinfo)
}

dbToMO <- function(plots=NULL) {
  if (is.null(plots)) mo <- getDataFromDB(DSN="azure-trg","select m.plotid,t.treeid,cd.ncoresmeasured,p.slope,p.aspect,t.collected,p.country,p.recorder,p.person_dna,p.altname,m.mappointid,m.mapper,m.sampletime,m.status,m.dbhcm,m.speciesid,m.compasssd,m.compasshd,m.compassh,m.compassdeg,m.compassaz,cd.ncoresmeasured,mp.median_wgs84e center_long,mp.median_wgs84n center_lat,mp.median_alt center_alt from tree t RIGHT JOIN mappedobject m ON t.treeid = m.treeid left join (select substring(c.treeid,1,5)::int plotid,c.treeid,count(cm.coreid) ncoresmeasured from core c full outer join (select distinct(coreid) coreid from coremeasure) cm on c.coreid=cm.coreid group by c.treeid) cd on m.treeid=cd.treeid left join plot p on p.plotid = m.plotid LEFT JOIN mappoint mp on mp.mappointid=p.centerpoint order by m.plotid,m.treeid")
  else mo <- getDataFromDB(DSN = "azure-trg",paste("select m.plotid,t.treeid,cd.ncoresmeasured,p.slope,p.aspect,t.collected,p.country,p.recorder,p.person_dna,p.altname,m.mappointid,m.mapper,m.sampletime,m.status,m.dbhcm,m.speciesid,m.compasssd,m.compasshd,m.compassh,m.compassdeg,m.compassaz,cd.ncoresmeasured,mp.median_wgs84e center_long,mp.median_wgs84n center_lat,mp.median_alt center_alt from tree t RIGHT JOIN mappedobject m ON t.treeid = m.treeid left join (select substring(c.treeid,1,5)::int plotid,c.treeid,count(cm.coreid) ncoresmeasured from core c full outer join (select distinct(coreid) coreid from coremeasure) cm on c.coreid=cm.coreid group by c.treeid) cd on m.treeid=cd.treeid left join plot p on p.plotid = m.plotid LEFT JOIN mappoint mp on mp.mappointid=p.centerpoint where m.plotid in (",paste(plots,collapse=","),") order by m.plotid,m.treeid",sep=""))
  if (nrow(mo)==0) return(NULL)
  mo <- consensusCoordinates(mo)
  mo$mo$altitude <- mo$mo$center_alt+mo$mo$z
  mo$mo$latitude <- mo$mo$y/100000 + mo$mo$center_lat
  mo$mo$longitude <- mo$mo$x/100000 + mo$mo$center_long
  return(mo$mo)
}

rwlToDB <- function(rwlDir="/Volumes/swarts/lab/ImageProcessingPipeline/RWtoDB",outDir="/Volumes/swarts/lab/swartsdb/ToBatchUpload/",updateCoreMeasure,updateCore,averageYears) {
  osm <- rwlDirToOSM(dirRWL = rwlDir,side="L")
  if (file.exists(file.path(rwlDir,"right"))) osm <- rbind(osm,rwlDirToOSM(dirRWL = file.path(rwlDir,"right"),side="R"))
  str(osm)
  names(osm)[which(names(osm)=="width")] <- "annualwidthmm"
  print(paste("RWLs from plots",paste(unique(osm$plot),collapse=",")))
  print(osm[which(osm$annualwidthmm==0),])
  hist(osm$annualwidthmm)
  print(osm[which(osm$annualwidthmm>10),c("sample","year","annualwidthmm")])
  print(osm[which(osm$annualwidthmm<.1),c("sample","year","annualwidthmm")])
  cm <- NULL
  if (file.exists(updateCoreMeasure)) {
    cm <- read.table(updateCoreMeasure,as.is=T,header=T,sep = "\t",na.strings = "NA")
    cm <- cm[which(!is.na(cm$year)),]
    cm <- cm[,which(colnames(cm)!="uniqueid")]
    ##put together core table
    cmdef <- as.data.frame(cbind(coreid=osm$sample,segment=as.numeric(osm$segment),side=osm$side,year=osm$year,annualwidthmm=osm$annualwidthmm,rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm)),rep(NA,nrow(osm))))
    colnames(cmdef) <- colnames(cm)
    if (file.exists(averageYears)) {
      avgy <- read.table(averageYears,as.is=T,header=T,sep = "\t",na.strings = "NA")
      avgy$coreid <- str_pad(avgy$coreid,9,"left","0")
      for (i in 1:nrow(avgy)) {
        yrs <- avgy$firstyear[i]:avgy$lastyear[i]
        cur <- data.frame(matrix(NA,nrow=length(yrs),ncol=ncol(cmdef)))
        colnames(cur) <- colnames(cmdef)
        cur$coreid <- rep(avgy$coreid[i],length(yrs))
        cur$segment <- rep(avgy$segment[i],length(yrs))
        cur$side <- rep(avgy$side[i],length(yrs))
        cur$year <- yrs
        cur$annualwidthmm <- rep(avgy$distmm[i]/length(yrs),length(yrs))
        cur$comment <- rep(paste("averagedUndatable_",avgy$firstyear[i],"-",avgy$lastyear[i],sep=""),length(yrs))
        reps <- as.data.frame(rbind(cmdef,cur) %>% group_by(coreid,year,side) %>% summarise(coreid,year,segment,side,n=dplyr::n()) %>% filter(n>1))
        overwrite <- which(cmdef$coreid%in%reps$coreid & cmdef$year%in%reps$year & cmdef$segment%in%reps$segment & cmdef$side%in%reps$side)
        cmdef[overwrite,]
        cmdef <- cmdef[which(1:nrow(cmdef)%in%overwrite==F),]  ##DELETE YEARS THAT EXISTS
        cmdef <- rbind(cmdef,cur)
      }
    }
  }
  core <- NULL
  if (file.exists(updateCore)) {
    core <- read.table(updateCore,as.is=T,header=T,sep = "\t",na.strings = "NA")
  }
  if (is.null(core) || is.null(cm)) return(NULL)
  matchupdate <- match(paste(cm$coreid,cm$year,sep=""),paste(osm$coreid,osm$year,sep=""))
  sink(paste(rwlDir,"UpdateIssues.txt",sep="/"))
  print("###############################Missing cores remaining:")
  for (plot in unique(osm$plot)) {
    cur <- osm[which(osm$plot==plot),]
    if (length(as.numeric(unique(cur$tree)))==length(1:max(as.numeric(unique(cur$tree))))) print(paste("Plot",as.numeric(plot),"all present!"))
    else print(paste("Missing cores for plot",as.numeric(plot),":",paste((1:max(as.numeric(unique(cur$tree))))[which((1:max(as.numeric(unique(cur$tree)))%in%as.numeric(unique(cur$tree)))==F)],collapse=",")))
    corespres <- as.data.frame(cur[,c("treeid","coreid","year")] %>% group_by(treeid) %>% summarise(n_distinct(coreid)))
    if (length(which(corespres$`n_distinct(coreid)`<2))>0) print(paste("Missing cores from treeids:",paste(corespres[which(corespres$`n_distinct(coreid)`<2),"treeid"],collapse=",")))
  }
  dups <- as.data.frame(osm %>% group_by(coreid,year) %>% summarise(n()))
  print("###########################Duplicated core_year combinations read in from rwl")
  print(osm[which(paste(osm$coreid,osm$year,sep="_")%in%paste(dups[which(dups$`n()`>1),"coreid"],dups[which(dups$`n()`>1),"year"],sep="_")),])
  print("###############################Problems with matching coreMeasure")
  print(paste("Couldn't find coreid_year in coremeasure that matches update coreid_year for following cores:",paste(unique(cm[which(is.na(matchupdate)),"coreid"]),collapse=",")))
  for (cur in unique(cm[which(is.na(matchupdate)),"coreid"])) {
    print(paste("########",cur,"########"))
    print(paste("Years measured rom rwls: ",min(osm[which(osm$coreid==cur),"year"]),"-",max(osm[which(osm$coreid==cur),"year"])," (",length(osm[which(osm$coreid==cur),"year"])," years)",sep=""))
    print("Rows from update without a width from rwls:")
    print(cm[which(is.na(matchupdate)),][which(cm[which(is.na(matchupdate)),"coreid"]==cur),])
  }
  ##CHeck to see if there are replicate core/years
  if (nrow(cmdef)>1) {
    reps <- as.data.frame(cmdef %>% group_by(coreid,year,side) %>% summarise(coreid,year,side,n=dplyr::n()))
    print(paste("Cores with duplicate coreid/year/side combinations",paste(unique(reps[which(reps$n>1),"coreid"]),collapse=","),sep=":"))
    matchupdate <- match(paste(cm$coreid,cm$year,cm$side,sep=""),paste(cmdef$coreid,cmdef$year,cmdef$side,sep=""))
    ##Check to make sure all coremeasure annos belong to a year
    print("Coremeasure annotations that don't have a corresponding measured ring")
    cm[which(is.na(matchupdate)),]
    if (length(which(is.na(matchupdate)))>0) {
      cmdef <- rbind(cmdef,cm[which(cm$coreid==cm$coreid[which(is.na(matchupdate))] & cm$year==cm$year[which(is.na(matchupdate))]),])
      matchupdate <- match(paste(cm$coreid,cm$year,sep=""),paste(cmdef$coreid,cmdef$year,sep=""))
    }
    if (length(matchupdate)==nrow(cm)) {
      for (i in as.numeric(4+which(apply(is.na(cm[5:ncol(cm)]),2,all)==F))) {
        cmdef[matchupdate[which(is.na(cm[,i])==F)],i] <- cm[which(is.na(cm[,i])==F),i]
      }
    } else {
      print("PROBLEM WITH MATCHING CORE MEASURE!!!!!!!!")
      return(NULL)
    }
    cmdef$segment <- as.numeric(cmdef$segment)
    cmdef$year <- as.numeric(cmdef$year)
    cmdef$annualwidthmm <- as.numeric(cmdef$annualwidthmm)
  }
  sink()
  #for (cur in c("break.","firescar","frostring","lightring","unkscar","iacrack","rxnwood","resinducttrauma","iadf")) {cmdef[,cur] <- as.boolean(cmdef[,cur])}
  ##make core update statements
  core$todb <- rep(NULL,nrow(core))
  for (x in core$coreid) {core$todb[which(core$coreid==x)] <- paste("Update core set cuttingdate =",as.numeric(core[which(core$coreid==x),"cuttingdate"]))}
  for (num in c("lastmeasuredyear","firstmeasuredyear","estimatedpith","dateconfidence","persondate")) {
    for (x in core$coreid) {
      if (is.na(core[which(core$coreid==x),num])) core$todb[which(core$coreid==x)] <- paste(core$todb[which(core$coreid==x)],", ",num, " = NULL",sep="")
      else core$todb[which(core$coreid==x)] <- paste(core$todb[which(core$coreid==x)],", ",num," = ",as.numeric(core[which(core$coreid==x),num]),sep="")
    }
  }
  for (bool in c("outerbark","innerpith")) {
    for (x in core$coreid) {
      if ((core[which(core$coreid==x),bool])=="") core$todb[which(core$coreid==x)] <- paste(core$todb[which(core$coreid==x)],", ",bool, " = NULL",sep="")
      else core$todb[which(core$coreid==x)] <- paste(core$todb[which(core$coreid==x)],", ",bool," = ",str_to_lower(core[which(core$coreid==x),bool]),sep="")
    }
  }
  for (x in core$coreid) {
    if ((core[which(core$coreid==x),"comment"])!="") core$todb[which(core$coreid==x)] <- paste(core$todb[which(core$coreid==x)],", comment =  CONCAT('",core[which(core$coreid==x),"comment"],";',(select comment from core where coreid = '",x,"'))",sep="")
  }
  for (x in core$coreid) {
    core$todb[which(core$coreid==x)] <- paste(core$todb[which(core$coreid==x)]," where coreid = '",x,"';",sep="")
  }
  write.table(core[,"todb"],paste(outDir,"/UpdateCoreToDB_",as.Date(Sys.Date(),format = "%y/%m/%d"),".txt",sep=""),col.names = F,row.names = F,sep="\t",quote = F)
  if (nrow(cmdef)>1) write.table(cmdef,paste(outDir,"/CoreMeasureToDB_",as.Date(Sys.Date(),format = "%y/%m/%d"),".txt",sep=""),col.names = T,row.names = F,sep="\t")
  return(osm)
}

imputeMissingCoreAspect <- function(osm) {
  osm$impCoreAspect <- osm$coreaspect
  osm[which(is.na(osm$impCoreAspect)),"impCoreAspect"] <- osm[which(is.na(osm$impCoreAspect)),"coreid"]
  osm$coreaspectF <- as.factor(osm$impCoreAspect)
  lma <- lmer(sqrt(width)~(1|coreaspectF)+as.factor(year)+ageatyearcoregeo,data=osm)
  eff <- ranef(lma)$coreaspectF$'(Intercept)'
  names(eff) <- rownames(ranef(lma)$coreaspectF)
  barplot(eff)
  
  aspect <- model.matrix( ~ coreaspectF - 1, data=osm)
  pc <- prcomp(aspect,center = TRUE,scale. = TRUE)
  summary(pc)
  plot(pc$center)
  plot(pc$rotation[,1],pc$rotation[,2])
  biplot(pc,)
  autoplot(pc, data = aspect, label=F, loadings = TRUE)
}

addPithToOSM <- function(osm,pithInput=params$pith) {
  if (is.null(pithInput)) {
    print("No pith values to add")
    return(osm)
  }
  print("Adding estimated pith values")
  pfiles <- strsplit(pithInput,",")[[1]]
  piths <- read.csv(pfiles[1],sep = ",",header=T,as.is = T)
  if (length(pfiles)>1) {
    for (curr in 2:length(piths)) {
      piths <- rbind(piths,read.csv(pfiles[curr],sep = ",",header=T,as.is = T))
    }
  }
  if (nrow(piths)<1) {
    print("No pith values to add")
    return(osm)
  }
  piths$core <- str_pad(unlist(lapply(strsplit(piths$Sample,"_"), `[[`, 1)), 9, pad = "0")
  piths$tree <- str_sub(piths$core,1,8)
  subCores <- aggregate(piths$InnerYear,list(tree=piths$tree),min)
  piths <- piths[which(piths$tree%in%subCores$tree & piths$InnerYear%in%subCores$x),]
  uni <- unique(piths[,c("core","tree","EstimatedPith")])
  osm$FirstYear <- uni[match(x = osm$treeid,uni$tree,nomatch = NA),"EstimatedPith"]+1
  return(osm)
}

addDetrendToOSM <- function(osm) {
  ####Could try by taking neg exp from pith date. If NA, make linear decay
  osm$negExp <- numeric(nrow(osm))
  osm$linear <- numeric(nrow(osm))
  osm$widthLog <- osm$width
  osm$widthLog[which(osm$widthLog==0)] <- .Machine$double.xmin
  osm$widthLog <- log(osm$widthLog)
  osm$FirstYearImp <- numeric(nrow(osm))
  osm$AgeAtYear <- numeric(nrow(osm))
  osm$pithDetrend <- rep(NA,nrow(osm))
  osm$geomIndex <- rep(NA,nrow(osm))
  pithDate <- F
  if ("FirstYear"%in%names(osm)) {
    osm$pithDetrend <- numeric(nrow(osm))
    pithDate <- T
  }
  for (samp in levels(factor(osm$sample))) {
    curr <- which(osm$sample%in%samp)
    FirstYearDate <- NA
    if (pithDate) {
      FirstYearDate <- unique(osm$FirstYear[curr])
      if (is.na(FirstYearDate)==F & min(osm$year[curr])<FirstYearDate) {
        printf("\n%s has min date less than estimated pith! FIX",samp)
        return(osm)
      }
    }
    if (is.na(FirstYearDate)) {
      tree <- which(osm$tree%in%osm$tree[which(osm$sample%in%samp)])
      impDate <- unique(osm$FirstYearImp[tree])
      if (length(impDate)>1) {
        FirstYearDate <- impDate[which(impDate>0)]
        printf("\nAlready estimated first year for tree %s as %s",unique(osm$tree[which(osm$sample%in%samp)]),FirstYearDate)
      } else if ("dbhcm"%in%names(osm) & is.na(osm$dbhcm[curr][1])==F) {
        core <- aggregate(osm$year[tree],list(osm$sample[tree]),min)
        core <- core[which(core[,2]==min(core[,2])),1]
        est <- which(osm$sample%in%core & is.na(osm$width)==F)
        ##Based on neg exp decay*mean(longest core)
        decay <- mean(osm$width[est])*exp(1/1:1000)
        missing <- (mean(osm$dbhcm[tree]/2)*10)-(sum(osm$width[est]))
        for (i in 1:length(decay)) {
          if (sum(decay[1:i]) > missing) {
            FirstYearDate <- min(osm$year[est])-i
            break
          }
        }
        printf("\nEstimated pith date for %s based on DBH=%s from early date %s as %s",samp,mean(osm$dbhcm[est]),min(osm$year[est]),FirstYearDate)
      } else {
        FirstYearDate <- min(osm$year[tree])-round(length(unique(osm$year[tree]))/2)
        printf("\nEstimated first year for %s as .5X rings: %s (Earliest observed is %s)",samp,FirstYearDate,min(osm$year[curr]))
      }
      osm$FirstYearImp[curr] <- FirstYearDate
    }
    osm$AgeAtYear[curr] <- osm$year[curr]-FirstYearDate
    #purely neg exp decay based on observed dates
    decay <- exp(1/(1:(max(osm$year[curr])-min(osm$year[curr])+1)))
    decay <- (decay/mean(decay))-1
    names(decay) <- seq(min(osm$year[curr]),max(osm$year[curr]),1)
    osm$negExp[curr] <- decay[match(osm$year[curr],names(decay))]
    #purely linear decay based on observed dates
    decayLin <- ((max(osm$year[curr])-min(osm$year[curr])+1):1)/(max(osm$year[curr])-min(osm$year[curr])+1)
    decayLin <- (decayLin/mean(decayLin))-1
    names(decayLin) <- seq(min(osm$year[curr]),max(osm$year[curr]),1)
    osm[curr,"linear"] <- decayLin[match(osm$year[curr],names(decayLin))]
    if (is.na(FirstYearDate)) return(osm)
    #use estimate pith dates as defined above
    decayP <- exp(1/(1:(max(osm$year[curr])-FirstYearDate+1)))
    (decayP/mean(decayP))-1
    names(decayP) <- seq(FirstYearDate,max(osm$year[curr]),1)
    osm$pithDetrend[curr] <- decayP[match(osm$year[curr],names(decayP))]
    #Ignore above and do the pith detrend based on geometric decay
    osm$pithDetrend <- detrendArea(osm,1)
    osm$geomIndex[curr] <- resid(lm(sqrt(width)~pithDetrend,data = osm[curr,]))
  }
  return(osm)
}

getBlockDiagDetrendingMatrix <- function(osm,type,plotSubset=NULL, outdir=params$outDir,datasetname=params$datasetName) {
  if (!type%in%c("negExp","linear","pithDetrend","ageatyearcoregeo")) {
    print("Type can be one of three options: negExp, linear, PithDetrend or ageatyearcoregeo")
    return(NULL)
  }
  if (!type%in%colnames(osm)) {
    printf("Type entered not found in colnames: %s",type)
    return(NULL)
  }
  geo <- matrix(data = 0,nrow = nrow(osm),ncol = nrow(osm))
  for (core in unique(osm$coreside)) {
    cols <- which(osm$coreside==core)
    negExp <- as.matrix(osm[cols,type])
    test <- matrix(data = numeric(),nrow = nrow(negExp),ncol = nrow(negExp))
    for (x in 1:length(negExp)) {
      test[,x] <- abs(negExp[x]-negExp)
    }
    minNotZero <- min(test[row(test) == (col(test) + 1)])
    test <- (max(test)+minNotZero-test)/(max(test)+minNotZero)
    geo[cols,cols] <- test
  }
  if (is.null(plotSubset)==F) {
    par(default.par)
    par(oma=c(0,0,0,5),xpd=T)
    treeSamp <- sample(unique(osm$tree),plotSubset,F)
    jpeg(file = paste(outdir,"/",datasetname,"_detrendingMatrix","trees",paste(treeSamp,collapse="_"),".jpeg",sep=""),units = "cm",width = 25,height = 20,res = 1000,bg = NA)
    #pdf(file = paste(outdir,"/",datasetname,"_detrendingMatrix","trees",paste(treeSamp,collapse="_"),".pdf",sep=""),width = 10,height = 10,bg = NA)
    subset <- which(osm$tree%in%treeSamp)
    cols <- colorRampPalette(bias = 1,alpha=T,colors = c("white","yellow","orange","red"),interpolate = "linear")
    plot(geo[subset[order(subset)],subset[order(subset)]],col=cols,breaks=30,border=NA,main = "Negative exponential fit based on estimated pith date")
    dev.off()
    printf("\nWrote detrending matrix visualization to %s",paste(outdir,"/",datasetname,"_detrendingMatrix","trees",paste(treeSamp,collapse="_"),".jpeg",sep=""))
  }
  printf("\nReturning detrending matrix based %s with dim:",type)
  print(str(geo))
  return(geo)
}

getDetrendMatrixLoop <- function(osm,type,whichDate,plotOut=F) {
  if (!type%in%c("negExp","linear")) {
    print("Type can be either negExp or linear")
    return(NULL)
  }
  if (!whichDate%in%names(osm) | is.numeric(osm[,whichDate])==F) {
    print("need to choose a numeric value for whichDate present in osm")
    return(NULL)
  }
  n <- max(osm[,"date"])-min(osm[,whichDate])
  f <- matrix(data = 0,nrow = n,ncol = 1)
  if (type=="negExp") {
    f <- exp(1/(1:n))
    names(f) <- seq(min(osm[,whichDate])+1,max(osm[,"date"]),1)
  } else if (type=="linear") {
    f <- n:1
    names(f) <- seq(min(osm[,whichDate])+1,max(osm[,"date"]),1)
  }
  test <- matrix(data = rep(NA,(length(f)*length(f))),nrow = length(f))
  for (x in 1:length(f)) {
    test[,x] <- abs(f[x]-f)
  }
  minNotZero <- min(test[row(test) == (col(test) + 1)])
  test <- (max(test)+minNotZero-test)/(max(test)+minNotZero)
  if (plotOut) {
    par(default.par)
    par(oma=c(0,0,0,5),xpd=T)
    jpeg(file = paste(params$outDir,"/",params$datasetName,"_detrendingMatrixLoopUniversal.jpeg",sep=""),units = "cm",width = 25,height = 20,res = 1000,bg = NA)
    cols <- colorRampPalette(bias = 1,alpha=T,colors = c("white","yellow","orange","red"),interpolate = "linear")
    plot(test,col=cols,breaks=30,border=NA,main = paste(type,"fit based on",whichDate))
    dev.off()
    printf("\nWrote detrending matrix visualization to %s",paste(params$outDir,"/",params$datasetName,"_detrendingMatrixLoopUniversal.jpeg",sep=""))
  }
  printf("\nReturning detrending matrix based %s with dim:",type)
  print(str(test))
  rownames(test)<- str_pad(1:n,width = 4,pad = 0)
  colnames(test)<- str_pad(1:n,width = 4,pad = 0)
  return(test)
}

getDetrendingMatrix <- function(osm,type,whichDate,plotOut=F) {
  if (!type%in%c("negExp","linear")) {
    print("Type can be either negExp or linear")
    return(NULL)
  }
  if (!whichDate%in%names(osm) | is.numeric(osm[,whichDate])==F) {
    print("need to choose a numeric value for whichDate present in osm")
    return(NULL)
  }
  n <- max(osm[,"date"])-min(osm[,whichDate])
  f <- matrix(data = 0,nrow = n,ncol = 1)
  if (type=="negExp") {
    f <- exp(1/(1:n))
    names(f) <- seq(min(osm[,whichDate])+1,max(osm[,"date"]),1)
  } else if (type=="linear") {
    f <- n:1
    names(f) <- seq(min(osm[,whichDate])+1,max(osm[,"date"]),1)
  }
  C <- chol(matrix(kronecker(f,rev(f)),nrow = n))
  M <- C%*%t(C)
  M2 <- diag(M)-M
  M3 <- M2
  M3[lower.tri(M3)] = t(M3)[lower.tri(M3)]
  M3 <- M3-min(M3)
  M3 <- 1-scale(M3, center = apply(M3, 2, min), scale = apply(M3, 2, max) - apply(M3, 2, min))
  colnames(M3) <- str_pad(1:nrow(M3),width = 4,pad = 0)
  rownames(M3) <- str_pad(1:nrow(M3),width = 4,pad = 0)
  if (plotOut) {
    par(default.par)
    par(oma=c(0,0,0,5),xpd=T)
    jpeg(file = paste(params$outDir,"/",params$datasetName,"_detrendingMatrixUniversal.jpeg",sep=""),units = "cm",width = 25,height = 20,res = 1000,bg = NA)
    cols <- colorRampPalette(bias = 1,alpha=T,colors = c("white","yellow","orange","red"),interpolate = "linear")
    plot(M3,col=cols,breaks=30,border=NA,main = paste(type,"fit based on",whichDate))
    dev.off()
    printf("\nWrote detrending matrix visualization to %s",paste(params$outDir,"/",params$datasetName,"_detrendingMatrixUniversal.jpeg",sep=""))
  }
  printf("\nReturning detrending matrix based %s with dim:",type)
  print(str(M3))
  return(M3)
}

getIncidenceMatrix <- function(osm,term) {
  M <- matrix(data = rep(0,(nrow(osm)*length(unique(osm[,term])))),ncol = length(unique(osm[,term])),dimnames = list(rownames=1:nrow(osm),colnames=unique(osm[,term])))
  for (t in colnames(M)) {
    M[which(osm[,term]==t),t] <- 1
  }
  printf("\nReturning incidence matrix for %s with dim:",term)
  print(str(M))
  return(M)
  return(M)
}

getStartYearMatrix <- function(osm,date) {
  stYear <- aggregate(osm[,date],list(osm$sample),min)
  M <- matrix(data = rep(0,nrow(osm)),ncol = 1,dimnames = list(rownames=osm$sample,colnames=c("startYearBP")))
  for (samp in 1:nrow(stYear)) {
    cur <- which(osm$sample==stYear[samp,1])
    M[cur] <- rep(max(osm[,date])-stYear[samp,2],length(cur))
  }
  printf("\nReturning starting year matrix as calculated by the max date of all cores and the min date for that treeid with dim:")
  print(str(M))
  return(M)
}

placeSampleOnDetrendMatrix <- function(osm,whichDate) {
  stYear <- aggregate(osm[,whichDate],list(osm$sample),min) #the first year of each core sample in years BP (where BP is the most recent date for any core)
  stCore <- aggregate(osm[,"date"],list(osm$sample),min) 
  endCore <- aggregate(osm[,"date"],list(osm$sample),max) 
  n <- max(osm[,"date"])-min(osm[,whichDate])
  M <- matrix(data = rep(0,nrow(osm)*n),ncol = n)
  for (samp in 1:nrow(stYear)) {
    printf("\n%s starts at year %s on the detrend curve and extends to year %s",stCore[samp,1],stCore[samp,2]-stYear[samp,2],(stCore[samp,2]-stYear[samp,2])+(endCore[samp,2]-stCore[samp,2]))
    for (cur in which(osm$sample==stYear[samp,1])) {
      M[cur,(osm[cur,"date"]-stYear[samp,2])] <- 1
    }
  }
  
  printf("\nReturning placement of samples on detrending matrix with min %s and max %s:",min(M),max(M))
  print(str(M))
  return(M)
}

getARIncidence <- function(AR,N) {
  ar.mat <- diag(nrow(AR))
  for (i in 1:N) {
    ar.mat[row(ar.mat) == (col(ar.mat) - i)] <- 1
    ar.mat[row(ar.mat) == (col(ar.mat) + i)] <- 1
  }
  #print(ar.mat[1:10,1:10])
  return(ar.mat)
}

getCoreMatrix <- function(osm,type="linear",addTreeIncidence=F) {
  if (!type%in%c("negExp","linear","pithDetrend","AR")) {
    print("Type can be one of four options: AR, negExp, linear or pithDetrend")
    return(NULL)
  }
  if (type!="AR" & !type%in%colnames(osm)) {
    printf("Type entered not found in colnames: %s",type)
    return(NULL)
  }
  printf("Making AR matrix for all")
  geo <- matrix(data = 0,nrow = nrow(osm),ncol = nrow(osm))
  for (core in unique(osm$sample)) {
    cols <- which(osm$sample==core)
    if (type == "AR") {
      test <- ARMA(as.matrix(osm[cols,"AgeAtYear"]))
    } else {
      test <- diag(osm[cols,type])
      negExp <- as.matrix(osm[cols,type])
      test <- kronecker.prod(negExp,t(negExp))
      #test <- matrix(data = numeric(),nrow = nrow(negExp),ncol = nrow(negExp))
      #for (x in 1:length(negExp)) {
      #  test[,x] <- abs(negExp[x]-negExp)
      #}
      #minNotZero <- min(test[row(test) == (col(test) + 1)])
      #test <- (max(test)+minNotZero-test)/(max(test)+minNotZero)
    }
    geo[cols,cols] <- test
  }
  #This adds a one to  "nest" within tree
  if (addTreeIncidence) {
    for (tree in unique(osm$tree)) {
      cols <- which(osm$tree==tree)
      test <- tcrossprod(as.matrix(rep(1,length(cols))),(as.matrix(rep(1,length(cols)))))
      geo[cols,cols] <- geo[cols,cols]+test
    }
  }
  
  #plot(geo,breaks = 20,border=NA)
  return(geo)
}

expandCov <- function(osm,cov,d.cov,d.inc) {
  osm$orig.order <- 1:nrow(osm)
  incF <- paste(d.inc,"F",sep="")
  osm[,incF] <- as.character(osm[,d.inc])
  covF <- paste(d.cov,"F",sep="")
  osm[,covF] <- as.character(osm[,d.cov])
  name.combF <- paste(d.cov,d.inc,sep=":")
  osm[,name.combF] <- paste(osm[,d.inc],osm[,d.cov],sep=":")
  ncov <- matrix(data = 0,nrow = nrow(osm),ncol=length(levels(as.factor(osm[,d.inc]))))
  osm <- osm[order(osm$year,osm$tree),]
  for (dt in levels(as.factor(osm[,name.combF]))) {
    print(dt)
    curyear <- osm[which(osm[,name.combF]%in%dt),]
  }
  osm[,incF] <- as.factor(osm[,d.inc])
  osm[,covF] <- as.factor(osm[,d.cov])
  return(osm)
  
}

checkNorm <- function(x,main="Check normality") {
  if (length(which(is.na(x)))>0) print("Remove NAs before checking for normality")
  x <- x[which(is.na(x)==F)]
  print(paste(length(x),"resulting rows"))
  default.par <- par()
  par(mfrow=c(2,1),oma=c(0,0,0,0),mar=c(2,2,1.1,0))
  hist(x,freq = F,main = paste(main,"-","hist"),breaks=50)
  normal <- rnorm(n = length(x),mean = mean(x),sd = sd(x))
  lines(density(normal),type = "l",col="firebrick")
  plot(x = sort(normal,decreasing = F),sort(normal,decreasing = F),col="firebrick",type="l",ylim=c(min(c(x,normal)),max(c(x,normal))),main=paste(main,"-","QQ"))
  points(sort(normal,decreasing = F),sort(x,decreasing = F),cex=.5)
  if (length(x)>5000) {
    print("Downsampling to 5000 for shapiro.test")
    x <- x[sample(x = x,size = 5000,replace = F)]
  }
  print(shapiro.test(x))
  par <- default.par
}

plotDetrendOneOld <- function(osm,pdfFile=NULL) {
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 5,height=12)
  par(mfrow=c(6,1),oma=c(2,0,5,0),mar=c(2,4,3.1,.1))
  ind=1
  for (core in unique(osm$coreid)) {
    print(core)
    if (ind>6) ind=1
    l <- which(osm$coreid==core & is.na(osm$width)==F)
    lm.curr <- lm(osm$width[l]~osm$pithDetrend[l])
    lm.log <- lm(osm$widthLog[l]~osm$pithDetrend[l])
    lm.sqrt <- lm(sqrt(osm$width[l])~osm$pithDetrend[l])
    d <- plot(osm$year[l],osm$pithDetrend[l], type = "l",col="red",ylab="cm",xlim=c(min(osm$year),max(osm$year)),ylim=c(-2,max(osm$width[l],na.rm = T)),main=core,xpd=T) #c(min(osm$year[which(osm$tree%in%osm$tree[l])]),max(osm$year[l]))
    abline(lm(osm$width[l]~osm$year[l]),col="gray")
    lines(x = osm$year[l],y = osm$width[l],col="#0000FF80",type = "l")
    lines(x = osm$year[l],y = (mean(osm$width[l]) + lm.log$residuals),col="#FF149380",type = "l")
    lines(x = osm$year[l],y = mean(osm$width[l]) + lm.sqrt$residuals,col="#bc7d3980",type = "l")
    legend("bottomleft",legend = c("pithDetrend","raw","log","sqrt"),col = c("red","#0000FF80","#FF149380","#bc7d3980"),lty = 1,cex=.5)
    ind=ind+1
  }
  if (is.null(pdfFile)==F) dev.off()
  par(default.par)
}

plotDetrendAll <- function(osm,pdfFile=NULL) {
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 5,height=12)
  par(mfrow=c(6,1),oma=c(2,0,5,0),mar=c(2,4,3.1,.1))
  ind=1
  for (core in unique(osm$coreid)) {
    plotDetrendOne(osm,core)
    ind=ind+1
  }
  if (is.null(pdfFile)==F) dev.off()
  par(default.par)
}

plotCoresAll <- function(osm, pdfFile=NULL) {
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 5,height=12)
  par(mfrow=c(6,1),oma=c(2,0,5,0),mar=c(2,4,3.1,.1))
  ind=1
  chronlm <- lmer(osm$detrendCD~(1|as.factor(osm$year)))
  years <- ranef(chronlm)$'as.factor(osm$year)'
  chron <- data.frame(year=as.numeric(rownames(years)),"rwi"=years$`(Intercept)`)
  #chronlm <- lm(osm$detrendCD~osm$treeid+osm$coreid+as.factor(osm$year))
  #years <- chronlm$effects[which(grepl("as.factor",names(chronlm$effects))==T)]
  #names(years) <- as.numeric(gsub(pattern = "as.factor(osm$year)","",names(years),fixed=T))
  #chron <- data.frame(year=c(min(as.numeric(names(years)))-1,as.numeric(names(years))),rwi=c(0,years))
  for (tree in unique(osm$treeid)) {
    cores <- unique(osm[which(osm$treeid==tree),"coreside"])
    if (length(cores)<2) {
      print(paste("Only one core for treeid:",tree))
      # if (all((is.null(chron)==F) & c("year","rwi")%in%colnames(chron))) {
      #   ylim <- c(min(min(y$detrendCD),min(x$detrendCD),min(chron$rwi)),max(max(y$detrendCD),max(x$detrendCD),max(chron$rwi)))
      # }else {ylim <- c(min(min(y$detrendCD),min(x$detrendCD)),max(max(y$detrendCD),max(x$detrendCD)))}
      # corr <- round(cor(x$detrendCD[which(x$year%in%commonyear)],y$detrendCD[which(y$year%in%commonyear)]),2)
      # d <- plot(x$year,x$detrendCD, type = "l",col="red",ylab="RWI",xlab="Year",xlim=c(min(c(x$year,y$year)),max(c(x$year,y$year))),ylim=ylim,main=paste(coreOne," vs. ",coreTwo," (r=",corr,")",sep=""),xpd=T) #c(min(osm$year[which(osm$tree%in%osm$tree[l])]),max(osm$year[l]))
      # lines(chron$year,chron$rwi,type="l")
      # legend("bottomleft",legend = c("chronology",cores[coreOne],"No core two"),col = c("grey","red","blue"),lty = 1,cex=.7)
    } else {
      for (coreOne in 1:(length(cores)-1)) {
        for (coreTwo in 2:(length(cores))) {
          plotTwoCores(osm,cores[coreOne],cores[coreTwo],chron=chron) ##add chronology
        }
      }
    }
  }
  if (is.null(pdfFile)==F) dev.off()
  par(default.par)
}

##chron should be a dataframe with year and rwi as numeric
plotDetrendOne <- function(osm,core,chron=NULL) {
  l <- which(osm$coreid==core & is.na(osm$width)==F)
  d <- plot(osm$year[l],osm$width[l], type = "l",col="firebrick",ylab="cm",xlim=c(min(osm$year),max(osm$year)),ylim=c(-2,max(osm$width[l],na.rm = T)),main=core,xpd=T) #c(min(osm$year[which(osm$tree%in%osm$tree[l])]),max(osm$year[l]))
  lines(osm$year[l],sqrt(osm$width[l]), type = "l",col="lightblue")
  #abline(lm(osm$width[l]~osm$year[l]),col="gray")
  if (all((is.null(chron)==F) & c("year","rwi")%in%colnames(chron))) {
    lines(x = chron$year,y = osm[l,"coremean"]+chron$rwi,col="grey",type = "l")
    legend("bottomleft",legend = c("chronology","raw","sqrt","sqrt-ExpCore","sqrt-ExpLinCore","sqrt-GeoCore","sqrt-GeoLinCore","sqrt-spline0.5"),col = c("grey","firebrick","lightblue","#0000FF80","#0000FF33","#FF149380","#FF149333","#bc7d3980"),lty = 1,cex=.5)
  } else {legend("bottomleft",legend = c("raw","sqrt","sqrt-ExpCore","sqrt-ExpLinCore","sqrt-GeoCore","sqrt-GeoLinCore","sqrt-spline0.5"),col = c("firebrick","lightblue","#0000FF80","#0000FF33","#FF149380","#FF149333","#bc7d3980"),lty = 1,cex=.5)}
  lines(x = osm$year[l],y = osm[l,"detrendCoreExp"],col="#0000FF80",type = "l")
  lines(x = osm$year[l],y = osm[l,"detrendCoreExpLin"],col="#0000FF33",type = "l")
  lines(x = osm$year[l],y = osm[l,"detrendCoreGeo"],col="#FF149380",type = "l")
  lines(x = osm$year[l],y = osm[l,"detrendCoreGeoLin"],col="#FF149333",type = "l")
  lines(x = osm$year[l],y = osm[l,"coremean"]+osm[l,"detrendCD"],col="#bc7d3980",type = "l")
  abline(v=unique(osm[l,"estpithcore"]))
}

plotTwoCores <- function(osm,coreOne,coreTwo,detrendType="sqrt-spline0.5",chron=NULL) {
  if (!detrendType%in%c("raw","sqrt-geoPithCore","sqrt-geoPithTree","sqrt-spline0.5")) {
    print("DetrendType must be one of these: raw, sqrt-geoPithCore,sqrt-geoPithTree,sqrt-spline0.5")
    return(NULL)
  }
  x <- osm[which(osm$coreside==coreOne & is.na(osm$width)==F),]
  y <- osm[which(osm$coreside==coreTwo & is.na(osm$width)==F),]
  commonyear <- x$year[which(x$year%in%y$year)]
  if (length(commonyear)<0) {
    print(paste("No common years between cores",coreOne,"and",coreTwo))
    return(NULL)
  }
  if (detrendType=="sqrt-spline0.5") {
    if (all((is.null(chron)==F) & c("year","rwi")%in%colnames(chron))) {
      ylim <- c(min(min(y$detrendCD),min(x$detrendCD),min(chron$rwi)),max(max(y$detrendCD),max(x$detrendCD),max(chron$rwi)))
    }else {ylim <- c(min(min(y$detrendCD),min(x$detrendCD)),max(max(y$detrendCD),max(x$detrendCD)))}
    corr <- round(cor(x$detrendCD[which(x$year%in%commonyear)],y$detrendCD[which(y$year%in%commonyear)]),2)
    d <- plot(x$year,x$detrendCD, type = "l",col="red",ylab="RWI",xlab="Year",xlim=c(min(c(x$year,y$year)),max(c(x$year,y$year))),ylim=ylim,main=paste(coreOne," vs. ",coreTwo," (r=",corr,")",sep=""),xpd=T) #c(min(osm$year[which(osm$tree%in%osm$tree[l])]),max(osm$year[l]))
    lines(y$year,y$detrendCD,col="blue",type = "l")
  } else {
    corr <- NULL
    print(paste(detrendType,"not yet implemented! Do it!"))
  }
  if (all((is.null(chron)==F) & c("year","rwi")%in%colnames(chron))) {
    lines(x = chron$year,y = chron$rwi,col="grey",type = "l")
    commony <- (y$year[which(y$year%in%chron$year)])
    commonx <- (x$year[which(x$year%in%chron$year)])
    legend("bottomleft",legend = c(paste(coreOne,"(r-chron: ",round(cor(x$detrendCD[which(x$year%in%commonx)],chron$rwi[which(chron$year%in%commonx)]),2),")",sep=""),paste(coreTwo,"(r-chron: ",round(cor(y$detrendCD[which(y$year%in%commony)],chron$rwi[which(chron$year%in%commony)]),2),")",sep="")),col = c("red","blue"),lty = 1,cex=.7)
  } else {legend("bottomleft",legend = c("chronology",coreOne,coreTwo),col = c("grey","red","blue"),lty = 1,cex=.7)}
  return(corr)
}

#This is designed to plot yearly BLUPs extracted from a model and named like in ProcessForPlot.R
plotBLUPByYear <- function(osm,gxe, plotid, modelname, pdfFile=NULL) {
  if (!all(c("blup_treeyear","blup_year","year","tree")%in%colnames(gxe))) {
    print("gxe dataframe must contail 'blup_treeyear','blup_year','year','tree'")
    return(NULL)
  }
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 5,height=12)
  par(mfrow=c(4,1),oma=c(2,0,5,0),mar=c(2,4,3.1,.1))
  ind=1
  col.cores <- c("#5aa0c3","#cd7780","#66a77b")
  for (tree in unique(gxe$tree)) {
    cur <- gxe[which(gxe$tree==tree),]
    cur.osm <- osm[which(osm$tree==tree),]
    minmax <- as.data.frame(cur.osm %>% group_by(core) %>% reframe(min=min(sqrt(width)),max=max(sqrt(width)),mean=mean(sqrt(width))))
    ylim <- c(min(min(minmax$min-minmax$mean),min(apply(gxe[,c('blup_treeyear','blup_year')],1,sum))),max(max(minmax$max-minmax$mean),max(apply(gxe[,c('blup_treeyear','blup_year')],1,sum))))
    add <- max(gxe[,c('blup_treeyear','blup_year')])-min(gxe[,c('blup_treeyear','blup_year')])
    d <- plot("",ylab="",xlab="Year",xlim=c(min(cur.osm$estpithcore),max(cur.osm$year)),ylim=c(ylim[1],ylim[2]+add),main="",yaxt="n",xpd=T) # #c(min(osm$year[which(osm$tree%in%osm$tree[l])]),max(osm$year[l]))
    title(main=paste(modelname," BLUPs:\n",plotid," - ",tree,sep=""),adj=0,cex=.5)
    title(ylab = "RWI",line = 1)
    abline(h=add)
    abline(v=min(cur.osm$estpithcore),col="orange")
    for (core in unique(cur.osm$coreid)) {
      lines(cur.osm[which(cur.osm$coreid==core),"year"],sqrt(cur.osm[which(cur.osm$coreid==core),"width"])-mean(sqrt(cur.osm[which(cur.osm$coreid==core),"width"])),col=col.cores[which(unique(cur.osm$coreid)==core)],type = "l")
      #lines(cur.osm[which(cur.osm$coreid==core),"year"],cur.osm[which(cur.osm$coreid==core),"ageatyearcoregeo"]+ylim[1],col=paste(col.cores[which(unique(cur.osm$coreid)==core)],"33",sep=""),type = "l")
    }
    lines(cur$year,cur$blup_year+cur$blup_treeyear,type = "l",col="black")
    lines(cur$year,cur$blup_year+ylim[2]+add/2+abs(min(cur$blup_year)),type = "l",col="#e7c684")
    lines(cur$year,cur$blup_treeyear+ylim[2]+add/2+abs(min(cur$blup_treeyear)),col="#aec593",type = "l")
    if (ind==1) legend("topright",legend=c(paste("sqrt(core ",c("a","b","c"),")",sep=""),"g+e+gxe_blup","year_blup","gxe_blup"),ncol=2,lwd=1,col = c(col.cores,"black","#e7c684","#aec593"),cex=.8,xpd = T,inset=c(0,-.2))
    ind <- ind+1
    if (ind>4) ind=1
  }
  if (is.null(pdfFile)==F) dev.off()
  par(default.par)
}
plotChronBLUP <- function(gxe,plotid) {
  if (!all(c("blup_treeyear","blup_year","year","name")%in%colnames(gxe))) {
    print("gxe dataframe must contail 'blup_treeyear','blup_year','year','name'")
    return(NULL)
  }
  if (all((is.null(chron)==F) & c("year","rwi")%in%colnames(chron))) {
    lines(x = chron$year,y = chron$rwi,col="grey",type = "l")
    commony <- (y$year[which(y$year%in%chron$year)])
    commonx <- (x$year[which(x$year%in%chron$year)])
    legend("bottomleft",legend = c(paste(coreOne,"(r-chron: ",round(cor(x$detrendCD[which(x$year%in%commonx)],chron$rwi[which(chron$year%in%commonx)]),2),")",sep=""),paste(coreTwo,"(r-chron: ",round(cor(y$detrendCD[which(y$year%in%commony)],chron$rwi[which(chron$year%in%commony)]),2),")",sep="")),col = c("red","blue"),lty = 1,cex=.7)
  } else {legend("bottomleft",legend = c("chronology",coreOne,coreTwo),col = c("grey","red","blue"),lty = 1,cex=.7)}
  return(corr)
}

plotDetrendLM <- function(osm,osm.detrend,square=T,pdfFile=NULL) {
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 5,height=12)
  par(mfrow=c(6,1),oma=c(2,0,5,0),mar=c(2,4,3.1,.1))
  ind=1
  if ("widthAdj"%in%names(osm.detrend)==F) {
    print("MUST have a column called widthAdj! This is supposed to be for detrended")
    return(F)
  }
  for (core in unique(osm$coreid)) {
    print(core)
    if (ind>6) ind=1
    l <- which(osm$coreid==core & is.na(osm$width)==F)
    tree <- unique(osm$tree[which(osm$coreid==core)])
    treeL <- which(osm.detrend$tree==tree)
    d <- plot(osm$year[l],osm$pithDetrend[l], type = "l",col="red",ylab="cm",xlim=c(min(osm$year[which(osm$tree%in%osm$tree[l])]),max(osm$year[l])),ylim=c(-2,max(osm$width[l],na.rm = T)),main=core,xpd=T)
    if (square) {
      abline(lm(osm$width[l]~osm$year[l]),col="gray")
      abline(lm((osm.detrend$widthAdj[treeL])^2~osm.detrend$date[treeL]),col="black")
      lines(x = (osm.detrend$date[treeL]),y = (osm.detrend$widthAdj[treeL])^2,col="#6fac5d80",type = "l")
      rawfit <- "fitRaw"
      lmname <- "fitLMDetrendSquared"
      lmfit <- "LMDetrendSquared"
    } else {
      abline(lm(sqrt(osm$width[l])~osm$year[l]),col="gray")
      abline(lm(osm.detrend$widthAdj[treeL]~osm.detrend$date[treeL]),col="black")
      lines(x = osm.detrend$date[treeL],y = osm.detrend$widthAdj[treeL],col="#6fac5d80",type = "l")
      rawfit <- "fitSqrtRaw"
      lmname <- "fitLMDetrend"
      lmfit <- "LMDetrend"}
    lines(x = osm$year[l],y = osm$width[l],col="#0000FF80",type = "l")
    lines(x = osm$year[l],y = sqrt(osm$width[l]),col="#bc7d3980",type = "l")
    legend("bottomleft",legend = c("pithDetrend",rawfit,lmname,"raw","sqrtRaw",lmfit),col = c("red","grey","black","#0000FF80","#bc7d3980","#6fac5d80"),lty = 1,cex=.5,ncol = 3)
    ind=ind+1
  }
  if (is.null(pdfFile)==F) dev.off()
  par(default.par)
}

plotFromLM <- function(osm.detrend,xterm="date",yterm,colorTerm=NULL,scaleCol=1,cex.size=.1,forcePt=F,pdfFile=NULL,colortitle=NULL,title=NULL) {
  if (yterm%in%names(osm.detrend)==F) {
    printf("No term called %s in osm.detrend!",yterm)
  }
  if (xterm%in%names(osm.detrend)==F) {
    printf("No term called %s in osm.detrend!",xterm)
  }
  if (is.null(colortitle)) colortitle <- colorTerm
  pal <- NULL
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 7,height=5)
  if (forcePt & (grepl(pattern = ":",x = yterm) | nrow(unique(osm.detrend[,c("tree",xterm)]))>length(unique(osm.detrend$tree)))) {
    if (grepl(pattern = ":",x = yterm)) {
      x <- strsplit(yterm,":")[[1]][1]
      y <- strsplit(yterm,":")[[1]][2]
      printf("Using %s for x and %s for y",x,yterm)
    } else {
      y <- yterm
      x <- xterm
    }
    agg <- unique(osm.detrend[,c("tree",yterm,x,colorTerm)])
    if (is.null(colorTerm)) {
      colorTerm <- "unique"
      col <- uniqueCols(length(unique(agg$tree)))
      cols <- data.frame(tree=unique(agg$tree),col,stringsAsFactors = F)
      agg$col <- cols[match(agg$tree,cols$tree),2] 
    } else {
      heatmap <- getHeatMapCol(agg[,colorTerm]*scaleCol)
      agg$col <- heatmap$cols
      pal <- heatmap$pal
    }
    if (is.null(title)) title <- paste("Random effect of ",sub(pattern = "E",replacement = "",x = yterm),sep="")
    if (is.null(pal)==F) par(oma=c(0,0,0,3))
    plot(0,ylab=yterm,xlab=x,xlim=c(min(as.numeric(agg[,x])),max(as.numeric(agg[,x]))),ylim=c(min(agg[,yterm]),max(agg[,yterm])),main=title,xpd=T)
    for (tree in agg$tree) {
      lines(as.numeric(agg[agg$tree==tree,x]),agg[agg$tree==tree,yterm],col=agg[agg$tree==tree,"col"],lwd=.1,pch=19,type="l",cex=cex.size,main=title,xpd=T)
    }
    if (is.null(pal)==F) subplot(fun = color.bar(pal,min = min(osm.detrend[,colorTerm]),title=colortitle,max = max(osm.detrend[,colorTerm])),hadj = 1,vadj = 0,x = c(par("usr")[2]+sd(osm.detrend[,x])/30,par("usr")[2]+sd(osm.detrend[,x])/3),y = c(min(osm.detrend[,yterm]),max(osm.detrend[,yterm])))
  } else {
    if (is.null(colorTerm)) {
      agg <- unique(osm.detrend[,c("tree",yterm,xterm)])
      colorTerm <- "unique"
      agg$col <- uniqueCols(length(unique(agg$tree)))
    } else {
      agg <- unique(osm.detrend[,c("tree",yterm,xterm,colorTerm)])
      heatmap <- getHeatMapCol(agg[,colorTerm]*scaleCol)
      agg$col <- heatmap$cols
      pal <- heatmap$pal
    }
    if (is.null(title)) title <- paste("Random effect of ",sub(pattern = "E",replacement = "",x = yterm)," for ",xterm,sep="")
    if (is.null(pal)==F) par(oma=c(0,0,0,3))
    plot(agg[,xterm],agg[,yterm],col=agg$col,ylab=yterm,xlab=xterm,cex=cex.size,pch=19,main=title,xpd=T)
    if (is.null(pal)==F) subplot(fun = color.bar(pal,min = min(osm.detrend[,colorTerm]),title=colortitle,max = max(osm.detrend[,colorTerm])),hadj = 1,vadj = 0,x = c(par("usr")[2]+sd(osm.detrend[,xterm])/30,par("usr")[2]+sd(osm.detrend[,xterm])/3),y = c(min(osm.detrend[,yterm]),max(osm.detrend[,yterm])))
  }
  if (is.null(outFile)==F) dev.off()
  par(default.par)
}

plotContinuousForTreeOnMap <- function(osm,targetCol,pdf=NULL,jpeg=NULL) {
  if (all(c("tree","plot","dbhcm","x","y","z",targetCol)%in%names(osm))==F) {
    printf("Must include %s in osm",paste(c("tree","plot","dbhcm","x","y","z",targetCol),collapse = ","))
    return(F)
  }
  if (is.null(pdf)==F) pdf(file = pdf,width = 7,height = 5)
  if (is.null(pdf)==T & (is.null(jpeg)==F)) jpeg(file = jpeg,width = 13,height = 10,units="cm",res = 600)
  par(default.par)
  subsD <- unique(osm[,c("tree","plot","dbhcm","x","y","z",targetCol)])
  SetUpCircularPlot()
  MapPoints(subsD,fillcol = "grey")
  heatmap <- getHeatMapCol(mp = (subsD[,targetCol]/max(subsD[,targetCol]))*100)
  if (is.null(heatmap)==F) MapPoints(subsD,fillcol = heatmap$cols)
  par.curr <- par()
  if (is.null(heatmap)==F) addColorScale(info = subsD,heatmap = heatmap,colorTerm = targetCol,title=paste("effect of",sub(pattern = "Eff","",targetCol)))
  if (is.null(pdf)==F | (is.null(jpeg)==F)) dev.off()
}

##GEometric decay function
growthArea <- function(scaling,range) {
  radius <- sqrt((scaling*range[1])/pi) #area = pi*r^2; area = 1
  areas <- (scaling*range[1:length(range)])
  for (area in 2:length(areas)) {
    #print(sqrt(area/pi))
    radius <- c(radius,(sqrt(areas[area]/pi)-sqrt((areas[area-1])/pi)))
  }
  return(radius)
}

###add geometric decay function to osm
detrendArea <- function(osm,scaling=1) {
  area <- data.frame("age"=min(osm$ageatyear):max(osm$ageatyear),"radius"=growthArea(scaling,min(osm$ageatyear):max(osm$ageatyear)))
  detrendArea <- area[match(osm$ageatyear,area$age),"radius"]
  return(detrendArea)
}

#takes an sf feature set of polylines ordered by x position and returns distance between them (for shiny app)
#deprecated
shortestDistanceOld <- function(rings, missing, centerX) {
  print("start shortestDistance")
  left <- rings[which((as.numeric(unlist(lapply(strsplit(rings$layerId,"_"),function(x) {x[1]})))<centerX)==T),]
  right <- rings[which(rings$layerId%in%left$layerId==F),]
  shortest_distance <- data.frame(year=left[2:nrow(left),]$year,width=as.numeric(st_distance(left[2:nrow(left),],left[1:(nrow(left)-1),],by_element = T)/10000),side="left")
  ###TODO missing rings assumed to be from left side only
  if (!is.null(missing)) {
    shortest_distance <- rbind(shortest_distance,data.frame(year=input$missingyearA,width=rep(0,length(missing)),side="left"))
    shortest_distance <- shortest_distance[order(shortest_distance$year),]
  }
  if(nrow(right)>1) rbind(shortest_distance,data.frame(year=right[2:nrow(right),]$year,width=as.numeric(st_distance(right[2:nrow(right),],right[1:(nrow(right)-1),],by_element = T)/10000),side="right"))
  return(shortest_distance)
}

detrendAndPlot <- function(osm,flex,range=c(min(osm$year),max(osm$year))) {
  print("start detrendAndPlot")
  plot(0,0,xlim = range,ylim = c(-2,2),xlab="Date",ylab=paste("Core mean plus spline =",flex,"residuals"))
  cols <- rainbow(length(unique(osm$coreid)))
  for (core in 1:length(unique(osm$coreid))) {
    l <- which(osm$coreid==unique(osm$coreid)[core])
    l <- l[which((is.na(osm[l,]$width)==F))]
    spl <- smooth.spline(x = list(x=osm$year[l],y = osm$width[l]),spar = flex)
    lm.curr <- lm(osm$width[l]~spl$y)
    #expon <- mean(osm$width[l])+spl$y[1]^(-.1*(1:length(osm$year[l])))
    #lm.expon <- lm(log(osm$width[l]+.0000000000001)~expon)
    lm.log <- lm(log(osm$width[l]+.001)~spl$y)
    # if want to include the core mean: lm.curr$coefficients[1]+mean(lm.curr$fitted.values)+
    lines(x = osm$year[l],y = lm.log$residuals,col=cols[core],type = "l")
  }
  par(default.par)
  return(T)
}

addSizeToDetrend <- function(osm.detrend) {
  if ("dbhcm"%in%names(osm.detrend)==F) {
    printf("osm.detrend does not have a variable called dbhcm!")
    return(osm.detrend)
  }
  osm.detrend$Size <- numeric(nrow(osm.detrend))
  for (tree in unique(osm.detrend$tree)) {
    curr <- osm.detrend[which(osm.detrend$tree==tree),]
    printf("\nTree %s dbhcm: %s",tree,mean(curr$dbhcm))
    ordD <- order(curr$date,decreasing = T)
    for (x in 1:length(ordD)) {
      osm.detrend$Size[which(osm.detrend$`DateAsFactor:tree`==curr[ordD[x],"DateAsFactor:tree"])] <- (curr$dbhcm[ordD[x]])-(2*(sum(curr$width[ordD[1:x]])/10))
    }
    #hist(osm.detrend$Size[which(osm.detrend$tree==tree)],main=tree,breaks=20)
  }
  return(osm.detrend)
}

addEffectsFromSommer <- function(osm.detrend,ans1) {
  for (ran in names(ans1$U)) {
    names <- strsplit(ran,":")[[1]]
    treeYear <- data.frame(eff=ans1$U[[ran]][[1]],var=names(ans1$U[[ran]][[1]]),stringsAsFactors = F)
    if (length(names)>1) {
      if (names[1]%in%unique(osm.detrend$DateAsFactor) & names[2]=="tree") {
        osm.detrend[,paste(ran,"E",sep="")] <- treeYear[match(osm.detrend[,names[2]],treeYear[,2]),1]
      } else {
        splitName <- str_split_fixed(treeYear[,2], ":", 2)
        treeYear[,names[1]] <- str_replace_all(splitName[,1],names[1],"")
        treeYear[,names[2]] <- str_replace_all(splitName[,2],names[2],"")
        osm.detrend[,paste(ran,"E",sep="")] <- treeYear[match(paste(osm.detrend[,names[1]],osm.detrend[,names[2]],sep=":"),paste(treeYear[,names[1]],treeYear[,names[2]],sep=":")),"e"]
      }
    } else {
      treeYear[,names[1]] <- str_replace_all(treeYear$var,names[1],"")
      osm.detrend[,paste(ran,"E",sep="")] <- treeYear[match(osm.detrend[,names[1]],treeYear[,names[1]]),"e"]
    }
  }
  osm.detrend$residuals <- ans1$residuals
  if (all(c("treeE","DateAsFactorE","DateAsFactor:treeE")%in%names(osm.detrend))) osm.detrend$widthAdj <- ((apply(cbind(osm.detrend[,"treeE"],osm.detrend[,"DateAsFactorE"],osm.detrend[,"DateAsFactor:treeE"]),MARGIN = 1,sum))+ans1$Beta$Estimate[which(ans1$Beta$Effect=="(Intercept)")])
  return(osm.detrend)
}
##Functions to interact with azure postgresDB

getDBColumnDF <- function(outDir) {
  system(paste("source /Users/kelly.swarts/.bash_profile; PGPASSWORD=piceaabies psql -h trg-srv.postgres.database.azure.com -d trgdb -U picea@trg-srv -c \"COPY (select table_name, count(*) as column_count from information_schema.columns where table_schema = 'public' GROUP by table_name order by column_count desc) TO STDOUT WITH CSV HEADER\" > ",outDir,"/TableColumnNum.csv",sep=""))
  db_col_num <- read.csv(paste(outDir,"/TableColumnNum.csv",sep=""),header = T,as.is = T)
}

getMapPointCoords <- function(mappointid,outDir=getwd(),dbworks=F,keepFile=F) {
  mo <- getDataFromDB(query = paste("SELECT wgs84collect FROM MapPoint WHERE mappointid =",mappointid,sep=" "))
  if (is.na(mo[1,1])) return(NULL)
  coords <- fromJSON(txt = mo[1,1])
  df <- as.data.frame(fromJSON(txt = coords[[1]]))
  for (r in 2:length(coords)) {
    df <- rbind(df,as.data.frame(fromJSON(txt = coords[[r]])))
  }
  return(df)
}

getSpecies <- function(dbworks=F) {
  mo <- getDataFromDB(query = paste("SELECT * FROM species",sep=" "))
  return(mo)
}

#' PlotMapLocations
#' @export
MapPlotProfiles <- function(plotids,elevation=T,ptCol="#c85044",ptSize=2,labelpoints=T,resolutionMeters=10,outFile=NULL,bndy=NULL,opacityHex="FF",addRange=F,forPub=F,keepOpen=F,axes=T) {
  wgs <- getDataFromDB(query = paste("SELECT p.plotid,p.centerpoint,p.country,p.collected,p.altname,m.median_wgs84n as y,m.median_wgs84e as x,m.median_alt as altitude from public.plot as p, public.mappoint as m where p.centerpoint = m.mappointid AND plotid in (",paste(plotids,collapse=","),") order by p.plotid",sep=""))
  if (length(which(is.na(wgs$x)))>0) print(paste("No spatial data for plots:",paste(wgs$plotid[which(is.na(wgs$x))],collapse=",")))
  wgs <- wgs[which(is.na(wgs$x)==F),]
  #if (nrow(wgs)<1) return(NULL)
  wgs$collected <- unlist(lapply(wgs$collected,FUN=function(x){strsplit(x = as.character(x),split = '-',fixed = T)[[1]][1]}))
  wgs <- wgs[which(is.na(wgs$collected)==F),]
  wgs$altname <- wgs$plotid
  return(PlotElevationProfile(wgs = wgs,plotids = plotids,ptCol = ptCol,ptSize = ptSize,labelpoints = labelpoints,resolutionMeters = resolutionMeters,outFile = outFile,bndy = bndy,opacityHex = opacityHex,forPub = forPub,keepOpen=keepOpen,axes=axes))
}

PlotElevationProfile <- function(wgs,plotids=NULL,ptCol="#c85044",ptSize=2,labelpoints=T,resolutionMeters=1000,outFile=NULL,bndy=NULL,opacityHex="FF",forPub=F,keepOpen=F,axes=T) {
  if (is.null(plotids)) plotids <- 1:nrow(wgs)
  crs <- "+proj=longlat +datum=WGS84 +no_defs +type=crs" #"+init=epsg:4326" WGS84 
  #newmap <- getMap(resolution = "high")
  #newmap <- spTransform(newmap, CRS = CRS(projargs = crs))
  focus <- data.frame("x"=((max(wgs$x)-min(wgs$x))),"y"=((max(wgs$y)-min(wgs$y))))
  bndy <- data.frame("x"=c(min(wgs$x)-focus$x, max(wgs$x)+focus$x),"y"=c(min(wgs$y)-(focus$y), max(wgs$y)+(focus$y)))
  z <- max(4,round(14-ceiling((max(bndy$x)-min(bndy$x)+(max(bndy$y)-min(bndy$y)))),digits = 0))
  correction <- 2-((90-abs(mean(bndy$y)))/90)
  xovery <- (max(bndy$x)-min(bndy$x))/((max(bndy$y)-min(bndy$y))*correction) # correction because globe
  height <- 6
  width <- 6
  if (is.null(outFile)==F) {
    if (grepl(pattern = "pdf",x = outFile,ignore.case = T)) pdf(file=outFile,width = width,height=height,useDingbats = F)
    else if (grepl(pattern = "png",x = outFile,ignore.case = T)) png(units = "cm",res = 300,file=outFile,width = 80,height=80*(height/(width)))
    else if (grepl(pattern = "svg",x = outFile,ignore.case = T))svg(file=outFile,width = width)
  }
  par(mar=c(4,4,0,0),mfrow=c(1,1))
  elev <- get_elev_raster(locations = bndy,clip = "locations", z = z,prj = crs)
  if (abs(ceiling((elev@data@max-elev@data@min)/resolutionMeters))<2) {
    print(paste("Changing resolution from",resolutionMeters,"to",resolutionMeters/10))
    resolutionMeters <- resolutionMeters/10
  }
  #wgs <- wgs[order(round(wgs$x,2),round(wgs$y,2),wgs$altitude),]
  wgs <- wgs[match(plotids,wgs$plotid),]
  out <- data.frame(y=numeric(0),plotid=numeric(0))
  for (i in 1:(nrow(wgs)-1)) {
    cur <- wgs[c(i,i+1),]
    cur[,name] <- name
    hike <- sfheaders::sf_linestring(obj = cur,x = "x",y = "y",linestring_id = name)
    alt <- raster::extract(elev,hike,along=T,method="bilinear")[[1]]
    #if (wgs$altitude[i]<wgs$altitude[i+1] & alt[1]>alt[length(alt)]) {alt <- alt[length(alt):1]}
    #if (wgs$altitude[i]>wgs$altitude[i+1] & alt[1]<alt[length(alt)]) {alt <- alt[length(alt):1]}
    #plot(1:length(alt),alt,type="l")
    out <- rbind(out,data.frame(y=alt,plotid=c(wgs$plotid[i],rep(NA,length(alt)-2),wgs$plotid[i+1])))
  }
  #out <- rbind(out,data.frame(y=wgs$altitude[i+1],plotid=wgs$plotid[i+1]))
  out$x <- seq(min(out$y),max(out$y),((max(out$y)-min(out$y))/nrow(out)))[1:nrow(out)]
  plot(out$x,out$y,type="l",ylab="m.a.s.l",cex.lab=2)
  points(out[which(is.na(out$plotid)==F),"x"],out[which(is.na(out$plotid)==F),"y"],pch=19,col=ptCol,cex=ptSize)
  if (labelpoints) text(out[which(is.na(out$plotid)==F),"x"],out[which(is.na(out$plotid)==F),"y"],out[which(is.na(out$plotid)==F),"plotid"],adj=c(0,0))
  if (is.null(outFile)==F) dev.off()
  par(mar=c(5, 4, 4, 2) + 0.1)
  return(list("wgs"=wgs,"bndy"=bndy,"elev"=elev,"width"=width,"height"=height,"out"=out))
}



#' PlotMapLocations
#' @export
MapPlotLocations <- function(plotids,plotname = NA,region=NULL,elevation=T,ptCol="#c85044",ptSize=2,labelpoints=T,resolutionMeters=1000,outFile=NULL,buffersize=0,bndy=NULL,colorByYear=F,kmlFile=NULL,opacityHex="FF",addRange=F,forPub=F,keepOpen=F,axes=T) {
  wgs <- getDataFromDB(query = paste("SELECT p.plotid,p.centerpoint,p.country,p.collected,p.altname,m.median_wgs84n as y,m.median_wgs84e as x,m.median_alt as altitude from public.plot as p, public.mappoint as m where p.centerpoint = m.mappointid AND plotid in (",paste(plotids,collapse=","),") order by p.plotid",sep=""))
  if (length(which(is.na(wgs$x)))>0) print(paste("No spatial data for plots:",paste(wgs$plotid[which(is.na(wgs$x))],collapse=",")))
  wgs <- wgs[which(is.na(wgs$x)==F),]
  #if (nrow(wgs)<1) return(NULL)
  if (is.null(kmlFile)==F) convertToKML(df=wgs,idCol="altname",keepCol=c("plotid","country","altitude"),outDir = dirname(kmlFile),outBase = basename(kmlFile))
  wgs$collected <- unlist(lapply(wgs$collected,FUN=function(x){strsplit(x = as.character(x),split = '-',fixed = T)[[1]][1]}))
  wgs <- wgs[which(is.na(wgs$collected)==F),]
  wgs$altname <- wgs$plotid
  return(GeneralizedCoordinateMapping(wgs = wgs,plotname = plotname,region = region,elevation = elevation,ptCol = ptCol,ptSize = ptSize,labelpoints = labelpoints,resolutionMeters = resolutionMeters,outFile = outFile,buffersize = buffersize,bndy = bndy,colorByYear = colorByYear,kmlFile = kmlFile,opacityHex = opacityHex,addRange = addRange,forPub = forPub,keepOpen=keepOpen,axes=axes))
}

#' MapFromDF
#' @export
MapFromDF <- function(wgs,xCol,yCol,labelCol,plotname="",region=NULL,elevation=T,ptCol="#c85044",ptSize=2,pch=21,lwd=1,colorCol=NULL,labelpoints=T,resolutionMeters=1000,outFile=NULL,bndy=NULL,buffersize=0,colorByYear=F,kmlFile=NULL,opacityHex="FF",addRange=F,forPub=F,keepOpen=F,axes=T) {
  if (all(c(xCol,yCol,labelCol)%in%colnames(wgs))==F) {
    print("Couldn't find xCol, yCol or label in wgs column names!")
    return(NULL)
  } else {
    wgs$x <- as.numeric(wgs[,which(names(wgs)==xCol)])
    wgs$y <- as.numeric(wgs[,which(names(wgs)==yCol)])
    wgs$altname <- wgs[,which(names(wgs)==labelCol)]
    wgs <- wgs[which(is.na(wgs$x)==F),]
    wgs <- wgs[which(is.na(wgs$y)==F),]
  }
  if (nrow(wgs)<1) return(NULL)
  if (is.null(kmlFile)==F) convertToKML(df=wgs,idCol="altname",keepCol=c("plotid","country","altitude"),outDir = dirname(kmlFile),outBase = basename(kmlFile))
  return(GeneralizedCoordinateMapping(wgs = wgs,plotname = plotname,region = region,elevation = elevation,ptCol = ptCol,ptSize = ptSize,labelpoints = labelpoints,resolutionMeters = resolutionMeters,outFile = outFile,buffersize = buffersize,bndy = bndy,colorByYear = colorByYear,kmlFile = kmlFile,opacityHex = opacityHex,addRange = addRange,forPub = forPub,keepOpen=keepOpen,axes=axes))
}

DEMForPlot <- function(plotid, outDir=NULL,estimateCompetition=F) {
  mo <- dbToMO(c(plotid))
  if (is.null(mo)) return(NULL)
  cur <- mo[which(is.na(mo$z)==F),]
  cur$markerdbh <- log(1+cur$dbhcm/10)
  xyz <- cur[,c("longitude","latitude","altitude","dbhcm")] 
  names(xyz) <- c("x","y","z","dbhcm")
  xyz.pt <- xyz
  coordinates(xyz.pt) <- ~ x + y
  class(xyz.pt)
  proj4string(xyz.pt) <-  "+init=epsg:4326"
  
  #xyz <- as.matrix(xyz)
  #text3D(x= xy[which(is.na(mo$treeid)==F),"x"], y= xy[which(is.na(mo$treeid)==F),"y"], z=mo[which(is.na(mo$treeid)==F),"compassh"], labels =  namedT)
  #plot3d(x = xyz$x,y = xyz$y,z = xyz$z,xlab = "E",ylab = "N",zlab = "Alt",type = "s",radius = dbh/2,pch=21,col=bgs,bg = cols,lwd=.2)
  #points3D(x = xy$x,y = xy$y,z = mo$compassh,pch=21,col=bgs,bg = cols,lwd=2,cex=dbh)
  #s=interp::interp(x = xyz$x,y = xyz$y,z = xyz$z,extrap = F,linear = F,duplicate = "strip",nx = 1000,ny = 1000,)
  #dim(s$z)
  #p <- cur %>% plot_ly(x = ~x,y = ~y,z = ~z,type = "surface",colors = "YlOrBr",opacity=1)
  #p %>% add_trace(x = ~x,y = ~y,z = ~z, mode = "markers", type = "scatter3d",marker = list(size = ~markerdbh, symbol = "y-up", color = "gray",opacity=.9, line=list(width=.00000000001,color="black")))
  r <- raster(xyz.pt,ncol=(max(xyz$x)-min(xyz$x))/((max(xyz$x)-min(xyz$x))+(max(xyz$y)-min(xyz$y)))*1000, nrow=(max(xyz$y)-min(xyz$y))/((max(xyz$x)-min(xyz$x))+(max(xyz$y)-min(xyz$y)))*1000)
  #circle <- lwgeom::st_minimum_bounding_circle(st_as_sf(xyz.pt),nQuadSegs = 30)
  r_new <- terra::rasterize(xyz[,c("x","y")], r, xyz[,c("z")], fun=median)
  gs <- gstat::gstat(formula=z~x+y, locations=~x+y, data=xyz, model = gstat::vgm(10, "Exp", 300),nmax=20, maxdist = 10,set=list(idp = 0))
  r_int <- interpolate(r_new, gs, debug.level=1)
  #rgb.pal <- colorRampPalette(c("snow1","snow2","light blue","blue","dark blue", "orange","red"), space = "rgb")
  #rgb.pal <- colorRampPalette(c("snow1","snow2","#91cc84","#468234","#285f2b"), space = "rgb")
  #rgb.pal <- colorRampPalette(c("snow1","#dbaf96","#bc7e59"), space = "rgb")
  pals <- getHeatMapCol(cur$z,color.range = c("snow1","#91cc84","#6097ce","#a265c2","#ca5572"))
  slope <- terrain(r_int, "slope", neighbors=8, unit="degrees")
  aspect <- terrain(r_int,"aspect", neighbors=8, unit="degrees")
  hill <- hillShade(terrain(r_int, 'slope'), terrain(r_int, 'aspect'), 40, 270)
  plot(hill, col=grey(0:100/100), legend=FALSE, main=paste("plot",plotid))
  plot(r_int,add=T,col=alpha(pals$pal(ceiling(max(r_int@data@values)*100)-floor(min(r_int@data@values))*100),.3), xlab="Long", ylab="Lat",legend.args=list(text=paste0("m.a.s.l"), side=4, font=2, line=3, cex=1),mar=c(0,0,0,0))
  points(x = cur$longitude,y=cur$latitude,pch=21,bg=pals$cols,col="black",cex=cur$markerdbh)
  rs <- stack(r_int,slope,aspect)
  ext <- raster::extract(rs,xyz.pt,"bilinear")
  colnames(ext) <- c("elevation","tree_slope","tree_aspect")
  cur <- cbind(cur,ext)
  if (estimateCompetition) {
    comp <- EstimateCompetition(plotid)
    cur <- cbind(cur,"competition"=comp$competition)
  }
  if (length(which(is.na(cur$treeid)==F))>0) hist(cur[which(is.na(cur$treeid)==F),]$tree_aspect,main=paste("momentary aspect for named trees in plot",plotid),xlab="momentary aspect")
  if (length(which(is.na(cur$treeid)==F))>0) hist(cur[which(is.na(cur$treeid)==F),]$tree_slope,main=paste("momentary slope for named trees in plot",plotid),xlab="momentary slope")
  if (dir.exists(outDir)) {
    writeRaster(x = ratify(r_int),filename = paste(outDir,"/",plotid,".tif",sep=""),format='GTiff',prj=T,byLayer=T,overwrite=T,options=c('TFW=NO'))
    if (length(which(is.na(cur$treeid)==F))>0) write.csv(cur[which(is.na(cur$treeid)==F),],paste(outDir,"/",plotid,"_namedTrees.csv",sep=""),row.names = F)
    write.csv(cur,paste(outDir,"/",plotid,"_allTrees.csv",sep=""),row.names = F)
  }
  return(list("rasterstack"=rs,cur))
}

DEMAnything <- function(xyz,zcol,global=T) {
  cur <- xyz[,c("x","y",zcol)]
  xyz <- cur
  colnames(xyz)[which(colnames(xyz)==zcol)] <- "z"
  xyz.pt <- xyz
  coordinates(xyz.pt) <- ~ x + y
  class(xyz.pt)
  proj4string(xyz.pt) <-  "+init=epsg:4326"
  r <- raster(xyz.pt,ncol=500, nrow=500)
  r_new <- terra::rasterize(xyz[,c("x","y")], r, xyz[,c("z")], fun=median)
  out <- tryCatch( {
    r_int <- interpolate(r_new, gs, debug.level=1)
  }, error=function(cond) {
    print(cond)
    return(NULL)
  })
  plot(r_int, xlab="Long", ylab="Lat",legend.args=list(text=paste0(zcol), side=4, font=2, line=3, cex=1),mar=c(0,0,0,0))
  points(x = xyz$x,y=xyz$y,pch=21,bg="black",col="black",cex=.5)
  return(r_int)
}

EstimateCompetition <- function(plotid) {
  mo <- dbToMO(c(plotid))
  if (is.null(mo)) return(NULL)
  cur <- mo[which(is.na(mo$z)==F),]
  cur$markerdbh <- log(1+cur$dbhcm/10)
  cur$treeid[which(is.na(cur$treeid))] <- "unnamed"
  xyz <- cur[,c("treeid","longitude","latitude","altitude","dbhcm")] 
  names(xyz) <- c("treeid","x","y","z","dbhcm")
  xyz.pt <- xyz
  coordinates(xyz.pt) <- ~ x + y
  class(xyz.pt)
  proj4string(xyz.pt) <-  "+init=epsg:4326"
  
  r <- raster(xyz.pt,ncol=500, nrow=500)
  r_new <- terra::rasterize(xyz[,c("x","y")], r, xyz[,c("z")], fun=median)
  #r_new[is.na(r_new[])] <- 0 ###THIS IS EQUIVALENT TO NA
  xyz$competition <- NA
  for (tr in cur$treeid[which(cur$treeid!="unnamed")]) {
    xyz.cur <- xyz[which(xyz$treeid!=tr),]
    gs.comp <- gstat::gstat(formula=dbhcm~x+y, locations=~x+y, data=xyz.cur, model = gstat::vgm(10, "Exp", 300),nmax=30, maxdist = 15,set=list(idp = 0))
    r_comp <- interpolate(r_new, gs.comp, debug.level=1)
    xyz$competition[which(xyz$treeid==tr)] <- raster::extract(stack(r_comp),xyz.pt[which(xyz$treeid==tr),],"bilinear")
    plot(r_comp, xlab="Long", main=tr,ylab="Lat",legend.args=list(text=paste0("dbh"), side=4, font=2, line=3, cex=1),mar=c(0,0,0,0))
    points(x = cur$longitude[which(xyz$treeid!=tr)],y=cur$latitude[which(xyz$treeid!=tr)],pch=21,bg="black",col="black",cex=cur$dbhcm[which(xyz$treeid!=tr)]*.05)
    points(x = cur$longitude[which(xyz$treeid==tr)],y=cur$latitude[which(xyz$treeid==tr)],pch=21,bg="red",col="red",cex=cur$dbhcm[which(xyz$treeid==tr)]*.05)
  }
  return(xyz)
}

#' GeneralizedCoordinateMapping
#' @description helper function that can be fed a specially formatted dataframe
#' @param wgs dataframe with coordinate info. must have columns "x","y","altname", and (only if using colorByYear) "collected"
#' @param plotname the main for the map
#' @param region can be "Europe_all","Europe_spruce","N_America_all","Americas_maize" 
#' @param bndy dataframe with "x" and "y" min and max coordinates
#' @return list with information for the map
#' @rdname MapFromDF
#' @rdname PlotMapLocations
#' @export
GeneralizedCoordinateMapping <- function(wgs,plotname = NA,region=NULL,elevation=T,ptCol="#c85044",ptSize=2,labelpoints=T,resolutionMeters=1000,outFile=NULL,buffersize=0,bndy=NULL,colorByYear=F,kmlFile=NULL,opacityHex="FF",addRange=F,forPub=F,keepOpen=F,axes=T) {
  crs <- "+proj=longlat +datum=WGS84 +no_defs +type=crs" #"+init=epsg:4326" WGS84 
  newmap <- getMap(resolution = "high")
  newmap <- spTransform(newmap, CRS = CRS(projargs = crs))
  focus <- data.frame("x"=((max(wgs$x)-min(wgs$x))*.1)+buffersize,"y"=((max(wgs$y)-min(wgs$y))*.1)+buffersize)
  if (is.null(bndy)) bndy <- data.frame("x"=c(min(wgs$x)-focus$x, max(wgs$x)+focus$x),"y"=c(min(wgs$y)-(focus$y), max(wgs$y)+(focus$y)))
  if (is.null(region)==F) {
    if (is.data.frame(region) & all(colnames(region)%in%c("x","y"))) {bndy <- region
    }else if (region=="Europe_spruce") {bndy <- data.frame("x"=c(-5,35),"y"=c(40,67)) #bndy <- data.frame("x"=c(-5,30),"y"=c(40,62))
    }else if (region=="Europe_all") {bndy <- data.frame("x"=c(-12,46),"y"=c(35,71))
    }else if (region=="N_America_all") {bndy <- data.frame("x"=c(-167,-50),"y"=c(16,80))
    }else if (region=="world") {bndy <- data.frame("x"=c(-170,170),"y"=c(-60,80))
    }else if (region=="Americas_maize") {bndy <- data.frame("x"=c(-130,-30),"y"=c(-55,50))}
  }
  z <- max(4,round(14-ceiling((max(bndy$x)-min(bndy$x)+(max(bndy$y)-min(bndy$y)))),digits = 0))
  if (is.null(region)==F) {if (region=="world") z <- 1}
  correction <- 2-((90-abs(mean(bndy$y)))/90)
  xovery <- (max(bndy$x)-min(bndy$x))/((max(bndy$y)-min(bndy$y))*correction) # correction because globe
  height <- 12
  width <- 12
  if (xovery>=1) {height <- round(12*(((max(bndy$y)-min(bndy$y))*correction)/(max(bndy$x)-min(bndy$x))),0)
  } else {width <- round(12*xovery,0)}
  if (is.null(outFile)==F) {
    if (grepl(pattern = "pdf",x = outFile,ignore.case = T)) pdf(file=outFile,width = width,height=height,useDingbats = F)
    else if (grepl(pattern = "png",x = outFile,ignore.case = T)) png(units = "cm",res = 300,file=outFile,width = 80,height=80*(height/(width)))
    else if (grepl(pattern = "svg",x = outFile,ignore.case = T))svg(file=outFile,width = width)
  }
  par(mar=c(5, 4, 4, 5) + 0.1)
  elev=NA
  if (forPub) {
    cex.main=3
    cex.lab=2
    xlab="Longitude"
    ylab="Latitude"
    yline=par("mgp")[1]-.75
  } else {
    cex.main=1
    cex.lab=1
    xlab="WGS84E"
    ylab="WGS84N"
    yline=par("mgp")[1]
  }
  if (elevation) {
    elev <- get_elev_raster(locations = bndy,clip = "locations", z = z,prj = crs)
    if (abs(ceiling((elev@data@max-elev@data@min)/resolutionMeters))<2) {
      print(paste("Changing resolution from",resolutionMeters,"to",resolutionMeters/10))
      resolutionMeters <- resolutionMeters/10
    }
    plot("", main=plotname,cex.main=cex.main,cex.lab=cex.lab,xlim=bndy$x,ylim=bndy$y,xlab=xlab,ylab="",xaxs="i", yaxs="i")
    if (elev@data@min<0) {plot(elev,col = c(colorRampPalette(colors = c("#0c75a5","#e5f3ff"))(abs(ceiling(elev@data@min/resolutionMeters))),colorRampPalette(colors = c("#f1edda","#316525"))(abs(ceiling(elev@data@max/resolutionMeters)))),add=T)
    } else {plot(elev,col = colorRampPalette(colors = c("#f1edda","#316525"))(abs(ceiling((elev@data@max-elev@data@min)/resolutionMeters))),add=T)}
    plot(newmap,xlim=bndy$x,ylim=bndy$y, asp = 1,add=T,mar=c(0,0,0,0),xpd=F)
  } else {
    plot(newmap, main=plotname,xlim=bndy$x,ylim=bndy$y,cex.main=cex.main,xlab=xlab,ylab="",xaxs="i",cex.lab=cex.lab, yaxs="i")
  }
  if (addRange) {
    rcol <- c("#cd1809","#e59448")
    rtrans <- "33" #"1A"
    intro <- readOGR(dsn= paste0("/Volumes/swarts/lab/Resources/GIS/Layers/chorological_maps_dataset 2/Picea abies/shapefiles/"),layer="Picea_abies_syn_plg_clip",verbose=FALSE)
    iso <- readOGR(dsn= paste0("/Volumes/swarts/lab/Resources/GIS/Layers/chorological_maps_dataset 2/Picea abies/shapefiles/"),layer="Picea_abies_pnt",verbose=FALSE)
    native <- readOGR(dsn= paste0("/Volumes/swarts/lab/Resources/GIS/Layers/chorological_maps_dataset 2/Picea abies/shapefiles/"),layer="Picea_abies_plg_clip",verbose=FALSE)
    plot(intro, border = rcol[2], col=paste(rcol[2],rtrans,sep=""), lwd=1,add=T)
    plot(native, border = rcol[1], col=paste(rcol[1],rtrans,sep=""), lwd=1,add=T) ##935314 #cb9950
    plot(iso, border = rcol[1], col=paste(rcol[1],rtrans,sep=""), lwd=1,add=T)
    legend("topleft",bg = "white",legend = c("native","introduced"),fill=paste(rcol,rtrans,sep=""),border = rcol,title="Norway spruce")
  }
  title(ylab=ylab,line=yline,cex.lab=cex.lab)
  if (colorByYear) {
    yrs <- unique(wgs$collected)[order(unique(wgs$collected))]
    if (length(ptCol)!=length(yrs)) ptCol <- uniqueCols(length(yrs),opacity = opacityHex)
    if (forPub) legend("bottomright",legend = yrs,pt.cex = ptSize,cex = 2.5,pt.bg = ptCol,pch = 21,bg = "white")
    else legend("bottomright",legend = yrs,pt.bg = ptCol,pch = 21,bg = "white")
    ptCol <- ptCol[match(x = wgs$collected,table = yrs,nomatch = NA)]
    ptCol[which(is.na(ptCol))] <- "#000000"
  } else if (nchar(ptCol[1])<9) ptCol <- paste(ptCol,opacityHex,sep="")
  points(wgs$x,wgs$y,bg=ptCol,col="black",pch = 21, cex=ptSize)
  if (labelpoints) text(wgs$x,wgs$y,col="black",font=2,wgs$altname,pos=4,cex=cex.lab,offset=ptSize/5)
  if (is.null(outFile)==F) dev.off()
  par(mar=c(5, 4, 4, 2) + 0.1)
  return(list("wgs"=wgs,"bndy"=bndy,"elev"=elev,"width"=width,"height"=height))
}

PlotLineMap <- function(spat,groupingCol=NULL,main=NULL,pdf=NULL,xlim = c(-110, -100), ylim = c(-53, 50),cex.pt=.2,printgrid=NULL,fourcorners=F) {
  latCol <- grep("^lat",names(spat),ignore.case = T)
  longCol <- grep("^lon",names(spat),ignore.case = T)
  if (length(latCol)!=1 | length(longCol)!=1) {
    print("Couldn't identify unique lat/long columns! Must start with 'lat' and 'lon', case insensistive")
    return(NULL)
  }
  legCol=NULL
  if (is.null(groupingCol) || groupingCol%in%names(spat)==F) {
    print("Can't find groupingCol! Your map will have no colors!")
  } else {
    legCol <- which(names(spat)==groupingCol)
    if (paste(groupingCol,"color",sep="_")%in%names(spat)) {
      colCol <- which(names(spat)==paste(groupingCol,"color",sep="_"))
    } else {
      print("Assigning colors randomly because couldn't find color column based on groupingCol (aka 'groupingCol_color'")
      cols <- cbind(unique(spat[,legCol]),uniqueCols(length(unique(spat[,legCol]))))
      spat[,paste(groupingCol,"color",sep="_")] <- cols[match(spat[,legCol],cols[,1]),2]
    }
  }
  ###Plot by group
  if (is.null(pdf)==F) pdf(pdf,width=4,height=4,useDingbats = F)
  newmap <- getMap(resolution = "less islands")
  #plot(newmap,ylim = c(-50, 50),asp=1.2)
  plot(newmap, xlim = xlim, ylim = ylim, asp = 1)
  if (is.null(printgrid)==F){
    abline(v = seq(-130,-30,printgrid),h = seq(-80,80,printgrid),col="light gray",lwd=.3) #every 10 degrees
    abline(v = seq(-130,-30,5),h = seq(-80,80,5),col="dark gray",lwd=.4) #every 10 degrees
    abline(v = seq(-130,-30,10),h = seq(-80,80,10),col="dark gray",lwd=.5) #every 10 degrees
  } 
  if (fourcorners) maps::map("state", boundary=FALSE, col="gray", add=TRUE) #abline(h=36.9991,v=-109.0452) #four corners
  #abline(h=41.000249,v=-109.030009) #utah, wy,co border
  title(main=main)
  spat <- spat[which(is.na(as.numeric(spat[,latCol]))==F),]
  spat <- spat[which(is.na(as.numeric(spat[,longCol]))==F),]
  if (is.null(legCol)) {col.pt <- "black"
  } else {col.pt <- spat[,colCol]}
  points(x = as.numeric(spat[,longCol]),y = as.numeric(spat[,latCol]),col=col.pt,pch = 19, cex=cex.pt)
  if (is.null(pdf)==F) dev.off()
  return(spat)
}

MapForMapPoint <- function(mo,whichMP) {
  par(xpd=T)
  ###########DEFS#############
  #good colors: c("#72a555","#d36337","#842f2f","#cda448","#8f692e")
  subspecies <- which(names(col.species)%in%mo$speciesid)
  col.species <- col.species[subspecies]
  names.species <- names.species[subspecies]
  dbh <- log(1+mo$dbhcm/10)
  
  #set up a circular plot
  SetUpCircularPlot()
  
  # Add points
  namedT <- mo[which(is.na(mo$treeid)==F),"treeid"]
  #namedT <- paste(substr(as.character(as.numeric(namedT)), 1, nchar(as.character(as.numeric(namedT))[1])-3),substr(as.character(as.numeric(namedT)), nchar(as.character(as.numeric(namedT))[1])-1, nchar(as.character(as.numeric(namedT))[1])),sep="-")
  namedT <- substr(as.character(as.numeric(namedT)), nchar(as.character(as.numeric(namedT))[1])-1, nchar(as.character(as.numeric(namedT))[1]))
  cols <- rep("#ff1493",nrow(mo))
  for (taxa in names(col.species)) {cols[which(mo$speciesid==taxa)] <- col.species[taxa]}
  cols[which(is.na(mo$treeid)==T)] <- paste(cols[which(is.na(mo$treeid)==T)],"CC",sep="")
  bgs <- rep("#808080",nrow(mo))
  for (status in names(col.status)) {bgs[which(mo$status==status)] <- col.status[status]}
  bgs[which(is.na(mo$treeid)==T)] <- paste(bgs[which(is.na(mo$treeid)==T)],"CC",sep="")
  points(with(mo, get.coords(compassaz, compasshd, 0, 0)),pch=21,col=bgs,bg = cols,lwd=2,cex=dbh)
  if (length(namedT)>0) text(with(mo[which(is.na(mo$treeid)==F),], get.coords(compassaz, compasshd, 0, 0)), labels =  namedT,col = bgs[which(is.na(mo$treeid)==F)],adj = c(0,2),cex=.75)
  #legend("bottomright",legend = c(names.species,names(col.status)),pch = 21,pt.bg = c(col.species,"#ff1493",rep("white",length(col.status))),col = c(rep("white",length(names.species)),col.status),pt.lwd = 2)
  legend("bottomright",title = "Species",legend = c(names.species),pch = 21,pt.bg = c(col.species,"#ff1493"),col = "white",pt.lwd = .5,cex=.8)
  legend("topright",title = "Status",legend = names(col.status),pch = 21,pt.bg = rep("white",length(col.status)),col = col.status,pt.lwd = 2,cex=.8)
  mp <- getMapPointCoords(mappointid = whichMP)
  result = tryCatch({
    text(x = -35,y = 25,pos = 4,labels = paste("Plot ID: ",unique(mo$plotid),"\nMappoint ID: ",unique(mo$mappointid),"\nWGS84N: ",format(round(median(mp$wgs84n), 6), nsmall = 6),"\nWGS84N: ",format(round(median(mp$wgs84e), 6), nsmall = 6),"\naltitude: ",format(round(median(mp$alt), 2), nsmall = 2),sep=""),cex=.9)
  }, warning = function(w) {
    text(x = -35,y = 25,pos = 4,labels = paste("Plot ID: ",unique(mo$plotid),"\nMappoint ID: ",unique(mo$mappointid),"\nWGS84N: NA\nWGS84N: NA\naltitude: NA",sep=""),cex=.9)
    print(e)
  }, error = function(e) {
    text(x = -35,y = 25,pos = 4,labels = paste("Plot ID: ",unique(mo$plotid),"\nMappoint ID: ",unique(mo$mappointid),"\nWGS84N: NA\nWGS84N: NA\naltitude: NA",sep=""),cex=.9)
    print(e)
  }, finally = {
    text(x = -35,y = 25,pos = 4,labels = paste("Plot ID: ",unique(mo$plotid),"\nMappoint ID: ",unique(mo$mappointid),"\nWGS84N: NA\nWGS84N: NA\naltitude: NA",sep=""),cex=.9)
  })
}

mapPlot <- function(mo,plotid,outpdf=NULL,alphaHexForUnnamed="CC",namedOnly=F,colorByColumn=NULL,treeLabels=T) {
  if (is.null(outpdf)==F) pdf(file = outpdf,width = 7,height = 7,useDingbats = F)
  par(xpd=T)
  mo$speciesid[is.na(mo$speciesid)] <- "UNK spp."
  subspecies <- which(names(col.species)%in%unique(mo$speciesid))
  col.species <- col.species[subspecies]
  names.species <- names.species[subspecies]
  
  SetUpCircularPlot()
  # Add points
  namedT <- mo[which(is.na(mo$treeid)==F),"treeid"]
  #namedT <- paste(substr(as.character(as.numeric(namedT)), 1, nchar(as.character(as.numeric(namedT))[1])-3),substr(as.character(as.numeric(namedT)), nchar(as.character(as.numeric(namedT))[1])-1, nchar(as.character(as.numeric(namedT))[1])),sep="-")
  #namedT <- substr(as.character(as.numeric(namedT)), nchar(as.character(as.numeric(namedT))[1])-1, nchar(as.character(as.numeric(namedT))[1]))
  namedT <- as.character(as.numeric(namedT))
  if (treeLabels==F) namedT <- numeric(0)
  cols <- rep("#ff1493",nrow(mo))
  for (taxa in names(col.species)) {cols[which(mo$speciesid==taxa)] <- col.species[taxa]}
  pal <- NULL
  lookup <- NULL
  if (is.null(colorByColumn)==F && colorByColumn%in%colnames(mo)) {
    if (is.numeric(mo[,colorByColumn])) {
      heatmap <- getHeatMapCol(mp = mo[,colorByColumn])
      cols <- heatmap$cols
      pal <- heatmap$pal
    } else  {
      lookup <- data.frame(name=unique(mo[,colorByColumn]),cols=uniqueCols(length(unique(mo[,colorByColumn]))))
      colnames(lookup) <- c(colorByColumn,"col")
      cols <- (merge(lookup, mo, by = colorByColumn))$col
    }
  } else {
    cols[which(is.na(mo$treeid)==T)] <- paste(cols[which(is.na(mo$treeid)==T)],alphaHexForUnnamed,sep="") #CC is 80, 4D is 40
  }
  bgs <- rep("#808080",nrow(mo))
  if (is.null(colorByColumn)==F) col.status <- col.status.grey
  for (status in names(col.status)) {bgs[which(mo$status==status)] <- col.status[status]}
  bgs[which(is.na(mo$treeid)==T)] <- paste(bgs[which(is.na(mo$treeid)==T)],alphaHexForUnnamed,sep="")
  if (is.null(mo)==F) {
    dbh <- log(1+mo$dbhcm/10)
    if (namedOnly) {
      points(x = mo[which(is.na(mo$treeid)==F),'x'],y = mo[which(is.na(mo$treeid)==F),'y'],pch=21,col=bgs[which(is.na(mo$treeid)==F)],bg = cols[which(is.na(mo$treeid)==F)],lwd=2,cex=dbh[which(is.na(mo$treeid)==F)])
    } else {
      points(x = mo$x,y = mo$y,pch=21,col=bgs,bg = cols,lwd=2,cex=dbh)
    }
    if (length(namedT)>0) text(with(mo[which(is.na(mo$treeid)==F),], get.coords(compassaz, compasshd, 0, 0)), labels =  namedT,col = bgs[which(is.na(mo$treeid)==F)],adj = c(0,2),cex=.75)
    if (is.null(pal)==F) {
      xrange <- (par("usr")[2]-par("usr")[1])
      yrange <- (par("usr")[4]-par("usr")[3])
      addColorScale(info = mo,heatmap = heatmap,colorTerm = colorByColumn,title=colorByColumn,xl <- c(par("usr")[2]-xrange/6,par("usr")[2]-xrange/7),yl <- c(par("usr")[3]+((yrange/18)),par("usr")[4]-((yrange/6)*5)))
      legend("topright",title = "Status",legend = names(col.status),pch = 21,pt.bg = rep("white",length(col.status)),col = col.status,pt.lwd = 2,cex=.8)
    } else {
      if (is.null(lookup)) legend("bottomright",title = "Species",legend = c(names.species),pch = 21,pt.bg = c(col.species,"#ff1493"),col = "white",pt.lwd = .5,cex=.8)
      else legend("bottomright",title = colorByColumn,legend = lookup[,1],pch = 21,pt.bg = lookup[,2],col = "white",pt.lwd = .5,cex=.8)
      legend("topright",title = "Status",legend = names(col.status),pch = 21,pt.bg = rep("white",length(col.status)),col = col.status,pt.lwd = 2,cex=.8)
    }
    mp <- getMapPointCoords(mappointid = min(mo$mappointid))
    labelString <- paste("Plot ID: ",unique(mo$plotid),"\nCenter Mappoint ID: ",unique(mo$mappointid),"\nWGS84N: ",format(round(median(mp$wgs84n), 6), nsmall = 6),"\nWGS84N: ",format(round(median(mp$wgs84e), 6), nsmall = 6),"\naltitude: ",format(round(median(mp$alt), 2), nsmall = 2),sep="")
    if (namedOnly) {labelString <- paste(labelString,"\nOnly sampled trees!",sep="")}
    text(x = -35,y = 25,pos = 4,labels = labelString,cex=.9)
  }
  if (is.null(outpdf)==F) dev.off()
}

processMappingMO <- function(plotids) {
  print(paste("Processing mappings for plotids: ",plotids))
  plots <- paste(as.numeric(plotids),collapse = " OR p.plotid=")
  mo <- getDataFromDB(query = paste("SELECT p.recorder, m.* FROM plot p INNER JOIN mappedobject m ON p.plotid = m.plotid where p.plotid=",plots,sep=""))
  if (is.null(mo)) return(mo)
  print(paste("Inital mapped objects from DB: ",nrow(mo)))
  cleanmo <- cleanMappoints(mo)
  if (is.null(cleanmo)) return(cleanmo)
  print(paste("Mapped object after cleaning: ",nrow(mo)))
  cleanmo <- consensusCoordinates(mo = cleanmo)
  if (is.null(cleanmo)) return(cleanmo)
  print(paste("Mapped object after generating consensus x,y,z for plot: ",nrow(mo)))
  return(cleanmo)
}

processMapping <- function(osm,mapFile=NULL) {
  plots <- paste(as.numeric(unique(osm$plot)),collapse = " OR p.plotid=")
  mo <- getDataFromDB(query = paste("SELECT p.recorder, m.* FROM plot p INNER JOIN mappedobject m ON p.plotid = m.plotid where p.plotid=",plots,sep=""))
  #col_orig <- c(rep("p",db_col_num[which(db_col_num$table_name=="plot"),2]),rep("m",db_col_num[which(db_col_num$table_name=="mappedobject"),2]))
  cleanmo <- cleanMappoints(mo)
  if (is.null(cleanmo)) return(cleanmo)
  print(paste("Mapped object after cleaning: ",nrow(mo)))
  cleanmo <- consensusCoordinates(mo = cleanmo)
  mat <- match(as.numeric(osm$tree),cleanmo$mo$treeid)
  centerLatLong <- getDataFromDB(query = paste("SELECT p.centerpoint,mp.median_wgs84e as lat,mp.median_wgs84n as long, mp.median_alt as alt FROM plot p INNER JOIN mappoint mp on p.centerpoint = mp.mappointid where p.plotid in (",plots,")",sep=""))
  cleanmo$mo$lat <- centerLatLong$lat
  cleanmo$mo$lat <- cleanmo$mo$lat+(cleanmo$mo$x/10000)
  cleanmo$mo$long <- centerLatLong$long
  cleanmo$mo$long <- cleanmo$mo$long+(cleanmo$mo$y/10000)
  cleanmo$mo$alt <- centerLatLong$alt
  cleanmo$mo$alt <- cleanmo$mo$alt + cleanmo$mo$z
  osm[,c("status","dbhcm","mapper","speciesid","x","y","z","lat","long","alt")] <- cleanmo$mo[mat,c("status","dbhcm","mapper","speciesid","x","y","z","lat","long","alt")]
  return(list(osm=osm,mo=cleanmo))
}

addSFToMOFromCenter <- function(mo) {
  if ("geometry"%in%colnames(mo$mo)) return(mo)
  cur <- mo$mo[,c("mappedobjectid","plotid","treeid","status","dbhcm","speciesid","x","y","z","lat","long","alt")]
  if (length(as.character(cur$tree[1]))<8) cur$tree <- cur$treeid
  cur$treeid <- paste(str_pad(cur$plotid,width = 5,side = "left",pad = "0"),str_pad(cur$tree,width = 3,side = "left",pad = "0"),sep="")
  cur$treeid[which(is.na(cur$tree))] <- NA
  pts <- st_sf(data.frame(layerId=cur$mappedobjectid),geometry=lapply(split(cur[,c("x","y","z")],seq(NROW(cur))),function(x){st_point(as.numeric(x[,1:3]),dim="XYZ")}))
  st_crs(pts) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs" #EPSG:3857" pseudo-mercator/google maps
  cur <- cbind(cur,pts)
  #plot(cur$geometry)
  mo$mo <- cur
  return(mo)
}

addPlotData <- function(osm,addCols=c("slope","aspect","mapper","coreaspect","treeid","samplingheightcm","personid"),outDir=NULL) {
  plots <- paste(as.numeric(unique(osm$plot)),collapse = " OR c.plotid=")
  query <- paste("SELECT * FROM plot p INNER JOIN tree t ON p.plotid = t.plotid RIGHT JOIN mappedobject m ON t.treeid = m.treeid INNER JOIN core c ON t.treeid=c.treeid where p.plotid=",plots,sep="")
  mo <- getDataFromDB(query = query)
  #col_orig <- c(rep("p",db_col_num[which(db_col_num$table_name=="plot"),2]),rep("t",db_col_num[which(db_col_num$table_name=="tree"),2]),rep("m",db_col_num[which(db_col_num$table_name=="mappedobject"),2]),rep("c",db_col_num[which(db_col_num$table_name=="core"),2]))
  #bindCol <- paste(col_orig,colnames(dbRaw),sep = "_")
  #keepCols <- which(bindCol%in%c("p_plotid","t_treeid","c_coreid","p_slope","p_aspect","p_altitude","t_collected.1","p_country","p_recorder","p_person_dna","p_altname","m_mappointid","m_mapper","m_sampletime","m_status","m_dbhcm.1","m_speciesid.1","m_compasssd","m_compasshd","m_compassh","m_compassdeg","m_compassaz","c_personid","c_samplingheightcm","c_coreaspect")==T)
  #db <- dbRaw[,keepCols]
  #colnames(db)[which(colnames(db)=="personid")] <- "coreperson"
  #colnames(db)[which(colnames(db)=="dbhcm.1")] <- "dbhcm"
  #colnames(db)[which(colnames(db)=="speciesid.1")] <- "speciesid"
  #colnames(db)[which(colnames(db)=="collected.1")] <- "tree_collected"
  mat <- match(osm$coreid,mo$coreid) 
  osm[,addCols] <- mo[mat,addCols]
  return(osm)
}

potentialDups <- function(mo,dbh = 8.3,hd = .3,az = 10.1,outdir) {
  close <- list()
  for (plot in unique(mo$plotid)) {
    for (i in which(mo$plotid==plot)) {
      if (TRUE%in%is.na(mo[i,c("dbhcm","compasshd","compassaz")])) {
        print(paste("Problem! Skip",mo[i,"mappedobjectid"]))
        next
      }
      currClose <- i
      for (j in (i+1):max(which(mo$plotid==plot))) {
        if (TRUE%in%is.na(mo[j,c("dbhcm","compasshd","compassaz")])) {
          print(paste("Problem! Skip",mo[j,"mappedobjectid"]))
          next
        }
        if ((mo$dbhcm[i] < mo$dbhcm[j]+dbh) & (mo$dbhcm[i] > mo$dbhcm[j]-dbh) & (mo$compasshd[i] < mo$compasshd[j]+hd) & (mo$compasshd[i] > mo$compasshd[j]-hd) & (mo$compassaz[i] < mo$compassaz[j]+az) & (mo$compassaz[i] > mo$compassaz[j]-az)) currClose <- c(currClose,j)
      }
      #print(currClose)
      if (length(currClose)>1) close[[as.character(i)]] <- currClose
    }
  }
  for (d in close) {
    print(mo[d,])
    write.table(x=mo[d,],file = paste(outdir,"/possibleDupMappedObjects.txt",sep = ""),append = T)
  }
  return(close)
}

get.coords <- function(a, d, x0, y0) {
  a <- ifelse(a <= 90, 90 - a, 450 - a)
  data.frame(x = x0 + d * cos(a / 180 * pi), 
             y = y0+ d * sin(a / 180 * pi))
}

get.coords.TRG <- function(coords, x0, y0, z0=NULL) {
  if (nrow(coords)>1) {
    printf("Input dataframe coords must only have one row!")
    return(F)
  }
  coords$compassaz <- ifelse(coords$compassaz <= 90, 90 - coords$compassaz, 450 - coords$compassaz)
  x <- x0 + coords$compasshd * cos(coords$compassaz / 180 * pi)
  y <- y0+ coords$compasshd * sin(coords$compassaz / 180 * pi)
  z <- NA
  if (is.null(z0)==F & is.na(coords$compassdeg)==F) {
    zOff <- round(tan(coords$compassdeg*(pi/180))*coords$compasshd,digits = 1)
    if ((zOff==coords$compassh)==F) #printf("Compassh value offsets by %g. Adjusted height accordingly\n",zOff-coords$compassh)
      z <- z0+zOff
  }
  return(data.frame(x=x,y=y,z=z))
}

#get distance matrix for mappedobjects with xyz data
get.xyz.dist <- function(osm,mo,plot,twod=F) {
  if (F%in%c("x","y","z")%in%names(mo)) {
    printf("Input dataframe coords must contain x,y,z information!")
    return(F)
  }
  if (nrow(mo)<2) {
    printf("Input dataframe coords must have more than one row!")
    return(F)
  }
  coords <- mo[which(as.numeric(mo[,which(grepl(pattern = "plot",x = names(mo),ignore.case = T))])==as.numeric(plot)),]
  coords <- coords[order(coords$treeid),]
  coords <- coords[match(unique(as.numeric(osm$tree)),table = as.numeric(coords$treeid)),]
  if (twod) {coords$z <- rep(1,nrow(coords))}
  d <- dist.xyz(a = as.matrix(coords[,c("x","y","z")]))
  colnames(d) <- formatC(coords$treeid, width = 2, format = "d", flag = "0")
  rownames(d) <- formatC(coords$treeid, width = 2, format = "d", flag = "0")
  return(d)
}

#get covariance structure from distance matrix
get.xyz.K <- function(mo,plot,namesCol) {
  d <- get.xyz.dist(mo,plot,namesCol)
  d <- d/max(as.numeric(d))
  d <- 1-d
  return(d)
}

cleanMappoints <- function(allMO) {
  cleanMO <- allMO[0, ]
  for (mp in unique(allMO$mappointid)) {
    currPlot <- allMO$plotid[which(allMO$mappointid==mp)[1]]
    mo <- allMO[which(allMO$mappointid==mp),]
    mo <- mo[which(duplicated(mo[,which(colnames(mo)%in%c("mappedobjectid","comment","sampletime")==F)])==F),]
    if ("ignore"%in%names(mo)) mo <- mo[which(mo$ignore%in%c("1")==F),]
    #dups <- potentialDups(mo)
    if (nrow(mo)<1) next
    if (F%in%is.na(mo$compasshd)==F) next
    cleanMO <- rbind(cleanMO,mo)
  }
  cleanMO$treeid <- as.numeric(apply(MARGIN = 1,X = as.matrix(cleanMO$treeid),FUN = substring,first=6,last=8))
  if (nrow(cleanMO)<1) return(NULL)
  return(cleanMO)
}

plotEachMappoint <- function(cleanmo,outdir,datasetName) {
  for (mp in unique(cleanmo$mappointid)) {
    currPlot <- cleanmo$plotid[which(cleanmo$mappointid==mp)[1]]
    mo <- cleanmo[which(cleanmo$mappointid==mp),]
    mo <- mo[which(duplicated(mo[,which(colnames(mo)%in%c("mappedobjectid","comment","sampletime")==F)])==F),]
    if ("ignore"%in%names(mo)) mo <- mo[which(mo$ignore==F),]
    if (nrow(mo)<1) next
    if (F%in%is.na(mo$compasshd)==F) next
    if (dir.exists(paste(outdir,"/",datasetName,"_PlotMaps",sep=""))==F) dir.create(paste(outdir,"/",datasetName,"_PlotMaps",sep=""))
    pdf(file = paste(outdir,"/",datasetName,"_PlotMaps/",currPlot,"_MP",mp,"_PlotMap2D.pdf",sep=""),width = 7,height = 7,useDingbats = F)
    MapForMapPoint(mo,mp)
    dev.off()
  }
  return(cleanMO)
}

#' Generate naive ringwidth values based on simple mean for tree/year
#' @param osm TRG parse ringwidth info in dataframe
#' @return dataframe with only tree/year information
#' @export
SimpleMeanForTrees <- function(osm) {
  osm$treeid <- str_sub(osm$coreid,1,8)
  osm$treeYear <- as.factor(paste(osm$treeid,osm$year,sep="_"))
  return(osm %>% group_by(plot,tree,treeid,treeYear,year) %>% summarise_at(vars(width), funs(mean(., na.rm=TRUE))))
}

#' Generate and plot MDS (metric) using cmdscale [stats]
#' @param dist this can be either a square dataframe/matrix representing a distance matrix generated elsewhere or a filename pointing to a TASSEL generated IBS distance matrix
#' @param info either a tab delimited file with headers where the samples must be in a column called "Taxon" that matches the rownames of the distance matrix or a data.frame with the same properties
#' @param group header from infoFile will be used for grouping data. Will automatically look for a column called "'groupCol'_color" or will autogenerate colors
#' @param k optional number of coordinates to model
#' @param main optional title for plot
#' @param ptSize optional, will output plots to pdf
#' @param pdfFile optional, will output plots to pdf
#' @param inner.transparency optional, hex color for transparency of fill (e.g., "80" is 50%). Defaults to "FF", 100%
#' @param outer.transparency optional, hex color for transparency of line (e.g., "80" is 50%). Defaults to "FF", 100%
#' @return MDS fit object
#' @export
MDS <- function(dist,info,group,group.line=NULL,heat=F,heatbg=F,k=2,main=NULL,ptSize=.6,relLineWidth=1.2,pdfFile=NULL,inner.transparency="FF",outer.transparency="FF",pdf.width=6,pdf.height=5,includeLegend=T,forpresentation=F,highlightTaxa=NULL,pairedOnly=F) {
  if (is.character(dist) && file.exists(dist)) {
    distances <- read.table(dist,sep="\t",header=F,skip=5,row.names=1,as.is=T) #hamming (IBS) distance matrix where only rownames are labelled (TASSEL format)
    if (nrow(distances)!=ncol(distances)) {printf("Cannot correctly read in %s! Make sure it's a valid tassel IBS distance matrix",dist)}
  } else if (nrow(dist)==ncol(dist)) {distances <- dist
  } else {
    printf("\nNon-valid entry for dist variable! Must be either a tassel IBS distance matrix file or a square data.frame or matrix")
    return(NULL)
  }
  if (is.character(info) && file.exists(info)) {col <- read.table(info,header=T,sep="\t",as.is=T)
  } else {col <- info}
  if ("fullsamplename"%in%tolower(names(col))==T) {
    names(col)[which(tolower(names(col))=="fullsamplename")] <- "Taxon"
    print("FullSampleName changed to Taxon in info file")
  }
  if ("taxa"%in%tolower(names(col))==T) {
    names(col)[which(tolower(names(col))=="taxa")] <- "Taxon"
    print("Taxa changed to Taxon in info file")
  }
  if ("taxon"%in%tolower(names(col))==F) {
    printf("\nProblem with info! Make sure there is a column in info called Taxon!")
    return(NULL)
  }
  taxonCol <- which("taxon"==tolower(names(col)))
  if (group%in%names(col)==F) {
    printf("\nProblem with info! Your group variable is not a viable header")
    return(NULL)
  }
  if (is.null(group.line)==F && group.line%in%names(col)==F) {
    printf("\nProblem with info! Your group.line variable is not a viable header")
    group.line <- NULL
  }
  if (nrow(col)<1) {
    printf("\nNo rows in info file!")
    return(NULL)
  }
  #if (is.null(group)==F) col[,paste(group,"_color",sep="")] <- unlist(lapply(col[,paste(group,"_color",sep="")],y=inner.transparency,function(x,y) {return(paste(str_sub(string = x,1,7),y,sep=""))}))
  #if (is.null(group.line)==F) col[,paste(group.line,"_color",sep="")] <- unlist(lapply(col[,paste(group.line,"_color",sep="")],y=outer.transparency,function(x,y) {return(paste(str_sub(string = x,1,7),y,sep=""))}))
  #str(col)
  rem.na <- apply(distances, 2, function(x) length(which(is.na(x)==T)))
  keep <- which(rem.na==0 | rem.na<max(rem.na)) #
  while (length(keep)<nrow(distances)) {
    if ((nrow(distances)-length(keep))>0) {
      printf("\nRemoving samples from distance matrix driving NA values: %s",paste(rownames(distances)[which(1:nrow(distances)%in%keep==F)],collapse=","))
      distances <- distances[keep,keep]
    }
    rem.na <- apply(distances, 2, function(x) length(which(is.na(x)==T)))
    keep <- which(rem.na==0 | rem.na<max(rem.na)) #
  }
  distances <- distances[which(rownames(distances)%in%col[,taxonCol]==T),which(rownames(distances)%in%col[,taxonCol]==T)]
  if (nrow(distances)<1) {
    printf("\nNo rows left in distances after filtering based on infofile!")
    return(NULL)
  }
  col <- col[which(col[,taxonCol]%in%rownames(distances)),]
  if (pairedOnly) {
    col <- as.data.frame(col %>% group_by(treeid) %>% filter(n()>1))
    distances <- distances[which(rownames(distances)%in%col[,taxonCol]==T),which(rownames(distances)%in%col[,taxonCol]==T)]
  }
  d <- as.matrix(distances)
  fit <- cmdscale(d,k=k,eig=T) # k is the number of dim
  #fit # view results
  if (heat==T) {
    heatmap <- getHeatMapCol(mp = col[,group],color.range = c("blue","yellow","red"))
    color.acc <- heatmap$cols
  }else {
    legend <- sort(levels(factor(col[,group][match(rownames(fit$points),col[,taxonCol],)],levels = unique(col[,group]))))
    #legend
    if (paste(group,"color",sep="_")%in%names(col)==F) {
      color.legend <- uniqueCols(N = length(legend),opacity = inner.transparency)
      col[,paste(group,"color",sep="_")] <- color.legend[match(as.character(col[,group]),as.character(legend))]
    } else {
      color.legend <- col[,paste(group,"color",sep="_")][match(legend,col[,group])]
    }
    color.acc <- col[,paste(group,"color",sep="_")][match(rownames(fit$points[]),col[,taxonCol],)]
  }
  legend.line <- NULL
  if (is.null(group.line)==F && group.line!=group) {
    if (heatbg==T) {
      heatmap.line <- getHeatMapCol(col[,group.line],color.range = c("blue","yellow","red"))
      color.line <- heatmap$cols
    }else {
      legend.line <- levels(factor(col[,group.line][match(rownames(fit$points[]),col[,taxonCol],)],levels = unique(col[,group.line])))
      #legend
      if (paste(group.line,"color",sep="_")%in%names(col)==F) {
        color.line.legend <- uniqueCols(length(legend.line),opacity = outer.transparency)
        col[,paste(group.line,"color",sep="_")] <- color.line.legend[match(as.character(col[,group.line]),as.character(legend.line))]
      } else {
        color.line.legend <- col[,paste(group.line,"color",sep="_")][match(legend.line,col[,group.line])]
      }
      color.line <- col[,paste(group.line,"color",sep="_")][match(rownames(fit$points[]),col[,taxonCol],)]
    }
  } else color.line <- color.acc
  
  # plot solution
  if (is.null(pdfFile)==F) pdf(pdfFile,useDingbats = F,width = pdf.width,height = pdf.height)
  #if (forpresentation)
  for (i in 1:(k-1)) {
    j <- i+1
    x <- fit$points[,i]
    y <- fit$points[,j]
    if (is.null(main)) main <- paste("Metric MDS by",group)
    plot(x, y, xlab=paste("Coordinate ",i," (",round((fit$eig[i]/sum(fit$eig))*100,1),"%)",sep=""), ylab=paste("Coordinate ",j," (",round((fit$eig[j]/sum(fit$eig))*100,1),"%)",sep=""),main=main,col=paste(str_sub(color.line,1,7),outer.transparency,sep=""),bg=paste(str_sub(color.acc,1,7),inner.transparency,sep=""),pch=21,cex=ptSize,lwd = ptSize*relLineWidth,xlim=c(min(fit$points[,1]),max(fit$points[,1])+(max(fit$points[,1])-min(fit$points[,1]))*.3))
    #text(x, y,rownames(fit$points),cex=.5)
    if (is.null(highlightTaxa)==F) {
      high <- which(rownames(fit$points)%in%highlightTaxa)
      points(x[high], y[high],col="black",bg=color.acc[high],pch=21,cex=ptSize,lwd = ptSize*relLineWidth)
    }
    if (heat==T) {
      xrange <- (par("usr")[2]-par("usr")[1])
      yrange <- (par("usr")[4]-par("usr")[3])
      if (includeLegend) addColorScale(info = col,heatmap = heatmap,colorTerm = group,xl <- c(par("usr")[2]-xrange/5,par("usr")[2]-xrange/6),yl <- c(par("usr")[3]+yrange/4,par("usr")[4]-yrange/4))
    }else {if (includeLegend) legend(x = "bottomright",legend = legend[order(legend)],col =  "white",pt.bg =paste(str_sub(color.legend[order(legend)],1,7),inner.transparency,sep=""),pch=21,cex=.75,pt.cex = 2)}
    if (is.null(legend.line)==F) {
      if (heatbg==T) {
        xrange <- (par("usr")[2]-par("usr")[1])
        yrange <- (par("usr")[4]-par("usr")[3])
        if (includeLegend) addColorScale(info = col,heatmap = heatmap,colorTerm = group,xl <- c(par("usr")[2]-xrange/5,par("usr")[2]-xrange/6),yl <- c(par("usr")[3]+yrange/4,par("usr")[4]-yrange/4))
      }else {if (includeLegend) legend(x = "topright",legend = legend.line[order(legend.line)],col =  color.line.legend[order(legend.line)],pt.bg ="white",pch=21,cex=.75,pt.cex = 1.5,pt.lwd = 2)}
    }
  }
  if (is.null(pdfFile)==F) dev.off()
  return(list(fit=fit,info=col,d=distances))
}

#Colors
putInRGBSpace <- function(df,Rvar=NULL,Gvar=NULL,Bvar=NULL,intermediateNull=0,opacityHex="80") {
  if (is.null(Rvar)==F) df <- df[which(is.na(df[,Rvar])==F),]
  if (is.null(Gvar)==F) df <- df[which(is.na(df[,Gvar])==F),]
  if (is.null(Bvar)==F) df <- df[which(is.na(df[,Bvar])==F),]
  if (nrow(df)==0) {
    print("Nothing left after filtering for NAs!")
    return(NULL)
  }
  if (is.null(Rvar)) {r <- rep(intermediateNull,nrow(df))
  }else {r <- ((df[,Rvar]-min(df[,Rvar]))/(max(df[,Rvar])-min(df[,Rvar])))*255}
  if (is.null(Gvar)) {g <- rep(intermediateNull,nrow(df))
  }else {g <- ((df[,Gvar]-min(df[,Gvar]))/(max(df[,Gvar])-min(df[,Gvar])))*255}
  if (is.null(Bvar)) {b <- rep(intermediateNull,nrow(df))
  }else {b <- ((df[,Bvar]-min(df[,Bvar]))/(max(df[,Bvar])-min(df[,Bvar])))*255}
  print(rgb(r,g,b,maxColorValue = 255))
  return(paste(rgb(r,g,b,maxColorValue = 255),opacityHex,sep=""))
}

#' get heatmap colors for a vector of numbers. Breaks generated by rounding, so if very small numbers, breaks can be increased by multiplying by a scalar
#' @param mp the vector of numbers
#' @param color.range the range of colors to generate the palette from
#' @return list where cols are the colors corresponding to the input vectors and pal is the colorRampPalette object
#' @export
getHeatMapCol <- function(mp,color.range=c("blue","green","yellow","orange","red")) {
  breaks <- round(mp,0)
  div <- 100
  while (length(seq(min(breaks,na.rm = T),max(breaks,na.rm = T),1))<3) {
    breaks <- round(mp*div,0)
    div <- div*10
  }
  if (all(is.na(breaks))) return(NULL)
  pal <- colorRampPalette(bias = 1,alpha=T,colors = color.range,interpolate = "linear")
  cols <- data.frame(breaks=seq(min(breaks,na.rm = T),max(breaks,na.rm = T),1),col=pal((max(breaks,na.rm = T)-min(breaks,na.rm = T)+1)),stringsAsFactors = F)
  return(list(cols=cols$col[match(breaks,cols$breaks)],pal=pal))
}

#' get N unique colors from the brewer palatte (if less than 72) or from rainbow if more
#' @param N number of unique colors
#' @param opacity in hex notation (https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4). Defaults to 50%
#' @return vector of unique hex colors
#' @export
uniqueCols <- function(N,opacity="80") {
  return(paste(iwanthue(N, hmin = 0, hmax = 360, cmin = 0, cmax = 180, lmin = 0, lmax = 100, plot = F, random = FALSE),opacity,sep=""))
  #if (N<75) {
    #qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    #col_max = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
   # iwanthue(N, hmin = 0, hmax = 360, cmin = 0, cmax = 180, lmin = 0, lmax = 100, plot = TRUE, random = FALSE)
    #return(paste(sample(col_max, N),opacity,sep=""))
  #} else {
  #  return(paste(substring(text = rainbow(n = N,alpha = 1),1,7),opacity,sep=""))
  #}
}

# Function to plot color bar
color.bar <- function(heatmap, min,max=-min, nticks=3, ticks=seq(min, max, len=nticks), title='',sideaxt=4) {
  lut <- heatmap$pal(100)
  scale = (length(lut)-1)/(max-min)
  #dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main='')
  title(main = title,adj=0,cex.main=.8,line = .5,xpd=T)
  axis(sideaxt, round(ticks,2), las=1,line = -.9,tick = F,cex.axis=.8,xpd=T)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

addColorScale <- function(info,heatmap,colorTerm,xl=NULL,yl=NULL,sideaxt=4,title=NULL) {
  xrange <- (par("usr")[2]-par("usr")[1])
  yrange <- (par("usr")[4]-par("usr")[3])
  if (is.null(xl)) xl <- c(par("usr")[2]-xrange/10,par("usr")[2]-xrange/20)
  if (is.null(yl)) yl <- c(par("usr")[3]+yrange/4,par("usr")[4]-yrange/4)
  if (is.null(title)) title <- colorTerm
  Hmisc::subplot(fun = color.bar(heatmap = heatmap,min = min(info[,colorTerm],na.rm = T),title=title,max = max(info[,colorTerm],na.rm = T),sideaxt = sideaxt),hadj = 1,vadj = 0,x = xl,y = yl)
}

#Setup plotting devices for different types of plots
SetUpCircularPlot <- function() {
  # Set up plotting device
  plot.new()
  par(mar=c(1, 1, 1, 2), oma=rep(0,4))
  plot.window(xlim=c(-30, 30), ylim=c(-30, 30), asp=1)
  xrange <- par("usr")[2]-par("usr")[1]
  yrange <- par("usr")[4]-par("usr")[3]
  #set up scalebar
  text(x = c(-29+xrange/100),y = c(-30+yrange/100),labels=c("2m"),cex=1)
  lines(x=c(-30+xrange/100,-30+xrange/100+2),y=c(-27, -27),lwd=2)
  lines(x=c(-30+xrange/100,-30+xrange/100),y=c(-27-yrange/200, -27+yrange/200),lwd=2)
  lines(x=c(-30+xrange/100+2,-30+xrange/100+2),y=c(-27-yrange/200, -27+yrange/200),lwd=2)
  #draw circles
  plotrix::draw.circle(0,0,11,lty = 2,border="grey")
  plotrix::draw.circle(0,0,21,lty = 1,border="black")
  plotrix::draw.circle(0,0,26,lty = 2,border="red")
  
  ###THIS IS THE OLD VERSION AND DOESN"T WORK ANYMORE. DON"T KNOW WHY
  #lines(get.coords(seq(0, 360, length.out=1000), 11, 0, 0),lty = 2,col="grey")
  #lines(get.coords(seq(0, 450, length.out=1000), 21, 0, 0),lty = 1,col="black")
  #lines(get.coords(seq(0, 450, length.out=1000), 26, 0, 0),lty = 2,col="red")
  
  # 45-degree lines
  apply(get.coords(seq(15,360, 15), 28, 0, 0), 1, 
        function(x) lines(rbind(x, c(0, 0)), lwd=2))
  
  # Plot white curves over black curves and add text
  sapply(c(11,21,26), function(x) {
    txt <- paste0(x, 'm')
    w <- strwidth(txt, cex=0.9)/2
    a <- atan(w/x)/pi*180
    lines(get.coords(seq(-a, a, length=30), x, 0, 0),lwd=2.5, col='white')
    lines(x = c(0,0),y = c(x-1,x+1),lwd=2.5, col='white')
    text(0, x, txt, cex=0.8)
  })
  text(x = c(0,30,0,-30),y = c(30,0,-30,0),labels=c("N","E","S","W"),cex=1.8)
  return(TRUE)
}

SetUpSquarePlot <- function(mo,mpr,plot) {
  mprcurr <- mpr[which(mpr$plotid==plot),]
  if (c("x")%in%names(mo)==F) {
    printf("Might be more efficient to run consensusCoordinates before MapPoints! Running now\n")
    map <- consensusCoordinates(mo)
  }
  # Set up plotting device
  plot.new()
  par(mar=c(2, 0, 0, 0), oma=rep(0, 4))
  plot.window(xlim=c(min(mo$x)-4,max(mo$x)+1), ylim=c(min(mo$y)-1,max(mo$y)+4), asp=1)
  text(0,0,unique(mprcurr$centerpoint))
  for (p in which(mprcurr$mappointid==unique(mprcurr$centerpoint))) {
    text(get.coords(a = mprcurr[p,"compassaz"],d=mprcurr[p,"compasshd"],0,0),labels = mprcurr[p,"mappointtarget"])
  }
  text(x = c(0),y = c(max(mo$y)+2),labels=c("N"),cex=1.8)
  text(x = c(min(mo$x)-4),y = c(max(mo$y)+4),pos = 4,labels = paste("Plot ID: ",plot,sep=""),cex=1)
  text(x = c(min(mo$x)-4),y = c(min(mo$y)-1),labels=c("2m"),cex=1)
  lines(x=c(min(mo$x),min(mo$x)+2),y=c(min(mo$y)-1,min(mo$y)-1),lwd=2)
  return(TRUE)
}

MapPoints <- function(map,fillcol="#00000000",outlinecol="#00000000",lwd=2,txt=NULL) {
  if (nrow(map)<1) return(NULL)
  if (c("x")%in%names(map)==F) {
    printf("Must run consensusCoordinates before MapPoints! Might be more efficient to do it once. Running now\n")
    map <- consensusCoordinates(map)
  }
  dbh <- log(1+map$dbhcm/10)
  points(x = map$x,y = map$y,pch=21,col=outlinecol,bg = fillcol,lwd=lwd,cex=dbh)
  if (is.null(txt)==F & length(txt)>0) text(x = map$x,y = map$y,labels = txt,pch=21,col=fillcol,adj = c(0,2),cex=.5)
  return(map)
}

#Expects inputs based on haglof data in trgdb
getMPRelations <- function(mo) {
  #if (length(unique(mo$plotid))<1) return(NULL)
  query <-  paste("SELECT p.centerpoint, p.ne, p.se, p.nw, p.sw, m.*, mp.transponderheightcm FROM plot p INNER JOIN mappedobject m ON p.plotid = m.plotid  INNER JOIN mappoint mp on m.mappointid = mp.mappointid where m.plotid in (",paste(unique(mo$plotid),collapse = ", "),") and m.mappointtarget IS NOT NULL ",sep="")
  mpr <- getDataFromDB(query = query)
  return(mpr)
}
consensusCoordinates <- function(mo) {
  mpr <- getMPRelations(mo)
  mo$x <- numeric(nrow(mo))
  mo$y <- numeric(nrow(mo))
  mo$z <- numeric(nrow(mo))
  if (is.null(mpr) || nrow(mpr)<1) {
    printf("No alternate mapping points included. Assume all mappings coming from 0,0,0\n")
    for (i in 1:nrow(mo)) {
      mo[i,c("x","y","z")] <- get.coords.TRG(coords = mo[i,],x0 = 0,y0 = 0,z0 = 0)
    }
    return(list(mo=mo,mpr=mpr))
  }
  for (plot in unique(mpr$plotid)) {
    mcurr <- mpr[which(mpr$plotid==plot),]
    center <- unique(mcurr$centerpoint)
    for (i in which(mo$mappointid==center)) {
      mo[i,c("x","y","z")] <- get.coords.TRG(coords = mo[i,],x0 = 0,y0 = 0,z0 = 0)
    }
    for (altp in mcurr[1,2:5]) {
      curralt <- mcurr[which(mcurr$mappointtarget%in%altp==T),]
      if (nrow(curralt)>1) {
        printf("More than one measurement from center for centerpoint %f and alternate point %f",center,altp)
        return(F)
      }
      if (nrow(curralt)>0) {
        st <- get.coords.TRG(curralt,0,0,0)
        for (i in which(mo$mappointid==altp)) {
          mo[i,c("x","y","z")] <- get.coords.TRG(coords = mo[i,],x0 = st$x,y0 = st$y,z0 = st$z)
        }
      }
    }
  }
  return(list(mo=mo,mpr=mpr))
}
uniqueCols <- function(N,opacity="80") {
  if (N<75) {
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_max = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    return(paste(sample(col_max, N),opacity,sep=""))
  } else {
    return(paste(substring(text = rainbow(n = N,alpha = 1),1,7),opacity,sep=""))
  }
}
potentialDupsByDist <- function(mo,plot,square=F,mpr=F,quantile=1,main=NULL) {
  if (c("x")%in%names(mo)==F) {
    printf("Must run consensusCoordinates before MapPoints! Might be more efficient to do it once. Running now\n")
    mo <- consensusCoordinates(mo)
  }
  mpVals <- unique(mo$mappointid)[order(unique(mo$mappointid))]
  mpCols <- c("#b8477a","#778d2a","#a05740","#626c8e","#285225","#793ea1","#d3582d","#8eb4ff","#61544e","#e086ff")[1:length(mpVals)]
  matchint <- match(mo$mappointid,table = mpVals)
  mo$outlinecol <- mpCols[matchint]
  
  m <- as.matrix(mo[,c("x","y","z")])
  rownames(m) <- mo$mappedobjectid
  mnorm <- scale(m, center=T, scale=colSums(m))
  d <- dist(m,method = "euclidean")
  distances <- as.matrix(d)
  diag(distances) <- NA
  rownames(distances) <- labels(d)
  colnames(distances) <- labels(d)
  quants <- quantile(x = d,na.rm = T,probs = c(quantile, NA)/100)
  sub <- unique(d[which((d<quants[paste(quantile,"%",sep="")])==T)])
  #unique cols
  cols <- uniqueCols(length(sub))
  smo <- mo[0,c("mappedobjectid","plotid","mappointid","Combined","status","dbhcm","x","y","z","outlinecol")]
  smo$dist <- numeric(0)
  smo$col <- character(0)
  for (s in sub) {
    pr <- rownames(which(distances==s, arr.ind = TRUE))
    cur <- mo[which(mo$mappedobjectid%in%pr),]
    cur$dist <- rep(s,nrow(cur))
    if (is.na(sd(cur$dbhcm)) | sd(cur$dbhcm)<(min(cur$dbhcm)*.2)) {
      add <- cbind(cur[,c("mappedobjectid","plotid","mappointid","Combined","status","dbhcm","x","y","z","outlinecol")],dist=s,col=cols[which(sub==s)])
      matchR <- which(do.call(paste0, smo[,-which(names(smo)%in%c("col"))]) %in% do.call(paste0, add[,-which(names(add)%in%c("col"))])==T)
      if (length(matchR)==0 || diff(matchR)!=1) {
        smo <- rbind(smo,add)
      }
    }
  }
  smo$col <- as.character(levels(smo$col))[smo$col]
  if (square==F) {
    SetUpCircularPlot()
  } else {SetUpSquarePlot(mo,mpr,whichPlot)}
  text(x = c(min(mo$x)-4),y = c(max(mo$y)+2),pos = 4,labels = paste("Potential Dups",main,sep="\n"),cex=1)
  uniq <- which(do.call(paste0, mo[,names(smo)[-which(names(smo)%in%c("dist","col"))]]) %in% do.call(paste0, smo[,-which(names(smo)%in%c("dist","col"))])==T)
  MapPoints(mo[-which(1:nrow(mo)%in%uniq),],fillcol="#0000000D",outlinecol = mo[-which(1:nrow(mo)%in%uniq),"outlinecol"],lwd=2)
  MapPoints(smo,fillcol=smo$col,lwd=2,outlinecol = smo$outlinecol,txt=smo$Combined)
  return(smo)
}
subsetForMP <- function(mo,whichMP) {
  return(mo[which(mo$mappointid%in%whichMP),])
}
subsetForPlot <- function(mo,whichPlot) {
  return(mo[which(mo$plotid%in%whichPlot),])
}

checkUniqueNamedTrees <- function(mo,file=NULL) {
  named <- mo$treeid[which(is.na(mo$treeid)==F)][order(as.numeric(mo$treeid[which(is.na(mo$treeid)==F)]))]
  dups <- named[which(duplicated(named)==T)]
  uniq <- unique(named)[which(unique(as.numeric(named))%in%(1:max(as.numeric(named)))==F)]
  if (length(dups)>0) {
    print(paste("duplicated trees:",paste(dups,collapse = ", ")))
  } else if (length(uniq)>0) {
    print(paste("missing trees:",paste(uniq,collapse = ", ")))
  } else if (length(unique(named))==length(1:max(as.numeric(substring(named,6,8))))) {
    print(paste(max(as.numeric(substring(named,6,8))),"trees unique and accounted for!"))
  }
}


convertToKML <- function(df = NULL,csvFile = NULL,outDir=NULL,outBase="out",idCol=NULL,keepCol=all,crsString="+proj=longlat +datum=WGS84 +no_defs +type=crs") { #WGS84 "+init=epsg:4326"
  if (is.null(df) & is.null(csvFile)==F) df <- read.csv(csvFile)
  colnames(df) <- tolower(colnames(df))
  if (all(c("latitude","longitude")%in%colnames(df))) {coordNames <- c("latitude","longitude")
  } else if (all(c("lat","long")%in%colnames(df))) {coordNames <- c("lat","long")
  } else if (all(c("x", "y")%in%colnames(df))) {coordNames <- c("x", "y") 
  } else {
    print("Couldn't find headers entitled latitude/longitude, lat/long or x/y!")
    return(NULL)
  }
  if (is.null(idCol)) idCol <- colnames(df)[1]
  if (keepCol[1]=="all") {
    keepCol <- colnames(df)
  } else {
    keepCol <- tolower(keepCol)
    keepCol <- keepCol[which(keepCol%in%colnames(df))]
  }
  coordinates(df) <- coordNames[c(2,1)]
  proj4string(df) <- CRS(crsString)
  df_ll <- spTransform(df, CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
  if (is.null(csvFile)==F & outBase=="out") outBase <- strsplit(x = basename(csvFile),split = ".",fixed = T)[[1]][1]
  if (is.null(outDir) & is.null(csvFile)==F) outDir <- dirname(csvFile)
  dir.create(file.path(outDir, outBase))
  out <- paste(outDir,"/",outBase,".kml",sep="")
  write.table('<?xml version="1.0" encoding="UTF-8"?>',out,quote = F,col.names = F,row.names = F)
  write.table('<kml xmlns="http://www.opengis.net/kml/2.2">',out,quote = F,col.names = F,row.names = F,append=T)
  write.table(paste('<Document id="',outBase,'">',sep=""),out,quote = F,col.names = F,row.names = F,append=T)
  for (i in seq(1,nrow(df_ll),1)) {
    write.table('\t<Placemark>',out,quote = F,col.names = F,row.names = F,append=T)
    write.table(paste('\t\t<name>',df_ll@data[i,which(names(df_ll)==idCol)],'</name>',sep=""),out,quote = F,col.names = F,row.names = F,append=T)
    write.table(paste('\t\t<description>',paste(paste(keepCol,df_ll@data[i,keepCol],sep="="),collapse=", "),'</description>',sep=""),out,quote = F,col.names = F,row.names = F,append=T)
    write.table('\t\t<Point>',out,quote = F,col.names = F,row.names = F,append=T)
    write.table(paste('\t\t\t<coordinates>',paste(df_ll@coords[i,c("x","y")],collapse = ","),'</coordinates>',sep=""),out,quote = F,col.names = F,row.names = F,append=T)
    write.table('\t\t</Point>',out,quote = F,col.names = F,row.names = F,append=T)
    write.table('\t</Placemark>',out,quote = F,col.names = F,row.names = F,append=T)
  }
  write.table('</Document>',out,quote = F,col.names = F,row.names = F,append=T)
  write.table('</kml>',out,quote = F,col.names = F,row.names = F,append=T)
  return(df_ll)
}

#####Shiny app type functions for fieldCheck

plotTag <- function(plotid) {
  res <- getDataFromDB(query=paste("SELECT (CenterPoint,NE,SE,SW,NW) from Plot WHERE PlotID = ",plotid,sep=""))
  mps <- strsplit(gsub("[(|)]","",res),",")[[1]]
  #qrc = sprintf(fmt = '{ "data": "%s", "mapPoints": { "center": "%s", "ne": "%s", "se": "%s", "sw": "%s", "nw": "%s" }, "link": "https://www.oeaw.ac.at/gmi/research/research-groups/kelly-swarts/" }',plot, mps[0], mps[1], mps[2], mps[3], mps[4])
  qrc = paste('{ "data": "',str_pad(plotid,5,"left",0),'", "mapPoints": { "center": "',mps[0],'", "ne": "',mps[1],'", "se": "',mps[2],'", "sw": "',mps[3],'", "nw": "',mps[4],'" }, "link": "https://www.oeaw.ac.at/gmi/research/research-groups/kelly-swarts/" }',sep="")
  return(qr_code(qrc,ecl = "Q"))
}

treeTagsForPlot <- function(plotid) {
  res <- getDataFromDB(query=paste("SELECT (TreeID) from Tree WHERE PlotID = ",plotid," order by treeid",sep=""))
  qrs <- list()
  for (tr in res$treeid) {
    qrc  <- sprintf('{ "data": "%s", "link": "https://www.oeaw.ac.at/gmi/research/research-groups/kelly-swarts/" }',tr)
    qrs[[tr]] <- qr_code(qrc,ecl = "Q")
  }
  return(qrs)
}

coreTagsForPlot <- function(plotid) {
  res <- getDataFromDB(query=paste("SELECT (CoreID) from Core WHERE PlotID =",plotid," order by coreid",sep=""))
  qrs <- list()
  for (core in res$coreid) {
    qrc  <- sprintf('{ "data": "%s" }',core)
    qrs[[core]] <- qr_code(qrc,ecl = "M")
  }
  return(qrs)
}

plotQRMatrix <- function(qr,cols=c("white","black"),main="getFromAttributes") {
  if (main=="getFromAttributes") main= fromJSON(attributes(qr)$string)$data
  main <- str_pad(main,nchar(main)+1,"right")
  par(mar=c(1.1,1.1,1.1,1.1),oma=c(0,5,5,0))
  fig <- plot(0, xlim=c(1,nrow(qr)),ylim=c(1,ncol(qr)),xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',yaxs='i',xaxs='i')
  for (i in 1:nrow(qr)) {
    for (j in 1:ncol(qr)) {
      col <- cols[1]
      if (qr[i,j]) col <- cols[2]
      polygon(x=c(i,i,i+1,i+1),y=c(j,j+1,j+1,j),lwd=0.0001,col = col,border = NA)
    }       
  }
  title(main=main,adj=1,line=0,cex.main=1.5,xpd=T)
  return(TRUE)
}

qrCodeSheet_Plots <- function(plotids,cols=c("white","black"),filename,main=NULL) {
  qrcodes <- list()
  for (plot in plotids) {
    qrcodes[[as.character(plot)]] <- plotTag(plot)
  }
  if (is.null(main)) main <- paste("Plots tags for plots ",paste(plotids,collapse=", "),sep="")
  pdf(filename,width = 8.3,height = 11.7)
  iter <- 1
  for (page in 1:ceiling(length(qrcodes)/54)) {
    par(mar=c(2,2,2,2))
    fig <- plot(0, xlim=c(0,600),ylim=c(0,900),xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',yaxs='i',xaxs='i')
    title(main=main,cex=1,adj=1,line=0)
    abline(v=seq(0,600,100))
    abline(h=seq(0,900,100))
    for (pj in rev(seq(1,900,100))) {
      for (pk in seq(1,600,100)) {
        qr <- qrcodes[[iter]]
        name <- fromJSON(attributes(qr)$string)$data
        if (nchar(name)==8) name <- paste(as.numeric(str_sub(name,1,5)),as.numeric(str_sub(name,6,8)),sep="-")
        for (i in 1:nrow(qr)) {
          for (j in 1:ncol(qr)) {
            col <- cols[1]
            if (qr[i,j]) col <- cols[2]
            shift <- round((100-nrow(qr))/2)
            polygon(x=c(pk+shift+i,pk+shift+i,pk+shift+i+1,pk+shift+i+1),y=c(pj+5+j,pj+5+j+1,pj+5+j+1,pj+5+j),lwd=0.0001,col = col,border = NA)
          }       
        }
        text(x = pk+50,y=pj+85,labels=name,cex=1,adj=.5)
        if (iter >= length(qrcodes)) {
          dev.off()
          return(qrcodes)
        }
        iter <- iter+1
      }
    }
  }
}

qrCodeSheet_Tree <- function(plotid,cols=c("white","black"),filename) {
  qrcodes <- c(list("plotid"=plotTag(plotid)), treeTagsForPlot(plotid))
  pdf(filename,width = 8.3,height = 11.7)
  iter <- 1
  for (page in 1:ceiling(length(qrcodes)/54)) {
    par(mar=c(2,2,2,2))
    fig <- plot(0, xlim=c(0,600),ylim=c(0,900),xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',yaxs='i',xaxs='i')
    title(main=paste("Tree tags for plot ",plotid,sep=""),cex=1,adj=1,line=0)
    abline(v=seq(0,600,100))
    abline(h=seq(0,900,100))
    for (pj in rev(seq(1,900,100))) {
      for (pk in seq(1,600,100)) {
        qr <- qrcodes[[iter]]
        name <- fromJSON(attributes(qr)$string)$data
        if (nchar(name)==8) name <- paste(as.numeric(str_sub(name,1,5)),as.numeric(str_sub(name,6,8)),sep="-")
        for (i in 1:nrow(qr)) {
          for (j in 1:ncol(qr)) {
            col <- cols[1]
            if (qr[i,j]) col <- cols[2]
            shift <- round((100-nrow(qr))/2)
            polygon(x=c(pk+shift+i,pk+shift+i,pk+shift+i+1,pk+shift+i+1),y=c(pj+5+j,pj+5+j+1,pj+5+j+1,pj+5+j),lwd=0.0001,col = col,border = NA)
          }       
        }
        text(x = pk+50,y=pj+85,labels=name,cex=1,adj=.5)
        if (iter >= length(qrcodes)) {
          dev.off()
          return(qrcodes)
        }
        iter <- iter+1
      }
    }
  }
}


qrCodeSheet_CoreSm <- function(plotid,cols=c("white","black"),filename) {
  qrcodes <- coreTagsForPlot(plotid)
  data <- lapply(qrcodes,function(x) {attr(x,which = "string")})
  write.csv(data.frame("data"=unname(unlist(data)),coreid=names(data),label=paste(as.numeric(substring(names(data),1,5)),"-",as.numeric(substring(names(data),6,8)),substring(names(data),9,9),sep="")),file = str_replace(filename,"pdf","csv"))
  pdf(filename,width = 8.3,height = 11.7)
  iter <- 1
  for (page in 1:ceiling(length(qrcodes)/192)) {
    par(mar=c(2,2,2,2))
    fig <- plot(0, xlim=c(0,600),ylim=c(0,900),xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',yaxs='i',xaxs='i')
    abline(v=seq(0,600,50))
    abline(h=seq(0,800,50))
    polygon(x=c(-1,-1,601,601),y=c(801,900,900,801),lwd=0.0001,col = "white",border = "white")
    if (page==1) { #add a plot tag
      pk <- 1
      pj <- 800
      qr <- plotTag(plotid)
      name <- fromJSON(attributes(qr)$string)$data
      if (nchar(name)==9) name <- paste(as.numeric(str_sub(name,1,5)),"-",as.numeric(str_sub(name,6,8)),str_sub(name,9,9),sep="")
      for (i in 1:nrow(qr)) {
        for (j in 1:ncol(qr)) {
          col <- cols[1]
          if (qr[i,j]) col <- cols[2]
          shift <- round((50-nrow(qr))/2)
          polygon(x=c(pk+shift+i,pk+shift+i,pk+shift+i+1,pk+shift+i+1),y=c(pj+5+j,pj+5+j+1,pj+5+j+1,pj+5+j),lwd=0.0001,col = col,border = NA)
        }       
      }
      text(x = pk+25,y=pj+85,labels=name,cex=1,adj=.5)
      text(x = 600,y=pj+85,labels=paste("Core tags for plot",plotid),cex=1,adj=1)
      text(x = 600,y=pj+35,labels=date(),cex=1,adj=1)
    }
    for (pj in rev(seq(1,800,50))) {
      for (pk in seq(1,600,50)) {
        qr <- qrcodes[[iter]]
        name <- fromJSON(attributes(qr)$string)$data
        if (nchar(name)==9) name <- paste(as.numeric(str_sub(name,1,5)),"-",as.numeric(str_sub(name,6,8)),str_sub(name,9,9),sep="")
        for (i in 1:nrow(qr)) {
          for (j in 1:ncol(qr)) {
            col <- cols[1]
            if (qr[i,j]) col <- cols[2]
            shift <- round((50-nrow(qr))/2)
            polygon(x=c(pk+shift+i,pk+shift+i,pk+shift+i+1,pk+shift+i+1),y=c(pj+2+j,pj+2+j+1,pj+2+j+1,pj+2+j),lwd=0.0001,col = col,border = NA)
          }       
        }
        text(x = pk+25,y=pj+40,labels=name,cex=.8,adj=.5)
        if (iter >= length(qrcodes)) {
          dev.off()
          return(qrcodes)
        }
        iter <- iter+1
      }
    }
  }
}


#Function for extracting weather data from EOBS files for locations(s) of interest###
#x: vector of longitude(s); x and y must have same length; REQUIRED
#y: vector of latitude(s), x and y must have same length; REQUIRED
#id: vector of location IDs; must have same length as x/y; OPTIONAL
#w: list of weather data variable(s); REQUIRED
#multiple: T=multiple weather variables (default); F=one weather variable; REQUIRED
extract.eobs <- function(x, y, id, w, multiple=T){
  if((multiple==F & (length(x)!=length(y))) | 
     (multiple==T & ((length(x)!=length(y)) | (length(x)!=length(id)) | (length(y) != length(id))))){
    print("Error: x, y, and/or id have different lengths")
  }else if(multiple==T){
    #If there are multiple weather variables:
    #Create data table from data for first weather variable
    for(i in 1:length(x)){
      #Get name of weather variable
      wvar <- names(w[[1]]$var)
      #Get latitude and longitude data
      lat <- ncvar_get(nc=w[[1]], varid="latitude")
      lon <- ncvar_get(nc=w[[1]], varid="longitude")
      #Get closest datapoint to location of interest
      mlat <- which.min(abs(lat-y[i]))
      mlon <- which.min(abs(lon-x[i]))
      #Subset weather data for datapoint closest to location of interest
      wsub <- as.vector(nc.get.var.subset.by.axes(f=w[[1]], v=wvar, axis.indices=list(X=mlon, Y=mlat)))
      #Get dates of weather data collection (no dates for elevation)
      if(wvar=="elevation"){
        date <- NA
      }else{
        date <- as.character(nc.get.time.series(f=w[[1]], time.dim.name="time"))
      }
      #Create table with location ID (optional), longitude, and latitude, and
      #latitude and longitude from closest point to location in EOBS weather data, and
      #measurements and dates of measurement of EOBS weather data
      if(i==1){
        wdat <- as.data.frame(matrix(nrow=length(wsub), ncol=7,
                                     dimnames=list(c(1:length(wsub)),
                                                   c("Loc_ID","Loc_long","Loc_lat","Date", "Longitude", "Latitude", wvar))))
        wdat$Loc_long <- x[i]
        wdat$Loc_lat <- y[i]
        wdat$Date <- date
        wdat$Longitude <- lon[mlon]
        wdat$Latitude <- lat[mlat]
        wdat[,7] <- wsub
        if(is.null(id)){
          wdat$Loc_ID <- i
        }else{
          wdat$Loc_ID <- id[i]
        }
      }else{
        wdati <- as.data.frame(matrix(nrow=length(wsub), ncol=7,
                                      dimnames=list(c(1:length(wsub)),
                                                    c("Loc_ID","Loc_long","Loc_lat","Date", "Longitude", "Latitude", wvar))))
        wdati$Loc_long <- x[i]
        wdati$Loc_lat <- y[i]
        wdati$Date <- date
        wdati$Longitude <- lon[mlon]
        wdati$Latitude <- lat[mlat]
        wdati[,7] <- wsub
        if(is.null(id)){
          wdati$Loc_ID <- i
        }else{
          wdati$Loc_ID <- id[i]
        }
        wdat <- rbind(wdat, wdati)
      }
      
    }
    #Add data from other weather variables
    if(length(w)>1){
      for(i in 1:length(x)){
        for(j in 2:length(w)){
          #Get name of weather variable
          wvar <- names(w[[j]]$var)
          #Get latitude and longitude data
          lat <- ncvar_get(nc=w[[j]], varid="latitude")
          lon <- ncvar_get(nc=w[[j]], varid="longitude")
          #Get closest datapoint to location of interest
          mlat <- which.min(abs(lat-y[i]))
          mlon <- which.min(abs(lon-x[i]))
          #Subset weather data for datapoint closest to location of interest
          wsub <- as.vector(nc.get.var.subset.by.axes(f=w[[j]], v=wvar, axis.indices=list(X=mlon, Y=mlat)))
          #Get dates of weather data collection (no dates for elevation)
          if(wvar=="elevation"){
            date <- NA
          }else{
            date <- as.character(nc.get.time.series(f=w[[j]], time.dim.name="time"))
          }
          #Add weather data to table
          if(is.null(id)){
            wdat[which(wdat$Loc_ID==i),6+j] <- wsub
          }else{
            wdat[which(wdat$Loc_ID==id[i]),6+j] <- wsub
          }
          colnames(wdat)[6+j] <- wvar
        } 
      }
    }
  }else{
    #If there is only one weather variable:
    #Create data table for the only weather variable
    for(i in 1:length(x)){
      #Get name of weather variable
      wvar <- names(w[[1]]$var)
      #Get latitude and longitude data
      lat <- ncvar_get(nc=w[[1]], varid="latitude")
      lon <- ncvar_get(nc=w[[1]], varid="longitude")
      #Get closest datapoint to location of interest
      mlat <- which.min(abs(lat-y[i]))
      mlon <- which.min(abs(lon-x[i]))
      #Subset weather data for datapoint closest to location of interest
      wsub <- as.vector(nc.get.var.subset.by.axes(f=w[[1]], v=wvar, axis.indices=list(X=mlon, Y=mlat)))
      #Get dates of weather data collection (no dates for elevation)
      if(wvar=="elevation"){
        date <- NA
      }else{
        date <- as.character(nc.get.time.series(f=w[[1]], time.dim.name="time"))
      }
      #Create table with location ID (optional), longitude, and latitude
      #and latitude and longitude from closest point to location in EOBS weather data
      #and measurements and dates of measurement of EOBS weather variable
      if(i==1){
        wdat <- as.data.frame(matrix(nrow=length(wsub), ncol=7,
                                     dimnames=list(c(1:length(wsub)),
                                                   c("Loc_ID","Loc_long","Loc_lat","Date", "Longitude", "Latitude", wvar))))
        wdat$Loc_long <- x[i]
        wdat$Loc_lat <- y[i]
        wdat$Date <- date
        wdat$Longitude <- lon[mlon]
        wdat$Latitude <- lat[mlat]
        wdat[,7] <- wsub
        if(is.null(id)){
          wdat$Loc_ID <- i
        }else{
          wdat$Loc_ID <- id[i]
        }
      }else{
        wdati <- as.data.frame(matrix(nrow=length(wsub), ncol=7,
                                      dimnames=list(c(1:length(wsub)),
                                                    c("Loc_ID","Loc_long","Loc_lat","Date", "Longitude", "Latitude", wvar))))
        wdati$Loc_long <- x[i]
        wdati$Loc_lat <- y[i]
        wdati$Date <- date
        wdati$Longitude <- lon[mlon]
        wdati$Latitude <- lat[mlat]
        wdati[,7] <- wsub
        if(is.null(id)){
          wdati$Loc_ID <- i
        }else{
          wdati$Loc_ID <- id[i]
        }
        wdat <- rbind(wdat, wdati)
      }
    }
  }
  #Return data table
  return(wdat)
}

# #Function for extracting weather data from EOBS files for locations(s) of interest####
# #x: vector of longitude(s); x and y must have same length; REQUIRED
# #y: vector of latitude(s), x and y must have same length; REQUIRED
# #id: vector of location IDs; must have same length as x/y; OPTIONAL
# #w: list of weather data variable(s); REQUIRED
# #multiple: T=multiple weather variables (default); F=one weather variable; REQUIRED
# x <- c(-10.39023437,-10.39023437,56.69527627,56.69527627)
# y <- c(41.26054899,69.81426115,69.81426115,41.26054899)
# extract.eobs.flip <- function(x, y, id, w, multiple=T){
#   if((multiple==F & (length(x)!=length(y))) | 
#      (multiple==T & ((length(x)!=length(y)) | (length(x)!=length(id)) | (length(y) != length(id))))){
#     print("Error: x, y, and/or id have different lengths")
#   }else if(multiple==T){
#     #If there are multiple weather variables:
#     #Create data table from data for first weather variable
#     mlat <- numeric()
#     mlon <- numeric()
#     for(i in 1:length(x)){
#       #Get name of weather variable
#       wvar <- names(w[[1]]$var)
#       #Get latitude and longitude data
#       lat <- ncvar_get(nc=w[[1]], varid="latitude")
#       lon <- ncvar_get(nc=w[[1]], varid="longitude")
#       #Get closest datapoint to location of interest
#       mlat <- c(mlat,which.min(abs(lat-y[i])))
#       mlon <- c(mlon,which.min(abs(lon-x[i])))
#     }
#       #Subset weather data for datapoint closest to location of interest started 4:18
#       wsub <- as.vector(nc.get.var.subset.by.axes(f=w[[1]], v=wvar, axis.indices=list(X=mlon, Y=mlat)))
#       #Get dates of weather data collection (no dates for elevation)
#       if(wvar=="elevation"){
#         date <- NA
#       }else{
#         date <- as.character(nc.get.time.series(f=w[[1]], time.dim.name="time"))
#         nloc <- length(wsub)/length(date)
#       }
#       latvec <- rep(mlon)
#       #Create table with location ID (optional), longitude, and latitude, and
#       #latitude and longitude from closest point to location in EOBS weather data, and
#       #measurements and dates of measurement of EOBS weather data
#       if(i==1){
#         wdat <- as.data.frame(matrix(nrow=length(wsub), ncol=7,
#                                      dimnames=list(c(1:length(wsub)),
#                                                    c("Loc_ID","Loc_long","Loc_lat","Date", "Longitude", "Latitude", wvar))))
#         wdat$Loc_long <- x[i]
#         wdat$Loc_lat <- y[i]
#         wdat$Date <- date
#         wdat$Longitude <- lon[mlon]
#         wdat$Latitude <- lat[mlat]
#         wdat[,7] <- wsub
#         if(is.null(id)){
#           wdat$Loc_ID <- i
#         }else{
#           wdat$Loc_ID <- id[i]
#         }
#       }else{
#         wdati <- as.data.frame(matrix(nrow=length(wsub), ncol=7,
#                                       dimnames=list(c(1:length(wsub)),
#                                                     c("Loc_ID","Loc_long","Loc_lat","Date", "Longitude", "Latitude", wvar))))
#         wdati$Loc_long <- x[i]
#         wdati$Loc_lat <- y[i]
#         wdati$Date <- date
#         wdati$Longitude <- lon[mlon]
#         wdati$Latitude <- lat[mlat]
#         wdati[,7] <- wsub
#         if(is.null(id)){
#           wdati$Loc_ID <- i
#         }else{
#           wdati$Loc_ID <- id[i]
#         }
#         wdat <- rbind(wdat, wdati)
#       }
#       
#     }
#     #Add data from other weather variables
#     if(length(w)>1){
#       for(i in 1:length(x)){
#         for(j in 2:length(w)){
#           #Get name of weather variable
#           wvar <- names(w[[j]]$var)
#           #Get latitude and longitude data
#           lat <- ncvar_get(nc=w[[j]], varid="latitude")
#           lon <- ncvar_get(nc=w[[j]], varid="longitude")
#           #Get closest datapoint to location of interest
#           mlat <- which.min(abs(lat-y[i]))
#           mlon <- which.min(abs(lon-x[i]))
#           #Subset weather data for datapoint closest to location of interest
#           wsub <- as.vector(nc.get.var.subset.by.axes(f=w[[j]], v=wvar, axis.indices=list(X=mlon, Y=mlat)))
#           #Get dates of weather data collection (no dates for elevation)
#           if(wvar=="elevation"){
#             date <- NA
#           }else{
#             date <- as.character(nc.get.time.series(f=w[[j]], time.dim.name="time"))
#           }
#           #Add weather data to table
#           if(is.null(id)){
#             wdat[which(wdat$Loc_ID==i),6+j] <- wsub
#           }else{
#             wdat[which(wdat$Loc_ID==id[i]),6+j] <- wsub
#           }
#           colnames(wdat)[6+j] <- wvar
#         } 
#       }
#     }
#   }else{
#     #If there is only one weather variable:
#     #Create data table for the only weather variable
#     for(i in 1:length(x)){
#       #Get name of weather variable
#       wvar <- names(w[[1]]$var)
#       #Get latitude and longitude data
#       lat <- ncvar_get(nc=w[[1]], varid="latitude")
#       lon <- ncvar_get(nc=w[[1]], varid="longitude")
#       #Get closest datapoint to location of interest
#       mlat <- which.min(abs(lat-y[i]))
#       mlon <- which.min(abs(lon-x[i]))
#       #Subset weather data for datapoint closest to location of interest
#       wsub <- as.vector(nc.get.var.subset.by.axes(f=w[[1]], v=wvar, axis.indices=list(X=mlon, Y=mlat)))
#       #Get dates of weather data collection (no dates for elevation)
#       if(wvar=="elevation"){
#         date <- NA
#       }else{
#         date <- as.character(nc.get.time.series(f=w[[1]], time.dim.name="time"))
#       }
#       #Create table with location ID (optional), longitude, and latitude
#       #and latitude and longitude from closest point to location in EOBS weather data
#       #and measurements and dates of measurement of EOBS weather variable
#       if(i==1){
#         wdat <- as.data.frame(matrix(nrow=length(wsub), ncol=7,
#                                      dimnames=list(c(1:length(wsub)),
#                                                    c("Loc_ID","Loc_long","Loc_lat","Date", "Longitude", "Latitude", wvar))))
#         wdat$Loc_long <- x[i]
#         wdat$Loc_lat <- y[i]
#         wdat$Date <- date
#         wdat$Longitude <- lon[mlon]
#         wdat$Latitude <- lat[mlat]
#         wdat[,7] <- wsub
#         if(is.null(id)){
#           wdat$Loc_ID <- i
#         }else{
#           wdat$Loc_ID <- id[i]
#         }
#       }else{
#         wdati <- as.data.frame(matrix(nrow=length(wsub), ncol=7,
#                                       dimnames=list(c(1:length(wsub)),
#                                                     c("Loc_ID","Loc_long","Loc_lat","Date", "Longitude", "Latitude", wvar))))
#         wdati$Loc_long <- x[i]
#         wdati$Loc_lat <- y[i]
#         wdati$Date <- date
#         wdati$Longitude <- lon[mlon]
#         wdati$Latitude <- lat[mlat]
#         wdati[,7] <- wsub
#         if(is.null(id)){
#           wdati$Loc_ID <- i
#         }else{
#           wdati$Loc_ID <- id[i]
#         }
#         wdat <- rbind(wdat, wdati)
#       }
#     }
#   }
#   #Return data table
#   return(wdat)
# }

#Function for subsetting extracted weather data by date(s)####
#wdat: data frame generated by extract.eobs function
#date_start: vector of start date(s) of interest; must have same length as date_stop; format="YYYY-MM-DD"
#date_stop: vector of end date(s) of interest; must have same length as date_start; format="YYYY-MM-DD"
subset.date <- function(wdat, date_start, date_stop){
  if(length(date_start)!=length(date_stop)){
    print("Error: date_start and date_stop have different lengths")
  }else{
    #Get year and day-in-year for start and end dates of interest
    day_start <- yday(as.Date(date_start))
    day_stop <- yday(as.Date(date_stop))
    year_start <- NULL
    year_stop <- NULL
    for(i in 1:length(date_start)){
      year_start <- c(year_start, strsplit(date_start[i], "-")[[1]][1])
      year_stop <- c(year_stop, strsplit(date_stop[i], "-")[[1]][1])
    }
    #Get year and day-in-year for weather data dates
    wdat_day <- NULL
    wdat_year <- NULL
    for(i in 1:nrow(wdat)){
      wdat_day <- c(wdat_day, yday(as.Date(wdat[i,"Date"])))
      wdat_year <- c(wdat_year, strsplit(wdat[i,"Date"], "-")[[1]][1])
    }
    #Subset weather data by date range(s)
    wsub <- NULL
    for(i in 1:length(date_start)){
      if(year_start[i]==year_stop[i]){
        wsub <- c(wsub, which((wdat_year==year_start[i] & wdat_day>=day_start[i] & wdat_day<=day_stop[i])))
      }else{
        wsub <- c(wsub, which(wdat_year==year_start[i] & wdat_day>=day_start[i]),
                  which(wdat$year>year_start[i] & wdat_year<year_stop[i]),
                  which(wdat_year==year_stop[i] & wdat_day<=day_stop[i]))
      }
    }
    #Return weather data subset by date range(s)
    return(wdat[unique(wsub),])
  }
}

eobsForPlotIDs <- function(plotids,weather=c("tn","tx","tg","rr","pp","hu"),environmentFolder="/Volumes/swarts/lab/Resources/EnvironmentalData/eobs_v28") {
  ##E-OBS classes
  olddir <- getwd()
  eobs.terms <- list()
  setwd(environmentFolder)
  eobs.terms[["tn"]] <- nc_open(list.files(pattern = "tn_"))
  eobs.terms[["tx"]] <- nc_open(list.files(pattern = "tx_"))
  eobs.terms[["tg"]] <- nc_open(list.files(pattern = "tg_"))
  eobs.terms[["rr"]] <- nc_open(list.files(pattern = "rr_"))
  eobs.terms[["pp"]] <- nc_open(list.files(pattern = "pp_"))
  eobs.terms[["hu"]] <- nc_open(list.files(pattern = "hu_"))
  eobs.terms[["fg"]] <- nc_open(list.files(pattern = "fg_"))
  eobs.terms[["qq"]] <- nc_open(list.files(pattern = "qq_"))
  eobs.terms[["elev"]] <- nc_open(list.files(pattern = "elev_"))
  
  spatial <- getDataFromDB(query = paste("Select * from plot p left join mappoint m on p.centerpoint = m.mappointid where p.plotid in (",paste(plotids,collapse = ","),") order by plotid;"))
  spatial <- spatial[which(is.na(spatial$median_wgs84e)==F),]
  mat <- extract.eobs(x=spatial$median_wgs84e, y=spatial$median_wgs84n, id=spatial$plotid, w=eobs.terms[which(names(eobs.terms)%in%weather)])
  mat$plot <- mat$Loc_ID
  mat$year <- str_sub(mat$Date,1,4)
  mat$month <- str_sub(mat$Date,6,7)
  mat$day <- str_sub(mat$Date,9,10)
  mat[,c("plotid","median_wgs84e","median_wgs84n","median_alt")] <- spatial[match(mat$plot,spatial$plotid),c("plotid","median_wgs84e","median_wgs84n","median_alt")]
  setwd(olddir)
  return(mat)
}

##positions is a data.frame of c("id","lat","long")
eobsForPositions <- function(positions,weather=c("tn","tx","tg","rr","pp","hu","elev"),environmentFolder="/Volumes/groups/swarts/lab/Resources/EnvironmentalData/eobs_v28") {
  ##E-OBS classes
  olddir <- getwd()
  eobs.terms <- list()
  setwd(environmentFolder)
  eobs.terms[["tn"]] <- nc_open(list.files(pattern = "tn_"))
  eobs.terms[["tx"]] <- nc_open(list.files(pattern = "tx_"))
  eobs.terms[["tg"]] <- nc_open(list.files(pattern = "tg_"))
  eobs.terms[["rr"]] <- nc_open(list.files(pattern = "rr_"))
  eobs.terms[["pp"]] <- nc_open(list.files(pattern = "pp_"))
  eobs.terms[["hu"]] <- nc_open(list.files(pattern = "hu_"))
  eobs.terms[["fg"]] <- nc_open(list.files(pattern = "fg_"))
  eobs.terms[["qq"]] <- nc_open(list.files(pattern = "qq_"))
  eobs.terms[["elev"]] <- nc_open(list.files(pattern = "elev_"))
  
  mat <- extract.eobs(x=positions$long, y=positions$lat, id=positions$id, w=eobs.terms[which(names(eobs.terms)%in%weather)])
  mat$id <- mat$Loc_ID
  mat$year <- str_sub(mat$Date,1,4)
  mat$month <- str_sub(mat$Date,6,7)
  mat$day <- str_sub(mat$Date,9,10)
  mat[,c("id","long","lat")] <- positions[match(mat$id,positions$id),c("id","long","lat")]
  setwd(olddir)
  return(mat)
}

eobsForMapPointIDs <- function(mappointids,weather=c("tn","tx","tg","rr","pp","hu","fg","qq","elev"),environmentFolder="/Volumes/swarts/lab/Resources/EnvironmentalData/eobs_v28") {
  ##E-OBS classes
  olddir <- getwd()
  eobs.terms <- list()
  setwd(environmentFolder)
  eobs.terms[["tn"]] <- nc_open(list.files(pattern = "tn_"))
  eobs.terms[["tx"]] <- nc_open(list.files(pattern = "tx_"))
  eobs.terms[["tg"]] <- nc_open(list.files(pattern = "tg_"))
  eobs.terms[["rr"]] <- nc_open(list.files(pattern = "rr_"))
  eobs.terms[["pp"]] <- nc_open(list.files(pattern = "pp_"))
  eobs.terms[["hu"]] <- nc_open(list.files(pattern = "hu_"))
  eobs.terms[["fg"]] <- nc_open(list.files(pattern = "fg_"))
  eobs.terms[["qq"]] <- nc_open(list.files(pattern = "qq_"))
  eobs.terms[["elev"]] <- nc_open(list.files(pattern = "elev_"))
  
  spatial <- getDataFromDB(query = paste("Select * from mappoint where mappointid in (",paste(mappointids,collapse = ","),") order by mappointid;"))
  spatial <- spatial[which(is.na(spatial$median_wgs84e)==F),]
  mat <- extract.eobs(x=spatial$median_wgs84e, y=spatial$median_wgs84n, id=spatial$mappointid, w=eobs.terms[which(names(eobs.terms)%in%weather)])
  mat$mappointid <- mat$Loc_ID
  mat$year <- str_sub(mat$Date,1,4)
  mat$month <- str_sub(mat$Date,6,7)
  mat$day <- str_sub(mat$Date,9,10)
  mat[,c("mappointid","median_wgs84e","median_wgs84n","median_alt")] <- spatial[match(mat$mappointid,spatial$mappointid),c("mappointid","median_wgs84e","median_wgs84n","median_alt")]
  setwd(olddir)
  return(mat)
}

#' Pull subsets of environmental data from weather dataframe already processed from E-OBS
#' 
#' @param wea A dataframe resulting from 'eobsForPlotIDs()'
#' @param var Variables found in the 'wea' table. Options from E-OBS include 'tn', 'tx', 'tg', 'rr', 'pp', 'hu','qq','elevation'
#' @param firstRange Date object specifying first date to include, inclusive. Defaults to "1950-12-01", the beginning of E-OBS data
#' @param lastRange Date object specifying last date to include, inclusive. Defaults to "2022-11-30", the end of E-OBS data current version
#' @param normalize will convert values in var to 0-1. Defaults to true
#' @return dataframe of weather variable where rows are Location_Year and columns are daily weather by variable
weatherByLocYear <- function(wea,var,firstRange=as.Date("1950-12-01"),lastRange=as.Date("2022-11-30"),normalize=T) {
  for (v in var) {
    if (v%in%names(wea)==F) {
      print(paste(v,"not in dataframe!"))
      return(NULL)
    }
  }
  wea <- wea[which(wea$Date>=firstRange & wea$Date<=lastRange),]
  wea$year <- as.numeric(format(wea$Date,"%Y"))
  if (format(firstRange,"%m%d")>format(lastRange,"%m%d")) {
    includenext <- which(format(wea$Date,"%m%d")>=format(firstRange,"%m%d"))
    wea$year[includenext] <- wea$year[includenext]+1
    keep <- which(as.numeric(format(wea$Date,"%Y"))!=wea$year | (format(wea$Date,"%m%d")<=format(lastRange,"%m%d")))
  } else {
    keep <- which(format(wea$Date,"%m%d")>=format(firstRange,"%m%d") & format(wea$Date,"%m%d")<=format(lastRange,"%m%d"))
  }
  wea <- wea[keep,c("Loc_ID","Date",var,"year")]
  if (normalize) {
    cur <- as.matrix(wea[,var])
    cur <- scale(cur) #This normalizes so that the mean is ~0 and the std dev for each column is 1
    #heatmap(cur[sample(1:nrow(cur),size = 100,replace = F),])
    wea[,var] <- cur
  }
  wea <- pivot_longer(wea,cols_vary = "slowest",cols = all_of(var),names_to = "wea")
  wea$mod <- paste(wea$wea,format(wea$Date,"%m%d"),sep="_")
  wea <- wea %>% pivot_wider(names_from = mod,values_from = value,id_cols = c(Loc_ID,year)) %>% as.data.frame()
  rownames(wea) <- paste(wea$Loc_ID,wea$year,sep=":")
  return(wea)
}

#' Similarity matrix for environmental data
#' 
#' @param wea A dataframe resulting from 'eobsForPlotIDs()'
#' @param var Variables found in the 'wea' table. Options from E-OBS include 'tn', 'tx', 'tg', 'rr', 'pp', 'hu','qq','elevation'
#' @param firstRange Date object specifying first date to include, inclusive. Defaults to "1950-12-01", the beginning of E-OBS data
#' @param lastRange Date object specifying last date to include, inclusive. Defaults to "2022-11-30", the end of E-OBS data current version
#' @return distance matrix of location:year relationships
weatherEuclideanSimilarityFromEOBS <- function(wea,var="tn",firstRange=as.Date("1950-11-01"),lastRange=as.Date("2022-10-31")) {
  cur <- weatherByLocYear(wea,var,firstRange,lastRange,normalize = T)
  cur <- as.matrix(cur[,3:ncol(cur)])
  missing <- which(colSums(is.na(cur))>0)
  print(paste("removing columns with missing data:",paste(names(missing),collapse=",")))
  cur <- cur[,-missing]
  #t(cur)[,1:3] %>% vioplot()
  d <- as.matrix(dist(cur,diag=T))
  d <- 1-(d/max(d))
  #samp <- sort(sample(1:nrow(d),100))
  #heatmap(d,Rowv = NA,Colv = NA,distfun = NA,hclustfun = NA,reorderfun = NA,symm=T,scale = "none")
  #heatmap(d[c(1:10,(nrow(d)-10):nrow(d)),c(1:10,(nrow(d)-10):nrow(d))],Rowv = NA,Colv = NA,distfun = NA,hclustfun = NA,reorderfun = NA,symm=T,scale = "none")
  return(d)
}
###Returns a dataframe of summary statistics for the target variable for the months desired
processWeather <- function(wea,var="tn",firstMonth=1,lastMonth=12,alias=NULL) {
  if (var%in%names(wea)==F) {
    print(paste(var,"not in dataframe!"))
    return(NULL)
  }
  if (firstMonth>0 && lastMonth>0) {
    cur <- wea[which(as.numeric(wea$month)>=firstMonth & as.numeric(wea$month)<=lastMonth),c("plot","year","Loc_long","Loc_lat",var)]
  } else  if (firstMonth<0 && lastMonth>0) {
    cur <- wea[which(as.numeric(wea$month)>=1 & as.numeric(wea$month)<=lastMonth),c("plot","year","Loc_long","Loc_lat",var)]
    prev <- wea[which(as.numeric(wea$month)>=abs(firstMonth) & as.numeric(wea$month)<=12),c("plot","year","Loc_long","Loc_lat",var)]
    prev$year <- prev$year+1
    cur <- rbind(cur[which(cur$year!=min(cur$year)),],prev[which(prev$year!=max(cur$year)),])
  } else {
    cur <- wea[which((as.numeric(wea$month)>=abs(firstMonth)) & (as.numeric(wea$month)<=abs(lastMonth))),c("plot","year","Loc_long","Loc_lat",var)]
    cur$year <- cur$year+1
  }
  if (is.null(alias)) alias <- paste(var,"_",firstMonth,"_",lastMonth,sep="")
  out <- as.data.frame(cur %>% dplyr::group_by(plot,year,Loc_long,Loc_lat) %>% dplyr::summarize(!!paste(alias,"_mean",sep=""):=mean(!!sym(var),na.rm=T),!!paste(alias,"_sum",sep=""):=sum(!!sym(var),na.rm=T),!!paste(alias,"_median",sep=""):=median(!!sym(var),na.rm=T),!!paste(alias,"_min",sep=""):=min(!!sym(var),na.rm=T),!!paste(alias,"_max",sep=""):=max(!!sym(var),na.rm=T),!!paste(alias,"_stderr",sep=""):=((sd(!!sym(var)))/dplyr::n())))
  out$plotYear <- paste(out$plot,out$year,sep=":")
  return(out)
}

###Helper function to add weather to a table based on matchTerm
##firstMonth can be negative and indicates month of previous year
addWeather <- function(all,wea,var="tn",firstMonth=1,lastMonth=12,matchTerm="plotYear",alias=NULL) {
  sumw <- processWeather(wea,var,firstMonth,lastMonth,alias)
  out <- cbind(all,sumw[match(all[,matchTerm],sumw[,matchTerm]),!names(sumw)%in%c("plot","year","Loc_long","Loc_lat",matchTerm)])
  return(out)
  
}

###Returns a dataframe of summary statistics for the target variable for the dates desired
processWeatherDate <- function(wea,var="tn",firstDate,lastDate) {
  if (var%in%names(wea)==F) {
    print(paste(var,"not in dataframe!"))
    return(NULL)
  }
  cur <- wea[which(as.numeric(wea$Date)>=firstDate & as.numeric(wea$Date)<=lastDate),c("Loc_ID","year","Loc_long","Loc_lat",var)]
  out <- as.data.frame(cur %>% dplyr::group_by(Loc_ID,year,Loc_long,Loc_lat) %>% dplyr::summarize(!!paste(var,"_",firstDate,"_",lastDate,"_mean",sep=""):=mean(!!sym(var),na.rm=T),!!paste(var,"_",firstDate,"_",lastDate,"_sum",sep=""):=sum(!!sym(var),na.rm=T),!!paste(var,"_",firstDate,"_",lastDate,"_median",sep=""):=median(!!sym(var),na.rm=T),!!paste(var,"_",firstDate,"_",lastDate,"_min",sep=""):=min(!!sym(var),na.rm=T),!!paste(var,"_",firstDate,"_",lastDate,"_max",sep=""):=max(!!sym(var),na.rm=T),!!paste(var,"_",firstDate,"_",lastDate,"_stderr",sep=""):=((sd(!!sym(var)))/dplyr::n())))
  out$range <- paste(firstDate,"_",lastDate,sep="")
  return(out)
}


#Plot a QQ based on dataframe read in from tassel GWAS output where y is the trait that you want to plot
scaleZeroOne <- function(x){(x-min(x))/(max(x)-min(x))}

tasselQQ <- function(stats,y) {
  cur <- stats[which(stats$Trait==y),]
  ordernull <- scaleZeroOne(1:nrow(cur))+min(cur$add_p,na.rm = T)
  qqplot(x = -log10(ordernull),y = -log10(cur$add_p),main=paste("QQ-Plot for",y),ylab="Negative log 10 p-value",xlab="Expected under no association") #can also use car::qqPlot
  abline(0,1,col="red")
  return(cur)
}

#biwavelet plot to check cross-dating
#timeSeries is a list of two with three columns, year (numeric), Date, and ringwidths named either ("detrendwidth","width","val)
#nharmonics determines how many harmonics will be plotted
waveletCoherence <- function(timeSeries,nharmonics=5) {
  names(timeSeries[[1]])[which(names(timeSeries[[1]])%in%c("detrendwidth","width"))] <- "val"
  names(timeSeries[[2]])[which(names(timeSeries[[2]])%in%c("detrendwidth","width"))] <- "val"
  t1 <- timeSeries[[1]]
  t2 <- timeSeries[[2]]
  range <- c(as.numeric(format(min(t1$date),format="%Y")),as.numeric(format(max(t1$date),format="%Y")))
  offone <- wtc(t1[,c("date","val")],t2[,c("date","val")],nrands=10)
  par(mfrow=c(3,1),oma = c(5, 0, 5, 1), mar = c(0, 4, 0, 5) + 0.1)
  plot(t1$year,t1$val,main="",type="l",col="red",ylim=c(min(c(t2$val,t1$val)),max(c(t2$val,t1$val))),xlab="date",ylab="RWI",xlim=range,xaxs="i")
  lines(t2$year,t2$val,col="blue")
  legend("topleft",legend=names(timeSeries),col=c("red","blue"),lwd=2)
  title(main=paste(p1,"vs",p2),outer = T)
  rsq <- data.frame(t1[,c("date","year")],meancor= apply(offone$rsq[1:2,],2,mean))
  plot(x = rsq$date,y = rsq$meancor,col="green",lwd=2,type="l")
  minima <- which(diff(sign(diff(rsq$meancor)))==2)+1
  text(rsq$date[minima],y = rsq$meancor[minima],labels = rsq$year[minima])
  if (nharmonics=="all") nharmonics <- length(offone$scale)
  plot(offone, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
       lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylim=c(offone$scale[1],offone$scale[nharmonics]),ylab = "Scale", xlab = "Period", 
       plot.cb = TRUE, main = "")
  abline(v=as.Date(paste(seq((10*round(min(t1$year/10))),(10*round(max(t1$year/10))),10), 12, 31, sep = "-")),col="white",lwd=1)
  return(offone)
}

biwaveletAgainstChronology <- function(chron,test,add=NULL,rmv=NULL) {
  p2 <- test
  plot <- as.numeric(substring(p2,1,5))
  t1 <- chron[which(chron$plot==plot),c("date","val","year")]
  t2 <- osm[which(osm$coreid==p2),c("date","detrendwidth","year")]
  t2$detrendwidth <- t2$detrendwidth-mean(t2$detrendwidth)
  names(t2)[which(grepl("width",names(t2)))] <- "val"
  if (is.null(rmv)==F) t2 <- cbind(t2[(length(rmv)+1):(nrow(t2)),c("date","year")],val=t2$val[which(t2$year%in%rmv==F)])
  if (is.null(add)==F) {
    addpos <- cbind(c(1,which(t2$year%in%c(add))),c(which(t2$year%in%c(add)),nrow(t2)))
    nval <- t2$val[addpos[1,1]:addpos[1,2]]
    for(ad in 2:nrow(addpos)) {
      nval <- c(nval,0,t2$val[(addpos[ad,1]+1):(addpos[ad,2])])
    }
    t2 <- cbind(rbind(data.frame(date=as.Date(paste(seq(min(t2$year)-length(add),min(t2$year)-1,1), 12, 31, sep = "-")),year=seq(min(t2$year)-length(add),(min(t2$year)-1),1)),t2[,c("date","year")]),val=nval)
  }
  uniq <- c(t1$date,t2$date)[which((duplicated(c(t1$date,t2$date)) | duplicated(c(t1$date,t2$date), fromLast = TRUE))==T)]
  t1 <- t1[t1$date%in%uniq,]
  t2 <- t2[t2$date%in%uniq,]
  timeSeries <- list(t1,t2)
  names(timeSeries) <- c(plot,p2)
  coh <- waveletCoherence(timeSeries)
  return(coh)
}




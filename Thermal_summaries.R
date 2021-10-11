#### Code description ####
## Shallow-deep torpor paper: script to compile raw (exported) IR csv files into manageable thermal summarized files, 
## and to re-make the RDS files used in the out_full functions and data frames in the Thermal_plots_models.R script
## Paper authors: A Shankar, INH Cisneros, S Thompson, CH Graham, DR Powers
## Code author: A Shankar
## Contact: nushiamme<at>gmail<dot>com

#### Do not need to run again unless base thermal files change! This script is time consuming! ####

#### Read in packages and set working directory ####
library(reshape2)
wd <- file.path("E:", "Google Drive", "IR_2018_csv", "Data")

###  MAHU07_0531 from 2018 was missing, added in on Oct 7, 2018
#### Bird folders ####
bird.folders.2018 <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", "BCHU05_0607",
                       "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                       "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                       "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU07_0531", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606")

bird.folders.2017 <- c("BC01_0610", "BC02_0612", "BC03_0617",
                       "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                       "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

bird.folders.all <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", #"BCHU05_0607",
                      "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                      "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                      "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU07_0531", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606",
                      "BC01_0610", "BC02_0612", "BC03_0617",
                      "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                      "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

#### Function to process and compile raw IR csv files into summarized thermal data frame ####
for(i in bird.folders.all) {
  setwd(paste0(wd, "/", i))
  
  
  #### Compile csv's and process ####
  ## Using plyr
  paths <- dir(pattern = "\\.csv$")
  names(paths) <- basename(paths)
  
  ThermFiles <- lapply(paths, read.csv, header=F)
  
  ### Creating a summary data frame of 
  # Can also create automatic lists of summaries: lapply(ThermFiles_na_omit[[i]], summary)
  Thermsumm <- data.frame(matrix(NA, nrow=length(ThermFiles), ncol=5))
  names(Thermsumm) <- c("Min", "Mean", "Max", "File") #,"sd"
  Thermsumm$File <- noquote(names(ThermFiles))
  for(i in 1:length(ThermFiles)) { 
    ThermFiles_na_omit <- vector("list",length(ThermFiles))
    names(ThermFiles_na_omit) <- names(ThermFiles)
    ThermFiles_na_omit[[i]]<- na.omit(data.frame(stack(ThermFiles[[i]][1:ncol(ThermFiles[[i]])])))[1]
    Thermsumm[i,1] <- min(ThermFiles_na_omit[[i]]$values)
    Thermsumm[i,2] <- mean(ThermFiles_na_omit[[i]]$values)
    Thermsumm[i,3] <- max(ThermFiles_na_omit[[i]]$values)
    #Thermsumm[i,4] <- sd(ThermFiles_na_omit[[i]]$values)
    ## Splitting the file name to get IndivID, date, and time
    Thermsumm$Indiv_ID <- 
      unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 1))
    Thermsumm$Date <- 
      unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 2))
    Thermsumm$Time <- 
      unlist(lapply(strsplit(as.character(Thermsumm$File), "\\_"), "[", 3))
    Thermsumm$Time <- 
      unlist(lapply(strsplit(as.character(Thermsumm$Time), "\\."), "[", 1))
    m.thermsumm <- melt(Thermsumm, id.vars=c("Indiv_ID", "Date", "Time"), 
                        measure.vars = c("Min", "Mean","Max")) #, "sd"
    m.thermsumm$Hour <- substr(m.thermsumm$Time,1,2)
    m.thermsumm$Hour <- factor(m.thermsumm$Hour, 
                               levels= c("19", "20", "21", "22", "23", "24", "01", "02",
                                         "03", "04", "05", "06"), ordered=T)
    file.name <- paste(m.thermsumm$Indiv_ID[1], "_", m.thermsumm$Date[1], "_summ.rds", sep="")
    saveRDS(m.thermsumm,file.name)
  }
}

## Compiling all the RDS files into a single list, to then summarize the temperatures
all_thermal <- data.frame(matrix(ncol = length(bird.folders.all), nrow=120))
colnames(all_thermal) <- bird.folders.all
all_amb <- data.frame(matrix(ncol = length(bird.folders.all), nrow=120))
colnames(all_amb) <- bird.folders.all
#all_thermal <- ls()

## Collating all the birds' max temps into one object, and min temps into another
for(i in bird.folders.all) {
  setwd(paste0(wd, "/", i))
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  var1 <- out$value[out$variable=="Max"]
  n <- length(var1)
  all_thermal[[paste(i)]] <- c(var1, rep(NA,120-n))
  
  
  var2 <- out$value[out$variable=="Min"]
  n <- length(var2)
  all_amb[[paste(i)]] <- c(var2, rep(NA,120-n))
}


m.all_thermal <- melt(all_thermal, na.rm=T)
## Change Indiv_IDs to match latest convention names for MAHU -> RIHU and BLHU -> BLUH
#m.all_thermal$Indiv_ID <- gsub('MA', 'RI', m.all_thermal$Individual) ## Changing species code for RIHU from MAHU to RIHU
#m.all_thermal$Indiv_ID <- gsub('BLHU', 'BLUH', m.all_thermal$Indiv_ID) ## Changing species code from BLHU to BLUH

setwd("E:/Google Drive/IR_2018_csv/Data")
write.csv(m.all_thermal,file = "Thermal_maxes.csv")
## m.all_amb <- melt(all_amb,na.rm=T) ## If you want just ambient temperatures
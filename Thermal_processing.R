#### Code description ####
## Shallow-deep torpor paper: Processing data for use in the models:
## Read in rds files and add in date and categories
## Output is All_data.csv
## Paper authors: A Shankar, INH Cisneros, S Thompson, CH Graham, DR Powers
## Code author: A Shankar
## Contact: nushiamme<at>gmail<dot>com

## Code layout:
## First read in packages and data files, then general functions
## Then process files to run models
## First of surface temperature ~ ambient temperature, and categories
## Then of proportion of time spent per category
## Then Figures


#### Read in packages ####
library(here)
library(plyr)

#### Read in files. Using here() package, so default working directory is the file that the .Rproj file is in. ####
# Can remake this thermal melted file if needed by running the Thermal_summaries.R script
here <- here::here
#thermal_maxes_melted <- read.csv(here("Data", "Thermal_maxes.csv")) ## Raw temperatures

# Other files
categories <- read.csv(here("Data", "Category_thresholds.csv"))
#interpolated <- read.csv(here("Data", "Interpolated_Thermal.csv")) ## Temperatures interpolated to 1 minute
categ_percentage <- read.csv(here("Data", "Category_percentages.csv"))
masses <- read.csv(here("Data", "Bird_masses.csv"))



#### Processing data to run surface vs ambient temperature models ####

## First read in the bird IDs
#### Bird folders ####
# bird.folders.2018 <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", "BCHU05_0607",
#                        "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
#                        "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
#                        "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU07_0531", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606")

# bird.folders.2017 <- c("BC01_0610", "BC02_0612", "BC03_0617",
#                        "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
#                        "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

bird.folders.all <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", #"BCHU05_0607",
                      "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                      "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                      "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU07_0531", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606",
                      "BC01_0610", "BC02_0612", "BC03_0617",
                      "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                      "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

## Stacking all the individual birds' data, keeping all melted columns from earlier (hour, min, max, etc.)
out_all <- data.frame(matrix(ncol = 6, nrow=109*length(bird.folders.all)))
names(out_all) <- c("Indiv_ID", "Date", "Time", "variable", "value", "Hour")
for(i in bird.folders.all) {
  wd2 <- file.path("C:", "Users", "nushi", "OneDrive - Cornell University", "IR_2018_csv", "Data")
  #wd2 <- file.path("E:", "Google Drive", "IR_2018_csv", "Data")
  setwd(paste0(wd2, "/", i))
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  out_all <- rbind(out,out_all)
}
dim(out_all) ## Check dimensions, should be ~ 9909 without MAHU07_0531 and 10108 with all 34 birds, by 6
out_all <- out_all[complete.cases(out_all),] ## Remove rows with NAs
dim(out_all) ## Check dimensions, now 6309 by 6/6471 by 6
out_amb <- out_all[out_all$variable=="Min",] ## Make a separate data frame with just minimum (~= ambient) values
#out_mean <- out_all[out_all$variable=="Mean",] ## Make a separate data frame with just mean Ts values
out_max <- out_all[out_all$variable=="Max",] ## Make a separate data frame with just maximum (~= surface) values
out_full <- merge(out_amb,out_max, by = c("Indiv_ID", "Date", "Time", "Hour")) ## Merge the two
out_full <- subset(out_full, select = -c(variable.x, variable.y)) ## Remove unnecessary columns
names(out_full) <- c("Indiv_ID", "Date", "Time", "Hour", "Amb_Temp", "Surf_Temp")
out_full$Year <- 0 ## Making a year column to make Indiv_ID in out_full match individual column in categories DF
head(out_full) 
out_full$pasted <- paste(out_full$Indiv_ID, "_", out_full$Date, sep="")
out_full$Year[which(!is.na(match(out_full$pasted,bird.folders.2017)))] <- 17
out_full$Year[which(!is.na(match(out_full$pasted,bird.folders.2018)))] <- 18
out_full$Indiv_ID <- lapply(out_full$Indiv_ID, function(x) {
  gsub("BC0", "BCHU0", x)
})
out_full$Indiv_ID <- lapply(out_full$Indiv_ID, function(x) {
  gsub("BL0", "BLHU0", x)
})
out_full$Indiv_ID <- lapply(out_full$Indiv_ID, function(x) {
  gsub("MA0", "MAHU0", x)
})
out_full$pasted <- paste(out_full$Indiv_ID, "_", out_full$Date, out_full$Year, sep="")
out_full$pasted <- gsub('MA', 'RI', out_full$pasted) ## Changing species code for RIHU from MAHU to RIHU from latest renaming
out_full$pasted <- gsub('BLHU', 'BLUH', out_full$pasted) ## Changing species code for RIHU from MAHU to RIHU from latest renaming
head(out_full) ## Check that the years match the separate bird.folders above
## Loops to fill in a "Category" column in the out_full dataset so that each surface temperature is 
## assigned a category according to individual thresholds laid out in the manually-assigned categories DF
## Great opportunity for machine learning here, but manual for now!

out_full$Category <- 0

for(i in 1:nrow(out_full)) {
  categ <- categories[categories$Individual==out_full$pasted[i],]
  if(out_full$Surf_Temp[i] > categ$Normo_min) {
    out_full$Category[i] <- "Normothermic"
  } else if(!is.na(categ$Shallow_min) & out_full$Surf_Temp[i] > categ$Shallow_min) {
    out_full$Category[i] <- "Shallow Torpor"
  } else if(is.na(categ$Shallow_min) & !is.na(categ$Shallow_max) & out_full$Surf_Temp[i] < categ$Shallow_max) {
    out_full$Category[i] <- "Shallow Torpor"
  } else if(!is.na(categ$Transition_min) & out_full$Surf_Temp[i] > categ$Transition_min) {
    out_full$Category[i] <- "Transition"
  } else if(is.na(categ$Transition_min) & !is.na(categ$Transition_max) & out_full$Surf_Temp[i] < categ$Transition_max) {
    out_full$Category[i] <- "Transition"
  } else if(!is.na(categ$Torpor_max) & out_full$Surf_Temp[i] < categ$Torpor_max) {
    out_full$Category[i] <- "Deep Torpor"
  }
}


## Add a column for capture masses
out_full$Cap_mass <- 0

for(i in 1:nrow(out_full)) {
  out_full$Cap_mass[i] <- masses$Capture_mass_g[masses$Indiv_ID==out_full$pasted[i]]
}

## Running an ancova on Surface ~ Ambient temperature
out_full$Indiv_numeric <- cumsum(!duplicated(out_full$pasted)) ## Making individual column numeric for the ancova, but this turns out to be unnecessary
#Reviewer and others say to include Indiv ID in the model anyway.
out_full$Species <- substr(out_full$Indiv_ID, 1, 4) ## Making a species column
out_full$Species_numeric <- cumsum(!duplicated(out_full$Species))
out_full$Category <- factor(out_full$Category, levels = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
out_full$Species <- gsub('MA', 'RI', out_full$Species) ## Changing species code for RIHU from MAHU to RIHU from latest renaming
out_full$Species <- gsub('BLHU', 'BLUH', out_full$Species) ## Changing species code for RIHU from MAHU to RIHU from latest renaming

## Indiv_ID is stored as a list for some reason. Flatten it out to save as csv
out_full$Indiv_ID <- vapply(out_full$Indiv_ID, paste, collapse = ", ", character(1L))

out_full$Indiv_ID <- gsub('MA', 'RI', out_full$Indiv_ID) ## Changing species code for RIHU from MAHU to RIHU from latest renaming
out_full$Indiv_ID <- gsub('BLHU', 'BLUH', out_full$Indiv_ID) ## Changing species code for RIHU from MAHU to RIHU from latest renaming

out_full$pasted <- paste(out_full$Indiv_ID, "_", as.character(str_pad(out_full$Date, 4, pad = "0")), out_full$Year, sep="")

write.csv(out_full, file = here("Data", "All_data.csv"))
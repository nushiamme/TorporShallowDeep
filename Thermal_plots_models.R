#### Code description ####
## Shallow-deep torpor paper: Figures and analyses (Figures 2, 4-6) and all models
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
library(plyr) ## Load this before loading dplyr (so that some functions in dplyr aren't masked)
library(dplyr) ## for summarize 
library(reshape2) ## for dcast() function 
library(MASS) ## To check the distribution of the data and run glm.nb
library(ggplot2)
library(scales) # To plot stacked bar as percentages
library(lme4) # Running multilevel mixed models
library(lmerTest) ## Optional, for p values on lmer models
library(lattice) ## qqplot to look at lmer model residuals
#library(viridis) # Source of the colors used here; but manually coded

#### Set working directory and read in files####
wd <- file.path("E:", "Google Drive", "IR_2018_csv", "Data")
setwd(wd)
# Can remake this thermal melted file if needed by running the Thermal_summaries script
thermal_maxes_melted <- read.csv("E:\\Google Drive\\IR_2018_csv\\Thermal_maxes.csv") ## Raw temperatures

# Other files
categories <- read.csv("Category_thresholds.csv")
interpolated <- read.csv("Interpolated_Thermal.csv") ## Temperatures interpolated to 1 minute
categ_percentage <- read.csv("Category_percentages.csv")
masses <- read.csv("Bird_masses.csv")


#### General functions ####
## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

#my_theme2 <- theme_classic(base_size = 30) + 
 # theme(axis.line = element_line(colour = "black"),
  #      text=element_text(family="Cambria"))

## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

## Standardize the color scheme
my_colors <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")


#### Models ####

#### Processing data to run surface vs ambient temperature models ####

## First read in the bird IDs
#### Bird folders ####
bird.folders.2018 <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", "BCHU05_0607",
                       "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                       "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                       "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606")

bird.folders.2017 <- c("BC01_0610", "BC02_0612", "BC03_0617",
                       "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                       "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

bird.folders.all <- c("BCHU01_0521", "BCHU02_0526", "BCHU03_0530", "BCHU04_0607", #"BCHU05_0607",
                      "BLHU01_0521", "BLHU03_0522", "BLHU04_0523", "BLHU05_0523", "BLHU06_0526", "BLHU07_0529", "BLHU08_0601", 
                      "BLHU09_0603", "BLHU12_0605", "BLHU13_0605", 
                      "MAHU02_0520", "MAHU03_0527", "MAHU05_0529", "MAHU06_0530", "MAHU10_0603", "MAHU12_0606", "MAHU13_0606",
                      "BC01_0610", "BC02_0612", "BC03_0617",
                      "BL01_0610", "BL02_0612", "BL03_0614", "BL04_0615",
                      "MA02_0611", "MA05_0615", "MA06_0616", "MA07_0617", "MA08_0619")

## Stacking all the individual birds' data, keeping all melted columns from earlier (hour, min, max, etc.)
out_all <- data.frame(matrix(ncol = 6, nrow=109*length(bird.folders.all)))
names(out_all) <- c("Indiv_ID", "Date", "Time", "variable", "value", "Hour")
for(i in bird.folders.all) {
  wd2 <- file.path("E:", "Google Drive", "IR_2018_csv", "Data", "RDS_files")
  setwd(paste0(wd2, "/", i))
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  out_all <- rbind(out,out_all)
}
dim(out_all) ## Check dimensions, should be ~ 9909 by 6
out_all <- out_all[complete.cases(out_all),] ## Remove rows with NAs
dim(out_all) ## Check dimensions, now 6309 by 6
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
out_full$Species <- substr(out_full$Indiv_ID, 1, 4) ## Making a species column
out_full$Species_numeric <- cumsum(!duplicated(out_full$Species))


#### Models of Surface temperature vs. ambient temperature ####
## Not including Category as a covariate, just doing a linear model of surface vs. ambient temperatures
## Terrible model!
mod.surf_amb_noCateg <- lm(Surf_Temp~Amb_Temp, data=out_full)
summary(mod.surf_amb_noCateg)
plot(mod.surf_amb_noCateg) ## Very skewed qq plot, bad fit

## (1|Categ) would allow intercepts to vary by category, but not slopes. i.e. you fix the slope
## (Amb_Temp|Categ) allows slopes and intercepts to vary by category

## First, multilevel model with random intercepts and fixed slope for all categories
mod_mixed <- lmer(Surf_Temp ~ Amb_Temp + (1|Category), data=out_full)
summary(mod_mixed)
coef(mod_mixed) ## Useful to see all the slopes and intercepts
plot(mod_mixed) ## Definitely many clusters of points
plot(ranef(mod_mixed)) ## plotting random effects of model
plot(residuals(mod_mixed)) ## plot residuals of model

## Random intercepts and random slopes, no species in this equation
mod_mixed_2 <- lmer(Surf_Temp ~ Amb_Temp + (Amb_Temp|Category), data=out_full)
summary(mod_mixed_2)
coef(mod_mixed_2) 
plot(mod_mixed_2)

## Accounting for species, Random intercepts and random slopes
mod_mixed_3 <- lmer(Surf_Temp ~ Amb_Temp + (Amb_Temp|Category) + (Amb_Temp|Species_numeric), data=out_full)
summary(mod_mixed_3)
coef(mod_mixed_3)
plot(mod_mixed_3)

## Same as above but now including mass.
## This is the final full (and best) model
mod_mixed_4 <- lmer(Surf_Temp ~ Amb_Temp + (Amb_Temp|Category) + Cap_mass + (Amb_Temp|Species_numeric), data=out_full)
summary(mod_mixed_4)
coef(mod_mixed_4)
plot(mod_mixed_4)
anova(mod_mixed_3, mod_mixed_4)
qqmath(~resid(mod_mixed_4)) ## Much better!!

## Accounting for individual and species, Random intercepts and random slopes.
## Gives identical results to above. So Indiv ID doesn't make a difference
## Ignore.
mod_mixed_5 <- lmer(Surf_Temp ~ Amb_Temp + (Amb_Temp|Category) + Cap_mass +
                      (Amb_Temp|Species_numeric/Indiv_numeric), data=out_full)
summary(mod_mixed_5)
coef(mod_mixed_5)
plot(mod_mixed_5)

## Run anova of model. Leaving out mod_mixed here because it doesn't make sense that categories would all have one fixed slope
an.mod <- anova(mod_mixed_2,mod_mixed_3, mod_mixed_4)
an.mod



#### Getting proportion of time each individual spent in each category to run model of proportions ####
## Summarize proportion of time spent in each state, by species (uses interpolated data)
data_species_summ <- interpolated %>%
  count(Species, Category) %>%
  group_by(Species) %>%
  mutate(perc = (n / sum(n))*100)

## By individual (uses interpolated data)
data_indiv_summ <- interpolated %>%
  group_by(Indiv_pasted, Category) %>%
  summarise (n = length(Category))
data_indiv_summ

## Summarize (binary) whether individuals used a particular category or not
indiv_categ_count <- ddply(data_indiv_summ, c("Indiv_pasted"), summarise, 
                           Normo=sum(Category=="Normothermic"), Shallow=sum(Category=="Shallow"),
                           Transition=sum(Category=="Transition"), Torpor=sum(Category=="Torpor"))
sum(indiv_categ_count$Normo) #no. indivs that used normo; should be all (33)
sum(indiv_categ_count$Shallow) #no. indivs that used shallow
sum(indiv_categ_count$Transition) #no. indivs that used transition
sum(indiv_categ_count$Torpor) #no. indivs that used deep torpor

# Summarise number of records per individual in each category. Using interpolated data
casted_indiv <-dcast(Indiv_pasted~Category,data=interpolated, fun.aggregate= length,value.var = 'Category')
prop_indiv_time <- data.frame(matrix(ncol = 5, nrow=nrow(casted_indiv)))
names(prop_indiv_time) <- c("Indiv_pasted", "Normothermic", "Shallow", "Transition", "Torpor")
prop_indiv_time$Indiv_pasted <- casted_indiv$Indiv_pasted
for(i in 1:nrow(casted_indiv)) {
  prop_indiv_time$Normothermic[i] <- round((casted_indiv$Normothermic[i]/(sum(casted_indiv$Normothermic[i], 
                                                                              casted_indiv$Shallow[i], casted_indiv$Transition[i],
                                                                              casted_indiv$Torpor[i])))*100,0) 
  prop_indiv_time$Shallow[i] <- round((casted_indiv$Shallow[i]/(sum(casted_indiv$Normothermic[i], 
                                                                    casted_indiv$Shallow[i], casted_indiv$Transition[i],
                                                                    casted_indiv$Torpor[i])))*100,0) 
  prop_indiv_time$Transition[i] <- round((casted_indiv$Transition[i]/(sum(casted_indiv$Normothermic[i], 
                                                                          casted_indiv$Shallow[i], casted_indiv$Transition[i],
                                                                          casted_indiv$Torpor[i])))*100,0) 
  prop_indiv_time$Torpor[i] <- round((casted_indiv$Torpor[i]/(sum(casted_indiv$Normothermic[i], 
                                                                  casted_indiv$Shallow[i], casted_indiv$Transition[i],
                                                                  casted_indiv$Torpor[i])))*100,0)
}
head(prop_indiv_time)

## Melted dataframe for proportion of time spent in diff categories by species. Uses interpolated data
m.prop <- melt(prop_indiv_time, id.vars = "Indiv_pasted", measure.vars = c("Normothermic", "Shallow", "Transition", "Torpor"))
tail(m.prop)
names(m.prop)[names(m.prop) == 'value'] <- 'freq'
m.prop$Species <- substr(m.prop$Indiv_pasted, 1, 4)
m.prop$Species <- as.factor(as.character(m.prop$Species))

#### GLM Models for proportion of time spent per category ####
## Not using individual-level models, doesn't make any sense to.
#Trying to test how species are different, not individuals

## This model has residual variance >> degrees of freedom
mod_glm_freq_sp <- glm(freq~variable*Species-1, data=m.prop, family=poisson())
summary(mod_glm_freq_sp)
coef(mod_glm_freq_sp)

## Because residual variance >> degrees of freedom, trying a quasipoisson
## But the dispersion parameter is still 12.5, which is much greater than 1, meaning it's overdispersed
mod_glm_freq_sp_quasi <- glm(freq~variable*Species-1, data=m.prop, family=quasipoisson())
summary(mod_glm_freq_sp_quasi)

## Running  a negative binomial model, definitely the best. No overdispserion now, much lower residual variance.
mod_glm_freq_sp_nb <- glm.nb(freq~variable*Species-1, data=m.prop)
summary(mod_glm_freq_sp_nb)
coef(mod_glm_freq_sp_nb)

## Predict from this model and add these values back into the m.prop data frame
m.prop$predicted <- predict(mod_glm_freq_sp_nb)
plot(mod_glm_freq_sp_nb)


#### Figures ####
## Figure 2: Single RIHU individual's temperatures plotted over the course of a night
## Points were modified for clarity in Illustrator
## 3D surface plots were constructed in ImageJ and added on in Illustrator/powerpoint
single <- "MAHU10_0603"
for(i in single) {
  setwd(paste0(wd, "/", i))
  
  #### Plotting ####
  out<- readRDS(file=paste(i, "_summ.rds", sep=""))
  
  ## Creating a meaningful time sequence
  birdTime <- out$Time
  TimeOrder1 <- seq(from = 1900, to = 2459, by = 1)
  TimeOrder2 <- seq(from = 0100, to = 0559, by = 1)
  TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2))
  TimeOrder <- factor(TimeOrder, as.character(TimeOrder))
  
  Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT"))
  
  TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)]))
  
  
  out$Time2 <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)]
  pdf("Rplot_trial.pdf", useDingbats = F, width = 13.333, height=7.5)
  ## For use in Adobe Illustrator
  thermplot <- ggplot(out, aes(Time2, value)) +
    geom_point(aes(shape=variable), size=3) + my_theme +
    theme(axis.text.x = element_text(angle=60, size=20, vjust=0.5), panel.grid.major.y = element_line(colour="grey", size=0.5),
          axis.text.y=element_text(size=20), legend.key.height = unit(3, 'lines'),  plot.title = element_text(hjust = 0.5)) +
    scale_shape_manual(values=c(0,1,2), labels=c("Ambient", "Mean surface", "Max surface"), name="Temperature") + 
    scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35)) + 
    ylab(Temp.lab) + xlab("Hour") #+ ggtitle(out$Indiv_ID[1])
  print(thermplot)
  dev.off()
  ggsave("Rplot_trial.pdf") ## For use in Adobe Illustrator
  print(thermplot)
  
  ## To use just round and colored points in R, plot this
  thermplot_col <- ggplot(out, aes(Time2, value)) +
    geom_point(aes(col=variable), size=3) + my_theme +
    theme(axis.text.x = element_text(angle=60, size=15, vjust=0.5), panel.grid.major.y = element_line(colour="grey", size=0.5),
          axis.text.y=element_text(size=15), legend.key.height = unit(3, 'lines'),  plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = c("black", "violet", "red"), 
                       labels=c("Ambient", "Mean surface", "Max surface"), name="Temperature") + 
    scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35)) +
    ylab(Temp.lab) + xlab("Hour") + ggtitle(out$Indiv_ID[1])
  print(thermplot_col)
}


## Figure 4: Surface vs ambient temperature, with one linear model fitted to each category
out_full$Category <- factor(out_full$Category, levels = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
## Plot surface vs ambient temperature
ggplot(out_full, aes(Amb_Temp, Surf_Temp)) + geom_point(aes(col=Category, shape=Category), size=2.5) + my_theme +
  scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35,40)) +
  scale_colour_manual(values=my_colors) +
  geom_smooth(aes(group=Category),method='lm') +
  scale_shape_manual(values = c(15:18)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines')) +
  xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
  guides(colour = guide_legend(override.aes = list(size=4)))


## Figure 5: Range of max surface temperatures per individual (or per night), colored by category
ggplot(thermal_maxes_melted, aes(variable, value)) + my_theme + geom_point(aes(col=Category), size=2, alpha=0.8) +  
  facet_grid(.~Species, scales = "free_x",space = "free_x") +
  ylab(Temp.lab) + xlab("Individual") + 
  #scale_color_manual(values = c('black','deepskyblue2', 'palegreen4', 'red')) +
  scale_color_manual(values=my_colors2) +
  guides(colour = guide_legend(override.aes = list(size=3.5))) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), axis.text.y=element_text(size=20),
        legend.key.height = unit(3, 'lines'))

## Figure 6: Stacked bar for proportion of nighttime spent in each category per species
## Figure 6a using original (non-model) interpolated values
m.categ <- melt(categ_percentage, id.vars="Species", measure.vars = c("Normothermic", "Shallow_torpor", "Transition", "Torpor"))
m.categ$variable <- revalue(m.categ$variable, c("Shallow_torpor"="Shallow Torpor", "Torpor"="Deep Torpor"))
ggplot(m.categ, aes(Species,value)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors2, name="Category") +
  scale_y_continuous(labels = percent_format()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.key.height = unit(3, 'lines'))


## Figure 6b: Using predicted values
ggplot(m.prop, aes(Species,predicted)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors, name="Category") +
  scale_y_continuous(labels = percent_format()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.key.height = unit(3, 'lines'))
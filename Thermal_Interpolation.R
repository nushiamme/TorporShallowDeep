## Code to interpolate thermal data to ensure even sampling across the night
## Shallow-deep torpor paper 
## Anusha Shankar, nushiamme<@>gmail<dot>com

## Read in packages
library(here)
library(plyr)
library(stringr) ## To pad a cell with zeros (str_pad function)

## Read in file
therm_all <- read.csv(here("Data", "All_data.csv"))

### Trying out spline fitting March 11, 2021
test <- therm_all[therm_all$Indiv_numeric==1,]
dataTest_interpol <- data.frame()
for(j in 1:length(unique(test$pasted))) {
  for(i in unique(test$pasted)) {  
    ## Create trial column
    trial <- test[test$pasted==i,]
    trial$Time <- as.numeric(as.character(trial$Time))
    times <- c(seq(2100,2400,1), seq(100,459,1))
    sur_temps <- trial$Surf_Temp
    amb_temps <- trial$Amb_Temp
    #temps <- as.data.frame(trial$value)
    names(sur_temps) <- "Surf_Temp"
    names(amb_temps) <- "Amb_Temp"
    time2 <- trial$Time
    
    ## Interpolate missing surface temperatures
    sfun <- splinefun(time2, sur_temps)
    
    ## Interpolate missing ambient temperatures
    afun <- splinefun(time2, amb_temps)
    
    ## Ordering time in the interpolated data frame
    TimeOrder1 <- seq(from = 2100, to = 2459, by = 1) ## Create seq for first half of night
    TimeOrder2 <- seq(from = 100, to = 459, by = 1) ## Seq for second half of night
    TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2)) ## Combine them, making all same length
    TimeOrder <- factor(TimeOrder, as.character(TimeOrder)) ## Make into a factor in the correct order
    
    Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT")) ## Make minute-by-minute to compare
    
    #TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)])) ## Final time variable
    birdTime <- as.character(times) ## Save the interpolated time as a separate vector
    birdTime <- str_pad(birdTime, width=4, side="left", pad="0") ## Make sure all times are 4 characters long, otherwise pad in front with zero
    ## Compile interpolated df
    interTemp <- data.frame(Indiv_pasted = i,
                            Surf_Temp = sfun(times),
                            Amb_Temp = afun(times),
                            Cap_mass = 0)
    
    interTemp <- data.frame(Indiv_pasted = i,
                            Surf_Temp = sfun(times),
                            Amb_Temp = afun(times),
                            Cap_mass = 0)
    
    interTemp$Category <- 0
    for(k in 1:nrow(interTemp)) {
      categ <- categories[categories$Individual==i,]
      if(interTemp$Surf_Temp[k] > categ$Normo_min) {
        interTemp$Category[k] <- "Normothermic"
      } else if(!is.na(categ$Shallow_min) & interTemp$Surf_Temp[k] > categ$Shallow_min) {
        interTemp$Category[k] <- "Shallow"
      } else if(is.na(categ$Shallow_min) & !is.na(categ$Shallow_max) & interTemp$Surf_Temp[k] < categ$Shallow_max) {
        interTemp$Category[k] <- "Shallow"
      } else if(!is.na(categ$Transition_min) & interTemp$Surf_Temp[k] > categ$Transition_min) {
        interTemp$Category[k] <- "Transition"
      } else if(is.na(categ$Transition_min) & !is.na(categ$Transition_max) & interTemp$Surf_Temp[k] < categ$Transition_max) {
        interTemp$Category[k] <- "Transition"
      } else if(!is.na(categ$Torpor_max) & interTemp$Surf_Temp[k] < categ$Torpor_max) {
        interTemp$Category[k] <- "Torpor"
      }
      interTemp$Cap_mass[k] <- masses$Capture_mass_g[masses$Indiv_ID==interTemp$Indiv_pasted[k]]
    }
    interTemp$Time <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)] ## Match times to correct ordered time and save into new column in interpolated dataset
    interTemp$Species <- substr(interTemp$Indiv_pasted, 1, 4)
    dataTest_interpol <- rbind(dataTest_interpol, interTemp) # add it to your df
  }
}

## Check if there's any NAs. 
sum(is.na(dataTest_interpol$Amb_Temp)) #Good if output is 0
sum(is.na(dataTest_interpol$Surf_Temp)) #Good if output is 0

## Test plot
ggplot(dataTest_interpol, aes(Time, Surf_Temp)) + my_theme2 +
  geom_line(aes(group=Indiv_pasted, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_pasted, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_discrete(breaks = levels(dataTest_interpol$Time)[c(T, rep(F, 20))])




##Have to first reorder time
times <- c(seq(2100,2459,1), seq(100,459,1))
TimeOrder1 <- seq(from = 2100, to = 2459, by = 1) ## Create seq for first half of night
TimeOrder2 <- seq(from = 100, to = 459, by = 1) ## Seq for second half of night
TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2)) ## Combine them, making all same length
TimeOrder <- factor(TimeOrder, as.character(TimeOrder)) ## Make into a factor in the correct order

Time_tobind<- data.frame(Time=TimeOrder)
#Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT")) ## Make minute-by-minute to compare
#TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)])) ## Final time variable

Time_tobind$Time <- as.numeric(as.character(Time_tobind$Time))
Time_tobind$Seq <- seq(1, length(Time_tobind$Time),1)

subsetTime <- Time_tobind %>%
  filter(Time %in% test$Time)

subsetTime$Seq2 <- seq(1, length(subsetTime$Time),1)
head(subsetTime)

test2 <- merge(test,subsetTime)

## From Erich
splinemod<-smooth.spline(y=test2$Surf_Temp, x=test2$Seq,
                             spar=0.3) ## How to decide spar? 

#generate smoothed VO2 data for each minute, based on above spline model
Pred_Temp <- Time_tobind
Pred_Temp$Surf_Temp <- 0
predmodel<- predict(splinemod, x=seq(0,length(Pred_Temp$Seq), length=length(Pred_Temp$Seq))) 
Pred_Temp$Surf_Temp<-predmodel$y 
plot(Pred_Temp$Seq,Pred_Temp$Surf_Temp)
lines(predmodel$x,predmodel$y,col="blue",lwd=2)



#### Tried but not the best ####
### Didn't try
library("gam")
fit3<-gam(wage ~ s(year,df=5)+s(age,df=5)+education,data=Wage)
plot(fit3,se=TRUE)

## Trying a piecewise polynomial spline
require(graphics)
ispl <- polySpline(interpSpline(Surf_Temp ~ Seq2, test2, bSpline = TRUE))
print(ispl)   # print the piecewise polynomial representation
dev.off()
plot(ispl)    # plots over the range of the knots
points(test2$Seq2, test2$Surf_Temp)
#plot(test$Time, test$Surf_Temp)

##
ss1<-smooth.spline(y=test2$Surf_Temp, x=test2$Seq2, df=3)
ss2<-smooth.spline(y=test2$Surf_Temp, x=test2$Seq2,df=15)
ss<-smooth.spline(y=test2$Surf_Temp, x=test2$Seq2)
plot(test2$Seq2,test2$Surf_Temp)
lines(test2$Seq2,ss1$y,col="blue",lwd=2)
lines(test2$Seq2,ss2$y,col="blue",lwd=2,lty=2)
lines(test2$Seq2,ss$y,col="red",lwd=2)
##### End trial interpolations ####



#### Interpolation, DO NOT rerun unless absolutely necessary - takes a lot of time ####
#### Interpolating to estimate proportion of time spent in each category
data_interpol <- data.frame()
for(j in 1:length(unique(therm_all$pasted))) {
  for(i in unique(therm_all$pasted)) {  
    ## Create trial column
    trial <- therm_all[therm_all$pasted==i,]
    trial$Time <- as.numeric(as.character(trial$Time))
    times <- c(seq(1930,2400,1), seq(100,530,1))
    sur_temps <- trial$Surf_Temp
    amb_temps <- trial$Amb_Temp
    #temps <- as.data.frame(trial$value)
    names(sur_temps) <- "Surf_Temp"
    names(amb_temps) <- "Amb_Temp"
    time2 <- trial$Time
    
    ## Interpolate missing surface temperatures
    sfun <- approxfun(time2, sur_temps, rule = 2)
    
    ## Interpolate missing ambient temperatures
    afun <- approxfun(time2, amb_temps, rule = 2)
    
    ## Ordering time in the interpolated data frame
    TimeOrder1 <- seq(from = 1900, to = 2459, by = 1) ## Create seq for first half of night
    TimeOrder2 <- seq(from = 100, to = 559, by = 1) ## Seq for second half of night
    TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2)) ## Combine them, making all same length
    TimeOrder <- factor(TimeOrder, as.character(TimeOrder)) ## Make into a factor in the correct order
    
    Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT")) ## Make minute-by-minute to compare
    
    #TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)])) ## Final time variable
    birdTime <- as.character(times) ## Save the interpolated time as a separate vector
    birdTime <- str_pad(birdTime, width=4, side="left", pad="0") ## Make sure all times are 4 characters long, otherwise pad in front with zero
    ## Compile interpolated df
    interTemp <- data.frame(Indiv_pasted = i,
                            Surf_Temp = sfun(times),
                            Amb_Temp = afun(times),
                            Cap_mass = 0)
    
    interTemp$Category <- 0
    for(k in 1:nrow(interTemp)) {
      categ <- categories[categories$Individual==i,]
      if(interTemp$Surf_Temp[k] > categ$Normo_min) {
        interTemp$Category[k] <- "Normothermic"
      } else if(!is.na(categ$Shallow_min) & interTemp$Surf_Temp[k] > categ$Shallow_min) {
        interTemp$Category[k] <- "Shallow"
      } else if(is.na(categ$Shallow_min) & !is.na(categ$Shallow_max) & interTemp$Surf_Temp[k] < categ$Shallow_max) {
        interTemp$Category[k] <- "Shallow"
      } else if(!is.na(categ$Transition_min) & interTemp$Surf_Temp[k] > categ$Transition_min) {
        interTemp$Category[k] <- "Transition"
      } else if(is.na(categ$Transition_min) & !is.na(categ$Transition_max) & interTemp$Surf_Temp[k] < categ$Transition_max) {
        interTemp$Category[k] <- "Transition"
      } else if(!is.na(categ$Torpor_max) & interTemp$Surf_Temp[k] < categ$Torpor_max) {
        interTemp$Category[k] <- "Torpor"
      }
      interTemp$Cap_mass[k] <- masses$Capture_mass_g[masses$Indiv_ID==interTemp$Indiv_pasted[k]]
    }
    interTemp$Time <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)] ## Match times to correct ordered time and save into new column in interpolated dataset
    interTemp$Species <- substr(interTemp$Indiv_pasted, 1, 4)
    data_interpol <- rbind(data_interpol, interTemp) # add it to your df
  }
}

## Check if there's any NAs. If sfun and afun have rule=1, yields NAs
sum(is.na(data_interpol$Amb_Temp)) #Good if output is 0
sum(is.na(data_interpol$Surf_Temp)) #Good if output is 0
## Add a column for capture masses
#for(i in 1:nrow(data_interpol)) {
#    data_interpol$Cap_mass[i] <- masses$Capture_mass_g[masses$Indiv_ID==data_interpol$Indiv_pasted[i]]
#}

write.csv(data_interpol, file = here("Data", "Interpolated_Thermal.csv"))



## Code to interpolate thermal data to ensure even sampling across the night
## Shallow-deep torpor paper 
## Anusha Shankar, nushiamme<@>gmail<dot>com

## Read in packages
library(here)
library(plyr)
library(stringr) ## To pad a cell with zeros (str_pad function)
library(segmented) ## Trying out a piecewise regression with this

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

## Mar 23, testing out the segmented package

set.seed(12)
xx <- 1:100
zz <- runif(100)
yy <- 2 + 1.5*pmax(xx - 35, 0) - 1.5*pmax(xx - 70, 0) + 15*pmax(zz - .5, 0) + 
  rnorm(100,0,2)
dati <- data.frame(x = xx, y = yy, z = zz)
out.lm <- lm(y ~ x, data = dati)
o <- segmented(out.lm, seg.Z = ~x, psi = list(x = c(30,60)),
               control = seg.control(display = FALSE)
)
dat2 = data.frame(x = xx, y = broken.line(o)$fit)

library(ggplot2)
ggplot(dati, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat2, color = 'blue')

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

############################################################################

### Trying out new interpolation functions and Erich's function
test <- therm_all[therm_all$Indiv_numeric==1,]
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
Torpor_smooth_state_func<-function(Pred_Temp, SPAR_SMOOTHINT, ENTRY_CUTOFF,AROUSAL_CUTOFF){
  # #interpolate data linearly
  # Pred_Temp$Temp_lin<-na.approx(Pred_Temp$Surf_Temp,
  #                           x=Pred_Temp$Seq, na.rm=F)
  
  splinemod<-smooth.spline(y=test2$Surf_Temp, x=test2$Seq,
                           spar=0.3) ## How to decide spar? 
  
  #generate smoothed temp data for each minute, based on above spline model
  Pred_Temp <- Time_tobind
  Pred_Temp$Surf_Temp <- 0
  predmodel<- predict(splinemod, x=seq(0,length(Pred_Temp$Seq), length=length(Pred_Temp$Seq))) 
  Pred_Temp$Surf_Temp<-predmodel$y 
  plot(Pred_Temp$Seq,Pred_Temp$Surf_Temp)
  lines(predmodel$x,predmodel$y,col="blue",lwd=2)
  
  #generate the first derivative (slope) of smoothed temp data for each minute, based on above spline model
  predmodel_deriv<-predict(splinemod, deriv=1, x=seq(0,length(Pred_Temp$Seq), length=length(Pred_Temp$Seq))) 
  Pred_Temp$Temp_smoothderiv<-predmodel_deriv$y
  plot(Pred_Temp$Temp_smoothderiv) 
  
  ############################################################################
  
  #SET ENTRY AND AROUSAL CUTOFFS
  
  #this is initially determined by trial and error, but it is only really used 
  #to generate some general bounds of the states in order to generate 2 
  #separate splines, one for entry and one for arousal, which is done in state funciton 2
  
  entry_cutoff<- -0.2 #(Temp/min)
  arousal_cutoff<- 0.35 #(Temp/min)
  
  ############################################################################
  
  #ANNOTATE EACH MINUTE OF Temp WITH A STABILITY LABEL BASED ON ITS RATE OF CHANGE
  #AND THE CUTOFF VALUES
  
  Pred_Temp$Stability<-"stable" #make everything stable to start
  
  #label everything with a slope of less than -.003 as "DECREASING"
  Pred_Temp$Stability[Pred_Temp$Temp_smoothderiv < entry_cutoff] <- "decreasing"
  
  #label everything with a slope of more than .005 as "INCREASING"
  Pred_Temp$Stability[Pred_Temp$Temp_smoothderiv > arousal_cutoff] <-"increasing"
  
  #copy the stability labels to state, which will be turned into actual states below
  Pred_Temp$State<-Pred_Temp$Stability
  
  ## plot temp, colored by stability
  ggplot(data=Pred_Temp)+ geom_point(aes(x=Seq, y=Surf_Temp), col="black",lwd=2)+
    geom_point(aes(x=Seq, y=Temp_smoothderiv*2, col=Stability))+
    my_theme  #+ theme(legend.position = "none")
  
  ############################################################################
  
  #Within the torpor nights, there are a few different patterns based on 
  #time resolution and data completeness. #these require slightly different
  #criteria/logic to label properly
  #1. Full nights where there is a full normal pattern 
  #2. NoPre nights with no or minimal pretorpor values (when the bird starts torpor pretty much immediately) 
  #3. NoPost night with no or minimal post torpor values
  #4. Dip nights when the bird never reaches stable torpor
  
  #FULL TORPOR 
  if(Pred_Temp$Torpor_Type[1]=="Full"){
    
    #where Temp INCREASING and min elapsed is AFTER the 1st stable period, label as AROUSAL
    Pred_Temp$State[Pred_Temp$Stability=="INCREASING" &
                   Pred_Temp$Seq >= first(Pred_Temp$Seq[Pred_Temp$Stability=="STABLE"])]<-"AROUSAL"
    
    #where Temp STABLE, and min elapsed is before the start of entry, as "NORMOPRE"
    Pred_Temp$State[Pred_Temp$Stability== "STABLE" & 
                   Pred_Temp$Seq <= last(Pred_Temp$Seq[Pred_Temp$Stability=="DECREASING"])] <- "NORMOPRE" 
    
    #where Temp stable, and min elapsed is before the start of AROUSAL, as "NORMOPOST"
    Pred_Temp$State[Pred_Temp$Stability== "STABLE" &
                   Pred_Temp$Seq >= last(Pred_Temp$Seq[Pred_Temp$Stability=="INCREASING"])] <- "NORMOPOST" 
    
    #where Temp is DECREASING, and min elapsed is BEFORE THE START OF NORMOPRE , as "FREAKOUT"
    Pred_Temp$State[Pred_Temp$State== "DECREASING" & 
                   Pred_Temp$Seq <= first(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"])] <- "FREAKOUT" 
    
    #where Temp is DECREASING, and min elapsed is AFTER the start of entry, as "ENTRY"
    Pred_Temp$State[Pred_Temp$State== "DECREASING" & 
                   Pred_Temp$Seq >= last(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"])] <- "ENTRY" 
    
    #where Temp stable, and min elapsed is AFTER the end of entry, and BEFORE the start of arousal, as TORPOR
    Pred_Temp$State[Pred_Temp$Seq <= (first(Pred_Temp$Seq[Pred_Temp$State=="AROUSAL"])) &
                   Pred_Temp$Seq >= (last(Pred_Temp$Seq[Pred_Temp$State=="ENTRY"]))] <- "TORPOR" 
  }#END FULL TORPOR
  
  ############################################################################
  
  #DIP 
  else if(Pred_Temp$Torpor_Type[1]=="Dip"){ 
    
    #where Temp INCREASING and min elapsed is AFTER the 1st stable period, label as AROUSAL
    Pred_Temp$State[Pred_Temp$Stability=="INCREASING" &
                   Pred_Temp$Seq >= first(Pred_Temp$Seq[Pred_Temp$Stability=="STABLE"])]<-"AROUSAL"
    
    #where Temp STABLE, and min elapsed is before the start of entry, as "NORMOPRE"
    Pred_Temp$State[Pred_Temp$Stability== "STABLE" & 
                   Pred_Temp$Seq <= last(Pred_Temp$Seq[Pred_Temp$Stability=="DECREASING"])] <- "NORMOPRE" 
    
    #where Temp stable, and min elapsed is before the start of AROUSAL, as "NORMOPOST"
    Pred_Temp$State[Pred_Temp$Stability== "STABLE" &
                   Pred_Temp$Seq >= last(Pred_Temp$Seq[Pred_Temp$Stability=="INCREASING"])] <- "NORMOPOST" 
    
    #where Temp is DECREASING, and min elapsed is BEFORE THE START OF NORMOPRE , as "FREAKOUT"
    Pred_Temp$State[Pred_Temp$State== "DECREASING" & 
                   Pred_Temp$Seq <= first(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"])] <- "FREAKOUT" 
    
    #where Temp is DECREASING, and min elapsed is AFTER the start of entry, as "ENTRY"
    Pred_Temp$State[Pred_Temp$State== "DECREASING" & 
                   Pred_Temp$Seq >= last(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"])] <- "ENTRY" 
    
    #where Temp stable, and min elapsed is AFTER the end of entry, and BEFORE the start of arousal, as TORPOR
    Pred_Temp$State[Pred_Temp$Seq <= (first(Pred_Temp$Seq[Pred_Temp$State=="AROUSAL"])) &
                   Pred_Temp$Seq >= (last(Pred_Temp$Seq[Pred_Temp$State=="ENTRY"]))] <- "TORPOR" 
    
  }#END DIP
  
  ############################################################################
  
  #NOPOST
  else if(Pred_Temp$Torpor_Type[1]=="NoPost"){
    
    #where Temp INCREASING and min elapsed is AFTER the 1st stable period, label as AROUSAL
    Pred_Temp$State[Pred_Temp$Stability=="INCREASING" &
                   Pred_Temp$Seq >= first(Pred_Temp$Seq[Pred_Temp$Stability=="STABLE"])]<-"AROUSAL"
    
    #where Temp STABLE, and min elapsed is before the start of entry, as "NORMOPRE"
    Pred_Temp$State[Pred_Temp$Stability== "STABLE" & 
                   Pred_Temp$Seq <= last(Pred_Temp$Seq[Pred_Temp$Stability=="DECREASING"])] <- "NORMOPRE" 
    
    #where Temp stable, and min elapsed is before the start of AROUSAL, as "NORMOPOST"
    # Pred_Temp$State[Pred_Temp$Stability== "STABLE" &
    #                Pred_Temp$Seq >= last(Pred_Temp$Seq[Pred_Temp$Stability=="INCREASING"])] <- "NORMOPOST" 
    Pred_Temp$State[Pred_Temp$Seq>length(Pred_Temp$Seq)-2] <- "NORMOPOST" 
    Pred_Temp$State[Pred_Temp$Seq>length(Pred_Temp$Seq)-3] <- "NORMOPOST" 
    
    #where Temp is DECREASING, and min elapsed is BEFORE THE START OF NORMOPRE , as "FREAKOUT"
    Pred_Temp$State[Pred_Temp$State== "DECREASING" & 
                   Pred_Temp$Seq <= first(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"])] <- "FREAKOUT" 
    
    #where Temp is DECREASING, and min elapsed is AFTER the start of entry, as "ENTRY"
    Pred_Temp$State[Pred_Temp$State== "DECREASING" & 
                   Pred_Temp$Seq >= last(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"])] <- "ENTRY" 
    
    #where Temp stable, and min elapsed is AFTER the end of entry, and BEFORE the start of arousal, as TORPOR
    Pred_Temp$State[Pred_Temp$Seq <= (first(Pred_Temp$Seq[Pred_Temp$State=="AROUSAL"])) &
                   Pred_Temp$Seq >= (last(Pred_Temp$Seq[Pred_Temp$State=="ENTRY"]))] <- "TORPOR" 
    
  }#END NOPOST
  
  ############################################################################
  
  #NO PRE TORPOR NORMOTHERMY
  else if(Pred_Temp$Torpor_Type[1]=="NoPre"){ 
    
    #where Temp INCREASING and min elapsed is AFTER the 1st stable period, label as AROUSAL
    Pred_Temp$State[Pred_Temp$Stability=="INCREASING" &
                   Pred_Temp$Seq >= first(Pred_Temp$Seq[Pred_Temp$Stability=="STABLE"])]<-"AROUSAL"
    
    #since htere is no stable period in nopres, just mark the first row (really the 0 row) as NORMOPRE
    Pred_Temp$State[Pred_Temp$Seq<1] <- "NORMOPRE" 
    
    #Since There is no stable period in nopres, just mark the first row (really the 0 row) as NORMOPRE
    Pred_Temp$State[Pred_Temp$Seq<1] <- "NORMOPRE" 
    
    #where Temp stable, and min elapsed is before the start of AROUSAL, as "NORMOPOST"
    Pred_Temp$State[Pred_Temp$Seq>length(Pred_Temp$Seq)-2] <- "NORMOPOST" 
    
    ##theres not going to be freakouts in these nopre nights
    #where Temp is DECREASING, and min elapsed is BEFORE THE START OF NORMOPRE , as "FREAKOUT"
    #Pred_Temp$State[Pred_Temp$State== "DECREASING" & 
    #               Pred_Temp$Seq <= first(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"])] <- "FREAKOUT" 
    
    #where Temp is DECREASING, and min elapsed is AFTER the start of entry, as "ENTRY"
    Pred_Temp$State[Pred_Temp$State== "DECREASING" & 
                   Pred_Temp$Seq >= last(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"])] <- "ENTRY" 
    
    #where Temp stable, and min elapsed is AFTER the end of entry, and BEFORE the start of arousal, as TORPOR
    Pred_Temp$State[Pred_Temp$Seq <= (first(Pred_Temp$Seq[Pred_Temp$State=="AROUSAL"])) &
                   Pred_Temp$Seq >= (last(Pred_Temp$Seq[Pred_Temp$State=="ENTRY"]))] <- "TORPOR" 
    
    #where Temp STABLE, and min elapsed is before the start of entry, as "NORMOPRE"
    Pred_Temp$State[Pred_Temp$Stability== "STABLE" & 
                   Pred_Temp$Seq <= first(Pred_Temp$Seq[Pred_Temp$State=="ENTRY"])] <- "NORMOPRE" 
    
    
  }
  #This one night is really messed up and work work without manually filling in 
  #the labels, which are really obvious. this is only needed for this preliminary
  #state function so it is really no different from any other night. 
  
  if(Pred_Temp$Pred_Temp[1]=="B12_9.5.19"){
    Pred_Temp$State[Pred_Temp$State=="STABLE" & Pred_Temp$Seq<600]<-"TORPOR"
    Pred_Temp$State[Pred_Temp$State=="STABLE" & Pred_Temp$Seq>=600&Pred_Temp$State!="NORMOPOST" ]<-"AROUSAL"
  }
  
  ##############################################################################     
  ##############################################################################    
  
  #RESTRUCTURE STATE AS A FACTOR
  #the above functions mess some structures up so this corrects. 
  
  Pred_Temp$State<-factor(Pred_Temp$State, levels=c("FREAKOUT","NORMOPRE","ENTRY","TORPOR","AROUSAL", "NORMOPOST"),ordered=F)
  Pred_Temp$State<-as.character(Pred_Temp$State)
  
  ##############################################################################
  
  #CALCULATE SEVERAL SPECIFIC VALUES FOR EACH NIGHT AND SAVE THEM TO Pred_Temp
  
  #Create vectors of single values for the boundaries of each state period
  Pred_Temp$Freak_Start<-min(Pred_Temp$Seq[Pred_Temp$State=="FREAKOUT"],na.rm=T)
  Pred_Temp$Freak_End<-max(Pred_Temp$Seq[Pred_Temp$State=="FREAKOUT"],na.rm=T)
  Pred_Temp$Normopre_Start<-min(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"],na.rm=T)
  Pred_Temp$Normopre_End<-max(Pred_Temp$Seq[Pred_Temp$State=="NORMOPRE"],na.rm=T)
  Pred_Temp$Entry_Start<-min(Pred_Temp$Seq[Pred_Temp$State=="ENTRY"],na.rm=T)
  Pred_Temp$Entry_End<-max(Pred_Temp$Seq[Pred_Temp$State=="ENTRY"],na.rm=T)
  Pred_Temp$Torpor_Start<-min(Pred_Temp$Seq[Pred_Temp$State=="TORPOR"],na.rm=T)
  Pred_Temp$Torpor_End<-max(Pred_Temp$Seq[Pred_Temp$State=="TORPOR"],na.rm=T)
  Pred_Temp$Arousal_Start<-min(Pred_Temp$Seq[Pred_Temp$State=="AROUSAL"],na.rm=T)
  Pred_Temp$Arousal_End<-max(Pred_Temp$Seq[Pred_Temp$State=="AROUSAL"],na.rm=T)
  Pred_Temp$Normopost_Start<-min(Pred_Temp$Seq[Pred_Temp$State=="NORMOPOST"],na.rm=T,na.rm=T)
  Pred_Temp$Normopost_End<-max(Pred_Temp$Seq[Pred_Temp$State=="NORMOPOST"],na.rm=T)
  
  ###########################################################################
  
  #Generate blank vectors that will be filled in state function 2
  #Turn them to NA rather than deleting them so that the orders of the variables wont get messed up
  
  Pred_Temp$Pre_Duration<-NA
  Pred_Temp$Entry_Duration<-NA
  Pred_Temp$Torpor_Duration<-NA
  Pred_Temp$Arousal_Duration<-NA
  Pred_Temp$Post_Duration<-NA
  Pred_Temp$Night_Duration<-NA
  
  ###########################################################################
  
  #Calculate a time vector as % of the night duration 
  Pred_Temp$Time_p<- NA #Pred_Temp$Seq/Pred_Temp$Night_Duration*100 #THIS is going to be na for now cuz night duraiton is na. 
  
  ###########################################################################
  
  #Generate blank vectors that will be filled in state function 2
  #Turn them to NA rather than deleting them so that the orders of the variables wont get messed up
  #Pred_Temp$Time_p_LOFF<-Pred_Temp$Seq[Pred_Temp$Seq==Pred_Temp$MinElapsed_LOFF[1]]/Pred_Temp$Night_Duration#SHOUDL BE 0%
  Pred_Temp$Time_p_Entry_Start<-NA
  Pred_Temp$Time_p_Entry_End<-NA
  Pred_Temp$Time_p_Torpor_Start<-NA
  Pred_Temp$Time_p_Torpor_End<-NA
  Pred_Temp$Time_p_Arousal_Start<-NA
  Pred_Temp$Time_p_Arousal_End<-NA
  #Pred_Temp$Time_p_LON<-Pred_Temp$Seq[Pred_Temp$Seq==Pred_Temp$MinElapsed_LON[1]]/Pred_Temp$Night_Duration#SHOUDL BE 100%
  
  ###########################################################################
  
  #Generate blank vectors that will be filled in state function 2
  #Turn them to NA rather than deleting them so that the orders of the variables wont get messed up
  Pred_Temp$Pre_Percent_Night<- NA
  Pred_Temp$Entry_Percent_Night<-NA
  Pred_Temp$Torpor_Percent_Night<-NA
  Pred_Temp$Arousal_Percent_Night<-NA
  Pred_Temp$Post_Percent_Night<-NA
  
  ################################################################################    
  ################################################################################    
  ################################################################################    
  ################################################################################    
  ################################################################################    
  
  #GENERATE A PLOT TO CHECK THAT THE FUNCTION IS WORKING
  
  plot<-ggplot(data=Pred_Temp)+
    
    geom_point(aes(x=Seq, y=Temp), col="black",lwd=2)+
    geom_line(aes(x=Seq,y=Temp_smooth, col=State),lwd=2, alpha=.5)+
    geom_line(aes(x=Seq, y=Temp_int), col="black",lwd=.5)+
    geom_smooth(method="loess",aes(x=Seq, y=Temp_int, col=State), lwd=1, alpha=.2,se=F)+
    scale_color_manual(values=c("green","red","purple","blue","orange", "red"))+
    #scale_y_continuous(limits = c(0,1.5))+
    my_theme  #+ theme(legend.position = "none")
  
  #print(plot)
  
  return(Pred_Temp)  
}



############################################################################

## Trying for a BLUH Indiv 10 (normo and shallow)
test <- therm_all[therm_all$Indiv_numeric==10,]
##Have to first reorder time
#times <- c(seq(2021,2459,1), seq(100,507,1))
TimeOrder1 <- seq(from = 2021, to = 2459, by = 1) ## Create seq for first half of night
TimeOrder2 <- seq(from = 100, to = 507, by = 1) ## Seq for second half of night
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

#generate smoothed temp data for each minute, based on above spline model
Pred_Temp <- Time_tobind
Pred_Temp$Surf_Temp <- 0
predmodel<- predict(splinemod, x=seq(0,length(Pred_Temp$Seq), length=length(Pred_Temp$Seq))) 
Pred_Temp$Surf_Temp<-predmodel$y 
plot(Pred_Temp$Seq,Pred_Temp$Surf_Temp,ylim = c(0,40))
#lines(predmodel$x,predmodel$y,col="blue",lwd=2)

#generate the first derivative (slope) of smoothed temp data for each minute, based on above spline model
predmodel_deriv<-predict(splinemod, deriv=1, x=seq(0,length(Pred_Temp$Seq), length=length(Pred_Temp$Seq))) 
Pred_Temp$Temp_smoothderiv<-predmodel_deriv$y
plot(Pred_Temp$Temp_smoothderiv) 

############################################################################

#SET ENTRY AND AROUSAL CUTOFFS

#this is initially determined by trial and error, but it is only really used 
#to generate some general bounds of the states in order to generate 2 
#separate splines, one for entry and one for arousal, which is done in state funciton 2

sh_entry_cutoff<- -0.05 #(Temp/min)
sh_arousal_cutoff<- 0.07 #(Temp/min)

############################################################################

#ANNOTATE EACH MINUTE OF VO2 WITH A STABILITY LABEL BASED ON ITS RATE OF CHANGE
#AND THE CUTOFF VALUES

Pred_Temp$Stability<-"stable" #make everything stable to start

#label everything with a slope of less than -.003 as "DECREASING"
Pred_Temp$Stability[Pred_Temp$Temp_smoothderiv < entry_cutoff] <- "decreasing"

#label everything with a slope of more than .005 as "INCREASING"
Pred_Temp$Stability[Pred_Temp$Temp_smoothderiv > arousal_cutoff] <-"increasing"

#copy the stability labels to state, which will be turned into actual states below
Pred_Temp$State<-Pred_Temp$Stability

## plot temp, colored by stability
ggplot(data=Pred_Temp)+ geom_point(aes(x=Seq, y=Surf_Temp, col=Stability),lwd=2)+
  geom_point(aes(x=Seq, y=Temp_smoothderiv*2, col=Stability))+
  my_theme  + ylab(Temp.lab) + xlab("Time")



############################################################################

#### Tried but not the best ####
### Didn't try
#library(gam)
#fit3<-gam(wage ~ s(year,df=5)+s(age,df=5)+education,data=Wage)
#plot(fit3,se=TRUE)

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



## Code to interpolate thermal data to ensure even sampling across the night
## Shallow-deep torpor paper 
## Anusha Shankar, nushiamme<@>gmail<dot>com

## Read in packages
library(here)
library(plyr)
library(stringr) ## To pad a cell with zeros (str_pad function)
library(segmented) ## Trying out a piecewise regression with this
library(chron)
library(gridExtra) ## for seeing interpol and raw plots side by side

## Generic standard error function
se <- function(dt) {
  sd(dt, na.rm=T) /  
  sqrt(length(dt[!is.na(dt)]))
}


## Read in file
therm_all <- read.csv(here("Data", "All_data.csv"))

## Process therm_all dataset for time
therm_all$Month <- substr(therm_all$Date, 1, 1)
therm_all$Day <- as.numeric(substr(therm_all$Date, 2, 3))
therm_all$Year <-as.numeric(paste0("20",therm_all$Year))
therm_all$Minute <- as.numeric(str_sub(therm_all$Time, -2))
therm_all$Hour2 <- therm_all$Hour
therm_all$Hour2[therm_all$Hour==24] <- 0
therm_all$Day2 <- therm_all$Day
therm_all$Day2[therm_all$Hour2<7] <- therm_all$Day2[therm_all$Hour2<7]+1
therm_all$Date_fixed1 <- as.POSIXct(paste(therm_all$Year, therm_all$Month, therm_all$Day, sep = "-"), 
                                         format='%Y-%m-%d')
therm_all$Date_fixed2 <- as.POSIXct(paste(therm_all$Year, therm_all$Month, therm_all$Day2, sep = "-"), 
                                    format='%Y-%m-%d')

therm_all$DateFormat <- as.POSIXct(paste(paste(therm_all$Year, therm_all$Month, therm_all$Day2, sep = "-"), 
                                         paste(str_pad(therm_all$Hour2, width=2, side="left", pad="0"), 
                                               str_pad(therm_all$Minute, width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                                   format='%Y-%m-%d %H:%M')
# therm_all$TimeFormat <- as.POSIXct(paste(str_pad(therm_all$Hour2, width=2, side="left", pad="0"),
#                                          str_pad(therm_all$Minute, width=2, side="left", pad="0"), "00", sep = ":"),
#                                    format='%H:%M')

therm_all <- therm_all[order(as.POSIXct(therm_all$DateFormat, format="%Y-%m-%d %H:%M")),]


# ## Apr 2, 2021
# trial <- therm_all[therm_all$pasted=="BCHU01_061017",]
# #trial$Time <- as.numeric(as.character(trial$Time))
# #times <- c(seq(1930,2400,10), seq(100,530,10))
# 
# ## Getting times to start and stop per night at the hour that the recordings started and stopped
# Night_start_hr <- head(trial$Hour, n=1) 
# Night_stop_hr <- tail(trial$Hour, n=1)
# if(trial$Minute[1]<=50){
#   Night_start_mn <- head(trial$Minute, n=1)
# } else if (trial$Minute[1] >50) {
#   Night_start_mn <- 50
#   }
# Night_stop_mn <- tail(trial$Minute, n=1)
# 
# date1 <- trial$Date_fixed1[trial$Hour>7]
# t1 <- merge(Night_start_hr, seq(from=Night_start_mn, to=50, by = 10))
# t1 <- rbind(t1, merge((Night_start_hr+1):23, seq(0, 50, by = 10)))
# t1 <- as.POSIXct(paste(t1$x, str_pad(t1$y, width=2, side="left", pad="0"), "00", sep=':'), 
#                  format='%H:%M:%S')
# t1 <- t1[order(t1)]
# t1
# #date1 <- date1[order(date1)]
# date2 <- trial$Date_fixed2[trial$Hour<7]
# t2 <- merge(0:(Night_stop_hr-1), seq(0, 50, by = 10))
# t2 <- rbind(t2, merge(Night_stop_hr, seq(0, Night_stop_mn, by = 10)))
# t2 <- as.POSIXct(paste(t2$x, str_pad(t2$y, width=2, side="left", pad="0"), "00", sep=':'), 
#                  format='%H:%M:%S')
# # t2 <- data.frame('Interval' = chron(time = paste(t2$x, ':', t2$y, ':', 0)))
# t2 <- t2[order(t2)]
# t2
# 
# #date2 <- date2[order(date2)]
# 
# dt1 <- merge(unique(date1), strftime(t1, format="%H:%M:%S"))
# dt1 <- as.POSIXct(paste(dt1$x, dt1$y),format="%Y-%m-%d %H:%M")
# dt2 <- merge(unique(date2), strftime(t2, format="%H:%M:%S"))
# dt2 <- as.POSIXct(paste(dt2$x, dt2$y),format="%Y-%m-%d %H:%M")
# 
# times_trial <- c(dt1, dt2)
# times_trial <- times_trial[order(times_trial)]
# times_num <- as.numeric(times_trial)
# 
# sur_temps <- trial$Surf_Temp
# amb_temps <- trial$Amb_Temp
# #temps <- as.data.frame(trial$value)
# names(sur_temps) <- "Surf_Temp"
# names(amb_temps) <- "Amb_Temp"
# time2 <- as.numeric(trial$DateFormat)
# 
# 
# 
# #time2 <- trial$DateFormat
# 
# ## Interpolate missing surface temperatures
# sfun <- approxfun(time2, sur_temps, rule = 2)
# 
# ## Interpolate missing ambient temperatures
# afun <- approxfun(time2, amb_temps, rule = 2)
# 
# data.frame(Indiv_pasted = i,
#            Time = times_trial,
#            Surf_Temp = sfun(times_trial),
#            Amb_Temp = afun(times_trial),
#            Cap_mass = 0)
# plot(times_trial,col="red")
# points(time2)
# 


### Trying out spline fitting March 11, 2021
## On Mar 30, 2021, trying to interpolate every 10 min instead of 1 min
#test <- therm_all[therm_all$Indiv_numeric==1,]
data_interpol <- data.frame()
for(j in 1:length(unique(therm_all$pasted))) {
  for(i in unique(therm_all$pasted)) {  
    ## Create trial column
    trial <- therm_all[therm_all$pasted==i,]
    #trial$Time <- as.numeric(as.character(trial$Time))
    #times <- c(seq(1930,2400,10), seq(100,530,10))
    
    ## Getting times to start and stop per night at the hour that the recordings started and stopped
    Night_start_hr <- head(trial$Hour, n=1) 
    Night_stop_hr <- tail(trial$Hour, n=1)
    if(trial$Minute[1]<=50){
      Night_start_mn <- head(trial$Minute, n=1)
    } else if (trial$Minute[1] >50) {
      Night_start_mn <- 50
    }
    Night_stop_mn <- tail(trial$Minute, n=1)
    
    date1 <- trial$Date_fixed1[trial$Hour>7]
    t1 <- merge(Night_start_hr, seq(Night_start_mn, 50, by = 10))
    t1 <- rbind(t1, merge((Night_start_hr+1):23, seq(0, 50, by = 10)))
    t1 <- as.POSIXct(paste(t1$x, str_pad(t1$y, width=2, side="left", pad="0"), "00", sep=':'), 
                     format='%H:%M:%S')
    t1 <- t1[order(t1)]
    
    #date1 <- date1[order(date1)]
    date2 <- trial$Date_fixed2[trial$Hour<7]
    t2 <- merge(0:(Night_stop_hr-1), seq(0, 50, by = 10))
    t2 <- rbind(t2, merge(Night_stop_hr, seq(0, Night_stop_mn, by = 10)))
    t2 <- as.POSIXct(paste(t2$x, str_pad(t2$y, width=2, side="left", pad="0"), "00", sep=':'), 
                     format='%H:%M:%S')
    # t2 <- data.frame('Interval' = chron(time = paste(t2$x, ':', t2$y, ':', 0)))
    t2 <- t2[order(t2)]
    
    dt1 <- merge(unique(date1), strftime(t1, format="%H:%M:%S"))
    dt1 <- as.POSIXct(paste(dt1$x, dt1$y),format="%Y-%m-%d %H:%M")
    dt2 <- merge(unique(date2), strftime(t2, format="%H:%M:%S"))
    dt2 <- as.POSIXct(paste(dt2$x, dt2$y),format="%Y-%m-%d %H:%M")
    
    times_trial <- c(dt1, dt2)
    times_trial <- times_trial[order(times_trial)]
    times_num <- as.numeric(times_trial)
    
    sur_temps <- trial$Surf_Temp
    amb_temps <- trial$Amb_Temp
    #temps <- as.data.frame(trial$value)
    names(sur_temps) <- "Surf_Temp"
    names(amb_temps) <- "Amb_Temp"
    time2 <- as.numeric(trial$DateFormat)
    
    #time2 <- trial$DateFormat
    
    ## Interpolate missing surface temperatures
    sfun <- approxfun(time2, sur_temps, rule = 2)
    
    ## Interpolate missing ambient temperatures
    afun <- approxfun(time2, amb_temps, rule = 2)
    
    interTemp <- data.frame(Indiv_pasted = i,
               Time = times_trial,
               Surf_Temp = sfun(times_trial),
               Amb_Temp = afun(times_trial),
               Cap_mass = 0)
    #Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "10 min"),"%H%M", tz="GMT")) ## Make minute-by-minute to compare
    
    #TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)])) ## Final time variable
    #birdTime <- as.character(times) ## Save the interpolated time as a separate vector
    #birdTime <- str_pad(birdTime, width=4, side="left", pad="0") ## Make sure all times are 4 characters long, otherwise pad in front with zero
    
    interTemp$Category <- 0
    for(k in 1:nrow(interTemp)) {
      categ <- categories[categories$Individual==i,]
      if(interTemp$Surf_Temp[k] > categ$Normo_min) {
        interTemp$Category[k] <- "Normothermic"
      } else if(!is.na(categ$Shallow_min) & interTemp$Surf_Temp[k] > categ$Shallow_min) {
        interTemp$Category[k] <- "Shallow Torpor"
      } else if(is.na(categ$Shallow_min) & !is.na(categ$Shallow_max) & interTemp$Surf_Temp[k] < categ$Shallow_max) {
        interTemp$Category[k] <- "Shallow Torpor"
      } else if(!is.na(categ$Transition_min) & interTemp$Surf_Temp[k] > categ$Transition_min) {
        interTemp$Category[k] <- "Transition"
      } else if(is.na(categ$Transition_min) & !is.na(categ$Transition_max) & interTemp$Surf_Temp[k] < categ$Transition_max) {
        interTemp$Category[k] <- "Transition"
      } else if(!is.na(categ$Torpor_max) & interTemp$Surf_Temp[k] < categ$Torpor_max) {
        interTemp$Category[k] <- "Deep Torpor"
      }
      interTemp$Cap_mass[k] <- masses$Capture_mass_g[masses$Indiv_ID==interTemp$Indiv_pasted[k]]
    }
    #interTemp$Time <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)] ## Match times to correct ordered time and save into new column in interpolated dataset
    interTemp$Species <- substr(interTemp$Indiv_pasted, 1, 4)
    data_interpol <- rbind(data_interpol, interTemp) # add it to your df
  }
}

data_interpol$Category <- factor(data_interpol$Category, levels=c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))


### Merge raw and interpolated data frames to compare sampling frequencies
data_interpol$RawInterpol <- "Interpolated"

Raw_tomerge <- therm_all[, c("pasted", "DateFormat", "Surf_Temp", "Amb_Temp",
                             "Cap_mass", "Category", "Species")]

names(Raw_tomerge)[names(Raw_tomerge)=="pasted"] <- "Indiv_pasted"
names(Raw_tomerge)[names(Raw_tomerge)=="DateFormat"] <- "Time"
Raw_tomerge$RawInterpol <- "Raw"

non_dupl_interpol <- data_interpol[!duplicated(data_interpol[c("Indiv_pasted", "Time", "Surf_Temp",
                                                               "Amb_Temp", "Cap_mass", "Category",
                                                               "Species", "RawInterpol")]),]

merged_raw_interpol <- rbind(Raw_tomerge, non_dupl_interpol)

merged_raw_interpol <- merged_raw_interpol[order(as.POSIXct(merged_raw_interpol$Time, format="%Y-%m-%d %H:%M")),]



## What was night length in minutes per individual?
Nightlength_raw <- vector()
Nightlength_interpol <- vector()
for(i in unique(therm_all$pasted)) {
  Nightlength_raw[i] <- difftime(tail(therm_all$DateFormat[therm_all$pasted==i], n=1), head(therm_all$DateFormat[therm_all$pasted==i], n=1), units='mins')
  Nightlength_interpol[i] <- difftime(tail(data_interpol$Time[data_interpol$Indiv_pasted==i], n=1), 
                                      head(data_interpol$Time[data_interpol$Indiv_pasted==i], n=1), units='mins')
}
Nightlength_raw
Nightlength_raw <- data.frame(pasted= names(Nightlength_raw), NightLength = Nightlength_raw)

Nightlength_interpol



# ## Mar 23, testing out the segmented package
# 
# set.seed(12)
# xx <- 1:100
# zz <- runif(100)
# yy <- 2 + 1.5*pmax(xx - 35, 0) - 1.5*pmax(xx - 70, 0) + 15*pmax(zz - .5, 0) + 
#   rnorm(100,0,2)
# dati <- data.frame(x = xx, y = yy, z = zz)
# out.lm <- lm(y ~ x, data = dati)
# o <- segmented(out.lm, seg.Z = ~x, psi = list(x = c(30,60)),
#                control = seg.control(display = FALSE)
# )
# dat2 = data.frame(x = xx, y = broken.line(o)$fit)
# 
# library(ggplot2)
# ggplot(dati, aes(x = x, y = y)) +
#   geom_point() +
#   geom_line(data = dat2, color = 'blue')


## Interpolated vs. raw data plot
interPlot <- ggplot(data_interpol, aes(Time, Surf_Temp)) + my_theme2 + facet_wrap(.~Indiv_pasted, scales="free_x") +
  geom_line(aes(group=Indiv_pasted, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_pasted, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90))

rawPlot <- ggplot(therm_all[therm_all$pasted=="BLUH03_061417",], aes(DateFormat, Surf_Temp)) + my_theme2 + #facet_wrap(.~pasted, scales="free_x") +
  geom_point(aes(group=pasted, col=Category), size=1.5) +
  #geom_line(aes(group=pasted, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90))

grid.arrange(interPlot, rawPlot, nrow=1, ncol=2)


## Trying to measure duration in each category
for(i in unique(therm_all$pasted)) {
  trial <- therm_all[therm_all$pasted==i,]
for(n in 1:(length(trial$Surf_Temp)-1)) {
  trial$Duration[n] <- difftime(trial$DateFormat[(n+1)], trial$DateFormat[n], units='mins')
  }
  therm_all$Duration[therm_all$pasted==i] <- trial$Duration
}

for(i in unique(therm_all$pasted)) {
  trial <- therm_all[therm_all$pasted==i,]
  for(n in 2:length(trial$Surf_Temp)) {
    trial$Duration2[1] <- NA
    trial$Duration2[n] <- difftime(trial$DateFormat[n], trial$DateFormat[(n-1)], units='mins')
  }
  therm_all$Duration2[therm_all$pasted==i] <- trial$Duration2
}

duration_categ_summ <- as.data.frame(therm_all %>%
  group_by(pasted, Category) %>%
  summarise(CategDuration = sum(Duration, na.rm=T)))

summary(therm_all$Duration)

nightlength_dur <- as.data.frame(duration_categ_summ %>%
  group_by(pasted) %>%
  summarise(DurTotal = sum(CategDuration,na.rm=T)))

merge(Nightlength_raw, nightlength_dur)

casted_dur <-dcast(pasted~Category,data=therm_all[!is.na(therm_all$Duration),], fun.aggregate= sum,value.var = 'Duration')
prop_dur <- data.frame(matrix(ncol = 5, nrow=nrow(casted_dur)))
names(prop_dur) <- c("pasted", "Normothermic", "Shallow Torpor", "Transition", "Deep Torpor")
prop_dur$pasted <- casted_dur$pasted
# ggplot(duration_categ_summ, aes(pasted, Sum)) + geom_point(aes(col=Category), size=2) + my_theme +
#   my_colors

casted_dur <- casted_dur %>% 
  rowwise() %>% 
  mutate(TotDur = sum(c(Normothermic, `Shallow Torpor`,Transition, `Deep Torpor`), na.rm=T))

for(i in 1:nrow(casted_dur)) {
  prop_dur$Normothermic[i] <- round((casted_dur$Normothermic[i]/casted_dur$TotDur[i])*100,0)
  prop_dur$`Shallow Torpor`[i] <- round((casted_dur$`Shallow Torpor`[i]/casted_dur$TotDur[i])*100,0) 
  prop_dur$Transition[i] <- round((casted_dur$Transition[i]/casted_dur$TotDur[i])*100,0)
  prop_dur$`Deep Torpor`[i] <- round((casted_dur$`Deep Torpor`[i]/casted_dur$TotDur[i])*100,0)
} 

m.prop_dur <- melt(prop_dur, id.vars = "pasted", measure.vars = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
tail(m.prop_dur)
names(m.prop_dur)[names(m.prop_dur) == 'value'] <- 'freq'
m.prop_dur$Species <- substr(m.prop_dur$pasted, 1, 4)
m.prop_dur$Species <- as.factor(as.character(m.prop_dur$Species))


casted_dur2 <-dcast(pasted~Category,data=therm_all[!is.na(therm_all$DurationSecond),], fun.aggregate= sum,value.var = 'DurationSecond')
prop_dur2 <- data.frame(matrix(ncol = 5, nrow=nrow(casted_dur2)))
names(prop_dur2) <- c("pasted", "Normothermic", "Shallow Torpor", "Transition", "Deep Torpor")
prop_dur2$pasted <- casted_dur2$pasted
# ggplot(duration_categ_summ, aes(pasted, Sum)) + geom_point(aes(col=Category), size=2) + my_theme +
#   my_colors

casted_dur2 <- casted_dur2 %>% 
  rowwise() %>% 
  mutate(TotDur = sum(c(Normothermic, `Shallow Torpor`,Transition, `Deep Torpor`), na.rm=T))

for(i in 1:nrow(casted_dur2)) {
  prop_dur2$Normothermic[i] <- round((casted_dur2$Normothermic[i]/casted_dur2$TotDur[i])*100,0)
  prop_dur2$`Shallow Torpor`[i] <- round((casted_dur2$`Shallow Torpor`[i]/casted_dur2$TotDur[i])*100,0) 
  prop_dur2$Transition[i] <- round((casted_dur2$Transition[i]/casted_dur2$TotDur[i])*100,0)
  prop_dur2$`Deep Torpor`[i] <- round((casted_dur2$`Deep Torpor`[i]/casted_dur2$TotDur[i])*100,0)
} 

m.prop_dur2 <- melt(prop_dur2, id.vars = "pasted", measure.vars = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
tail(m.prop_dur2)
names(m.prop_dur2)[names(m.prop_dur2) == 'value'] <- 'freq'
m.prop_dur2$Species <- substr(m.prop_dur2$pasted, 1, 4)
m.prop_dur2$Species <- as.factor(as.character(m.prop_dur2$Species))



## Fig 6a.2 Prop of time using duration: Using predicted values
m.prop_dur_plot <- ggplot(m.prop_dur, aes(Species,freq)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors, name="Category", guide=F) +
  scale_y_continuous(labels = percent_format()) #+
  #guides(colour = guide_legend(override.aes = list(size=3))) +
  #theme(legend.key.height = unit(3, 'lines'))

## Figure 6b.2: Using predicted values
m.prop_dur_pred_plot <- ggplot(m.prop_dur, aes(Species,predicted)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors, name="Category") +
  scale_y_continuous(labels = percent_format()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.key.height = unit(3, 'lines'))

grid.arrange(m.prop_dur_plot, m.prop_dur_pred_plot, ncol=2,  widths = c(1.5, 2))

## Using later time to calculate category
## Fig 6a.2 Prop of time using duration: Using predicted values
m.prop_dur_plot2 <- ggplot(m.prop_dur2, aes(Species,freq)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors, name="Category", guide=F) +
  scale_y_continuous(labels = percent_format()) #+
#guides(colour = guide_legend(override.aes = list(size=3))) +
#theme(legend.key.height = unit(3, 'lines'))

## Figure 6b.2: Using predicted values
m.prop_dur_pred_plot2 <- ggplot(m.prop_dur2, aes(Species,predicted)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors, name="Category") +
  scale_y_continuous(labels = percent_format()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.key.height = unit(3, 'lines'))

grid.arrange(m.prop_dur_plot2, m.prop_dur_pred_plot2, ncol=2,  widths = c(1.5, 2))


## Calculate rate of change of temperatures in each category
TransitionRate <- therm_all[therm_all$Category=="Transition",]
NormoRate <- therm_all[therm_all$Category=="Normothermic",]
ShallowRate <- therm_all[therm_all$Category=="Shallow Torpor",]
DeepRate <- therm_all[therm_all$Category=="Deep Torpor",]

rate_func <- function(dat) {
  for(i in unique(dat$pasted)) {
    trial <- dat[dat$pasted==i,]
    trial$Rate <- NA
    for(n in 2:length(trial$Surf_Temp)) {
      trial$Rate[n] <- (trial$Surf_Temp[n] - trial$Surf_Temp[n-1])/trial$Duration2[n]
    }
    for(n in 1:length(trial$Surf_Temp-1)) {
      trial$Rate2[n] <- (trial$Surf_Temp[n+1] - trial$Surf_Temp[n])/trial$Duration[n]
    }
    dat$Rate[dat$pasted==i] <- trial$Rate
    dat$Rate2[dat$pasted==i] <- trial$Rate2
    return(dat)
  }
}

TransitionRate <- rate_func(TransitionRate)
NormoRate <-  rate_func(NormoRate)
ShallowRate <- rate_func(ShallowRate)
DeepRate <- rate_func(DeepRate)

head(NormoRate)
head(ShallowRate)
head(TransitionRate)
head(DeepRate)

sd(abs(TransitionRate$Rate2), na.rm = T)
summary(abs(NormoRate$Rate2))
summary(abs(ShallowRate$Rate2))
summary(abs(TransitionRate$Rate2))
summary(abs(DeepRate$Rate2))

se(TransitionRate$Rate2)
psych::describe(TransitionRate$Rate)

Rates <- rbind(NormoRate, ShallowRate, TransitionRate, DeepRate)

Rates$Category <- factor(Rates$Category, levels=c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))

### Rate of change of temperature in deg C/min
ggplot(Rates, aes(Category, abs(Rate2))) + geom_boxplot() + geom_point() + my_theme

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



#### Submitted Interpolation, DO NOT rerun unless absolutely necessary - takes a lot of time ####
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



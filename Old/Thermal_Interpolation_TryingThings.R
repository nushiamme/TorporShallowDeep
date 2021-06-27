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

################ Duration not interpolation #############

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


casted_dur2 <-dcast(pasted~Category,data=therm_all[!is.na(therm_all$Duration2),], fun.aggregate= sum,value.var = 'Duration2')
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

## Running  a negative binomial model, definitely the best. No overdispserion now, much lower residual variance.
mod_glm_freq_sp_nb <- glm.nb(freq~variable*Species-1, data=m.prop_dur)
summary(mod_glm_freq_sp_nb)
coef(mod_glm_freq_sp_nb)

## Predict from this model and add these values back into the m.prop data frame
m.prop_dur$predicted <- predict(mod_glm_freq_sp_nb)
plot(mod_glm_freq_sp_nb)

## With later durations
## Running  a negative binomial model, definitely the best. No overdispserion now, much lower residual variance.
mod_glm_freq_sp_nb2 <- glm.nb(freq~variable*Species-1, data=m.prop_dur2)
summary(mod_glm_freq_sp_nb2)
coef(mod_glm_freq_sp_nb2)

## Predict from this model and add these values back into the m.prop data frame
m.prop_dur2$predicted <- predict(mod_glm_freq_sp_nb2)
plot(mod_glm_freq_sp_nb2)




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

grid.arrange(m.prop_dur_plot, m.prop_dur_pred_plot, ncol=2,  widths = c(1.5, 2.4))

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

grid.arrange(m.prop_dur_plot2, m.prop_dur_pred_plot2, ncol=2,  widths = c(1.5, 2.4))


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
ggplot(Rates, aes(Category, abs(Rate2))) + geom_boxplot(aes(fill=Category), alpha=0.6) + geom_point() + my_theme +
  scale_fill_manual(values=my_colors) + ylab("Rate of temperature change")


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



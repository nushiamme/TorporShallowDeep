## Code to interpolate thermal data to ensure even sampling across the night
## Shallow-deep torpor paper 
## Anusha Shankar, nushiamme<@>gmail<dot>com
## IGNORE this script, it's been folded into the "Thermal_plots_models.R" script

## Read in packages
library(here)
library(plyr)
library(stringr) ## To pad a cell with zeros (str_pad function)
library(segmented) ## Trying out a piecewise regression with this
library(chron)
library(gridExtra) ## for seeing interpol and raw plots side by side
library(MASS) ## To check the distribution of the data and run glm.nb


## Read in file
therm_all <- read.csv(here("Data", "All_data.csv"))

#### General functions ####
## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_colors <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")


## Generic standard error function
se <- function(dt) {
  sd(dt, na.rm=T) /  
  sqrt(length(dt[!is.na(dt)]))
}


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

therm_all$Category <- factor(therm_all$Category, levels=c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))

################ Duration of time spent per category #############

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



## Measure duration in each category (in minutes)
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

duration_categ_summ <- as.data.frame(m.prop_dur %>%
                                       group_by(pasted, variable) %>%
                                       summarise(CategDuration = sum(freq, na.rm=T)))

indiv_categ_summ <- as.data.frame(duration_categ_summ %>%
                                       group_by(pasted, variable) %>%
                                       summarise(Categories = count(variable, na.rm=T)))

duration_categ_spp <- as.data.frame(m.prop_dur %>%
                                       group_by(Species, variable) %>%
                                       summarise(CategDuration = mean(freq, na.rm=T)))


duration_categ_summ <- duration_categ_summ[duration_categ_summ$CategDuration>0,]
nrow(duration_categ_summ[duration_categ_summ$variable=="Normothermic",])
nrow(duration_categ_summ[duration_categ_summ$variable=="Shallow Torpor",])
nrow(duration_categ_summ[duration_categ_summ$variable=="Transition",])
nrow(duration_categ_summ[duration_categ_summ$variable=="Deep Torpor",])

#### Write to csv ####
names(m.prop_dur2) <- c("pasted", "variable", "freq2", "Species", "predicted2")
m.prop_dur_both <- merge(m.prop_dur, m.prop_dur2, by = c("pasted", "variable", "Species"))
write.csv(m.prop_dur_both, file = here("Data", "Prop_Duration_Categories.csv"))




#### Models ####
## This model has residual deviance >> degrees of freedom
mod_glm_freq_sp <- glm(freq~variable*Species-1, data=m.prop_dur, family=poisson())
summary(mod_glm_freq_sp)
coef(mod_glm_freq_sp)

## Because residual variance >> degrees of freedom, trying a quasipoisson
## But the dispersion parameter is still 12.5, which is much greater than 1, meaning it's overdispersed
mod_glm_freq_sp_quasi <- glm(freq~variable*Species-1, data=m.prop_dur, family=quasipoisson())
summary(mod_glm_freq_sp_quasi)

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

emmeans(mod_glm_freq_sp_nb,specs=~variable*Species)


#### Figures ####
## Fig 6a Prop of time using duration: Using predicted values
m.prop_dur_plot <- ggplot(m.prop_dur, aes(Species,freq)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors, name="Category", guide=F) +
  scale_y_continuous(labels = percent_format()) #+
  #guides(colour = guide_legend(override.aes = list(size=3))) +
  #theme(legend.key.height = unit(3, 'lines'))

## Figure 6b: Using predicted values
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


## Calculate rate of change of temperatures in each category (in deg C per minute)
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
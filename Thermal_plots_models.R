#### Code description ####
## Shallow-deep torpor paper: Figures and analyses (Figures 2, 4-6) and all models
## Paper authors: A Shankar, INH Cisneros, S Thompson, CH Graham, DR Powers
## Code author: A Shankar
## Contact: nushiamme<at>gmail<dot>com

## Code layout:
## First read in packages and data files, then general functions
## Then run models
  ## First of surface temperature ~ ambient temperature, and categories
  ## Then of proportion of time spent per category
## Then Figures


#### Read in packages ####
library(here)
library(plyr) ## Load this before loading dplyr (so that some functions in dplyr aren't masked)
library(dplyr) ## for summarize 
library(reshape2) ## for dcast() function 
library(MASS) ## To check the distribution of the data and run glm.nb
library(ggplot2)
library(scales) # To plot stacked bar as percentages
library(nlme) ## for gls model to compare them with lmer, and to account for autocorrelation
library(lme4) # Running multilevel mixed models
library(lmerTest) ## Optional, for p values on lmer models
library(lattice) ## qqplot to look at lmer model residuals
#library(viridis) # Source of the colors used here; but manually coded
library(stringr) ## To pad a cell with zeros (str_pad function)
library(emmeans)
library(sjPlot) ## Plotting interaction term model - VERY USEFUL OR
library(ggeffects) ## Also for plotting predicted values 
library(glmmTMB) ## To plot random effects with sjPlot 

#### Read in files. Using here() package, so default working directory is the file that the .Rproj file is in. ####
# Can remake this thermal melted file if needed by running the Thermal_summaries.R script
here <- here::here
thermal_maxes_melted <- read.csv(here("Data", "Thermal_maxes.csv")) ## Raw temperatures

# Other files
categories <- read.csv(here("Data", "Category_thresholds.csv"))
interpolated <- read.csv(here("Data", "Interpolated_Thermal.csv")) ## Temperatures interpolated to 10 min
categ_percentage <- read.csv(here("Data", "Category_percentages.csv"))
masses <- read.csv(here("Data", "Bird_masses.csv"))
therm_all <- read.csv(here("Data", "All_data.csv"))

#setwd("C:\\Users\\nushi\\OneDrive - Cornell University\\Shallow_Torpor")

#### General functions ####
## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))


## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))
STemp.lab <- expression(atop(paste("Surface Temperature (", degree,"C)")))
ATemp.lab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))

## Standardize the color scheme
my_colors <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")
my_gradient <- c("#823de9", "#7855ce", "#6e6eb2", "#648697", "#599e7c", "#4fb760", "#45cf45") ## Keep only if doing Categ*Sp temp plot
my_gradient2 <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
                  "#8A7C64", "#599861")


therm_all$Category <- factor(therm_all$Category, levels=c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))

##Structuring time
birdTime <- therm_all$Time
TimeOrder1 <- seq(from = 1900, to = 2459, by = 1)
TimeOrder2 <- seq(from = 100, to = 559, by = 1)
TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2))
TimeOrder <- factor(TimeOrder, as.character(TimeOrder))

birdTime <- as.factor(as.character(str_pad(birdTime, 4, pad = "0")))
therm_all$Time2 <- TimeOrder[match(birdTime,TimeOrder,nomatch=NA)]


#### Models of Surface temperature vs. ambient temperature ####
## Not including Category as a covariate, just doing a linear model of surface vs. ambient temperatures
## Terrible model!
mod.surf_amb_noCateg <- gls(Surf_Temp~Amb_Temp, data=therm_all)
summary(mod.surf_amb_noCateg)
plot(mod.surf_amb_noCateg) ## Very skewed qq plot, bad fit

## (1|Categ) would allow intercepts to vary by category, but not slopes. i.e. you fix the slope
## (Amb_Temp|Categ) allows slopes and intercepts to vary by category

## t test of differences in temp between years
t.test(therm_all$Amb_Temp[therm_all$Year==17], therm_all$Amb_Temp[therm_all$Year==18])

mean(therm_all$Amb_Temp[therm_all$Year==2017])
sd(therm_all$Amb_Temp[therm_all$Year==2017], na.rm=TRUE)

## First, multilevel model with random intercepts and fixed slope for all categories
mod_mixed <- lmer(Surf_Temp ~ Amb_Temp + Category, data=therm_all)
summary(mod_mixed)
coef(mod_mixed) ## Useful to see all the slopes and intercepts
plot(mod_mixed) ## Definitely many clusters of points
plot(ranef(mod_mixed)) ## plotting random effects of model
plot(residuals(mod_mixed)) ## plot residuals of model

## Random intercepts and random slopes, no species in this equation
mod_mixed_2 <- glm(Surf_Temp ~ Amb_Temp + Category, data=therm_all)
summary(mod_mixed_2)
coef(mod_mixed_2) 
plot(mod_mixed_2)

## Accounting for species, Random intercepts and random slopes
mod_mixed_3 <- glm(Surf_Temp ~ Amb_Temp + Category + Species_numeric, data=therm_all)
summary(mod_mixed_3)
coef(mod_mixed_3)
plot(mod_mixed_3)

## Same as above but now including mass.
## This was the final full (and best) model. Earlier had species and category as fixed effects
## And used lmer earlier, for mixed effects
mod_mixed_4 <- glm(Surf_Temp ~ Amb_Temp + Category + Cap_mass + Species_numeric, data=therm_all)
summary(mod_mixed_4)
coef(mod_mixed_4)
plot(mod_mixed_4)
anova(mod_mixed_3, mod_mixed_4)
qqmath(~resid(mod_mixed_4)) ## Much better!!

## Accounting for individual and species, Random intercepts and random slopes.
## Gives identical results to above. So Indiv ID doesn't make a difference
## Ignore.
mod_mixed_5 <- lmer(Surf_Temp ~ Amb_Temp + Category + Cap_mass + Species + 
                      (1|Indiv_numeric), data=therm_all, REML = FALSE)
summary(mod_mixed_5)
coef(mod_mixed_5)
plot(mod_mixed_5)
acf(resid(mod_mixed_5))

## Benjamin suggested
mod_BVD <- lmer(data=therm_all, Surf_Temp ~ Amb_Temp + Category + Amb_Temp:Category + 
                  Species + Cap_mass + (1|Indiv_numeric) + ## Always keep this
                  Species:Category + (1|Indiv_numeric:Category))
                  #Species:Amb_Temp:Category + 
#(0+Amb_Temp|Indiv_numeric:Category)) ## this might be unnecessary

therm_all$Species <- as.factor(therm_all$Species)
# mod_BVD_sp <- lmer(data=therm_all, Surf_Temp ~ 
#                      Amb_Temp + 
#                      Category + 
#                      Amb_Temp:Category + 
#                      Species + 
#                      Cap_mass +
#                      Amb_Temp:Species:Category + 
#                      #Species:Amb_Temp:Category + ## to allow emmeans to balance category means across species
#                      (1|Indiv_numeric)  + 
#                      (1|Indiv_numeric:Category))
# #(0+Amb_Temp|Indiv_numeric:Category)) ## this might be unnecessary


mod_BVD_sp_cor1 <- nlme::lme(data=therm_all, fixed=Surf_Temp ~ 
                     Amb_Temp + 
                     Category + 
                     Amb_Temp:Category + 
                     Species + 
                     Cap_mass +
                     Amb_Temp:Species:Category,  
                     random= ~1|Indiv_numeric/Category, 
                  correlation=corAR1(form=~1|Indiv_numeric/Category))

## better AICc
mod_BVD_sp_cor2 <- nlme::lme(data=therm_all, fixed=Surf_Temp ~ 
                               Amb_Temp + 
                               Category + 
                               Amb_Temp:Category + 
                               Species + 
                               Cap_mass +
                               Species:Category,  
                             random= ~1|Indiv_numeric/Category, 
                             correlation=corAR1(form=~1|Indiv_numeric/Category))


# mod_BVD_sp_cor2 <- nlme::lme(data=therm_all, fixed=Surf_Temp ~ 
#                           Amb_Temp + 
#                           Category + 
#                           Amb_Temp:Category + 
#                           Species + 
#                           Cap_mass +
#                           Species:Category,  
#                         random= ~1|Indiv_numeric,
#                         correlation=corAR1(form=~1|Indiv_numeric))


summary(mod_BVD_sp, correlation=T)
confint(mod_BVD)
acf(resid(mod_BVD_sp))
em <- emmeans(mod_BVD,  ~Species:Category)
em
em1 <- emmeans(mod_BVD_sp_cor1,  ~Species:Category)
em1
summary(mod_BVD_sp_cor1)
confint(mod_BVD_sp_cor1)
acf(resid(mod_BVD_sp_cor1))
em2 <- emmeans(mod_BVD_sp_cor2,  ~Species:Category)
em2
summary(mod_BVD_sp_cor2)
confint(mod_BVD_sp_cor2)
acf(resid(mod_BVD_sp_cor2))

plot(residuals(mod_BVD_sp_cor1),type="b")
abline(h=0,lty=3)

class(summary(mod_BVD_sp_cor2))

summary(mod_BVD_sp_cor2)$tTable

## SO USEFUL interaction effects plot!!
plot_model(mod_BVD_sp_cor2, type = "int", terms = "Species*Category")[[1]] + my_theme

## For selecting best model with AIC
library(MuMIn)
MuMIn::model.sel(mod_BVD, mod_BVD_sp_cor1, mod_mixed_5)
MuMIn::model.sel(mod_BVD_sp_cor1, mod_BVD_sp_cor2)
anova(mod_BVD, mod_BVD_sp_cor1, mod_mixed_5)


# em <- emmeans(mod_BVD_sp_cor2,  ~Species:Category)
# em
#emtrends(mod_BVD_sp, ~Category, var="")

predict_gam(mod_BVD_sp_cor1, values = list(f1 = c(0.5, 1, 1.5))) %>%
  ggplot(aes(x2, fit)) +
  geom_smooth_ci(f1)

therm_all$fit <- predict(mod_BVD_sp_cor1)

ggplot(therm_all,aes(Amb_Temp, Surf_Temp, group=interaction(Category, Species), col=Category, shape=Species)) + 
  geom_smooth(aes(y=fit, lty=Species), method="lm", size=0.8) +
  geom_point(alpha = 0.3) + xlab(ATemp.lab) +
  ylab(STemp.lab) +
  my_theme

ggplot(fortify(mod_BVD), aes(Amb_Temp, Surf_Temp, color=Category)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line")


## BVD:
##First look at random effects' residual random effects. Then look at Intercept of random effects
## To see how much variation they explain. In this mod_BVD, sd(Indiv_numeric)'s intercept is
## about half the residual sd, which is a good thing.


#Species:Category + Amb:Category, data=therm_all))
  Species:Category + ## If I don't include this term and use emmeans, it means the species with more representation will 
    ##weight the category averages more than species that have less representation
    Amb:Category ## more optional
  #Species:Amb:Category ## Will allow emmeans to balance category means across species, instead
  ## of giving species with more individual representation more weight
  
  ## A random effect doesn't get a mean estimate, it's variation. Random effects won't for the most part change mean estimates
  ## Only effect sources of uncertainty.

  
  ## confint() help get conf intervals
  ## Run emmeans on the model to get each category's estimates
  ## TRY THIS: Use nlme to fit random effects, lme(), can fit autocorr terms. 
  ## Has fixed effect, random, and lag1 autocorr func (give the autocorr fun a time var)
  ## Time within indiv
  ## gam can fit a spline term to the model to account for variation in temperature..
  ## over time
  ## OR can use lmer with extra package for spline term
  
## Could also do
(1+Amb:Category|Indiv) 
(1+amb|indiv) ## This means estimating covariance between slopes and intercept also
(0+amb|indiv) ## 0 means not estimating intercept

## Could scale data set to average amb temp
data$Amb <- scale(data$Amb)


# Testing autocorrelation
simdat <- start_event(simdat, column="Time", event=c("Subject", "Trial"), label.event="Event")
head(simdat)
m4AR1 <- glm(Surf_Temp ~ Amb_Temp + Category + Cap_mass + Species_numeric, data=therm_all
  
  Y ~ te(Time, Trial)+s(Subject, bs='re'), data=simdat, rho=r1, AR.start=simdat$start.event)

## Run anova of model. Leaving out mod_mixed here because it doesn't make sense that categories would all have one fixed slope
an.mod <- anova(mod_mixed_2,mod_mixed_3, mod_mixed_4)
an.mod

datapoints_raw <- vector()
datapoints_interpol <- vector()
for(i in unique(therm_all$pasted)){
  datapoints_raw[i] <- length(therm_all$Surf_Temp[therm_all$pasted==i])
  datapoints_interpol[i] <- length(data_interpol$Surf_Temp[data_interpol$Indiv_pasted==i])
}


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
                           Normothermic=sum(Category=="Normothermic"), Shallow=sum(Category=="Shallow Torpor"),
                           Transition=sum(Category=="Transition"), Torpor=sum(Category=="Deep Torpor"))
sum(indiv_categ_count$Normothermic) #no. indivs that used normo; should be all (33)
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

##### March 30, 2021: trying out non-interpolated proportion of time spent in each category ####
#### Getting proportion of time each individual spent in each category to run model of proportions ####
## Summarize proportion of time spent in each state, by species (uses non-interpolated, raw, data)

##What is the average time between samples?
# for(i in length(unique(therm_all$pasted))) {
#   therm_all
# }



therm_all$Month <- substr(therm_all$Date, 1, 1)
therm_all$Day <- as.numeric(substr(therm_all$Date, 2, 3))
therm_all$Year <-as.numeric(paste0("20",therm_all$Year))
therm_all$Minute <- as.numeric(str_sub(therm_all$Time, -2))
therm_all$Hour2 <- therm_all$Hour
therm_all$Hour2[therm_all$Hour==24] <- 0
therm_all$Day[therm_all$Hour2<7] <- therm_all$Day[therm_all$Hour2<7]+1
library(lubridate)
therm_all$DateFormat <- as.POSIXct(paste(paste(therm_all$Year, therm_all$Month, therm_all$Day, sep = "-"), 
                                paste(str_pad(therm_all$Hour2, width=2, side="left", pad="0"), 
                                      str_pad(therm_all$Minute, width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                                format='%Y-%m-%d %H:%M')
therm_all$TimeFormat <- as.POSIXct(paste(str_pad(therm_all$Hour2, width=2, side="left", pad="0"),
                                          str_pad(therm_all$Minute, width=2, side="left", pad="0"), "00", sep = ":"),
                                   format='%H:%M')



dattry <- therm_all
##Order by date/time
dattry <- dattry[order(as.POSIXct(dattry$DateFormat, format="%Y-%m-%d %H:%M")),]

#dattry %>% mutate(across(DateFormat, ymd_hm)) %>% arrange(DateFormat)

# data_TimeInterval <- data.frame(Indiv=numeric(0), Time=numeric(0), diff=numeric(0))
# Time1 <- data.frame()
# Time2 <- data.frame(Indiv=numeric(0), Time=numeric(0), diff=numeric(0))
#TimeInterval <- data.frame(Indiv=)

for(i in unique(dattry$pasted)) {
  dattry$diff[dattry$pasted==i][1] <- NA
  #dattry$TimeShift[dattry$pasted==i][1] <- NA
  #dattry$TimeShift[2:length(dattry$TimeShift[dattry$pasted==i])] <- as.POSIXct(dattry$DateFormat[2:length(dattry$DateFormat[dattry$pasted==i])])
  for(n in 2:length(dattry$Time[dattry$pasted==i])) {
      ## Create trial column
      trial <- dattry[dattry$pasted==i,]
      # dattry$diff[dattry$pasted==i][n] <- c(NA, as.duration(trial$DateFormat[trial$Time>1900][2],
      #                                                       trial$D[trial$Time>1900][1]),
      #                                       NA, as.duration(trial$Time[trial$Time<700]))
      dattry$diff[dattry$pasted==i][n] <- difftime(trial$DateFormat[n],trial$DateFormat[n-1], units="mins")
      #dattry$diff2[dattry$pasted==i][n] <- difftime(dattry$DateFormat[dattry$pasted==i][n],dattry$TimeShift[dattry$pasted==i][n], units="mins")
    }
}
unique(dattry$diff)
dattry[dattry$Indiv_numeric==1,]
summary(dattry$diff)

ggplot(dattry, aes(diff)) + geom_histogram()

ggplot(dattry, aes(x=diff)) +
  stat_density(aes(y=..count..), color="black", fill="blue", alpha=0.3) +
  scale_x_continuous(breaks=c(0,1,10,100,300,1000), trans="log1p", expand=c(0,0)) +
  scale_y_continuous(breaks=c(0,125,250,375,500,625,750,1000,10000,100000), expand=c(0,0)) +
  theme_bw()



####

data_species_summ <- data_interpol %>%
  count(Species, Category) %>%
  group_by(Species) %>%
  mutate(perc = (n / sum(n))*100)

## By individual
data_indiv_summ_interpol <- data_interpol %>%
  group_by(Indiv_pasted, Category) %>%
  summarise (n = length(Category))
head(data_indiv_summ)

## By individual
data_indiv_summ_raw <- therm_all %>%
  group_by(pasted, Category) %>%
  summarise (n = length(Category))
head(data_indiv_summ_raw)

## Summarize (binary) whether individuals used a particular category or not
indiv_categ_count <- ddply(data_indiv_summ_raw, c("Indiv_pasted"), dplyr::summarise, 
                           Normothermic=sum(Category=="Normothermic"), ShallowTorpor=sum(Category=="Shallow"),
                           Transition=sum(Category=="Transition"), DeepTorpor=sum(Category=="Torpor"))

sum(indiv_categ_count$Normothermic) #no. indivs that used normo; should be all (33)
sum(indiv_categ_count$ShallowTorpor) #no. indivs that used shallow
sum(indiv_categ_count$Transition) #no. indivs that used transition
sum(indiv_categ_count$DeepTorpor) #no. indivs that used deep torpor

# Summarise number of records per individual in each category. Using interpolated data
casted_indiv <-dcast(pasted~Category,data=therm_all, fun.aggregate= length,value.var = 'Category')
names(casted_indiv) <- c("pasted", "Normothermic", "ShallowTorpor", "Transition", "DeepTorpor")
prop_indiv_time <- data.frame(matrix(ncol = 5, nrow=nrow(casted_indiv)))
names(prop_indiv_time) <- c("pasted", "Normothermic", "ShallowTorpor", "Transition", "DeepTorpor")
prop_indiv_time$pasted <- casted_indiv$pasted
for(i in 1:nrow(casted_indiv)) {
  prop_indiv_time$Normothermic[i] <- round((casted_indiv$Normothermic[i]/(sum(casted_indiv$Normothermic[i], 
                                                                              casted_indiv$ShallowTorpor[i], casted_indiv$Transition[i],
                                                                              casted_indiv$DeepTorpor[i])))*100,0) 
  prop_indiv_time$ShallowTorpor[i] <- round((casted_indiv$ShallowTorpor[i]/(sum(casted_indiv$Normothermic[i], 
                                                                    casted_indiv$ShallowTorpor[i], casted_indiv$Transition[i],
                                                                    casted_indiv$DeepTorpor[i])))*100,0) 
  prop_indiv_time$Transition[i] <- round((casted_indiv$Transition[i]/(sum(casted_indiv$Normothermic[i], 
                                                                          casted_indiv$ShallowTorpor[i], casted_indiv$Transition[i],
                                                                          casted_indiv$DeepTorpor[i])))*100,0) 
  prop_indiv_time$DeepTorpor[i] <- round((casted_indiv$DeepTorpor[i]/(sum(casted_indiv$Normothermic[i], 
                                                                  casted_indiv$ShallowTorpor[i], casted_indiv$Transition[i],
                                                                  casted_indiv$DeepTorpor[i])))*100,0)
}
head(prop_indiv_time)
write.csv(prop_indiv_time, file=here("Data", "Prop_summ.csv"))

## Melted dataframe for proportion of time spent in diff categories by species. Uses interpolated data
m.prop <- melt(prop_indiv_time, id.vars = "Indiv_pasted", measure.vars = c("Normothermic", "ShallowTorpor", "Transition", "DeepTorpor"))
tail(m.prop)
names(m.prop)[names(m.prop) == 'value'] <- 'freq'
m.prop$Species <- substr(m.prop$Indiv_pasted, 1, 4)
m.prop$Species <- as.factor(as.character(m.prop$Species))

##### End non-interpolation proportion of time ####


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
mod_glm_freq_sp_nb <- glm.nb(freq~variable*Species-1, data=m.prop_dur)
summary(mod_glm_freq_sp_nb)
coef(mod_glm_freq_sp_nb)

## Predict from this model and add these values back into the m.prop data frame
m.prop_dur$predicted <- predict(mod_glm_freq_sp_nb)
plot(mod_glm_freq_sp_nb)

##With latter category rather than former
## Running  a negative binomial model, definitely the best. No overdispserion now, much lower residual variance.
mod_glm_freq_sp_nb2 <- glm.nb(freq~variable*Species-1, data=m.prop_dur2)
summary(mod_glm_freq_sp_nb2)
coef(mod_glm_freq_sp_nb2)

## Predict from this model and add these values back into the m.prop data frame
m.prop_dur2$predicted <- predict(mod_glm_freq_sp_nb2)
plot(mod_glm_freq_sp_nb2)



## emmeans for means and uncertainty per category
##


#### Figures ####
## Figure 2: Single RIHU individual's temperatures plotted over the course of a night
## Points were modified for clarity in Illustrator
## 3D surface plots were constructed in ImageJ and added on in Illustrator/powerpoint
single <- "MAHU10_0603"
wd2 <- file.path("C:", "Users", "nushi", "OneDrive - Cornell University", "IR_2018_csv", "Data")
for(i in single) {
  setwd(paste0(wd2, "/", i))## Or wherever .rds files are stored
  
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

therm_all$Categ_Sp <- paste0(therm_all$Category, "_", therm_all$Species)

## Figure 4: Surface vs ambient temperature, with one linear model fitted to each category
#therm_all$Category <- factor(therm_all$Category, levels = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
## Plot surface vs ambient temperature
ggplot(therm_all, aes(Amb_Temp, Surf_Temp)) + geom_point(aes(col=Category, shape=Category), size=2.5) + my_theme +
  scale_y_continuous(breaks = seq(0,40,5)) +
  scale_colour_manual(values=my_colors) +
  geom_smooth(aes(group=Category, col=Categ_Sp),method='lm') +
  scale_shape_manual(values = c(15:18)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines')) +
  xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) #+
  #guides(colour = guide_legend(override.aes = list(size=4)))

## Figure 4 tweaking: Surface vs ambient temperature, with one linear model fitted to each category
#therm_all$Category <- factor(therm_all$Category, levels = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
## Plot surface vs ambient temperature
ggplot(therm_all, aes(Amb_Temp, Surf_Temp)) + 
  geom_point(aes(col=Indiv_ID, shape=Category), size=2.5) + my_theme2 +
  #scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35,40)) +
  scale_colour_manual(values=c(my_colors, my_gradient2)) +
  facet_grid(.~Species) +
  #scale_color_manual(values = c(my_colors)) +
  geom_smooth(aes(group=Category, col=Category),method='lm') +
  scale_shape_manual(values = c(15:18)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines')) +
  xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) #+
#guides(colour = guide_legend(override.aes = list(size=4)))

## Figure 4  using model outputs!!
## library(sjPlot)
plot_model(mod_BVD_sp_cor2, type = "int", terms = "Species*Category")[[1]] + 
  my_theme +
  geom_point(data=therm_all, aes(x=Amb_Temp, y=Surf_Temp, col=Category), size=2.5,
             inherit.aes = F) +
  scale_colour_manual(values=my_colors) +
  scale_fill_manual(values=my_colors) +
  xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
  ggtitle("")


## Line plots of mean and SD per category and species
plot_model(mod_BVD_sp_cor2, type = "int", terms = "Category*Species", line.size=1.3,
           point.size=1.3)[[2]] +
  my_theme + #scale_x_discrete(expand=c(0.1, 0.2)) +
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
  ggtitle("")


## Random effects
plot_model(mod_BVD, type = "re", aes(color=Category))
plot_grid(p)

#library(ggeffects)
dfpred <- ggpredict(mod_BVD, terms = c("Amb_Temp","Category", "Species"))
ggplot(dfpred, aes(x, predicted)) + my_theme +
  geom_point(data=therm_all, aes(x=Amb_Temp, y=Surf_Temp, col=Category), size=2.5,
             inherit.aes = F) +
  geom_line(aes(color=group)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), show.legend = F, alpha=0.15) +
  scale_colour_manual(values=my_colors) +
  scale_fill_manual(values=my_colors) +
  xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
  ggtitle("")

ggplot(dfpred, aes(group, predicted)) + my_theme +
  #geom_line() +
  geom_point(aes(col=facet))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, col=facet), width=.2,
                position=position_dodge(0.05)) +
  #scale_colour_manual(values=my_colors) +
  #scale_fill_manual(values=my_colors) +
  #xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
  ggtitle("")

dfpred2 <- ggpredict(mod_BVD, terms = c("Species","Category"))
plot(dfpred2, add.data = T, line.size=1.5, dot.alpha=0.2) + scale_colour_manual(values=my_colors) +
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
  ggtitle("") + my_theme + theme(legend.key.height =  unit(3, 'lines'))

## just MAHU to troubleshoot
ggplot(therm_all[therm_all$Species=="MAHU",], aes(Amb_Temp, Surf_Temp)) + 
  geom_point(aes(col=as.factor(Indiv_numeric), shape=Category), size=2.5) + my_theme2 +
  scale_colour_manual(values=c(my_colors, my_gradient2)) +
  #facet_grid(.~Species) +
  #scale_color_manual(values = c(my_colors)) +
  geom_smooth(aes(group=Category, col=Category),method='lm') +
  scale_shape_manual(values = c(15:18)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines')) +
  xlab(expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab(expression(atop(paste("Surface Temperature (", degree,"C)")))) #+


## Repeat colors across facets
therm_all %>%
  group_by(Species) %>%
  mutate(dummy_var = as.character(x = factor(x = Indiv_ID,
                                             labels = seq_len(length.out = n_distinct(x = Indiv_ID))))) %>%
  ungroup() %>%
  ggplot(aes(Amb_Temp, Surf_Temp)) + 
  geom_point(aes(col=dummy_var, shape=Category), size=2.5) + my_theme2 +
  #scale_y_continuous(breaks = c(5,10,15,20,21,22,23,24,25,26,27,28,29,30,35,40)) +
  scale_colour_manual(values=c(my_colors, my_gradient2)) +
  facet_grid(.~Species) +
  #scale_color_manual(values = c(my_colors)) +
  geom_smooth(aes(group=Category, col=Category),method='lm') +
  scale_shape_manual(values = c(15:18)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines')) +
  xlab(expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab(expression(atop(paste("Surface Temperature (", degree,"C)"))))

## Figure 5: Range of max surface temperatures per individual (or per night), colored by category
ggplot(thermal_maxes_melted, aes(variable, value)) + my_theme + geom_point(aes(col=Category), size=2, alpha=0.8) +  
  facet_grid(.~Species, scales = "free_x",space = "free_x") +
  ylab(Temp.lab) + xlab("Individual") + 
  #scale_color_manual(values = c('black','deepskyblue2', 'palegreen4', 'red')) +
  scale_color_manual(values=my_colors) +
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
  scale_fill_manual(values=my_colors, name="Category") +
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


#### New figures, from Reviewer suggestion, Feb 2021 ####



## Try to fit interpolated version of the same thing
ggplot(therm_all[therm_all$Species=="BCHU",], aes(Time2, Surf_Temp)) + 
  facet_wrap(.~Indiv_numeric, scales = "free_x") + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.key.height = unit(3, 'lines')) +
  scale_color_manual(values=my_colors) + ylab(Temp.lab)


ggplot(therm_all[therm_all$Species=="BCHU",], aes(Time2, Surf_Temp)) + 
  facet_wrap(.~Indiv_numeric, scales = "free_x") + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.key.height = unit(3, 'lines')) +
  scale_color_manual(values=my_colors) + ylab(Temp.lab)
#+ geom_line(aes(col=Category), size=1.2) +  theme(axis.text.x = element_text(angle=40))

# All individuals in one plot
ggplot(therm_all[therm_all$Species=="MAHU",], aes(Time2, Surf_Temp)) + my_theme +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(legend.key.height = unit(3, 'lines'), axis.text.x = element_blank())

## Faceted by individual
ggplot(therm_all[therm_all$Species=="MAHU",], aes(Time2, Surf_Temp)) + my_theme +
  facet_wrap(.~Indiv_numeric, scales = "free_x",) + 
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) + xlab("Time of night") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.key.height = unit(3, 'lines'), strip.background = element_blank(),
        strip.text.x = element_blank())

# All individuals in one plot
ggplot(therm_all[therm_all$Species=="BLHU",], aes(Time2, Surf_Temp)) + my_theme +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) + xlab("Time of night") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.key.height = unit(3, 'lines'))

## Faceted by individual
ggplot(therm_all[therm_all$Species=="MAHU",], aes(Time2, Surf_Temp)) + 
  facet_wrap(.~Indiv_numeric, scales = "free_x") + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.key.height = unit(3, 'lines'))

ggplot(therm_all[therm_all$Indiv_ID=="MAHU10",], aes(DateFormat, Surf_Temp)) + 
  theme_classic(base_size = 30) + #theme(panel.border = element_rect(colour = "white", fill=NA)) +
  geom_line(aes(group=Indiv_numeric), size=1, col='grey80') +
  geom_point(aes(col=Category), size=4) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) + xlab("Time of night") +
  theme(axis.text.x = element_text(vjust=0.5, size=15),
        legend.key.height = unit(3, 'lines')) #+
  # scale_x_date(date_breaks = '5 minutes',
  #              date_labels = '%H%M')

mahu03_categ <- categories[categories$Individual=="RIHU03_052718",]
## Faceted by individual
## On March 25, 2021  changed shallow/transition threshold for MAHU03 from 20 to 22.5
ggplot(therm_all[therm_all$pasted=="RIHU03_052718",], aes(DateFormat, Surf_Temp)) + my_theme2 +
  #facet_wrap(.~Indiv_ID, scales = "free_x") + 
  scale_y_continuous(breaks = c(mahu03_categ$Normo_min, mahu03_categ$Shallow_max, mahu03_categ$Shallow_min, mahu03_categ$Transition_max, mahu03_categ$Transition_min)) +
  geom_point(aes(col=Category), size=1.5, shape=0) +
  geom_point(data=interpolated[interpolated$Indiv_pasted=="RIHU03_052718",],
             aes(as.POSIXct(Time), Surf_Temp, col=Category), size=1.5, alpha=0.8) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(angle=90, size=8),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines'))


bchu01_categ <- categories[categories$Individual=="BCHU01_061017",]
## Faceted by individual
## On March 25, 2021  changed shallow/transition threshold for MAHU03 from 20 to 22.5
ggplot(therm_all[therm_all$pasted=="BCHU01_061017",], aes(DateFormat, Surf_Temp)) + my_theme2 +
  #facet_wrap(.~Indiv_ID, scales = "free_x") + 
  scale_y_continuous(breaks = c(bchu01_categ$Normo_min, bchu01_categ$Shallow_max, bchu01_categ$Shallow_min, bchu01_categ$Transition_max, bchu01_categ$Transition_min)) +
  geom_point(aes(col=Category), size=1.5, shape=0) +
  geom_point(data=interpolated[interpolated$Indiv_pasted=="BCHU01_061017",],
             aes(as.POSIXct(Time), Surf_Temp, col=Category), size=1.5, alpha=0.8) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(angle=90, size=8),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines'))



# ### Plotting Ts - Ta and Ta
# therm_all$Ts_Ta <- therm_all$Surf_Temp - therm_all$Amb_Temp
# ggplot(therm_all[therm_all$Species=="MAHU",], aes(DateFormat, Surf_Temp)) + my_theme2 +
#   facet_wrap(.~Indiv_numeric, scales = "free_x") + 
#   geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
#   geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
#   geom_line(aes(group=Indiv_numeric, y=Ts_Ta), col="red", linetype="dashed") +
#   scale_color_manual(values=my_colors) + ylab(Temp.lab) +
#   theme(axis.text.x = element_text(angle=90, vjust=0.5),
#         legend.key.height = unit(3, 'lines'))
  
## Trying out a cubic spline
fit<-lm(Surf_Temp ~ bs(Time2,knots = c(25,40,60)),data = therm_all[therm_all$Indiv_numeric==22,])
## Faceted by individual
ggplot(therm_all[therm_all$Species=="MAHU",], aes(Time2, Surf_Temp)) + 
  facet_wrap(.~Indiv_numeric, scales = "free_x") + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.key.height = unit(3, 'lines')) +
  points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")


# All individuals in one plot
ggplot(therm_all[therm_all$Species=="MAHU",], aes(Time2, Surf_Temp)) + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), axis.text.y=element_text(size=20),
        legend.key.height = unit(3, 'lines'))


## INCORPORATE change into text and models - Changed RIHU02_061117 from shallow to deep torpor
## Changed thresholds from 30-30 to 30-30-29-29-26-26
## Changed RIHU03_052718 thresholds from to 29-29-22.5-22.5-14-14 to 29-29-20.5-20.5-14-14


## Individual Indiv_ID = MAHU02 (there are two indivs with that ID), Unique: RIHU02_061117, numeric 22
ggplot(therm_all[therm_all$Indiv_numeric==22,], aes(Time2, Surf_Temp)) + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab)

## Individual BCHU01_052118, changed threshold for transition
## From 29	29	27	27	12	12
## To 
ggplot(therm_all[therm_all$pasted=="BCHU01_052118",], aes(DateFormat, Surf_Temp)) + my_theme2 + facet_grid(.~pasted, scales="free_x") +
  geom_line(aes(group=Indiv_numeric, col=Category), size=0.5) +
  geom_point(aes(col=Category), size=2) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab)

ggplot(therm_all[therm_all$pasted=="RIHU10_060318",], aes(DateFormat, Surf_Temp)) + my_theme2 + facet_grid(.~pasted, scales="free_x") +
  geom_line(aes(group=Indiv_numeric, col=Category), size=0.5) +
  geom_point(aes(col=Category), size=2) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab)
ggplot(data_interpol[data_interpol$Indiv_pasted=="RIHU10_060318",], aes(Time, Surf_Temp)) + my_theme2 + #facet_grid(.~pasted, scales="free_x") +
  geom_line(aes(group=Indiv_pasted, col=Category), size=0.5) +
  geom_point(aes(col=Category), size=2) +
  geom_line(aes(group=Indiv_pasted, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab)

ggplot(therm_all[therm_all$pasted=="RIHU03_052718",], aes(Time2, Surf_Temp)) + my_theme2 + facet_grid(.~pasted, scales="free_x") +
  geom_line(aes(group=Indiv_numeric, col=Category), size=0.5) +
  geom_point(aes(col=Category), size=2) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  scale_y_continuous(breaks=1:35) +   
  theme(panel.grid.major.y = element_line(color='black'),
        panel.grid.minor.y = element_line(color='black'))



ggplot(therm_all, aes(Duration)) + geom_histogram() + my_theme



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
library(nlme) ## for gls model to compare them with lmer
library(lme4) # Running multilevel mixed models
library(lmerTest) ## Optional, for p values on lmer models
library(lattice) ## qqplot to look at lmer model residuals
#library(viridis) # Source of the colors used here; but manually coded
library(stringr) ## To pad a cell with zeros (str_pad function)

#### Read in files. Using here() package, so default working directory is the file that the .Rproj file is in. ####
# Can remake this thermal melted file if needed by running the Thermal_summaries.R script
here <- here::here
thermal_maxes_melted <- read.csv(here("Data", "Thermal_maxes.csv")) ## Raw temperatures

# Other files
categories <- read.csv(here("Data", "Category_thresholds.csv"))
interpolated <- read.csv(here("Data", "Interpolated_Thermal.csv")) ## Temperatures interpolated to 1 minute
categ_percentage <- read.csv(here("Data", "Category_percentages.csv"))
masses <- read.csv(here("Data", "Bird_masses.csv"))
therm_all <- read.csv(here("Data", "All_data.csv"))


#### General functions ####
## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))


## Axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

## Standardize the color scheme
my_colors <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")

#### Models of Surface temperature vs. ambient temperature ####
## Not including Category as a covariate, just doing a linear model of surface vs. ambient temperatures
## Terrible model!
mod.surf_amb_noCateg <- gls(Surf_Temp~Amb_Temp, data=therm_all)
summary(mod.surf_amb_noCateg)
plot(mod.surf_amb_noCateg) ## Very skewed qq plot, bad fit

## (1|Categ) would allow intercepts to vary by category, but not slopes. i.e. you fix the slope
## (Amb_Temp|Categ) allows slopes and intercepts to vary by category

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
## This is the final full (and best) model
mod_mixed_4 <- glm(Surf_Temp ~ Amb_Temp + Category + Cap_mass + Species_numeric, data=therm_all)
summary(mod_mixed_4)
coef(mod_mixed_4)
plot(mod_mixed_4)
anova(mod_mixed_3, mod_mixed_4)
qqmath(~resid(mod_mixed_4)) ## Much better!!

## Accounting for individual and species, Random intercepts and random slopes.
## Gives identical results to above. So Indiv ID doesn't make a difference
## Ignore.
mod_mixed_5 <- lmer(Surf_Temp ~ Amb_Temp + Category + Cap_mass + (1|Species_numeric:Indiv_numeric), data=therm_all)
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
#therm_all$Category <- factor(therm_all$Category, levels = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
## Plot surface vs ambient temperature
ggplot(therm_all, aes(Amb_Temp, Surf_Temp)) + geom_point(aes(col=Category, shape=Category), size=2.5) + my_theme +
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

##Structuring time
birdsTime <- therm_all$Time
TimeOrder1 <- seq(from = 1900, to = 2459, by = 1)
TimeOrder2 <- seq(from = 0100, to = 0559, by = 1)
TimeOrder <- c(TimeOrder1, paste0("0", TimeOrder2))
TimeOrder <- factor(TimeOrder, as.character(TimeOrder))

Time_unordered<- as.factor(format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 min"),"%H%M", tz="GMT"))

TimeFinal <- droplevels(na.omit(TimeOrder[match(Time_unordered, TimeOrder,nomatch=NA)]))


therm_all$Time2 <- TimeOrder[match(birdsTime,TimeOrder,nomatch=NA)]

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
ggplot(therm_all[therm_all$Species=="BCHU",], aes(Time2, Surf_Temp)) + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.key.height = unit(3, 'lines'))

## Faceted by individual
ggplot(therm_all[therm_all$Species=="BLHU",], aes(Time2, Surf_Temp)) + my_theme2 +
  facet_wrap(.~Indiv_numeric, scales = "free_x") + 
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.key.height = unit(3, 'lines'))

# All individuals in one plot
ggplot(therm_all[therm_all$Species=="BLHU",], aes(Time2, Surf_Temp)) + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  scale_color_manual(values=my_colors) + ylab(Temp.lab) +
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


## INCORPORATE change into text and models - Changed MAHU02 from shallow to deep torpor
## Changed thresholds from 30-30 to 30-30-29-29-26-26

## Individual MAHU02
ggplot(therm_all[therm_all$Indiv_numeric==22,], aes(Time2, Surf_Temp)) + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab)

## Individual BCHU01, changed threshold for transition
## From 29	29	27	27	12	12
## To 
ggplot(therm_all[therm_all$Indiv_ID=="BCHU01",], aes(Time2, Surf_Temp)) + my_theme2 +
  geom_line(aes(group=Indiv_numeric, col=Category), size=1.5) +
  geom_line(aes(group=Indiv_numeric, y=Amb_Temp), linetype="dashed") +
  scale_color_manual(values=my_colors) + ylab(Temp.lab)


#### Code description ####
## Shallow-deep torpor paper: Figures and analyses (Figures 2,3,4) and all models
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
library(plyr) ## Load this before loading dplyr (so that some functions in dplyr aren't masked)
library(dplyr) ## for summarize 
library(here)
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
library(lubridate)
library(gridExtra)


#### Read in files. Using here() package, so default working directory is the file that the .Rproj file is in. ####
# Can remake this thermal melted file if needed by running the Thermal_summaries.R script
here <- here::here
#thermal_maxes_melted <- read.csv(here("Data", "Thermal_maxes.csv")) ## Raw temperatures

# Other files
categories <- read.csv(here("Data", "Category_thresholds.csv"))
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
#my_gradient <- c("#823de9", "#7855ce", "#6e6eb2", "#648697", "#599e7c", "#4fb760", "#45cf45") ## Keep only if doing Categ*Sp temp plot
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


## Structuring time and Date
therm_all$Month <- as.numeric(substr(therm_all$Date, 1, 1))
therm_all$Day <- as.numeric(substr(therm_all$Date, 2, 3))
therm_all$Year <-as.numeric(paste0("20",therm_all$Year))
therm_all$Minute <- as.numeric(str_sub(therm_all$Time, -2))
therm_all$Hour2 <- therm_all$Hour
therm_all$Hour2[therm_all$Hour==24] <- 0
therm_all$Day[therm_all$Hour2<7 & therm_all$Day<31] <- therm_all$Day[therm_all$Hour2<7 & therm_all$Day<31]+1
therm_all$Month[therm_all$Day==31 & therm_all$Hour2<7] <- therm_all$Month[therm_all$Day==31 & therm_all$Hour2<7]+1
therm_all$Day[therm_all$Hour2<7 & therm_all$Day==31] <- 1


therm_all[therm_all$pasted=="RIHU07_053118",]

therm_all$DateFormat <- as.POSIXct(paste(paste(therm_all$Year, therm_all$Month, therm_all$Day, sep = "-"), 
                                         paste(str_pad(therm_all$Hour2, width=2, side="left", pad="0"), 
                                               str_pad(therm_all$Minute, width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                                   format='%Y-%m-%d %H:%M')
therm_all$TimeFormat <- as.POSIXct(paste(str_pad(therm_all$Hour2, width=2, side="left", pad="0"),
                                         str_pad(therm_all$Minute, width=2, side="left", pad="0"), "00", sep = ":"),
                                   format='%H:%M')

therm_all <- therm_all[order(as.POSIXct(therm_all$DateFormat, format="%Y-%m-%d %H:%M")),]


#### Models of Surface temperature vs. ambient temperature ####
## Not including Category as a covariate, just doing a linear model of surface vs. ambient temperatures
## Terrible model!
mod.surf_amb_noCateg <- gls(Surf_Temp~Amb_Temp, data=therm_all)
summary(mod.surf_amb_noCateg)
plot(mod.surf_amb_noCateg) ## Very skewed qq plot, bad fit

## (1|Categ) would allow intercepts to vary by category, but not slopes. i.e. you fix the slope
## (Amb_Temp|Categ) allows slopes and intercepts to vary by category

## t test of differences in temp between years
t.test(therm_all$Amb_Temp[therm_all$Year==2017], therm_all$Amb_Temp[therm_all$Year==2018])

mean(therm_all$Amb_Temp[therm_all$Year==2017])
sd(therm_all$Amb_Temp[therm_all$Year==2017], na.rm=TRUE)

mean(therm_all$Amb_Temp[therm_all$Year==2018])
sd(therm_all$Amb_Temp[therm_all$Year==2018], na.rm=TRUE)

#### Linear mixed effects model ####
therm_all$Species <- as.factor(therm_all$Species)
mod_cor <- nlme::lme(data=therm_all, fixed=Surf_Temp ~ 
                        Amb_Temp + 
                        Category + 
                        Amb_Temp:Category + 
                        Species + 
                        Cap_mass +
                        Year +
                        Species:Category,
                      random= ~1|Indiv_numeric/Category, 
                      correlation=corAR1(form=~1|Indiv_numeric/Category))

acf(resid(mod_cor), plot=F)
summary(mod_cor, correlation=T)
intervals(mod_cor)
acf(resid(mod_cor))
em <- emmeans(mod_cor,  ~Species:Category)
em
plot(residuals(mod_cor),type="b")
abline(h=0,lty=3)
summary(mod_cor)$tTable

#emtrends(mod_cor, ~Species|Category, var="mean(Surf_Temp)")

therm_all$fit <- predict(mod_cor)

## Figure 3: Plotting Ts ~ Ta with species in shapes and categories in color
ggplot(therm_all,aes(Amb_Temp, Surf_Temp, group=interaction(Category), fill=Category, shape=Species)) + 
  geom_smooth(aes(y=fit, lty=Species), method="lm", size=0.8) +
  geom_abline(linetype='dashed') +
  geom_point(alpha = 0.6, size=3, color='grey30') + xlab(ATemp.lab) + scale_fill_manual(values = my_colors) +
  scale_shape_manual(values = c(21, 22, 24)) +
  ylab(STemp.lab) + guides(shape = guide_legend(override.aes = list(size=3, fill='black')), color = guide_legend(override.aes = list(size=2))) +
  my_theme

# ggplot(fortify(mod_cor), aes(Amb_Temp, Surf_Temp, color=Category)) +
#   stat_summary(fun.data=mean_se, geom="pointrange") +
#   stat_summary(aes(y=.fitted), fun.y=mean, geom="line")

#### Calculate proportion of the night spent in each thermal category ####
## Measure duration in each category (in minutes)
for(i in unique(therm_all$pasted)) {
  trial <- therm_all[therm_all$pasted==i,]
  for(n in 1:(length(trial$Surf_Temp)-1)) {
    trial$Duration[n] <- difftime(trial$DateFormat[(n+1)], trial$DateFormat[n], units='mins')
  }
  therm_all$Duration[therm_all$pasted==i] <- trial$Duration
}

dur_categ_summ <- as.data.frame(therm_all %>%
                                       group_by(pasted, Category) %>%
                                       summarise(CategDuration = sum(Duration, na.rm=T)))


nightlength_dur <- as.data.frame(dur_categ_summ %>%
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

duration_categ_summ <- as.data.frame(m.prop_dur %>%
                                       group_by(pasted, variable) %>%
                                       summarise(CategDuration = sum(freq, na.rm=T)))

# indiv_categ_summ <- as.data.frame(duration_categ_summ %>%
#                                     group_by(pasted, variable) %>%
#                                     summarise(Categories = count(variable, na.rm=T)))

duration_categ_spp <- as.data.frame(m.prop_dur %>%
                                      group_by(Species, variable) %>%
                                      summarise(CategDuration = mean(freq, na.rm=T)))

duration_categ_summ <- duration_categ_summ[duration_categ_summ$CategDuration>0,]
# Check that they make sense
nrow(duration_categ_summ[duration_categ_summ$variable=="Normothermic",])
nrow(duration_categ_summ[duration_categ_summ$variable=="Shallow Torpor",])
nrow(duration_categ_summ[duration_categ_summ$variable=="Transition",])
nrow(duration_categ_summ[duration_categ_summ$variable=="Deep Torpor",])

#### Write to csv ####
write.csv(m.prop_dur, file = here("Data", "Prop_Duration_Categories.csv"))




#### GLM Models for proportion of time spent per category ####
## Not using individual-level models, doesn't make any sense to.
#Trying to test how species are different, not individuals

## This model has residual variance >> degrees of freedom
mod_glm_freq_sp <- glm(freq~variable*Species-1, data=m.prop_dur, family=poisson())
summary(mod_glm_freq_sp)
coef(mod_glm_freq_sp)

## Because residual variance >> degrees of freedom, trying a quasipoisson
## But the dispersion parameter is still 12.5, which is much greater than 1, meaning it's overdispersed
mod_glm_freq_sp_quasi <- glm(freq~variable*Species-1, data=m.prop_dur, family=quasipoisson())
summary(mod_glm_freq_sp_quasi)
coef(mod_glm_freq_sp_quasi)

## Running  a negative binomial model, definitely the best. No overdispserion now, much lower residual variance.
mod_glm_freq_sp_nb <- glm.nb(freq~variable*Species-1, data=m.prop_dur)
summary(mod_glm_freq_sp_nb)
coef(mod_glm_freq_sp_nb)

## Predict from this model and add these values back into the m.prop data frame
m.prop_dur$predicted <- predict(mod_glm_freq_sp_nb)
plot(mod_glm_freq_sp_nb)


#### Figures ####
## Figure 2: Single RIHU individual's temperatures plotted over the course of a night
## Points were modified for clarity in Illustrator
## 3D surface plots were constructed in ImageJ and added on in Illustrator/powerpoint
single <- "RIHU10_0603"
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

## Figure 3: Surface vs ambient temperature, with one linear model fitted to each category
#therm_all$Category <- factor(therm_all$Category, levels = c("Normothermic", "Shallow Torpor", "Transition", "Deep Torpor"))
## Plot surface vs ambient temperature
ggplot(therm_all, aes(Amb_Temp, Surf_Temp)) + geom_point(aes(col=Category, shape=Category), size=3, alpha=0.8) + my_theme +
  scale_y_continuous(breaks = seq(0,40,5)) +
  scale_colour_manual(values=my_colors) +
  geom_smooth(aes(group=Category, col=Categ_Sp),method='lm') +
  scale_shape_manual(values = c(15:18)) +
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5), axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), legend.key.height = unit(1.5, 'lines')) +
  xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) 

## Figure 3 tweaking: Surface vs ambient temperature, with one linear model fitted to each category
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
  ylab( expression(atop(paste("Surface Temperature (", degree,"C)"))))

# #library(ggeffects)
# dfpred <- ggpredict(mod_cor, terms = c("Amb_Temp","Category", "Species"))
# ggplot(dfpred, aes(x, predicted)) + my_theme +
#   geom_point(data=therm_all, aes(x=Amb_Temp, y=Surf_Temp, col=Category), size=2.5,
#              inherit.aes = F) +
#   geom_line(aes(color=group)) +
#   geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), show.legend = F, alpha=0.15) +
#   scale_colour_manual(values=my_colors) +
#   scale_fill_manual(values=my_colors) +
#   xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
#   ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
#   ggtitle("")
# 
# ggplot(dfpred, aes(group, predicted)) + my_theme +
#   #geom_line() +
#   geom_point(aes(col=facet))+
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high, col=facet), width=.2,
#                 position=position_dodge(0.05)) +
#   #scale_colour_manual(values=my_colors) +
#   #scale_fill_manual(values=my_colors) +
#   #xlab( expression(atop(paste("Ambient Temperature (", degree,"C)")))) + 
#   ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
#   ggtitle("")
# 
# dfpred2 <- ggpredict(mod_cor, terms = c("Species","Category"))
# plot(dfpred2, add.data = T, line.size=1.5, dot.alpha=0.2) + scale_colour_manual(values=my_colors) +
#   ylab( expression(atop(paste("Surface Temperature (", degree,"C)")))) +
#   ggtitle("") + my_theme + theme(legend.key.height =  unit(3, 'lines'))
# 
# 

## Figure 4a: Range of max surface temperatures per individual (or per night), colored by category
# ggplot(therm_all, aes(pasted, Surf_Temp)) + my_theme + geom_point(aes(col=Category), size=2, alpha=0.8) +  
#   facet_grid(.~Species, scales = "free_x",space = "free_x") +
#   ylab(Temp.lab) + xlab("Individual") + 
#   #scale_color_manual(values = c('black','deepskyblue2', 'palegreen4', 'red')) +
#   scale_color_manual(values=my_colors) +
#   guides(colour = guide_legend(override.aes = list(size=3.5))) +
#   theme(axis.text.x = element_text(angle=30, size=15, vjust=1, hjust=1), axis.text.y=element_text(size=20),
#         legend.key.height = unit(3, 'lines'))

#Fig. 4a with numeric individual IDs instead of full IDs
fig.4a <- ggplot(therm_all, aes(as.factor(as.numeric(Indiv_numeric)), Surf_Temp)) + my_theme + geom_point(aes(col=Category), size=2, alpha=0.8) +  
  facet_grid(.~Species, scales = "free_x",space = "free_x") +
  ylab(Temp.lab) + xlab("Individual") + 
  #scale_color_manual(values = c('black','deepskyblue2', 'palegreen4', 'red')) +
  scale_color_manual(values=my_colors) +
  guides(colour = guide_legend(override.aes = list(size=3.5))) +
  theme(axis.text.y=element_text(size=20),
        legend.key.height = unit(3, 'lines'))

## Figure 4: Stacked bar for proportion of nighttime spent in each category per species
## Figure 4b1 using original values
fig.4b1 <- ggplot(duration_categ_spp, aes(Species, CategDuration)) + my_theme + 
  geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors, name="Category") +
  scale_y_continuous(labels = percent_format()) +
  #guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.position = "none")


## Figure 4b2: Using predicted values
fig.4b2 <- ggplot(m.prop_dur, aes(Species,predicted)) + my_theme + geom_bar(aes(fill=variable), position = "fill", stat="identity") +
  #facet_grid(.~Species, scales = "free_x",space = "free_x") +
  xlab("Species") + ylab("Percentages") +
  scale_fill_manual(values=my_colors, name="Category") +
  scale_y_continuous(labels = percent_format()) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(legend.key.height = unit(3, 'lines'))

## Figure 4b - arrange the two plots
grid.arrange(fig.4b1, fig.4b2, nrow=1, ncol=2, widths = c(1.75, 2.4))

#### TRYING out Heterothermy Index ####
## HI = sqrt((sum((Tb_opt - Tb_i)^2))/n-1)
Tb_opt <- unique(therm_all$Surf_Temp)[which.max(tabulate(match(therm_all$Surf_Temp, unique(therm_all$Surf_Temp))))] ## get mode of values
hi_all <- as.data.frame(therm_all %>%
  group_by(pasted) %>%
  mutate(HI = sqrt((sum((Tb_opt - Surf_Temp)^2))/length(Surf_Temp)-1))) %>%
  distinct(pasted, .keep_all=TRUE)
hi_all


ggplot(hi_all, aes(Species, HI)) + geom_boxplot() + my_theme

for(i in unique(therm_all$pasted)) {
  trial <- therm_all[therm_all$pasted==i,]
  for(n in 1:(length(trial$Surf_Temp)-1)) {
TTUF01df$HITb<-((39.117-TTUF01df$Tb)^2)

TTUF01HI<-ddply(TTUF01df, .(Day ),summarize,
                HIsum= sum(HITb), 
                HIcount= length(HITb))

TTUF01HI <- subset(TTUF01HI, HIcount>30)
TTUM03HI$HI <- sqrt(TTUM03HI$HIsum/(TTUM03HI$HIcount-1))

plot(TTUF01HI$Day, TTUF01HI$HI)
  }
}

setwd("~/Research/1_Manuscripts/2_Tupaia tana/Stats and figures/Mammalian Tb")
#setwd("~/Documents/Research/1_Manuscripts/2_Tupaia tana/Stats and figures/Mammalian Tb")

library(nlme)
library(lme4)
library(MASS)
library(AICcmodavg)
library(segmented)
library(plyr)
library(calibrate)

read.csv('TupaiaTbfull.csv')->Ttdf
summary(Ttdf)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

subset(Ttdf, Animal=="TTUM03")->TTUM03df
subset(Ttdf, Animal=="TTF04")->TTF04df
subset(Ttdf, Animal=="TTUF01")->TTUF01df

#mode for full data per individual (not useful includes both active and resting)
mode(TTUM03df$Tb)
mode(TTF04df$Tb)
mode(TTUF01df$Tb)

#HI analysis

#first get mode for active Tb

as.data.frame(with(TTUM03df, tapply(Tb,LIGHT, mode)))
#daytime                                   39.565
#nightime                                  36.075

as.data.frame(with(TTUF01df, tapply(Tb,LIGHT, mode)))
#daytime                                   39.117
#nightime                                  35.629

as.data.frame(with(TTF04df, tapply(Tb,LIGHT, mode)))
#daytime                                  39.093
#nightime                                 36.103

#diff from mode per sample
TTUM03df$HITb<-((39.565-TTUM03df$Tb)^2)

as.data.frame(with(TTUM03df, tapply(HITb,Day, sum)))->TTUM03HI
with(TTUM03df, tapply(HITb,Day, length))->TTUM03HI$HICount

TTUM03HI<-ddply(TTUM03df, .(Day ),summarize,
                HIsum= sum(HITb), 
                HIcount= length(HITb))

subset(TTUM03HI, HIcount>30)->TTUM03HI
sqrt(TTUM03HI$HIsum/(TTUM03HI$HIcount-1))->TTUM03HI$HI


TTUF01df$HITb<-((39.117-TTUF01df$Tb)^2)

TTUF01HI<-ddply(TTUF01df, .(Day ),summarize,
                HIsum= sum(HITb), 
                HIcount= length(HITb))

subset(TTUF01HI, HIcount>30)->TTUF01HI
sqrt(TTUF01HI$HIsum/(TTUF01HI$HIcount-1))->TTUF01HI$HI

plot(TTUF01HI$Day, TTUF01HI$HI)

TTF04df$HITb<-((39.093-TTF04df$Tb)^2)

TTF04HI<-ddply(TTF04df, .(Day ),summarize,
               HIsum= sum(HITb), 
               HIcount= length(HITb))

subset(TTF04HI, HIcount>30)->TTF04HI
sqrt(TTF04HI$HIsum/(TTF04HI$HIcount-1))->TTF04HI$HI

plot(TTF04HI$Day, TTF04HI$HI)

write.csv(TTUM03HI, "TTUM03HI.csv")
write.csv(TTF04HI, "TTF04HI.csv")
write.csv(TTUF01HI, "TTUF01HI.csv")
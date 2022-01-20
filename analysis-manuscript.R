# load
library(tidyverse)
library(car)
library(MuMIn)
library(party)
library(caret)
library(randomForest) 
library(lme4)
library(lsmeans)
library(lmerTest)

source("summarySE.R")

# data available upon request:
## GenderXyear: gender prevalence over time
## BGdata: song entries as rows (at week level) with DICTION (lyrical variables) for bands
## SGdata: song entries as rows (at week level) with DICTION (lyrical variables) for singers


##############################
# Analysis band gender
##############################

## 1. Selection TIME groups
### Plot with gender over time
GenderXyear$Gender <- ordered(GenderXyear$Gender, levels = c("Male", "Female", "Both"))

a <- ggplot(data=GenderXyear, aes(x=Year, y=BG.Cent, fill=Gender, color=Gender)) +
    geom_line(aes(linetype=Gender))+
    geom_point() +
    scale_linetype_manual(values=c("dotted","solid", "dotted"))+
    ylim(0,100) +
    labs(x="Year", y = "Percentage")+
    theme_classic()
a2 <- a  + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") 
ii=1960:2015
a2 + scale_x_continuous(breaks=ii)

# save
ggsave("Figure1_gender.pdf", width=25, height=15, units = c("cm"),
       dpi=300, device = "pdf")


modelBG.male= lm(Year ~ male.BG, data=GenderXyear)
summary(modelBG.male)
modelBG.female= lm(Year ~ female.BG, data=GenderXyear)
summary(modelBG.female)
modelBG.both= lm(Year ~ both.BG, data=GenderXyear)
summary(modelBG.both)

### Model tree
bands_gender_tree <- ctree(as.factor(BandGender) ~ year, data = BGdata)
plot(BGtree)

singers_gender_tree <- ctree(as.factor(SGnames) ~ year, data = BGdata)
plot(singers_gender_tree)

## 2. Selection lyrical themes
### Random Forest
mod1 <- train(as.factor(BandGender) ~  totwd + numeric+ambiv+self+tenacity+level+collect+praise+satisfy+inspire+blame+hardship+aggress+accomp+communic+cognitiv+passive+spatial+familiar+temporal+present+humint+concrete+past+central+rapport+cooperat+diverse+exclude+liberate+denial+motion+insist+embell+variety+complex, 
             data= BGdata,
             method = "cforest",
             tuneLength = 1,
             trControl = trainControl(method = "repeatedcv",
                                      repeats = 10))
mod1
vis1 = varImp(mod1)
plot(vis1)

## 3. Lyrics and Gender over time
BGenderF= as.factor(BGdata$BandGender)
YearF.5= as.factor(BGdata$YG.5)
ArtistF= as.factor(BGdata$artist)
TitleF= as.factor(BGdata$title)

m.totwd <- lmer(totwd ~ BGenderF*YearF.5+(1|ArtistF)+(1|TitleF),data=BGdata) #Significant interaction
anova(m.totwd)
t.totwd <- ctree(as.factor(BandGender) ~ year+totwd, data = BGdata)
plot(t.totwd)

m.self <- lmer(self ~ BGenderF*YearF.5+(1|ArtistF)+(1|TitleF),data=BGdata) #Significant interaction
anova(m.self)
t.self <- ctree(as.factor(BandGender) ~ year+self, data = BGdata)
plot(t.self)

m.concrete <- lmer(concrete ~ BGenderF*YearF.5+(1|ArtistF)+(1|TitleF),data=BGdata) #NONE
anova(m.concrete)
m.complex <- lmer(complex ~ BGenderF*YearF.5+(1|ArtistF)+(1|ArtistF),data=BGdata) #NONE
anova(m.complex)
m.var <- lmer(variety ~ BGenderF*YearF.5+(1|ArtistF)+(1|ArtistF),data=BGdata) #NONE
anova(m.var)


# Figures
## total number of words
p.totwd<- summarySE(BGdata, measurevar="totwd",groupvars=c("YG.5","BGenderF"))
plot.totwd.BG<- ggplot(p.totwd, aes(x=YG.5, y=totwd, group = BGenderF, shape=BGenderF, linetype=BGenderF))+ 
    geom_errorbar(aes(ymin=totwd-se, ymax=totwd+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line(aes(linetype=BGenderF)) +
    geom_point()+
    scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
    ylim(0,500)+
    labs(x="year", y = "totwd")+
    theme_classic()
plot.totwd.BG

## self.reference
p.self<- summarySE(BGdata, measurevar="self",groupvars=c("YG.5","BGenderF"))
plot.totwd.BG<- ggplot(p.self, aes(x=YG.5, y=self, group = BGenderF, shape=BGenderF, linetype=BGenderF))+ 
    geom_errorbar(aes(ymin=self-se, ymax=self+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line(aes(linetype=BGenderF)) +
    geom_point()+
    scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
    ylim(25,75)+
    labs(x="year", y = "self-reference")+
    theme_classic()
plot.totwd.BG



##############################
# Analysis singer gender
##############################

### Plot with gender over time
GenderXyear$Gender <- ordered(GenderXyear$Gender, levels = c("Male", "Female", "Both"))

a <- ggplot(data=GenderXyear, aes(x=Year, y=SG.Cent, fill=Gender)) +
    geom_line(aes(linetype=Gender))+
    geom_point() +
    scale_linetype_manual(values=c("dotted","solid", "dotted"))+
    ylim(0,100) +
    labs(x="Year", y = "Percentage")+
    theme_classic()
a2 <- a + scale_fill_brewer(palette="Greys") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ii=1960:2015
a2 + scale_x_continuous(breaks=ii)

modelSG.male= lm(Year ~ male.SG, data=GenderXyear)
summary(modelSG.male)
modelSG.female= lm(Year ~ female.SG, data=GenderXyear)
summary(modelSG.female)
modelSG.both= lm(Year ~ both.SG, data=GenderXyear)
summary(modelSG.both)

### Model tree
SGtree <- ctree(as.factor(SingerGender) ~ year, data = SGdata)
plot(SGtree)

## 2. Selection lyrical themes
### Random Forest
set.seed(615247)
library(caret)
mod2 <- train(as.factor(SingerGender) ~  numeric+ambiv+self+tenacity+level+collect+praise+satisfy+inspire+blame+hardship+aggress+accomp+communic+cognitiv+passive+spatial+familiar+temporal+present+humint+concrete+past+central+rapport+cooperat+diverse+exclude+liberate+denial+motion+insist+embell+variety+complex, 
             data= SGdata,
             method = "cforest",
             tuneLength = 1,
             trControl = trainControl(method = "repeatedcv",
                                      repeats = 10))
vis2 = varImp(mod2)
plot(vis2)


## 3. Lyrics and Gender over time
SGenderF= as.factor(SGdata$SingerGender)
SG.YearF.5= as.factor(SGdata$YG.5)
SG.ArtistF= as.factor(SGdata$artist)
SG.TitleF= as.factor(SGdata$title)

m.totwd.SG <- lmer(totwd ~ SGenderF*SG.YearF.5+(1|SG.ArtistF)+(1|SG.TitleF),data=SGdata) #Significant interaction
anova(m.totwd.SG)
t.totwd.SG <- ctree(as.factor(SingerGender) ~ year+totwd, data = SGdata)
plot(t.totwd.SG)
m.present.SG <- lmer(present ~ SGenderF*SG.YearF.5+(1|SG.ArtistF)+(1|SG.TitleF),data=SGdata) #Significant interaction
anova(m.present.SG) #Signifincat intereaction
t.present.SG <- ctree(as.factor(SingerGender) ~ year+present, data = SGdata)
plot(t.present.SG)
m.var.SG <- lmer(variety ~ SGenderF*SG.YearF.5+(1|SG.ArtistF)+(1|SG.TitleF),data=SGdata) #Significant interaction
anova(m.var.SG) #NONSIG
t.var.SG <- ctree(as.factor(SingerGender) ~ year+variety, data = SGdata)
plot(t.var.SG)

m.self.SG <- lmer(self ~ SGenderF*SG.YearF.5+(1|SG.ArtistF)+(1|SG.TitleF),data=SGdata) 
anova(m.self.SG) #NONSIG
m.collect.SG <- lmer(collect ~ SGenderF*SG.YearF.5+(1|SG.ArtistF)+(1|SG.TitleF),data=SGdata) 
anova(m.collect.SG) #NONSIG
m.var.SG <- lmer(variety ~ SGenderF*SG.YearF.5+(1|SG.ArtistF)+(1|SG.TitleF),data=SGdata)
anova(m.var.SG) #NONSIG

# Figures
p.totwd.SG<- summarySE(SGdata, measurevar="totwd",groupvars=c("YG.5","SGenderF"))
plot.totwd.SG<- ggplot(p.totwd.SG, aes(x=YG.5, y=totwd, group = SGenderF, shape=SGenderF, linetype=SGenderF))+ 
    geom_errorbar(aes(ymin=totwd-se, ymax=totwd+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line(aes(linetype=SGenderF)) +
    geom_point()+
    scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
    ylim(0,500)+
    labs(x="year", y = "totwd")+
    theme_classic()
plot.totwd.SG

p.present.SG<- summarySE(SGdata, measurevar="present",groupvars=c("YG.5","SGenderF"))
plot.present.SG<- ggplot(p.present.SG, aes(x=YG.5, y=present, group = SGenderF, shape=SGenderF, linetype=SGenderF))+ 
    geom_errorbar(aes(ymin=present-se, ymax=present+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line(aes(linetype=SGenderF)) +
    geom_point()+
    scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
    ylim(0,50)+
    labs(x="year", y = "present")+
    theme_classic()
plot.present.SG

p.var.SG<- summarySE(SGdata, measurevar="variety",groupvars=c("YG.5","SGenderF"))
plot.var.SG<- ggplot(p.var.SG, aes(x=YG.5, y=variety, group = SGenderF, shape=SGenderF, linetype=SGenderF))+ 
    geom_errorbar(aes(ymin=variety-se, ymax=variety+se), width=.1, 
                  position=position_dodge(0.005)) +
    geom_line(aes(linetype=SGenderF)) +
    geom_point()+
    scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
    ylim(.25,.5)+
    labs(x="year", y = "variety")+
    theme_classic()
plot.var.SG


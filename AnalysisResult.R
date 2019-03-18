# 571 Final Project
# Ishita Chordia & Justin Petelka

df0 = read.csv("Documents/Info571/long_table.csv")
View(df0)
df = df0[df0$platform == "Facebook" | df0$platform == "Instagram" ,]
levels(df$pol)[levels(df$pol)=="I prefer not to answer"] <- ""
df$platform = factor(df$platform)
df$pol = ordered(df$pol, levels = c("AAA", "AA", "A", "BBB"))  ##CHANGE THIS
df$edu = ordered(df$edu, levels = c("AAA", "AA", "A", "BBB")) ## CHANGE THIS

#Number of subjects
df$subject = factor(df$subject)
numSubjects = sum(count(levels(df$subject))$freq)

# Usage & Fight by Platform
library(plyr)
ddply(df, ~platform, summarise, Use.sum= sum(use), Fight.sum = sum(fight))

#Usage by Policy and Platform
#ddply(df, ~platform+pol, summarise, use.sum = sum(use))
with(df, interaction.plot(platform, pol, use, col = 2:9, trace.label = "Policy", xlab = "", ylab = "Number of Users", fun = function(use) sum(use, na.rm = TRUE))) # interaction?

#Usage by Education and Platform
#ddply(df, ~platform+edu, summarise, use.sum = sum(use))
with(df, interaction.plot(platform, edu, use, col = 2:9, trace.label = "Education", xlab = "", ylab = "Number of Users", fun = function(use) sum(use, na.rm = TRUE))) # interaction?

#Fights by Policy and Platfrom and Education
#with(df, interaction.plot(platform, pol, fight, col = 2:9, trace.label = "Policy", ylab = "Number of Fighters", fun = function(fight) sum(fight, na.rm = TRUE))) # interaction?
#with(df, interaction.plot(platform, edu, fight, col = 2:9, trace.label = "Education", ylab = "Number of Fighteres", fun = function(fight) sum(fight, na.rm = TRUE))) # interaction?

# Aggregate DF
aggdf <-aggregate(cbind(df$use, df$fight), by=list(df$platform), 
                       FUN=sum, na.rm=TRUE)
aggdf$pctFight = aggdf$V2 / aggdf$V1

# Aggregate by Education and Platform
aggdf_Educ <-aggregate(cbind(df$use, df$fight), by=list(df$platform,df$edu), 
                    FUN=sum, na.rm=TRUE)
aggdf_Educ$pctFight = aggdf_Educ$V2 / aggdf_Educ$V1
colnames(aggdf_Educ)[colnames(aggdf_Educ)=="Group.1"] <- "platform"
colnames(aggdf_Educ)[colnames(aggdf_Educ)=="Group.2"] <- "edu"
with(aggdf_Educ, interaction.plot(platform, edu, pctFight, col = 2:9, trace.label = "Education", xlab = "", ylab = "Percent of Users That Fight")) 

# Aggregate by Politics and Platform
aggdf_Pol <-aggregate(cbind(df$use, df$fight), by=list(df$platform,df$pol), 
                       FUN=sum, na.rm=TRUE)
aggdf_Pol$pctFight = aggdf_Pol$V2 / aggdf_Pol$V1
colnames(aggdf_Pol)[colnames(aggdf_Pol)=="Group.1"] <- "platform"
colnames(aggdf_Pol)[colnames(aggdf_Pol)=="Group.2"] <- "pol"
with(aggdf_Pol, interaction.plot(platform, pol, pctFight, col = 2:9, trace.label = "Policy", xlab = "", ylab = "Percent of Users That Fight")) 


## This is where we introduce the hypothesis of what we think is going on, including interaction effects and main effects

## Hypothesis 1: Usage GLMM
library(lme4) # for glmer
library(car) # for Anova
# set sum-to-zero contrasts for the Anova call
contrasts(df$edu) <- "contr.sum"
contrasts(df$pol) <- "contr.sum"
contrasts(df$platform) <- "contr.sum"
df$use = factor(df$use)
# Education, Policy, Platfrom are all Fixed effects. Subject is a random effect.
m = glmer(use ~ (edu * pol * platform) + (1|subject), data=df, family=binomial, nAGQ=0)
Anova(m, type=3)
# Post-hoc effects
library(multcomp) # for glht
library(emmeans) # for emm, emmeans
summary(glht(m, emm(pairwise ~ platform*X2)), test=adjusted(type="holm"))

## Hypothesis 2: Fight GLMM
dfFight = df[df$use == "1",]
contrasts(dfFight$edu) <- "contr.sum"
contrasts(dfFight$pol) <- "contr.sum"
contrasts(dfFight$platform) <- "contr.sum"
df$fight = factor(df$fight)
m = glmer(fight ~ (edu * pol * platform) + (1|subject), data=dfFight, family=binomial, nAGQ=0)
Anova(m, type=3)
# Post-hoc effects
summary(glht(m, emm(pairwise ~ platform)), test=adjusted(type="holm"))


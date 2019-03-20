# 571 Final Project
# Ishita Chordia & Justin Petelka

df0 = read.csv("Documents/Info571/long_table2.csv")
df = df0[df0$platform == "Facebook" | df0$platform == "Instagram" ,]
df$platform = factor(df$platform)
df$pol = ordered(df$pol_recode, levels = c("Liberal", "Moderate", "Conservative", "I prefer not to say")) 
levels(df$pol)[levels(df$pol)=="I prefer not to say"] <- NA

#Number of subjects
df$subject = factor(df$subject)
numSubjects = sum(count(levels(df$subject))$freq)

# Usage by Platform
library(plyr)
ddply(df, ~platform, summarise, Use.sum= sum(use), Fight.sum = sum(fight))
aggSubject <-aggregate(cbind(df$use, df$fight), by=list(df$subject), FUN=sum, na.rm=TRUE)
aggSubject$V1 = factor(aggSubject$V1)
summary(aggSubject$V1)

#Fight by Platfrom 
dfUsage = df[df$use == 1 ,]
ddply(dfUsage, ~platform, summarise, Fight.sum = sum(fight))
aggSubject <-aggregate(cbind(dfUsage$use, dfUsage$fight), by=list(dfUsage$subject), FUN=sum, na.rm=TRUE)
aggSubject$V2 = factor(aggSubject$V2)
summary(aggSubject$V2)

# Policy
aggPolicy <-aggregate(cbind(df$use, df$fight), by=list(df$pol), FUN=sum, na.rm=TRUE)
colnames(aggPolicy)[colnames(aggPolicy)=="Group.1"] <- "Political Leaning"
colnames(aggPolicy)[colnames(aggPolicy)=="V1"] <- "Users"
colnames(aggPolicy)[colnames(aggPolicy)=="V2"] <- "Arguers"
#aggPolicy = aggPolicy[1:3,]
barplot(`colnames<-`(t(aggPolicy[-1]), aggPolicy[,1]), beside=TRUE, 
        legend.text = TRUE, col = c("red", "darkblue"), 
        ylim=c(0, 200), ylab = "Number of People",
        args.legend = list(x = "topright", bty = "n", inset=c(-0.05, 0)))
dev.copy(pdf,'Documents/Info571/571Project/policy.pdf')
dev.off()

#Usage by Policy and Platform
#ddply(df, ~platform+pol, summarise, use.sum = sum(use))
with(df, interaction.plot(platform, pol, use, col = 2:9, trace.label = "Policy", xlab = "", ylab = "Number of Users", fun = function(use) sum(use, na.rm = TRUE))) # interaction?
dev.copy(pdf,'Documents/Info571/571Project/usage_platformpolicy.pdf')
dev.off()

#Fights by Policy and Platfrom
#with(df, interaction.plot(platform, pol, fight, col = 2:9, trace.label = "Policy", xlab = "", ylab = "Number of Arguers", fun = function(fight) sum(fight, na.rm = TRUE))) # interaction?


# Aggregate DF
aggdf <-aggregate(cbind(df$use, df$fight), by=list(df$platform), 
                  FUN=sum, na.rm=TRUE)
aggdf$pctFight = aggdf$V2 / aggdf$V1

# Aggregate by Politics and Platform
aggdf_Pol <-aggregate(cbind(df$use, df$fight), by=list(df$platform,df$pol), 
                      FUN=sum, na.rm=TRUE)
aggdf_Pol$pctFight = aggdf_Pol$V2 / aggdf_Pol$V1
colnames(aggdf_Pol)[colnames(aggdf_Pol)=="Group.1"] <- "platform"
colnames(aggdf_Pol)[colnames(aggdf_Pol)=="Group.2"] <- "pol"
with(aggdf_Pol, interaction.plot(platform, pol, pctFight, col = 2:9, trace.label = "Policy", xlab = "", ylab = "Percent of Users That Argue")) 
dev.copy(pdf,'Documents/Info571/571Project/arguments_platformpolicy.pdf')
dev.off()

## This is where we introduce the hypothesis of what we think is going on, including interaction effects and main effects

## Hypothesis 1: Usage GLMM
library(lme4) # for glmer
library(car) # for Anova
# set sum-to-zero contrasts for the Anova call
contrasts(df$pol) <- "contr.sum"
contrasts(df$platform) <- "contr.sum"
df$use = factor(df$use)
# Policy, Platfrom are all Fixed effects. Subject is a random effect.
m = glmer(use ~ (pol*platform) + (1|subject), data=df, family=binomial, nAGQ=0)
Anova(m, type=3)
# Post-hoc effects
#library(multcomp) # for glht
#library(emmeans) # for emm, emmeans
#summary(glht(m, emm(pairwise ~ platform*X2)), test=adjusted(type="holm"))

## Hypothesis 2: Fight GLMM
dfFight = df[df$use == "1",]
contrasts(dfFight$pol) <- "contr.sum"
contrasts(dfFight$platform) <- "contr.sum"
df$fight = factor(df$fight)
m = glmer(fight ~ (pol * platform) + (1|subject), data=dfFight, family=binomial, nAGQ=0)
Anova(m, type=3)
# Post-hoc effects
summary(glht(m, emm(pairwise ~ pol)), test=adjusted(type="holm"))


ERNA <- read.csv("ERNA_data.csv", header = TRUE)

library(car)
library(lme4)
library(emmeans)
library(magrittr)
library(AICcmodavg)
library(stats)
ERNA.crop <- ERNA[1:1124, ]

ERNA.ex.rep <- ERNA.crop[ERNA.crop$replaced_YorN !="Y",]

ERNA.wet <- ERNA.crop[ERNA.crop$treatment !="dry" ,]

ERNA.dry <- ERNA.crop[ERNA.crop$treatment != "wet" ,]

#post-hoc tukey test  ht ~ population
ERNA_ht_pop_anova <- aov(length_cm_20220915 ~ Population, data = ERNA.crop)
summary(ERNA_ht_pop_anova)
ERNA_ht_pop_tt <- TukeyHSD(ERNA_ht_pop_anova)
ERNA_ht_pop_tt$Population
summary(ERNA_ht_pop_tt)
head(ERNA_ht_pop_tt)
plot(ERNA_ht_pop_tt)
str(ERNA_ht_pop_tt)
ERNA_ht_pop_tt_df <- as.data.frame(ERNA_ht_pop_tt$Population)
ERNA_ht_pop_tt_df$`p adj`

hist(ERNA_ht_pop_tt_df$`p adj`)
sig.dif.tt <- ERNA_ht_pop_tt_df[ERNA_ht_pop_tt_df$`p adj` <= .05,]
hist(sig.dif.tt$`p adj`)

#post-hoc emmeans  ht ~ seed zone
ht_sz_lm <- lmer(log(length_cm_20220915) ~ seed_zone + (1|block), data = ERNA.crop)
pair.ht.sz.lm <- emmeans(ht_sz_lm, specs = pairwise ~ seed_zone)
pair.ht.sz.lm
pair.ht.sz.lm$emmean[1,2]
exp(3.83)
#post hoc emmeans ht ~ pop

ht_pop_lm <- lmer(log(length_cm_20220915) ~ Population + (1|block), data = ERNA.crop)
pair.ht.pop.lm <- emmeans(ht_pop_lm, specs = pairwise ~ Population)
summary(pair.ht.pop.lm)

survival_glm <- glm(survival_20221108 ~ Population, 
                    data = ERNA, family = binomial (link ="logit"))
survival_pair <- emmeans(survival_glm, specs = pair)

Anova(ht_pop_lm)
TukeyHSD(ht_pop_lm)

exp(0.65)

days_mort_pop_an <- aov( days_until_mortality ~ Population, data=ERNA)
summary(days_mort_pop_an)

days_mort_sz_an <- aov( days_until_mortality ~ seed_zone, data=ERNA)
summary(days_mort_pop_an)

#add colors to tx groups

boxplot(length_cm_20220915 ~ Population*treatment,data=ERNA.crop, main="ERNA Plant Height by Population", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)


boxplot(length_cm_20220915 ~ seed_zone,data=ERNA, main="ERNA Plant Height by Seed Zone", 
        xlab="Seed Zone", ylab="Height(cm)", cex.axis=0.25, las=2)

boxplot(days_until_mortality ~ Population, data=ERNA, main = "ERNA Days until mortality by Population", 
        xlab = "Population",cex.axis=0.25, las=2, ylab = "Days until mortality")

boxplot(days_until_mortality ~ seed_zone, data=ERNA, main = "ERNA Days until mortality by Seed Zone", 
        xlab = "Seed Zone", ylab = "Days until mortality",cex.axis=0.25, las=2)

boxplot(days_until_flower ~ Population, data=ERNA.crop, main = "ERNA Days until flowering by Population", 
        xlab = "Population",cex.axis=0.25, las=2, ylab = "Days until flower")

survival_glm <- glm(survival_20221108 ~ Population, 
                    data = ERNA, family = binomial (link ="logit"))
summary(survival_glm)

survival.pred <- predict(survival_glm, pop.list, se.fit = TRUE, type = "response", interval = "confidence" )
survival.pred
survival_mat <- matrix(data = survival.pred$fit, nrow = 1, ncol = 20)
barplot(survival_mat, ylim = c(0,1), xlab = "Population", ylab = "Survival Rate", main = "Survival Rate by Population")

ERNA.pop.list.df <- as.data.frame(unique(ERNA.crop$Population))
colnames(ERNA.pop.list.df) <- "Population"

#add elevation
ERNA_elev <- read.csv("20230118_ERNA_elev.csv", header = TRUE)

ERNA.crop[ , 'elev'] <- NA

for (dd in 1:length(ERNA.pop.list)) { 
  ERNA.crop$elev[grepl(ERNA.pop.list[dd], ERNA.crop$Population)] <- ERNA_elev$elevation[dd] 
}

#add precip
ERNA_precip <- read.csv("20221129_ERNA_pptAnnual.csv", header = TRUE)

ERNA.pop.list <- unique(as.character(ERNA.crop$Population))

ERNA.crop[ , 'Ppt_Annual'] <- NA

for (dd in 1:length(ERNA.pop.list)) { 
  ERNA.crop$Ppt_Annual[grepl(ERNA.pop.list[dd], ERNA.crop$Population)] <- ERNA_precip$Mean_Annual_Ppt[dd] 
  }

#add min winter temp
ERNA_temp <- read.csv("20230118_ERNA_tminWinter.csv", header = TRUE)

ERNA.crop[ , 'min_wint_temp'] <- NA

for (dd in 1:length(ERNA.pop.list)) { 
  ERNA.crop$min_wint_temp[grepl(ERNA.pop.list[dd], ERNA.crop$Population)] <- ERNA_temp$Mean_MinWinter_Temp[dd] 
}

ERNA.crop$min_wint_temp

#lm ht ~ wint temp

plot(length_cm_20220915 ~ min_wint_temp, data = ERNA.crop)

temp_lm <- lmer(log(length_cm_20220915) ~ min_wint_temp + (1|block), data = ERNA.crop)
summary(temp_lm)

predict.lm(temp_lm, ERNA.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )

plot(temp_lm) 


#run lm height ~ ppt
hist(ERNA.crop$length_cm_20220915)

plot(length_cm_20220915 ~ Ppt_Annual, data = ERNA.crop)

ppt_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual + (1|block), data = ERNA.crop)


summary(ppt_lm)

plot(ppt_lm)



predict(ppt_lm, ERNA.pop.list, se.fit = TRUE, type = "response", interval = "confidence" )

library(car)
Anova(ppt_elev_lm)

boxplot(length_cm_20220915 ~ Ppt_Annual,data=ERNA.crop, main="ERNA Plant Height by Elevation", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)


#run lm ht ~ elev
plot(length_cm_20220915 ~ elev, data = ERNA.crop)

library(lme4)
elev_lm <- lmer(log(length_cm_20220915) ~ elev + (1|block), data = ERNA.crop)
summary(elev_lm)
plot(elev_lm)

#check if column added correctly 
elev.list <-unique(ERNA.crop$elev)
elev.list
ppt.list <- unique(ERNA.crop$Ppt_Annual)
ppt.list

elev_check <- cbind.data.frame(ERNA.pop.list, elev.list)
colnames(elev_check) <- colnames(ERNA_elev)
identical(elev_check, ERNA_elev)

ppt_check <- cbind.data.frame(ERNA.pop.list,ppt.list)
ERNA_precip
colnames(ppt_check) <- colnames(ERNA_precip)
ppt_check
identical(ppt_check, ERNA_precip)

#plot w/ treatments
boxplot(length_cm_20220915 ~ Population,data=ERNA.wet, main="ERNA Plant Height by Population - Wet", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)

boxplot(length_cm_20220915 ~ Population,data=ERNA.dry, main="ERNA Plant Height by Population - Dry", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)

#plot elev
boxplot(length_cm_20220915 ~ elev,data=ERNA.crop, main="ERNA Plant Height by Elevation", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)

#AIC for climate models
climate_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual+ min_wint_temp + elev + (1|block), data = ERNA.crop)
temp.precip.lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual+ min_wint_temp + (1|block), data = ERNA.crop)
ppt_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual + (1|block), data = ERNA.crop)
ppt.elev <- lmer(log(length_cm_20220915) ~ Ppt_Annual+ elev + (1|block), data = ERNA.crop)
temp.elev <- lmer(log(length_cm_20220915) ~ elev + min_wint_temp + (1|block), data = ERNA.crop)


models <- list(climate_lm,temp.precip.lm,ppt_lm, elev_lm, temp_lm, ppt.elev, temp.elev)
mod.names <- c('climate', 'temp.ppt', 'ppt', 'elev', 'temp','ppt.elev', 'temp.elev')
aictab(cand.set = models, modnames = mod.names )

elev_lm
temp_lm
ppt_lm

summary(climate_lm)
climate_lm_aov <- anova(climate_lm)
summary(climate_lm_aov)

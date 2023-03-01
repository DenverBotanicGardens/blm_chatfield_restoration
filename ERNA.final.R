ERNA <- read.csv("ERNA_data.csv", header = TRUE, na.strings = "NA")
library(dplyr)
library(car)
library(lme4)
library(emmeans)
library(magrittr)
library(AICcmodavg)
library(stats)
library(tidyverse)
ERNA.crop <- ERNA[1:1124, ]

ERNA.pop.list <- unique(as.character(ERNA.crop$Population))


ERNA.ex.rep <- ERNA.crop[ERNA.crop$replaced_YorN !="Y",]

ERNA.wet <- ERNA.crop[ERNA.crop$treatment !="dry" ,]

ERNA.dry <- ERNA.crop[ERNA.crop$treatment != "wet" ,]

#add elevation
ERNA_elev <- read.csv("20230118_ERNA_elev.csv", header = TRUE)

ERNA.crop[ , 'elev'] <- NA

for (dd in 1:length(ERNA.pop.list)) { 
  ERNA.crop$elev[grepl(ERNA.pop.list[dd], ERNA.crop$Population)] <- ERNA_elev$elevation[dd] 
}

#add precip
ERNA_precip <- read.csv("20221129_ERNA_pptAnnual.csv", header = TRUE)

ERNA.crop[ , 'Ppt_Annual'] <- NA

for (dd in 1:length(ERNA.pop.list)) { 
  ERNA.crop$Ppt_Annual[grepl(ERNA.pop.list[dd], ERNA.crop$Population)] <- ERNA_precip$Mean_Annual_Ppt[dd] 
}

unique(ERNA.crop$Population)
unique(ERNA.crop$Ppt_Annual)

#add min winter temp
ERNA_temp <- read.csv("20230118_ERNA_tminWinter.csv", header = TRUE)

ERNA.crop[ , 'min_wint_temp'] <- NA

for (dd in 1:length(ERNA.pop.list)) { 
  ERNA.crop$min_wint_temp[grepl(ERNA.pop.list[dd], ERNA.crop$Population)] <- ERNA_temp$Mean_MinWinter_Temp[dd]
}
unique(ERNA.crop$Population)
unique(ERNA.crop$min_wint_temp)

#add goeg distance
ERNA_distance <- read.csv("ERNA_distance.csv")
ERNA.crop[ , 'dist_km'] <- NA


for (dd in 1:length(ERNA.pop.list)) { 
  ERNA.crop$dist_km[grepl(ERNA.pop.list[dd], ERNA.crop$Population)] <- ERNA_distance$sld_km[dd]
}

#emmeans tests

ht_sz_lm <- lmer(log(length_cm_20220915) ~ seed_zone + (1|block), data = ERNA.crop)
pair.ht.sz.lm <- emmeans(ht_sz_lm, specs = pairwise ~ seed_zone)
pairs(pair.ht.sz.lm, type = "response") 
summary(pair.ht.sz.lm)

ht_pop_lm <- lmer(log(length_cm_20220915) ~ Population + (1|block), data = ERNA.crop)
pair.ht.pop.lm <- emmeans(ht_pop_lm, specs = pairwise ~ Population)
summary(pair.ht.pop.lm)

survival_glm <- glm(survival_20221108 ~ Population, 
                    data = ERNA, family = binomial (link ="logit"))
survival_pair <- emmeans(survival_glm, specs = pair)



days_mort_pop_an <- aov( days_until_mortality ~ Population, data=ERNA)
summary(days_mort_pop_an)

days_mort_sz_an <- aov( days_until_mortality ~ seed_zone, data=ERNA)
summary(days_mort_pop_an)

#flowering proportion
flower.prop <- ERNA.crop %>% group_by(Population) %>% summarise(flowering.prop=sum(flowering_Y_N_20221108, na.rm = TRUE)) 
flower.prop
ERNA.crop %>%
  group_by(Population) %>%
  tally()

flower_glm <- glm(flowering_Y_N_20221108 ~ Population, 
                    data = ERNA.crop, family = binomial (link ="logit"))
flower.pred <- predict(flower_glm, ERNA.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
flower.pred
flower_mat <- matrix(data = flower.pred$fit, nrow = 1, ncol = 20)
flower_mat
#survival proportion

#re-scale climate variables

ERNA.crop$Ppt_Annual_Z <- (ERNA.crop$Ppt_Annual - mean(ERNA.crop$Ppt_Annual)) / sd(ERNA.crop$Ppt_Annual)
ERNA.crop$min_wint_temp_Z <- (ERNA.crop$min_wint_temp - mean(ERNA.crop$min_wint_temp)) / sd(ERNA.crop$min_wint_temp)
ERNA.crop$elev_Z <- (ERNA.crop$elev - mean(ERNA.crop$elev)) / sd(ERNA.crop$elev)
ERNA.crop$dist_km_Z <- (ERNA.crop$dist_km - mean(ERNA.crop$dist_km)) / sd(ERNA.crop$dist_km)

#re-scale for chatfield difference

#AIC for climate models
climate_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + min_wint_temp_Z + elev_Z + dist_km_Z + (1|block), data = ERNA.crop)
temp.precip.lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + min_wint_temp_Z + (1|block), data = ERNA.crop)
ppt_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + (1|block), data = ERNA.crop)
ppt.elev <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + elev_Z + (1|block), data = ERNA.crop)
temp.elev <- lmer(log(length_cm_20220915) ~ elev_Z + min_wint_temp_Z + (1|block), data = ERNA.crop)
elev_lm <- lmer(log(length_cm_20220915) ~ elev_Z + (1|block), data = ERNA.crop)
temp_lm <- lmer(log(length_cm_20220915) ~ min_wint_temp + (1|block), data = ERNA.crop)
dist_lm <- lmer(log(length_cm_20220915) ~ dist_km_Z + (1|block), data = ERNA.crop)
dist.ppt <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z+  dist_km_Z + (1|block), data = ERNA.crop)
dist.elev <- lmer(log(length_cm_20220915) ~ elev_Z + dist_km_Z + (1|block), data = ERNA.crop)
dist.temp <- lmer(log(length_cm_20220915) ~ dist_km_Z + min_wint_temp_Z + (1|block), data= ERNA.crop)
dist.ppt.elev <- lmer(log(length_cm_20220915) ~ dist_km_Z+ Ppt_Annual_Z + elev_Z + (1|block), data=ERNA.crop)


models <- list(climate_lm,temp.precip.lm,ppt_lm, elev_lm, temp_lm, ppt.elev, temp.elev,dist_lm, 
               dist.ppt, dist.elev, dist.temp, dist.ppt.elev)
mod.names <- c('climate', 'temp.ppt', 'ppt', 'elev', 'temp','ppt.elev', 'temp.elev', 'dist', 'dist.ppt', 'dist.elev',
               'dist.temp', 'dist.ppt.elev')
aictab(cand.set = models, modnames = mod.names )

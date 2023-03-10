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
ERNA.pop.list.df <- as.data.frame(unique(ERNA.crop$Population))
colnames(ERNA.pop.list.df) <- "Population"

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
#add ht means and sd

ht_msd <- ERNA.crop %>%                            
  group_by(Population) %>%
  summarise_at(vars(length_cm_20220915),
               list(ht_mean = mean,
                    ht_sd = sd), na.rm = TRUE) %>% 
  as.data.frame(ht_msd)

ht_msd <- left_join(ht_msd, ERNA_precip, by = "Population" )
ht_msd <- left_join(ht_msd, ERNA_temp, by = "Population" )
ht_msd <- left_join(ht_msd, ERNA_elev, by = "Population" )
ht_msd <- left_join(ht_msd, ERNA_distance, by = "Population" )

ht_msd


#ppt
plot(ht_msd$ht_mean ~ ht_msd$Mean_Annual_Ppt, pch = 19, ylim = c(15,70),
     main = "ERNA population mean height by source annual precipitation", xlab= "Mean annual precipitation (mm)", ylab = "Height (cm)")
abline(v=443.7064, col="blue")
arrows(ht_msd$Mean_Annual_Ppt, (ht_msd$ht_mean-ht_msd$ht_sd), ht_msd$Mean_Annual_Ppt, (ht_msd$ht_mean+ht_msd$ht_sd), length=0.05, angle = 90, code=3, lwd = .75)

#temp
plot(ht_msd$ht_mean ~ ht_msd$Mean_MinWinter_Temp, pch = 19, ylim = c(15,70),
     main = "ERNA population mean height by source minimum winter temperature", xlab= "Mean minimum winter temperature (C)", ylab = "Height (cm)")
abline(v=-7.714918, col="blue")
arrows(ht_msd$Mean_MinWinter_Temp, (ht_msd$ht_mean-ht_msd$ht_sd), ht_msd$Mean_MinWinter_Temp, (ht_msd$ht_mean+ht_msd$ht_sd), length=0.05, angle = 90, code=3, lwd = .75)

#elev
plot(ht_msd$ht_mean ~ ht_msd$elevation, pch = 19, ylim = c(15,70),
     main = "ERNA population mean height by source elevation", xlab= "Elevation (m)", ylab = "Height (cm)")
abline(v=5500, col="blue")
arrows(ht_msd$elevation, (ht_msd$ht_mean-ht_msd$ht_sd), ht_msd$elevation, (ht_msd$ht_mean+ht_msd$ht_sd), length=0.05, angle = 90, code=3, lwd = .75)

#dist
plot(ht_msd$ht_mean ~ ht_msd$sld_km, pch = 19, ylim = c(15,70),
     main = "ERNA population mean height by source distance from CG", xlab= "Distance (km)", ylab = "Height (cm)")
abline(v=0, col="blue")
arrows(ht_msd$sld_km, (ht_msd$ht_mean-ht_msd$ht_sd), ht_msd$sld_km, (ht_msd$ht_mean+ht_msd$ht_sd), length=0.05, angle = 90, code=3, lwd = .75)


#emmeans tests

ht_sz_lm <- lmer(log(length_cm_20220915) ~ seed_zone + (1|block), data = ERNA.crop)
pair.ht.sz.lm <- emmeans(ht_sz_lm, specs = pairwise ~ seed_zone)
ht.sz.ratio <- pairs(pair.ht.sz.lm, type = "response") 
as.data.frame(ht.sz.ratio)
pair.ht.sz.lm
ht.sz.ratio
heatmap(ht.sz.ratio)
str(ht.sz.ratio)
ht.sz.ratio@contrast

ht_pop_lm <- lmer(log(length_cm_20220915) ~ Population + (1|block), data = ERNA.crop)
pair.ht.pop.lm <- emmeans(ht_pop_lm, specs = pairwise ~ Population)
summary(pair.ht.pop.lm)

survival_glm <- glm(survival_20221108 ~ Population, 
                    data = ERNA, family = binomial (link ="logit"))
survival_pair <- emmeans(survival_glm, specs = pair)

#odds ratio heat map


palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = mydata.cor, col = palette, symm = TRUE)

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
survival.prop <- ERNA.crop %>% group_by(Population) %>% summarise(survival.prop=sum(survival_20221108, na.rm = TRUE)) 
survival.prop
ERNA.crop %>%
  group_by(Population) %>%
  tally()
survival_glm <- glm(survival_20221108 ~ Population, 
                    data = ERNA, family = binomial (link ="logit"))

survival.pred <- predict(survival_glm, ERNA.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
survival.pred
survival_mat <- matrix(data = survival.pred$fit, nrow = 1, ncol = 20)
survival_mat
barplot(survival_mat, ylim = c(0,1), xlab = "Population", ylab = "Survival Rate", main = "Survival Rate by Population")

#re-scale climate variables

ERNA.crop$Ppt_Annual_Z <- (ERNA.crop$Ppt_Annual - mean(ERNA.crop$Ppt_Annual)) / sd(ERNA.crop$Ppt_Annual)
ERNA.crop$min_wint_temp_Z <- (ERNA.crop$min_wint_temp - mean(ERNA.crop$min_wint_temp)) / sd(ERNA.crop$min_wint_temp)
ERNA.crop$elev_Z <- (ERNA.crop$elev - mean(ERNA.crop$elev)) / sd(ERNA.crop$elev)
ERNA.crop$dist_km_Z <- (ERNA.crop$dist_km - mean(ERNA.crop$dist_km)) / sd(ERNA.crop$dist_km)

#re-scale for chatfield difference
ERNA.crop$Ppt_Annual_chat <- (ERNA.crop$Ppt_Annual - 443.7064) / sd(ERNA.crop$Ppt_Annual)
ERNA.crop$min_wint_temp_chat <- (ERNA.crop$min_wint_temp - (-7.714918)) / sd(ERNA.crop$min_wint_temp)
ERNA.crop$elev_chat <- (ERNA.crop$elev - 5500) / sd(ERNA.crop$elev)
ERNA.crop$dist_km_chat <- (ERNA.crop$dist_km - .0034) / sd(ERNA.crop$dist_km)

#correlation between climate variables
plot(min_wint_temp ~ elev, data = ERNA.crop) #correlated?
plot(Ppt_Annual ~ min_wint_temp, data = ERNA.crop)
plot(elev ~ dist_km, data = ERNA.crop)

#AIC for climate models
climate_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + min_wint_temp_Z + elev_Z + dist_km_Z + (1|block), data = ERNA.crop)
temp.precip.lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + min_wint_temp_Z + (1|block), data = ERNA.crop)
ppt_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + (1|block), data = ERNA.crop)
ppt.elev <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + elev_Z + (1|block), data = ERNA.crop)
temp.elev <- lmer(log(length_cm_20220915) ~ elev_Z + min_wint_temp_Z + (1|block), data = ERNA.crop)
elev_lm <- lmer(log(length_cm_20220915) ~ elev_Z + (1|block), data = ERNA.crop)
temp_lm <- lmer(log(length_cm_20220915) ~ min_wint_temp_Z + (1|block), data = ERNA.crop)
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

#AIC for survival
climate_lm <- glmer(survival_20221108 ~ Ppt_Annual_Z + min_wint_temp_Z + elev_Z + dist_km_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
temp.precip.lm <- glmer(survival_20221108 ~ Ppt_Annual_Z + min_wint_temp_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
ppt_lm <- glmer(survival_20221108 ~ Ppt_Annual_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
ppt.elev <- glmer(survival_20221108 ~ Ppt_Annual_Z + elev_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
temp.elev <- glmer(survival_20221108 ~ elev_Z + min_wint_temp_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
elev_lm <- glmer(survival_20221108 ~ elev_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
temp_lm <- glmer(survival_20221108 ~ min_wint_temp_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
dist_lm <- glmer(survival_20221108 ~ dist_km_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
dist.ppt <- glmer(survival_20221108 ~ Ppt_Annual_Z+  dist_km_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
dist.elev <- glmer(survival_20221108 ~ elev_Z + dist_km_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
dist.temp <- glmer(survival_20221108 ~ dist_km_Z + min_wint_temp_Z + (1|block), data= ERNA.crop,family = binomial (link ="logit"))
dist.ppt.elev <- glmer(survival_20221108 ~ dist_km_Z+ Ppt_Annual_Z + elev_Z + (1|block), data=ERNA.crop,family = binomial (link ="logit"))


models <- list(climate_lm,temp.precip.lm,ppt_lm, elev_lm, temp_lm, ppt.elev, temp.elev,dist_lm, 
               dist.ppt, dist.elev, dist.temp, dist.ppt.elev)
mod.names <- c('climate', 'temp.ppt', 'ppt', 'elev', 'temp','ppt.elev', 'temp.elev', 'dist', 'dist.ppt', 'dist.elev',
               'dist.temp', 'dist.ppt.elev')
aictab(cand.set = models, modnames = mod.names )

#AIC aim 2 chatfield difference
climate_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_chat + min_wint_temp_chat + elev_chat + dist_km_chat + (1|block), data = ERNA.crop)
temp.precip.lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_chat + min_wint_temp_chat + (1|block), data = ERNA.crop)
ppt_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_chat + (1|block), data = ERNA.crop)
elev_lm <- lmer(log(length_cm_20220915) ~ elev_chat + (1|block), data = ERNA.crop)
temp_lm <- lmer(log(length_cm_20220915) ~ min_wint_temp_chat + (1|block), data = ERNA.crop)
dist_lm <- lmer(log(length_cm_20220915) ~ dist_km_chat + (1|block), data = ERNA.crop)
dist.elev <- lmer(log(length_cm_20220915) ~ elev_chat + dist_km_chat + (1|block), data = ERNA.crop)

models <- list(climate_lm,temp.precip.lm,ppt_lm, elev_lm, temp_lm, dist_lm, dist.elev)
mod.names <- c('climate', 'temp.ppt', 'ppt', 'elev', 'temp', 'dist', 'dist.elev')
aictab(cand.set = models, modnames = mod.names )

plot(length_cm_20220915 ~ dist_km_chat, data = ERNA.crop)
plot(length_cm_20220915 ~ elev_chat, data = ERNA.crop)
plot(length_cm_20220915 ~ Ppt_Annual_chat, data = ERNA.crop)
plot(length_cm_20220915 ~ min_wint_temp_chat, data = ERNA.crop)

#plot climate ~traits

ht_msd <- ERNA.crop %>%                            
  group_by(Population) %>%
  summarise_at(vars(length_cm_20220915),
               list(mean = mean,
                    sd = sd), na.rm = TRUE) %>% 
  as.data.frame(ht_msd)
        
ht_msd



plot(ht_msd$Population ~ ERNA.crop$dist_km)
mean(ERNA.crop$length_cm_20220915)
str(ERNA.crop$length_cm_20220915)

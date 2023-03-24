ERNA <- read.csv("ERNA_data.csv", header = TRUE, na.strings = "NA")
library(dplyr)
library(car)
library(lme4)
library(emmeans)
library(magrittr)
library(AICcmodavg)
library(stats)
library(tidyverse)
library(MuMIn)
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

unique(ERNA.crop$seed_zone)

#squared msd climate values
ht_msd$Mean_Annual_Ppt2 <- ht_msd$Mean_Annual_Ppt^2
ht_msd$Mean_MinWinter_Temp2 <- ht_msd$Mean_MinWinter_Temp^2
ht_msd$elev2 <- ht_msd$elevation^2

#survival prop and se
ERNA_rates <- read.csv("ERNA_rates.csv")

ERNA_rates <- left_join(ERNA_rates, ERNA_precip, by = "Population" )
ERNA_rates <- left_join(ERNA_rates, ERNA_temp, by = "Population" )
ERNA_rates <- left_join(ERNA_rates, ERNA_elev, by = "Population" )
ERNA_rates <- left_join(ERNA_rates, ERNA_distance, by = "Population" )


#ppt
plot(ERNA_rates$Survival_Rate ~ ERNA_rates$Mean_Annual_Ppt, pch = 19, ylim = c(.6,1), xlab= "Mean annual precipitation (mm)", ylab = "Survival Rate")
abline(v=443.7064, col="blue")
arrows(ERNA_rates$Mean_Annual_Ppt, (ERNA_rates$Survival_Rate-ERNA_rates$Standard_Error_sv), ERNA_rates$Mean_Annual_Ppt, (ERNA_rates$Survival_Rate+ERNA_rates$Standard_Error_sv), length=0.05, angle = 90, code=3, lwd = .75)
ERNA_rates$Mean_Annual_Ppt2 <- ERNA_rates$Mean_Annual_Ppt^2
summary(lm(ERNA_rates$Survival_Rate ~ ERNA_rates$Mean_Annual_Ppt + ERNA_rates$Mean_Annual_Ppt2))
summary(lm(ERNA_rates$Survival_Rate ~ ERNA_rates$Mean_Annual_Ppt))

#temp
plot(ERNA_rates$Survival_Rate ~ ERNA_rates$Mean_MinWinter_Temp, pch = 19, ylim = c(.6,1), xlab= "Mean minimum winter temperature (C)", ylab = "Survival Rate")
arrows(ERNA_rates$Mean_MinWinter_Temp, (ERNA_rates$Survival_Rate-ERNA_rates$Standard_Error_sv), ERNA_rates$Mean_MinWinter_Temp, (ERNA_rates$Survival_Rate+ERNA_rates$Standard_Error_sv), length=0.05, angle = 90, code=3, lwd = .75)
abline(v=-7.714918, col="blue")

ggplot(data = ERNA_rates, aes(y=Survival_Rate, x=Mean_MinWinter_Temp)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=Survival_Rate-Standard_Error_sv, ymax=Survival_Rate+Standard_Error_sv), width=.2)

#elev
plot(ERNA_rates$Survival_Rate ~ ERNA_rates$elevation, pch = 19, ylim = c(.6,1), xlab= "Elevation (ft)", ylab = "Survival Rate")
abline(v=5500, col="blue")
arrows(ERNA_rates$elevation, (ERNA_rates$Survival_Rate-ERNA_rates$Standard_Error_sv), ERNA_rates$elevation, (ERNA_rates$Survival_Rate+ERNA_rates$Standard_Error_sv), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(ERNA_rates$Survival_Rate ~ ERNA_rates$elevation))

#dist
plot(ERNA_rates$Survival_Rate ~ ERNA_rates$sld_km, pch = 19, ylim = c(.6,1), xlab= "Distance (km)", ylab = "Survival Rate")
abline(v=0, col="blue")
arrows(ERNA_rates$sld_km, (ERNA_rates$Survival_Rate-ERNA_rates$Standard_Error_sv), ERNA_rates$sld_km, (ERNA_rates$Survival_Rate+ERNA_rates$Standard_Error_sv), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(ERNA_rates$Survival_Rate ~ ERNA_rates$sld_km))

#flower rate msd plots
#ppt
plot(ERNA_rates$Flower_Rate ~ ERNA_rates$Mean_Annual_Ppt, pch = 19, ylim = c(0,1), xlab= "Mean annual precipitation (mm)", ylab = "Flower Rate")
abline(v=443.7064, col="blue")
arrows(ERNA_rates$Mean_Annual_Ppt, (ERNA_rates$Flower_Rate-ERNA_rates$Standard_Error_fl), ERNA_rates$Mean_Annual_Ppt, (ERNA_rates$Flower_Rate+ERNA_rates$Standard_Error_fl), length=0.05, angle = 90, code=3, lwd = .75)
ERNA_rates$Mean_Annual_Ppt2 <- ERNA_rates$Mean_Annual_Ppt^2
summary(lm(ERNA_rates$Flower_Rate ~ ERNA_rates$Mean_Annual_Ppt + ERNA_rates$Mean_Annual_Ppt2))
summary(lm(ERNA_rates$Flower_Rate ~ ERNA_rates$Mean_Annual_Ppt))

#temp
plot(ERNA_rates$Flower_Rate ~ ERNA_rates$Mean_MinWinter_Temp, pch = 19, ylim = c(0,1), xlab= "Mean minimum winter temperature (C)", ylab = "Flower Rate")
arrows(ERNA_rates$Mean_MinWinter_Temp, (ERNA_rates$Flower_Rate-ERNA_rates$Standard_Error_fl), ERNA_rates$Mean_MinWinter_Temp, (ERNA_rates$Flower_Rate+ERNA_rates$Standard_Error_fl), length=0.05, angle = 90, code=3, lwd = .75)
abline(v=-7.714918, col="blue")
ERNA_rates$Mean_MinWinter_Temp2 <- ERNA_rates$Mean_MinWinter_Temp^2
summary(lm(ERNA_rates$Flower_Rate ~ ERNA_rates$Mean_Annual_Ppt + ERNA_rates$Mean_MinWinter_Temp2))

ggplot(data = ERNA_rates, aes(y=Flower_Rate, x=Mean_MinWinter_Temp)) +
  geom_point() +
  stat_smooth(method="lm", formula = y ~ x + I(x^2), color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=Flower_Rate-Standard_Error_fl, ymax=Flower_Rate+Standard_Error_fl), width=.2)

#elev
plot(ERNA_rates$Flower_Rate ~ ERNA_rates$elevation, pch = 19, ylim = c(0,1), xlab= "Elevation (ft)", ylab = "Flower Rate")
abline(v=5500, col="blue")
arrows(ERNA_rates$elevation, (ERNA_rates$Flower_Rate-ERNA_rates$Standard_Error_fl), ERNA_rates$elevation, (ERNA_rates$Flower_Rate+ERNA_rates$Standard_Error_fl), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(ERNA_rates$Flower_Rate ~ ERNA_rates$elevation))

#dist
plot(ERNA_rates$Flower_Rate ~ ERNA_rates$sld_km, pch = 19, ylim = c(0,1), xlab= "Distance (km)", ylab = "Flower Rate")
abline(v=0, col="blue")
arrows(ERNA_rates$sld_km, (ERNA_rates$Flower_Rate-ERNA_rates$Standard_Error_fl), ERNA_rates$sld_km, (ERNA_rates$Flower_Rate+ERNA_rates$Standard_Error_fl), length=0.05, angle = 90, code=3, lwd = .75)
ERNA_rates$sld_km2 <- ERNA_rates$sld_km^2
summary(lm(ERNA_rates$Flower_Rate ~ ERNA_rates$sld_km))
summary(lm(ERNA_rates$Flower_Rate ~ ERNA_rates$sld_km + ERNA_rates$sld_km2))
ggplot(data = ERNA_rates, aes(y=Survival_Rate, x=Mean_MinWinter_Temp)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=Flower_Rate-Standard_Error_fl, ymax=Flower_Rate+Standard_Error_fl), width=.2)



#add ht means and sd

ht_msd <- ERNA.crop %>%                            
  group_by(Population) %>%
  summarise(ht_mean = mean(length_cm_20220915,na.rm = TRUE),
            ht_sd = sd(length_cm_20220915,na.rm = TRUE),
            n=n(), 
            ht_se = ht_sd/sqrt(n)) %>% 
  as.data.frame(ht_msd)

ht_msd <- left_join(ht_msd, ERNA_precip, by = "Population" )
ht_msd <- left_join(ht_msd, ERNA_temp, by = "Population" )
ht_msd <- left_join(ht_msd, ERNA_elev, by = "Population" )
ht_msd <- left_join(ht_msd, ERNA_distance, by = "Population" )

ht_msd


#ppt
plot(ht_msd$ht_mean ~ ht_msd$Mean_Annual_Ppt, pch = 19, ylim = c(20,60),
     main = "ERNA population mean height by source annual precipitation", xlab= "Mean annual precipitation (mm)", ylab = "Height (cm)")
abline(v=443.6064, col="blue")
arrows(ht_msd$Mean_Annual_Ppt, (ht_msd$ht_mean-ht_msd$ht_se), ht_msd$Mean_Annual_Ppt, (ht_msd$ht_mean+ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(ht_msd$ht_mean ~ ht_msd$Mean_Annual_Ppt + ht_msd$Mean_Annual_Ppt2))
summary(lm(ht_msd$ht_mean ~ ht_msd$Mean_Annual_Ppt))

#temp
plot(ht_msd$ht_mean ~ ht_msd$Mean_MinWinter_Temp, pch = 19, ylim = c(20,60),
     main = "ERNA population mean height by source minimum winter temperature", xlab= "Mean minimum winter temperature (C)", ylab = "Height (cm)")
abline(v=-7.714918, col="blue")
arrows(ht_msd$Mean_MinWinter_Temp, (ht_msd$ht_mean-ht_msd$ht_se), ht_msd$Mean_MinWinter_Temp, (ht_msd$ht_mean+ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(ht_msd$ht_mean ~ ht_msd$Mean_MinWinter_Temp))
summary(lm(ht_msd$ht_mean ~ ht_msd$Mean_MinWinter_Temp + ht_msd$Mean_MinWinter_Temp2))

#elev
plot(ht_msd$ht_mean ~ ht_msd$elevation, pch = 19, ylim = c(20,60),
     main = "ERNA population mean height by source elevation", xlab= "Elevation (ft)", ylab = "Height (cm)")
abline(v=5500, col="blue")
arrows(ht_msd$elevation, (ht_msd$ht_mean-ht_msd$ht_se), ht_msd$elevation, (ht_msd$ht_mean+ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(ht_msd$ht_mean ~ ht_msd$elevation))
summary(lm(ht_msd$ht_mean ~ ht_msd$elevation + ht_msd$elev2))

lmer(log(length_cm_20220915) ~ elev_Z + elev_Z2 + (1|block), data = ERNA.crop)

summary(lmer(log(length_cm_20220915) ~ elev_Z + elev_Z2 + (1|block), data = ERNA.crop))
Anova(lmer(log(length_cm_20220915) ~ elev_Z + elev_Z2 + (1|block), data = ERNA.crop))
r.squaredGLMM
ggplot(data = ERNA.crop, aes(y=log(length_cm_20220915), x=elev)) +
  geom_point() +
  stat_smooth(method="lm", formula = y ~ x + I(x^2), color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=ht_mean-ht_se, ymax=ht_mean+ht_se), width=.2)

#dist
plot(ht_msd$ht_mean ~ ht_msd$sld_km, pch = 19, ylim = c(20,60),
     main = "ERNA population mean height by source distance from CG", xlab= "Distance (km)", ylab = "Height (cm)")
abline(v=0, col="blue")
arrows(ht_msd$sld_km, (ht_msd$ht_mean-ht_msd$ht_se), ht_msd$sld_km, (ht_msd$ht_mean+ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(ht_msd$ht_mean ~ ht_msd$sld_km))

#emmeans tests

ht_sz_lm <- lmer(log(length_cm_20220915) ~ seed_zone_ID + (1|block), data = ERNA.crop)
pair.ht.sz.lm <- emmeans(ht_sz_lm, specs = pairwise ~ seed_zone_ID)
plot(pair.ht.sz.lm, comparisons = TRUE, xlab = " EMM Height (logged)", ylab = "Seed Zone")
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

#emmeans plot
ht_pop_lm <- lmer(log(length_cm_20220915) ~ Pop_ID + (1|block), data = ERNA.crop)
pair.ht.pop <- emmeans(ht_pop_lm, specs = pairwise ~ Pop_ID)
plot(pair.ht.pop, comparisons = TRUE, xlab = " EMM Height (logged)", ylab = "Population")

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


flower_glm <- glm(flowering_Y_N_20221108 ~ Pop_ID, 
                    data = ERNA.crop, family = binomial (link ="logit"))
flower_glm_pair <- emmeans(flower_glm, specs = pairwise ~ Pop_ID)
plot(flower_glm_pair, comparisons = TRUE, xlab = " EMM Flowering rate", ylab = "Population")

flower.pred <- predict(flower_glm, ERNA.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
flower.pred
flower_mat <- matrix(data = flower.pred$fit, nrow = 1, ncol = 20)
flower_mat
flower_se <- matrix(data = flower.pred$se.fit, nrow = 1, ncol = 20)
flower_se

#survival proportion
survival.prop <- ERNA.crop %>% group_by(Population) %>% summarise(survival.prop=sum(survival_20221108, na.rm = TRUE)) 
survival.prop
ERNA.crop %>%
  group_by(Population) %>%
  tally()
survival_glm <- glm(survival_20221108 ~ Pop_ID, 
                    data = ERNA.crop, family = binomial (link ="logit"))
summary(survival_glm)
survival_pair <- emmeans(survival_glm, specs = pairwise ~ Pop_ID)
plot(survival_glm_pair, comparisons = TRUE, xlab = " EMM survival rate", ylab = "Population")
summary(survival_pair)
survival.pred <- predict(survival_glm, ERNA.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
survival.pred
survival_mat <- matrix(data = survival.pred$fit, nrow = 1, ncol = 20)
survival_mat
survival_se <- matrix(data = survival.pred$se.fit, nrow = 1, ncol = 20)
survival_se
barplot(survival_mat, ylim = c(0,1), xlab = "Population", ylab = "Survival Rate", main = "Survival Rate by Population")
survival_df <- as.data.frame(survival_mat)

#re-scale climate variables

ERNA.crop$Ppt_Annual_Z <- (ERNA.crop$Ppt_Annual - mean(ERNA.crop$Ppt_Annual)) / sd(ERNA.crop$Ppt_Annual)
ERNA.crop$min_wint_temp_Z <- (ERNA.crop$min_wint_temp - mean(ERNA.crop$min_wint_temp)) / sd(ERNA.crop$min_wint_temp)
ERNA.crop$elev_Z <- (ERNA.crop$elev - mean(ERNA.crop$elev)) / sd(ERNA.crop$elev)
ERNA.crop$dist_km_Z <- (ERNA.crop$dist_km - mean(ERNA.crop$dist_km)) / sd(ERNA.crop$dist_km)

#re-scale for chatfield difference
ERNA.crop$Ppt_Annual_chat <- (ERNA.crop$Ppt_Annual - 443.7064) / sd(ERNA.crop$Ppt_Annual)
ERNA.crop$min_wint_temp_chat <- (ERNA.crop$min_wint_temp - (-7.714918)) / sd(ERNA.crop$min_wint_temp)
ERNA.crop$elev_chat <- (ERNA.crop$elev - 5500) / sd(ERNA.crop$elev)
ERNA.crop$dist_km_chat <- (ERNA.crop$dist_km) / sd(ERNA.crop$dist_km)

#squared climate values
ERNA.crop$Ppt_Annual_Z2 <- ERNA.crop$Ppt_Annual_Z^2
ERNA.crop$min_wint_temp_Z2 <- ERNA.crop$min_wint_temp_Z^2
ERNA.crop$elev_Z2 <- ERNA.crop$elev_Z^2

#correlation between climate variables
plot(min_wint_temp ~ elev, data = ERNA.crop) #correlated?
plot(Ppt_Annual ~ min_wint_temp, data = ERNA.crop)
plot(elev ~ dist_km, data = ERNA.crop)

#AIC for height
ht.temp.precip.lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + Ppt_Annual_Z2 + min_wint_temp_Z + min_wint_temp_Z2 + (1|block), data = ERNA.crop)
ht.ppt_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_Z + Ppt_Annual_Z2 + (1|block), data = ERNA.crop)
ht.elev_lm <- lmer(log(length_cm_20220915) ~ elev_Z + elev_Z2 + (1|block), data = ERNA.crop)
ht.temp_lm <- lmer(log(length_cm_20220915) ~ min_wint_temp_Z + min_wint_temp_Z2 + (1|block), data = ERNA.crop)
ht.dist_lm <- lmer(log(length_cm_20220915) ~ dist_km_Z + (1|block), data = ERNA.crop)


models <- list(ht.temp.precip.lm, ht.ppt_lm, ht.elev_lm, ht.temp_lm, ht.dist_lm)
mod.names <- c('temp.precip','ppt', 'elev', 'temp', 'dist')
aictab(cand.set = models, modnames = mod.names)

plot(allEffects(ht.elev_lm), main = "Effect of Elevation on ERNA height", xlab = 
      "Elevation(adjusted)", ylab = "Height (cm) logged")

ht.tx <- lmer(log(length_cm_20220915) ~ Population + treatment + (1|block), data = ERNA.crop)
ht.pop <- lmer(log(length_cm_20220915) ~ Population +  (1|block), data = ERNA.crop)
models <- list(ht.tx, ht.pop)
mod.names <- c('Pop.tx', 'Pop')
aictab(cand.set = models, modnames = mod.names )

#AIC for survival
sv.temp.precip.lm <- glmer(survival_20221108 ~ Ppt_Annual_Z + Ppt_Annual_Z2 + min_wint_temp_Z + min_wint_temp_Z2 + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.ppt_lm <- glmer(survival_20221108 ~ Ppt_Annual_Z + Ppt_Annual_Z2 + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.elev_lm <- glmer(survival_20221108 ~ elev_Z + elev_Z2 + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.temp_lm <- glmer(survival_20221108 ~ min_wint_temp_Z + min_wint_temp_Z2 + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.dist_lm <- glmer(survival_20221108 ~ dist_km_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))


models <- list(sv.temp.precip.lm, sv.ppt_lm, sv.elev_lm, sv.temp_lm, sv.dist_lm)
mod.names <- c('temp.ppt', 'ppt', 'elev', 'temp', 'dist')
aictab(cand.set = models, modnames = mod.names )

sv.tx <- glmer(survival_20221108 ~ Population + treatment + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.pop <- glmer(survival_20221108 ~ Population +  (1|block), data = ERNA.crop,family = binomial (link ="logit"))
models <- list(sv.tx, sv.pop)
mod.names <- c('Pop.tx', 'Pop')
aictab(cand.set = models, modnames = mod.names )

#AIC flower rate 
fl.temp.precip.lm2 <- glmer(flowering_Y_N_20221108 ~ Ppt_Annual_Z + Ppt_Annual_Z2 + min_wint_temp_Z + min_wint_temp_Z2 + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
fl.ppt_lm2<- glmer(flowering_Y_N_20221108 ~ Ppt_Annual_Z + Ppt_Annual_Z2 + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
fl.elev_lm2 <- glmer(flowering_Y_N_20221108 ~ elev_Z + elev_Z2 + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
fl.temp_lm2 <- glmer(flowering_Y_N_20221108 ~ min_wint_temp_Z + min_wint_temp_Z2 + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
fl.dist_lm2 <- glmer(flowering_Y_N_20221108 ~ dist_km_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))

fl.temp.precip.lm <- glmer(flowering_Y_N_20221108 ~ Ppt_Annual_Z  + min_wint_temp_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
fl.ppt_lm <- glmer(flowering_Y_N_20221108 ~ Ppt_Annual_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
fl.elev_lm <- glmer(flowering_Y_N_20221108 ~ elev_Z +  (1|block), data = ERNA.crop, family = binomial (link ="logit"))
fl.temp_lm <- glmer(flowering_Y_N_20221108 ~ min_wint_temp_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
fl.dist_lm <- glmer(flowering_Y_N_20221108 ~ dist_km_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))


models <- list(fl.temp.precip.lm2, fl.ppt_lm2, fl.elev_lm2, fl.temp_lm2, fl.dist_lm2, fl.temp.precip.lm, fl.ppt_lm, fl.elev_lm, fl.temp_lm, fl.dist_lm)
mod.names <- c('temp.ppt2', 'ppt2', 'elev2', 'temp2', 'dist2','temp.ppt', 'ppt', 'elev', 'temp', 'dist')
aictab(cand.set = models, modnames = mod.names )

#AIC aim 2 chatfield difference
ht.ch.climate_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_chat + min_wint_temp_chat + elev_chat + dist_km_chat + (1|block), data = ERNA.crop)
ht.ch.temp.precip.lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_chat + min_wint_temp_chat + (1|block), data = ERNA.crop)
ht.ch.ppt_lm <- lmer(log(length_cm_20220915) ~ Ppt_Annual_chat + (1|block), data = ERNA.crop)
ht.ch.elev_lm <- lmer(log(length_cm_20220915) ~ elev_chat + (1|block), data = ERNA.crop)
ht.ch.temp_lm <- lmer(log(length_cm_20220915) ~ min_wint_temp_chat + (1|block), data = ERNA.crop)
ht.ch.dist_lm <- lmer(log(length_cm_20220915) ~ dist_km_chat + (1|block), data = ERNA.crop)
summary(ht.ch.ppt_lm )
ht.ch.dist.elev <- lmer(log(length_cm_20220915) ~ elev_chat + dist_km_chat + (1|block), data = ERNA.crop)

models <- list(ht.ch.climate_lm,ht.ch.temp.precip.lm,ht.ch.ppt_lm, ht.ch.elev_lm, ht.ch.temp_lm, ht.ch.dist_lm, ht.ch.dist.elev)
mod.names <- c('climate', 'temp.ppt', 'ppt', 'elev', 'temp', 'dist', 'dist.elev')
aictab(cand.set = models, modnames = mod.names )

#AIC aim 2 chatfield difference survival
sv.ch.climate_lm <- glmer(survival_20221108 ~ Ppt_Annual_Z + min_wint_temp_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.ch.temp.precip.lm <- glmer(survival_20221108 ~ Ppt_Annual_Z + min_wint_temp_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.ch.ppt_lm <- glmer(survival_20221108 ~ Ppt_Annual_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.ch.elev_lm <- glmer(survival_20221108 ~ elev_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.ch.temp_lm <- glmer(survival_20221108 ~ min_wint_temp_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.ch.dist_lm <- glmer(survival_20221108 ~ dist_km_Z + (1|block), data = ERNA.crop,family = binomial (link ="logit"))
sv.ch.dist.elev <- glmer(survival_20221108 ~ dist_km_Z + elev_chat + (1|block), data = ERNA.crop,family = binomial (link ="logit"))

models <- list(sv.ch.climate_lm,sv.ch.temp.precip.lm,sv.ch.ppt_lm, sv.ch.elev_lm, sv.ch.temp_lm, sv.ch.dist_lm, sv.ch.dist.elev)
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

plot(Ppt_Annual_Z ~ Ppt_Annual_chat, data = ERNA.crop)

plot(ht_msd$Population ~ ERNA.crop$dist_km)
mean(ERNA.crop$length_cm_20220915)
str(ERNA.crop$length_cm_20220915)

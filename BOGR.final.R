BOGR <- read.csv(file = 'BOGR_data.csv') 
BOGR.crop <- BOGR[1:1135,]

BOGR.pop.list <- unique(as.character(BOGR.crop$Population))
BOGR.pop.list.df <- as.data.frame(unique(BOGR.crop$Population))
colnames(BOGR.pop.list.df) <- "Population"
BOGR.pop.list

library(car)
library(lme4)
library(emmeans)
library(magrittr)
library(AICcmodavg)
library(stats)
library(gt)
library(htmltools)
library(dplyr)
library(tidyr)
library(effects)
library(plotrix)
library(ggplot2)

#exclude data subset (e.g. replaced plants)
BOGR.ex.rep <- BOGR.crop[BOGR.crop$replaced_Y_N !="Y",]

BOGR.wet <- BOGR.crop[BOGR.crop$Treatment !="dry" ,]

BOGR.dry <- BOGR.crop[BOGR.crop$Treatment != "wet" ,]

#add climate variables
BOGR_precip <- read.csv("20221129_BOGR_pptAnnual.csv", header = TRUE)
BOGR.crop[  , 'Ppt_Annual'] <- NA

for (dd in 1:length(BOGR.pop.list)) { 
  BOGR.crop$Ppt_Annual[grepl(BOGR.pop.list[dd], BOGR.crop$Population)] <- BOGR_precip$Mean_Annual_Ppt[dd] 
}

BOGR_elev <- read.csv("20230125_BOGR_elev.csv", header = TRUE)
BOGR.crop[ , 'elev'] <- NA


for (dd in 1:length(BOGR.pop.list)) { 
  BOGR.crop$elev[grepl(BOGR.pop.list[dd], BOGR.crop$Population)] <- BOGR_elev$elevation[dd] 
}

BOGR_temp <- read.csv("20230118_BOGR_tminWinter.csv", header = TRUE)

BOGR.crop[ , 'min_wint_temp'] <- NA

for (dd in 1:length(BOGR.pop.list)) { 
  BOGR.crop$min_wint_temp[grepl(BOGR.pop.list[dd], BOGR.crop$Population)] <- BOGR_temp$Mean_MinWinter_Temp[dd] 
}

BOGR_distance <- read.csv("BOGR_distance.csv")
BOGR.crop[ , 'dist_km'] <- NA
for (dd in 1:length(BOGR.pop.list)) { 
  BOGR.crop$dist_km[grepl(BOGR.pop.list[dd], BOGR.crop$Population)] <- BOGR_distance$sld_km [dd] 
}
unique(BOGR.crop$Population)
unique(BOGR.crop$dist_km)
unique(BOGR_distance$Population)

#emmeans comparisons
new_order_duf <- with(BOGR.crop, reorder(Pop_code, days_until_flowering, mean,  na.rm=T))
BOGR.days.flower.lm <- lmer(log(days_until_flowering) ~ new_order_duf + (1|Block), data=BOGR.crop)
BOGR.days.flower.pair <- emmeans(BOGR.days.flower.lm, specs = pairwise ~ new_order_duf)
plot(BOGR.days.flower.pair, comparisons = TRUE, xlab = " EMM Days until flowering (logged)", ylab = "Population")

new_order_duf_sz <- with(BOGR.crop, reorder(seed_zone, days_until_flowering, mean,  na.rm=T))
BOGR.days.flower.sz.lm <- lmer(log(days_until_flowering) ~ new_order_duf_sz + (1|Block), data=BOGR.crop)
BOGR.days.flower.sz.pair <- emmeans(BOGR.days.flower.sz.lm, specs = pairwise ~ new_order_duf_sz)
plot(BOGR.days.flower.sz.pair, comparisons = TRUE, xlab = " EMM Days until flowering (logged)", ylab = "Seed Zone")

new_order <- with(BOGR.crop, reorder(Pop_code, length_cm_20220801, mean,  na.rm=T))
BOGR.ht.pop.lm <- lmer(log(length_cm_20220801) ~ new_order + (1|Block), data = BOGR.crop)
BOGR.pair.ht.pop.lm <- emmeans(BOGR.ht.pop.lm, specs = pairwise ~ new_order)
plot(BOGR.pair.ht.pop.lm, comparisons = TRUE, xlab = " EMM Height (logged)", ylab = "Population")

new_order_sz <- with(BOGR.crop, reorder(seed_zone, length_cm_20220801, mean,  na.rm=T)) 
BOGR.ht.sz.lm <- lmer(log(length_cm_20220801) ~ new_order_sz + (1|Block), data = BOGR.crop)
BOGR.pair.ht.sz.lm <- emmeans(BOGR.ht.sz.lm, specs = pairwise ~ new_order_sz)
plot(BOGR.pair.ht.sz.lm, comparisons = TRUE, xlab = " EMM Height (logged)", ylab = "Seed Zone")

new_order_inf <- with(BOGR.crop, reorder(Pop_code, num_inf_20220927, mean,  na.rm=T))
BOGR_inf_glm <- glmer(num_inf_20220927 ~ new_order_inf + (1|Block), 
                       data = BOGR.crop, family = poisson (link ="log"))
BOGR_pair_inf <- emmeans(BOGR_inf_glm, specs = pairwise ~ new_order_inf)
plot(BOGR_pair_inf, comparisons = TRUE, xlab = " EMM Number of Inflourescences", ylab = "Population")
summary(BOGR_pair_inf)

new_order_inf_sz <- with(BOGR.crop, reorder(seed_zone, num_inf_20220927, mean,  na.rm=T))
BOGR_inf_sz_glm <- glmer( num_inf_20220927 ~ new_order_inf_sz + (1|Block), 
                          data = BOGR.crop, family = poisson (link ="log"))
BOGR_pair_sz_inf <- emmeans(BOGR_inf_sz_glm, specs = pairwise ~ new_order_inf_sz)
plot(BOGR_pair_sz_inf, comparisons = TRUE, xlab = " EMM Number of Inflourescences", ylab = "Seed Zone")
summary(BOGR_pair_sz_inf)

plot(num_inf_20220927 ~ length_cm_20220801, data = BOGR.crop, main = "BOGR number of inf x height")
abline(lm(num_inf_20220927 ~ length_cm_20220801, data = BOGR.crop))


#flowering proportion
BOGR.flower.prop <- BOGR.crop %>% group_by(Population) %>% summarise(BOGR.flowering.prop=sum(flowering_Y_N_20220927, na.rm = TRUE)) %>% print(n=21)

BOGR.crop %>%
  group_by(Population) %>%
  tally()%>% print(n=21)
unique(BOGR.crop$Population)

BOGR_flower_glm <- glm(flowering_Y_N_20220927 ~ Population, 
                  data = BOGR.crop, family = binomial (link ="logit"))
BOGR_flower_pair <- emmeans(BOGR_flower_glm, specs = pairwise ~ Population)
plot(BOGR_flower_pair, comparisons = TRUE, xlab = " EMM Flowering rate", ylab = "Population", xlim = c(-2,2))

BOGR.flower.pred <- predict(BOGR_flower_glm, BOGR.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
BOGR.flower.pred
BOGR_flower_mat <- matrix(data = BOGR.flower.pred$fit, nrow = 1, ncol = 21)
BOGR_flower_mat
BOGR_flower_se <- matrix(data = BOGR.flower.pred$se.fit, nrow = 1, ncol = 21)
BOGR_flower_se

#survival proportion
BOGR.survival.prop <- BOGR.crop %>% group_by(Population) %>% summarise(BOGR.survival.prop=sum(survival_20220927, na.rm = TRUE)) %>% print(n=21)
BOGR.survival.prop
BOGR.crop %>%
  group_by(Population) %>%
  tally()%>% print(n=21)
BOGR.survival.glm <- glm(survival_20220927 ~ Population, 
                    data = BOGR.crop, family = binomial (link ="logit"))
BOGR.survival.glm

BOGR.survival.pred <- predict(BOGR.survival.glm, BOGR.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
as.data.frame(BOGR.survival.pred)
BOGR_survival_mat <- matrix(data = survival.pred$fit, nrow = 1, ncol = 21)
BOGR_survival_mat
BOGR_survival_se <- matrix(data = survival.pred$se.fit, nrow = 1, ncol = 21)
BOGR_survival_se
barplot(survival_mat, ylim = c(0,1), xlab = "Population", ylab = "Survival Rate", main = "Survival Rate by Population")

unique(BOGR_elev$Population)

#survival prop and se
BOGR_rates <- read.csv("BOGR_rates.csv")

BOGR_rates <- left_join(BOGR_rates, BOGR_precip, by = "Population" )
BOGR_rates <- left_join(BOGR_rates, BOGR_temp, by = "Population" )
BOGR_rates <- left_join(BOGR_rates, BOGR_elev, by = "Population" )
BOGR_rates <- left_join(BOGR_rates, BOGR_distance, by = "Population" )

#ppt
plot(BOGR_rates$Survival_Rate ~ BOGR_rates$Mean_Annual_Ppt, pch = 19, ylim = c(.7,1), xlab= "Mean annual precipitation (mm)", ylab = "Survival Rate")
abline(v=443.7064, col="blue")
arrows(BOGR_rates$Mean_Annual_Ppt, (BOGR_rates$Survival_Rate-BOGR_rates$Standard_Error_sv), BOGR_rates$Mean_Annual_Ppt, (BOGR_rates$Survival_Rate+BOGR_rates$Standard_Error_sv), length=0.05, angle = 90, code=3, lwd = .75)
BOGR_rates$Mean_Annual_Ppt2 <- BOGR_rates$Mean_Annual_Ppt^2
summary(lm(BOGR_rates$Survival_Rate ~ BOGR_rates$Mean_Annual_Ppt + BOGR_rates$Mean_Annual_Ppt2))
summary(lm(BOGR_rates$Survival_Rate ~ BOGR_rates$Mean_Annual_Ppt))

ggplot(data = BOGR_rates, aes(y=Survival_Rate, x=Mean_Annual_Ppt)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=Survival_Rate-Standard_Error_sv, ymax=Survival_Rate+Standard_Error_sv), width=.2)

summary(lm(BOGR_rates$Survival_Rate ~ BOGR_rates$Mean_Annual_Ppt + BOGR_rates$Mean_Annual_Ppt2))
summary(lm(BOGR_rates$Survival_Rate ~ BOGR_rates$Mean_Annual_Ppt))

#temp
plot(BOGR_rates$Survival_Rate ~ BOGR_rates$Mean_MinWinter_Temp, pch = 19, ylim = c(.7,1), xlab= "Mean minimum winter temperature (C)", ylab = "Survival Rate")
arrows(BOGR_rates$Mean_MinWinter_Temp, (BOGR_rates$Survival_Rate-BOGR_rates$Standard_Error_sv), BOGR_rates$Mean_MinWinter_Temp, (BOGR_rates$Survival_Rate+BOGR_rates$Standard_Error_sv), length=0.05, angle = 90, code=3, lwd = .75)
abline(v=-7.714918, col="blue")

ggplot(data = BOGR_rates, aes(y=Survival_Rate, x=Mean_MinWinter_Temp)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=Survival_Rate-Standard_Error_sv, ymax=Survival_Rate+Standard_Error_sv), width=.2)

summary(lm(BOGR_rates$Survival_Rate ~ BOGR_rates$Mean_MinWinter_Temp + BOGR_rates$Mean_MinWinter_Temp2))
summary(lm(BOGR_rates$Survival_Rate ~ BOGR_rates$Mean_MinWinter_Temp))

#elev
plot(BOGR_rates$Survival_Rate ~ BOGR_rates$elevation, pch = 19, ylim = c(.7,1), xlab= "Elevation (ft)", ylab = "Survival Rate")
abline(v=5500, col="blue")
arrows(BOGR_rates$elevation, (BOGR_rates$Survival_Rate-BOGR_rates$Standard_Error_sv), BOGR_rates$elevation, (BOGR_rates$Survival_Rate+BOGR_rates$Standard_Error_sv), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_rates$Survival_Rate ~ BOGR_rates$elevation))

#dist
plot(BOGR_rates$Survival_Rate ~ BOGR_rates$sld_km, pch = 19, ylim = c(.7,1), xlab= "Distance (km)", ylab = "Survival Rate")
abline(v=0, col="blue")
arrows(BOGR_rates$sld_km, (BOGR_rates$Survival_Rate-BOGR_rates$Standard_Error_sv), BOGR_rates$sld_km, (BOGR_rates$Survival_Rate+BOGR_rates$Standard_Error_sv), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_rates$Survival_Rate ~ BOGR_rates$sld_km))

#flower rate msd plots
#ppt
plot(BOGR_rates$Flower_Rate ~ BOGR_rates$Mean_Annual_Ppt, pch = 19, ylim = c(.6,1), xlab= "Mean annual precipitation (mm)", ylab = "Flower Rate")
abline(v=443.7064, col="blue")
arrows(BOGR_rates$Mean_Annual_Ppt, (BOGR_rates$Flower_Rate-BOGR_rates$Standard_Error_fl), BOGR_rates$Mean_Annual_Ppt, (BOGR_rates$Flower_Rate+BOGR_rates$Standard_Error_fl), length=0.05, angle = 90, code=3, lwd = .75)
BOGR_rates$Mean_Annual_Ppt2 <- BOGR_rates$Mean_Annual_Ppt^2
summary(lm(BOGR_rates$Flower_Rate ~ BOGR_rates$Mean_Annual_Ppt + BOGR_rates$Mean_Annual_Ppt2))
summary(lm(BOGR_rates$Flower_Rate ~ BOGR_rates$Mean_Annual_Ppt))

#temp
plot(BOGR_rates$Flower_Rate ~ BOGR_rates$Mean_MinWinter_Temp, pch = 19, ylim = c(.6,1), xlab= "Mean minimum winter temperature (C)", ylab = "Flower Rate")
arrows(BOGR_rates$Mean_MinWinter_Temp, (BOGR_rates$Flower_Rate-BOGR_rates$Standard_Error_fl), BOGR_rates$Mean_MinWinter_Temp, (BOGR_rates$Flower_Rate+BOGR_rates$Standard_Error_fl), length=0.05, angle = 90, code=3, lwd = .75)
abline(v=-7.714918, col="blue")

ggplot(data = BOGR_rates, aes(y=Flower_Rate, x=Mean_MinWinter_Temp)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=Flower_Rate-Standard_Error_fl, ymax=Flower_Rate+Standard_Error_fl), width=.2)

#elev
plot(BOGR_rates$Flower_Rate ~ BOGR_rates$elevation, pch = 19, ylim = c(.6,1), xlab= "Elevation (ft)", ylab = "Flower Rate")
abline(v=5500, col="blue")
arrows(BOGR_rates$elevation, (BOGR_rates$Flower_Rate-BOGR_rates$Standard_Error_fl), BOGR_rates$elevation, (BOGR_rates$Flower_Rate+BOGR_rates$Standard_Error_fl), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_rates$Flower_Rate ~ BOGR_rates$elevation))

ggplot(data = BOGR_rates, aes(y=Flower_Rate, x=elevation)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=Flower_Rate-Standard_Error_fl, ymax=Flower_Rate+Standard_Error_fl), width=.2)
summary(lm(BOGR_rates$Flower_Rate ~ BOGR_rates$elevation))


#dist
plot(BOGR_rates$Flower_Rate ~ BOGR_rates$sld_km, pch = 19, ylim = c(.6,1), xlab= "Distance (km)", ylab = "Flower Rate")
abline(v=0, col="blue")
arrows(BOGR_rates$sld_km, (BOGR_rates$Flower_Rate-BOGR_rates$Standard_Error_fl), BOGR_rates$sld_km, (BOGR_rates$Flower_Rate+BOGR_rates$Standard_Error_fl), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_rates$Flower_Rate ~ BOGR_rates$sld_km))

#add ht msd
BOGR_ht_msd <- BOGR.crop %>%                            
  group_by(Population) %>%
  summarise(ht_mean = mean(length_cm_20220801,na.rm = TRUE),
            ht_sd = sd(length_cm_20220801,na.rm = TRUE),
            n=n(), 
            ht_se = ht_sd/sqrt(n)) %>% 
  as.data.frame(BOGR_ht_msd)


BOGR_ht_msd <- left_join(BOGR_ht_msd, BOGR_precip, by = "Population" )
BOGR_ht_msd <- left_join(BOGR_ht_msd, BOGR_temp, by = "Population" )
BOGR_ht_msd <- left_join(BOGR_ht_msd, BOGR_elev, by = "Population" )
BOGR_ht_msd <- left_join(BOGR_ht_msd, BOGR_distance, by = "Population" )

BOGR_ht_msd

#ppt
plot(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_Annual_Ppt, pch = 19, ylim = c(10,30), xlab= "Mean annual precipitation (mm)", ylab = "Height (cm)")
abline(v=443.7064, col="blue")
arrows(BOGR_ht_msd$Mean_Annual_Ppt, (BOGR_ht_msd$ht_mean-BOGR_ht_msd$ht_se), BOGR_ht_msd$Mean_Annual_Ppt, (BOGR_ht_msd$ht_mean+BOGR_ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
BOGR_ht_msd$Mean_Annual_Ppt2 <- BOGR_ht_msd$Mean_Annual_Ppt^2
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_Annual_Ppt + BOGR_ht_msd$Mean_Annual_Ppt2))
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_Annual_Ppt))

ggplot(data = BOGR_ht_msd, aes(y=ht_mean, x=Mean_Annual_Ppt)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=ht_mean-ht_se, ymax=ht_mean+ht_se), width=.2)

#temp
plot(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp, pch = 19, ylim = c(10,30), xlab= "Mean minimum winter temperature (C)", ylab = "Height (cm)")
arrows(BOGR_ht_msd$Mean_MinWinter_Temp, (BOGR_ht_msd$ht_mean-BOGR_ht_msd$ht_se), BOGR_ht_msd$Mean_MinWinter_Temp, (BOGR_ht_msd$ht_mean+BOGR_ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
abline(v=-7.714918, col="blue")
BOGR_ht_msd$Mean_MinWinter_Temp2 <- BOGR_ht_msd$Mean_MinWinter_Temp^2
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp + BOGR_ht_msd$Mean_MinWinter_Temp2))
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp))

ggplot(data = BOGR.crop, aes(y=length_cm_20220801, x=min_wint_temp)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") 
r.squaredGLMM(lmer(log(length_cm_20220801) ~ min_wint_temp_Z + (1|Block), data = BOGR.crop))                                                                  

BOGR_ht_msd$Mean_MinWinter_Temp2 <- BOGR_ht_msd$Mean_MinWinter_Temp^2
BOGR.ht.temp.msd.lm <- lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp)
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp))
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp + BOGR_ht_msd$Mean_MinWinter_Temp2))

#elev
plot(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$elevation, pch = 19, ylim = c(10,30), xlab= "Elevation (ft)", ylab = "Height (cm)")
abline(v=5500, col="blue")
arrows(BOGR_ht_msd$elevation, (BOGR_ht_msd$ht_mean-BOGR_ht_msd$ht_se), BOGR_ht_msd$elevation, (BOGR_ht_msd$ht_mean+BOGR_ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$elevation))

#dist
plot(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$sld_km, pch = 19, ylim = c(10,30), xlab= "Distance (km)", ylab = "Height (cm)")
abline(v=0, col="blue")
arrows(BOGR_ht_msd$sld_km, (BOGR_ht_msd$ht_mean-BOGR_ht_msd$ht_se), BOGR_ht_msd$sld_km, (BOGR_ht_msd$ht_mean+BOGR_ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$sld_km))

#add inf msd
BOGR_inf_msd <- BOGR.crop %>%                            
  group_by(Population) %>%
  summarise(inf_mean = mean(num_inf_20220927,na.rm = TRUE),
            inf_sd = sd(num_inf_20220927,na.rm = TRUE),
            n=n(), 
            inf_se = inf_sd/sqrt(n)) %>% 
  as.data.frame(BOGR_inf_msd)


BOGR_inf_msd <- left_join(BOGR_inf_msd, BOGR_precip, by = "Population" )
BOGR_inf_msd <- left_join(BOGR_inf_msd, BOGR_temp, by = "Population" )
BOGR_inf_msd <- left_join(BOGR_inf_msd, BOGR_elev, by = "Population" )
BOGR_inf_msd <- left_join(BOGR_inf_msd, BOGR_distance, by = "Population" )

BOGR_inf_msd

#ppt
plot(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_Annual_Ppt, pch = 19, ylim = c(0,100), xlab= "Mean annual precipitation (mm)", ylab = "number of inflorescences")
abline(v=443.7064, col="blue")
arrows(BOGR_inf_msd$Mean_Annual_Ppt, (BOGR_inf_msd$inf_mean-BOGR_inf_msd$inf_se), BOGR_inf_msd$Mean_Annual_Ppt, (BOGR_inf_msd$inf_mean+BOGR_inf_msd$inf_se), length=0.05, angle = 90, code=3, lwd = .75)
BOGR_inf_msd$Mean_Annual_Ppt2 <- BOGR_ht_msd$Mean_Annual_Ppt^2
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_Annual_Ppt + BOGR_inf_msd$Mean_Annual_Ppt2))
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_Annual_Ppt))

ggplot(data = BOGR_inf_msd, aes(y=inf_mean, x=Mean_Annual_Ppt)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=inf_mean-inf_se, ymax=inf_mean+inf_se), width=.2)

#temp
plot(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_MinWinter_Temp, pch = 19, ylim = c(0,100), xlab= "Mean minimum winter temperature (C)", ylab = "number of inflorescences")
abline(v=-7.714918, col="blue")
arrows(BOGR_inf_msd$Mean_MinWinter_Temp, (BOGR_inf_msd$inf_mean-BOGR_inf_msd$inf_se), BOGR_inf_msd$Mean_MinWinter_Temp, (BOGR_inf_msd$inf_mean+BOGR_inf_msd$inf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_MinWinter_Temp))
BOGR_inf_msd$Mean_MinWinter_Temp2 <- BOGR_ht_msd$Mean_MinWinter_Temp^2
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_MinWinter_Temp + BOGR_inf_msd$Mean_MinWinter_Temp2))


ggplot(data = BOGR_inf_msd, aes(y=inf_mean, x=Mean_Annual_Ppt)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=inf_mean-inf_se, ymax=inf_mean+inf_se), width=.2)

#elev
plot(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$elevation, pch = 19, ylim = c(0,100), xlab = "Elevation (ft)", ylab = "number of inflorescences")
abline(v=5500, col="blue")
arrows(BOGR_inf_msd$elevation, (BOGR_inf_msd$inf_mean-BOGR_inf_msd$inf_se), BOGR_inf_msd$elevation, (BOGR_inf_msd$inf_mean+BOGR_inf_msd$inf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$elevation))

#dist
plot(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$sld_km, pch = 19, ylim = c(0,100), xlab= "Distance (km)", ylab = "number of inflorescences")
abline(v=0, col="blue")
arrows(BOGR_inf_msd$sld_km, (BOGR_inf_msd$inf_mean-BOGR_inf_msd$inf_se), BOGR_inf_msd$sld_km, (BOGR_inf_msd$inf_mean+BOGR_inf_msd$inf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$sld_km))

#add number of days to flower msd
BOGR_duf_msd <- BOGR.crop %>%                            
  group_by(Population) %>%
  summarise(duf_mean = mean(days_until_flowering,na.rm = TRUE),
            duf_sd = sd(days_until_flowering,na.rm = TRUE),
            n=n(), 
            duf_se = duf_sd/sqrt(n)) %>% 
  as.data.frame(BOGR_duf_msd)


BOGR_duf_msd <- left_join(BOGR_duf_msd, BOGR_precip, by = "Population" )
BOGR_duf_msd <- left_join(BOGR_duf_msd, BOGR_temp, by = "Population" )
BOGR_duf_msd <- left_join(BOGR_duf_msd, BOGR_elev, by = "Population" )
BOGR_duf_msd <- left_join(BOGR_duf_msd, BOGR_distance, by = "Population" )

BOGR_duf_msd

#ppt
plot(BOGR_duf_msd$duf_mean ~ BOGR_duf_msd$Mean_Annual_Ppt, pch = 19, ylim = c(50,125), xlab= "Mean annual precipitation (mm)", ylab = "Days until flowering")
abline(v=443.7064, col="blue")
arrows(BOGR_duf_msd$Mean_Annual_Ppt, (BOGR_duf_msd$duf_mean-BOGR_duf_msd$duf_se), BOGR_duf_msd$Mean_Annual_Ppt, (BOGR_duf_msd$duf_mean+BOGR_duf_msd$duf_se), length=0.05, angle = 90, code=3, lwd = .75)

summary(lm(BOGR_duf_msd$duf_mean ~ BOGR_duf_msd$Mean_Annual_Ppt))

#temp
plot(BOGR_duf_msd$duf_mean ~ BOGR_duf_msd$Mean_MinWinter_Temp, pch = 19, ylim = c(50,125), xlab= "Mean minimum winter temperature (C)", ylab = "Days until flowering")
abline(v=-7.714918, col="blue")
arrows(BOGR_duf_msd$Mean_MinWinter_Temp, (BOGR_duf_msd$duf_mean-BOGR_duf_msd$duf_se), BOGR_duf_msd$Mean_MinWinter_Temp, (BOGR_duf_msd$duf_mean+BOGR_duf_msd$duf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_duf_msd$duf_mean ~ BOGR_duf_msd$Mean_MinWinter_Temp))

ggplot(data = BOGR_duf_msd, aes(y=duf_mean, x=Mean_MinWinter_Temp)) +
  geom_point() +
  geom_smooth(method="lm", color="blue", fill = "blue") +
  geom_errorbar(aes(ymin=duf_mean-duf_se, ymax=duf_mean+duf_se), width=.2)

#elev
plot(BOGR_duf_msd$duf_mean ~ BOGR_duf_msd$elevation, pch = 19, ylim = c(50,125), xlab = "Elevation (ft)", ylab = "Days until flowering")
abline(v=5500, col="blue")
arrows(BOGR_duf_msd$elevation, (BOGR_duf_msd$duf_mean-BOGR_duf_msd$duf_se), BOGR_duf_msd$elevation, (BOGR_duf_msd$duf_mean+BOGR_duf_msd$duf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_duf_msd$duf_mean ~ BOGR_duf_msd$elevation))

#dist
plot(BOGR_duf_msd$duf_mean ~ BOGR_duf_msd$sld_km, pch = 19, ylim = c(50,125), xlab= "Distance (km)", ylab = "Days until flowering")
abline(v=0, col="blue")
arrows(BOGR_duf_msd$sld_km, (BOGR_duf_msd$duf_mean-BOGR_duf_msd$duf_se), BOGR_duf_msd$sld_km, (BOGR_duf_msd$duf_mean+BOGR_duf_msd$duf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_duf_msd$duf_mean ~ BOGR_duf_msd$sld_km))


#rescale climate variables
BOGR.crop$Ppt_Annual_Z <- (BOGR.crop$Ppt_Annual - mean(BOGR.crop$Ppt_Annual)) / sd(BOGR.crop$Ppt_Annual)
BOGR.crop$min_wint_temp_Z <- (BOGR.crop$min_wint_temp - mean(BOGR.crop$min_wint_temp)) / sd(BOGR.crop$min_wint_temp)
BOGR.crop$elev_Z <- (BOGR.crop$elev - mean(BOGR.crop$elev)) / sd(BOGR.crop$elev)
BOGR.crop$dist_km_Z <- (BOGR.crop$dist_km - mean(BOGR.crop$dist_km)) / sd(BOGR.crop$dist_km)

#re-scale for chatfield difference
BOGR.crop$Ppt_Annual_chat <- (BOGR.crop$Ppt_Annual - 443.7064) / sd(BOGR.crop$Ppt_Annual)
BOGR.crop$min_wint_temp_chat <- (BOGR.crop$min_wint_temp - (-7.714918)) / sd(BOGR.crop$min_wint_temp)
BOGR.crop$elev_chat <- (BOGR.crop$elev - 5500) / sd(BOGR.crop$elev)
BOGR.crop$dist_km_chat <- (BOGR.crop$dist_km - .0034) / sd(BOGR.crop$dist_km)

#squared climate variables
BOGR.crop$Ppt_Annual_Z2 <- BOGR.crop$Ppt_Annual_Z^2
BOGR.crop$min_wint_temp_Z2 <- BOGR.crop$min_wint_temp_Z^2
BOGR.crop$elev_Z2 <- BOGR.crop$elev_Z^2

#predict var df

BOGR.ppt <- as.data.frame(unique(BOGR.crop$Ppt_Annual_Z)) 
BOGR.temp <- as.data.frame(unique(BOGR.crop$min_wint_temp_Z))
BOGR.temp.precip <- cbind(BOGR.ppt, BOGR.temp)
colnames(BOGR.temp.precip)
names(BOGR.temp.precip)[names(BOGR.temp.precip) == "unique(BOGR.crop$Ppt_Annual_Z)"] <- "Ppt_Annual_Z"
names(BOGR.temp.precip)[names(BOGR.temp.precip) == "unique(BOGR.crop$min_wint_temp_Z)"] <- "min_wint_temp_Z"
BOGR.temp.precip$Block <- unique(BOGR.crop$Block)

#AIC survival
BOGR.sv.temp.precip.lm <- glmer(survival_20220927 ~ Ppt_Annual_Z+ min_wint_temp_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_sv_ppt_lm <- glmer(survival_20220927 ~ Ppt_Annual_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_sv_temp_lm <- glmer(survival_20220927 ~ min_wint_temp_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_sv_elev_lm <- glmer(survival_20220927 ~ elev_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_sv_dist_lm <- glmer(survival_20220927 ~ dist_km_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))

models <- list( BOGR.sv.temp.precip.lm, BOGR_sv_ppt_lm, BOGR_sv_temp_lm, BOGR_sv_elev_lm, BOGR_sv_dist_lm)
mod.names <- c( 'temp.ppt',  'ppt','temp', 'elev', 'dist')
aictab(cand.set = models, modnames = mod.names )

r.squaredGLMM(BOGR_sv_temp_lm)

BOGR_sv_temp_lm <- glmer(survival_20220927 ~ min_wint_temp_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))

BOGR.sv.temp.effect <- effects::effect(term= "min_wint_temp_Z", mod= BOGR_sv_temp_lm)
BOGR.sv.temp.effect.df <- as.data.frame(BOGR.sv.temp.effect)

BOGR_sv_plot <- ggplot() + 
  geom_point(data=subset(BOGR.crop), aes(min_wint_temp_Z,survival_20220927)) + 
  geom_line(data=BOGR.sv.temp.effect.df, aes(x=min_wint_temp_Z, y=fit), color="blue") +
  geom_ribbon(data=BOGR.sv.temp.effect.df, aes(x=min_wint_temp_Z, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Mean Minimum Winter Temperature", y="Survival")

#AIC models ~ ht
BOGR.temp.precip.lm2 <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z + Ppt_Annual_Z2 + min_wint_temp_Z + min_wint_temp_Z2 + (1|Block), data = BOGR.crop)
BOGR_ppt_lm2 <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z + Ppt_Annual_Z2 + (1|Block), data = BOGR.crop)
BOGR_temp_lm2 <- lmer(log(length_cm_20220801) ~ min_wint_temp_Z + min_wint_temp_Z2 + (1|Block), data = BOGR.crop)
BOGR_elev_lm2 <- lmer(log(length_cm_20220801) ~ elev_Z + elev_Z2 + (1|Block), data = BOGR.crop)
BOGR_dist_lm2 <- lmer(log(length_cm_20220801) ~ dist_km_Z + (1|Block), data = BOGR.crop)

BOGR.temp.precip.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z  + min_wint_temp_Z  + (1|Block), data = BOGR.crop)
BOGR_ppt_lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z +  (1|Block), data = BOGR.crop)
BOGR_temp_lm <- lmer(log(length_cm_20220801) ~ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_elev_lm <- lmer(log(length_cm_20220801) ~ elev_Z + (1|Block), data = BOGR.crop)
BOGR_dist_lm <- lmer(log(length_cm_20220801) ~ dist_km_Z + (1|Block), data = BOGR.crop)

models <- list( BOGR.temp.precip.lm2, BOGR_ppt_lm2, BOGR_temp_lm2, BOGR_elev_lm2, BOGR_dist_lm2, BOGR.temp.precip.lm, BOGR_ppt_lm, BOGR_temp_lm, BOGR_elev_lm, BOGR_dist_lm)
mod.names <- c('temp.ppt2', 'ppt','temp2', 'elev2', 'dist2', 'temp.ppt', 'ppt','temp', 'elev', 'dist')
aic.sum <- aictab(cand.set = models, modnames = mod.names )
aic.sum

r.squaredGLMM(BOGR.height)

BOGR.height <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z  + min_wint_temp_Z + (1|Block) , data = BOGR.crop)


BOGR.ht.ppt.effect <- effects::effect(term= "Ppt_Annual_Z", mod= BOGR.height, default.levels=15)
BOGR.ht.ppt.effect.df <- as.data.frame(BOGR.ht.ppt.effect)
BOGR.ht.temp.effect <- effects::effect(term= "min_wint_temp_Z", mod= BOGR.height)
BOGR.ht.temp.effect.df <- as.data.frame(BOGR.ht.temp.effect)

BOGR_ht_plot <- ggplot() + 
  geom_point(data=subset(BOGR.crop), aes(Ppt_Annual_Z, log(length_cm_20220801))) + 
  geom_line(data=BOGR.ht.ppt.effect.df, aes(x=Ppt_Annual_Z, y=fit), color="blue") +
  geom_ribbon(data=BOGR.ht.ppt.effect.df, aes(x=Ppt_Annual_Z, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Mean Annual Precipitation", y="Height (logged)")

BOGR_ht_plot2 <- ggplot() + 
  geom_point(data=subset(BOGR.crop), aes(min_wint_temp_Z, log(length_cm_20220801))) + 
  geom_line(data=BOGR.ht.temp.effect.df, aes(x=min_wint_temp_Z, y=fit), color="blue") +
  geom_ribbon(data=BOGR.ht.temp.effect.df, aes(x=min_wint_temp_Z, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Mean Minimum Winter Temperature", y="Height (logged)")

#AIC models ~inf
BOGR.inf.temp.precip.lm <- glmer( num_inf_20220927 ~ Ppt_Annual_Z + Ppt_Annual_Z2 + min_wint_temp_Z + min_wint_temp_Z2 + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.temp.lm <- glmer( num_inf_20220927 ~ min_wint_temp_Z + min_wint_temp_Z2 + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.ppt.lm <- glmer( num_inf_20220927 ~ Ppt_Annual_Z + Ppt_Annual_Z2 + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.elev.lm <- glmer( num_inf_20220927~ elev_Z + elev_Z2 + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.dist.lm <- glmer( num_inf_20220927~ dist_km_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))

AIC(BOGR.inf.temp.precip.lm)
AIC(BOGR.inf.temp.lm) 
AIC(BOGR.inf.ppt.lm) 
AIC(BOGR.inf.elev.lm) 
AIC(BOGR.inf.dist.lm) 

r.squaredGLMM(BOGR.inf.temp.precip.lm)

BOGR.inf.temp.precip.lm <- glmer( num_inf_20220927 ~ Ppt_Annual_Z + Ppt_Annual_Z2 + min_wint_temp_Z + min_wint_temp_Z2 + (1|Block), data = BOGR.crop, family = poisson (link ="log"))

BOGR.inf.ppt.effect <- effects::effect(term= "Ppt_Annual_Z", mod= BOGR.inf.temp.precip.lm, default.levels=15)
BOGR.inf.ppt.effect.df <- as.data.frame(BOGR.inf.ppt.effect)
BOGR.inf.temp.effect <- effects::effect(term= "min_wint_temp_Z", mod= BOGR.inf.temp.precip.lm)
BOGR.inf.temp.effect.df <- as.data.frame(BOGR.inf.temp.effect)

BOGR_inf_plot <- ggplot() + 
  geom_point(data=subset(BOGR.crop), aes(min_wint_temp_Z,num_inf_20220927)) + 
  geom_line(data=BOGR.inf.temp.effect.df, aes(x=min_wint_temp_Z, y=fit), color="blue") +
  geom_ribbon(data=BOGR.inf.temp.effect.df, aes(x=min_wint_temp_Z, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Mean Minimum Winter Temperature", y="Number of Inflourescences")

BOGR_inf_plot2 <- ggplot() + 
  geom_point(data=subset(BOGR.crop), aes(Ppt_Annual_Z,num_inf_20220927)) + 
  geom_line(data=BOGR.inf.ppt.effect.df, aes(x=Ppt_Annual_Z, y=fit), color="blue") +
  geom_ribbon(data=BOGR.inf.ppt.effect.df, aes(x=Ppt_Annual_Z, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Mean Annual Precipitation", y="Number of Inflourescences")

#AIC models ~ days until flowering
BOGR.duf.temp.precip.lm <- lmer(log(days_until_flowering) ~ Ppt_Annual_Z+ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_duf_ppt_lm <- lmer(log(days_until_flowering) ~ Ppt_Annual_Z + (1|Block), data = BOGR.crop)
BOGR_duf_temp_lm <- lmer(log(days_until_flowering) ~ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_duf_elev_lm <- lmer(log(days_until_flowering) ~ elev_Z + (1|Block), data = BOGR.crop)
BOGR_duf_dist_lm <- lmer(log(days_until_flowering) ~ dist_km_Z + (1|Block), data = BOGR.crop)

models <- list( BOGR.duf.temp.precip.lm, BOGR_duf_ppt_lm, BOGR_duf_temp_lm, BOGR_duf_elev_lm, BOGR_duf_dist_lm)
mod.names <- c('temp.ppt', 'ppt','temp', 'elev', 'dist')
aictab(cand.set = models, modnames = mod.names )

r.squaredGLMM(BOGR.duf.temp.precip.lm)

BOGR.duf.temp.precip.lm <- lmer(log(days_until_flowering) ~ Ppt_Annual_Z+ min_wint_temp_Z + (1|Block), data = BOGR.crop)

BOGR.duf.ppt.effect <- effects::effect(term= "Ppt_Annual_Z", mod= BOGR.duf.temp.precip.lm, default.levels=15)
BOGR.duf.ppt.effect.df <- as.data.frame(BOGR.duf.ppt.effect)
BOGR.duf.temp.effect <- effects::effect(term= "min_wint_temp_Z", mod= BOGR.duf.temp.precip.lm)
BOGR.duf.temp.effect.df <- as.data.frame(BOGR.duf.temp.effect)

BOGR_duf_plot <- ggplot() + 
  geom_point(data=subset(BOGR.crop), aes(min_wint_temp_Z,log(days_until_flowering))) + 
  geom_line(data=BOGR.duf.temp.effect.df, aes(x=min_wint_temp_Z, y=fit), color="blue") +
  geom_ribbon(data=BOGR.duf.temp.effect.df, aes(x=min_wint_temp_Z, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Mean Minimum Winter Temperature", y="Days Until Flower (logged)")

BOGR_duf_plot2 <- ggplot() + 
  geom_point(data=subset(BOGR.crop), aes(Ppt_Annual_Z,log(days_until_flowering))) + 
  geom_line(data=BOGR.duf.ppt.effect.df, aes(x=Ppt_Annual_Z, y=fit), color="blue") +
  geom_ribbon(data=BOGR.duf.ppt.effect.df, aes(x=Ppt_Annual_Z, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Mean Annual Precipitation", y="Days Until Flower (logged)")

#AIC flower rate
BOGR.fl.temp.precip.lm <- glmer(flowering_Y_N_20220927 ~ Ppt_Annual_Z+ min_wint_temp_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_fl_ppt_lm <- glmer(flowering_Y_N_20220927 ~ Ppt_Annual_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_fl_temp_lm <- glmer(flowering_Y_N_20220927 ~ min_wint_temp_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_fl_elev_lm <- glmer(flowering_Y_N_20220927 ~ elev_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_fl_dist_lm <- glmer(flowering_Y_N_20220927 ~ dist_km_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))

models <- list( BOGR.fl.temp.precip.lm, BOGR_fl_ppt_lm, BOGR_fl_temp_lm, BOGR_fl_elev_lm, BOGR_fl_dist_lm)
mod.names <- c( 'temp.ppt',  'ppt','temp', 'elev', 'dist')
aictab(cand.set = models, modnames = mod.names )

BOGR_fl_elev_lm <- glmer(flowering_Y_N_20220927 ~ elev_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))

r.squaredGLMM(BOGR_fl_elev_lm)

BOGR.fl.elev.effect <- effects::effect(term= "elev_Z", mod= BOGR_fl_elev_lm)
BOGR.fl.elev.effect.df <- as.data.frame(BOGR.fl.elev.effect)

BOGR_fl_plot <- ggplot() + 
  geom_point(data=subset(BOGR.crop), aes(elev_Z,flowering_Y_N_20220927)) + 
  geom_line(data=BOGR.fl.elev.effect.df, aes(x=elev_Z, y=fit), color="blue") +
  geom_ribbon(data=BOGR.fl.elev.effect.df, aes(x=elev_Z, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Elevation (ft)", y="Flowering")

library(dplyr)
ggplot(data = BOGR.crop, aes(y=length_cm_20220801, x= min_wint_temp)) +
  geom_point() +
  geom_smooth(method="lm", se = TRUE, color ="blue", fill = "blue")

ggplot(data = BOGR.crop, aes(y=length_cm_20220801, x= min_wint_temp)) +
  geom_point() +
  geom_smooth(method="lm",formula = y ~ poly(x, 2), se = TRUE, color ="blue", fill = "blue")
  
BOGR_ppt_lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual + (1|Block), data = BOGR.crop)
r.squaredGLMM(BOGR_ppt_lm)
library(MuMIn)

#height

ggplot(data = BOGR.crop, aes(y=length_cm_20220801, x= Ppt_Annual)) +
  geom_point() +
  geom_smooth(method="glm",formula = y ~ poly(x, 2), se = TRUE, color ="blue", fill = "blue")

#duf
BOGR_duf_temp_lm <- lmer(log(days_until_flowering) ~ min_wint_temp_Z  + (1|Block), data = BOGR.crop)
r.squaredGLMM(BOGR_duf_temp_lm)

ggplot(data = BOGR.crop, aes(y=days_until_flowering, x= min_wint_temp)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ poly(x, 2), se = TRUE, color ="blue", fill = "blue")

#survival
BOGR_sv_temp_lm <- glmer(survival_20220927 ~ min_wint_temp_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
r.squaredGLMM(BOGR_sv_temp_lm)

ggplot(data = BOGR.crop, aes(y=survival_20220927, x= min_wint_temp)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ x, se = TRUE, color ="blue", fill = "blue")

#num inf
BOGR.inf.precip.lm <- glmer( num_inf_20220927 ~ Ppt_Annual_Z + Ppt_Annual_Z2  + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
r.squaredGLMM(BOGR.inf.precip.lm)

ggplot(data = BOGR.crop, aes(y=num_inf_20220927, x= Ppt_Annual)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ poly(x, 2), se = TRUE, color ="blue", fill = "blue")

#treatment AIC
BOGR_fl_pop_lm <- glmer(flowering_Y_N_20220927 ~ Population + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_fl_tx_lm <- glmer(flowering_Y_N_20220927 ~ Population + Treatment + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))

models <- list(BOGR_fl_pop_lm, BOGR_fl_tx_lm )
mod.names <- c( 'pop', 'tx')
aictab(cand.set = models, modnames = mod.names )

BOGR_inf_pop_lm <- glmer( num_inf_20220927 ~  Population  + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR_inf_tx_lm <- glmer( num_inf_20220927 ~  Population + Treatment + (1|Block), data = BOGR.crop, family = poisson (link ="log"))

models <- list(BOGR_duf_pop_lm, BOGR_duf_tx_lm )
mod.names <- c( 'pop', 'tx')
aictab(cand.set = models, modnames = mod.names )

BOGR_sv_pop_lm <- glmer(survival_20220927 ~ Population + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_sv_tx_lm <- glmer(survival_20220927 ~ Population + Treatment + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))

models <- list(BOGR_sv_pop_lm, BOGR_sv_tx_lm )
mod.names <- c( 'pop', 'tx')
aictab(cand.set = models, modnames = mod.names )

BOGR_ht_pop_lm <- lmer(log(length_cm_20220801) ~ Population + (1|Block), data = BOGR.crop)
BOGR_ht_tx_lm <- lmer(log(length_cm_20220801) ~ Population + Treatment + (1|Block), data = BOGR.crop)

models <- list(BOGR_ht_pop_lm, BOGR_ht_tx_lm )
mod.names <- c( 'pop', 'tx')
aictab(cand.set = models, modnames = mod.names )





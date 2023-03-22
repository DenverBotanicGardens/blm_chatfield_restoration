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
BOGR.days.flower.lm <- lmer(log(days_until_flowering) ~ Pop_code + (1|Block), data=BOGR.crop)
BOGR.days.flower.pair <- emmeans(BOGR.days.flower.lm, specs = pairwise ~ Pop_code)
plot(BOGR.days.flower.pair, comparisons = TRUE, xlab = " EMM Days until flowering", ylab = "Population")

BOGR.days.flower.sz.lm <- lmer(log(days_until_flowering) ~ seed_zone_code + (1|Block), data=BOGR.crop)
BOGR.days.flower.sz.pair <- emmeans(BOGR.days.flower.sz.lm, specs = pairwise ~ seed_zone_code)
plot(BOGR.days.flower.sz.pair, comparisons = TRUE, xlab = " EMM Days until flowering", ylab = "Seed Zone")

BOGR.ht.pop.lm <- lmer(log(length_cm_20220801) ~ Population + (1|Block), data = BOGR.crop)
BOGR.pair.ht.pop.lm <- emmeans(BOGR.ht.pop.lm, specs = pairwise ~ Population)
plot(BOGR.pair.ht.pop.lm, comparisons = TRUE, xlab = " EMM Height (logged)", ylab = "Population")

summary(BOGR.pair.ht.pop.lm)
str(BOGR.crop$Pop_code)

BOGR.ht.sz.lm <- lmer(log(length_cm_20220801) ~ seed_zone_code + (1|Block), data = BOGR.crop)
BOGR.pair.ht.sz.lm <- emmeans(BOGR.ht.sz.lm, specs = pairwise ~ seed_zone_code)
plot(BOGR.pair.ht.sz.lm, comparisons = TRUE, xlab = " EMM Height (logged)", ylab = "Seed Zone")
summary(BOGR.pair.ht.sz.lm)

BOGR.inf.days.pop.lm

BOGR_inf_glm <- glmer( num_inf_20220927 ~ Pop_code + (1|Block), 
                       data = BOGR.crop, family = poisson (link ="log"))
BOGR_pair_inf <- emmeans(BOGR_inf_glm, specs = pairwise ~ Pop_code)
plot(BOGR_pair_inf, comparisons = TRUE, xlab = " EMM Number of Inflourescences", ylab = "Population")
summary(BOGR_pair_inf)

BOGR_inf_sz_glm <- glmer( num_inf_20220927 ~ seed_zone + (1|Block), 
                          data = BOGR.crop, family = poisson (link ="log"))
BOGR_pair_sz_inf <- emmeans(BOGR_inf_sz_glm, specs = pairwise ~ seed_zone)
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
plot(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_Annual_Ppt, pch = 19, ylim = c(10,30),
     main = "BOGR population mean height by source annual precipitation", xlab= "Mean annual precipitation (mm)", ylab = "Height (cm)")
abline(v=443.7064, col="red")
arrows(BOGR_ht_msd$Mean_Annual_Ppt, (BOGR_ht_msd$ht_mean-BOGR_ht_msd$ht_se), BOGR_ht_msd$Mean_Annual_Ppt, (BOGR_ht_msd$ht_mean+BOGR_ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
BOGR_ht_msd$Mean_Annual_Ppt2 <- BOGR_ht_msd$Mean_Annual_Ppt^2
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_Annual_Ppt + BOGR_ht_msd$Mean_Annual_Ppt2))
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_Annual_Ppt))

#temp
plot(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp, pch = 19, ylim = c(10,30),
     main = "B. gracilis height by source minimum winter temperature", xlab= "Mean minimum winter temperature (C)", ylab = "Height (cm)")
lines(BOGR_ht_msd$ht_mean[order(BOGR_ht_msd$ht_mean)], predict(BOGR.ht.temp.msd.lm, interval = "confidence")[, "lwr"][order(BOGR_ht_msd$ht_mean)]);  
lines(BOGR_ht_msd$ht_mean[order(BOGR_ht_msd$ht_mean)], predict(BOGR.ht.temp.msd.lm, interval = "confidence")[, "upr"][order(BOGR_ht_msd$ht_mean)])
arrows(BOGR_ht_msd$Mean_MinWinter_Temp, (BOGR_ht_msd$ht_mean-BOGR_ht_msd$ht_se), BOGR_ht_msd$Mean_MinWinter_Temp, (BOGR_ht_msd$ht_mean+BOGR_ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)

ggplot(data = BOGR_ht_msd, aes(y=ht_mean, x=Mean_MinWinter_Temp)) +
  geom_point() +
  geom_smooth(method="lm", color="red", fill = "blue") +
  geom_errorbar(aes(ymin=ht_mean-ht_se, ymax=ht_mean+ht_se), width=.2)
                                                                  

BOGR_ht_msd$Mean_MinWinter_Temp2 <- BOGR_ht_msd$Mean_MinWinter_Temp^2
BOGR.ht.temp.msd.lm <- lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp)
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp))
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$Mean_MinWinter_Temp + BOGR_ht_msd$Mean_MinWinter_Temp2))

summary(BOGR.crop$days_until_flowering)
 
abline(v=-7.714918, col="red" + BOGR_ht_msd, ht_mean ~ Mean_MinWinter_Temp)
#elev
plot(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$elevation, pch = 19, ylim = c(10,30),
     main = "BOGR population mean height by source elevation", xlab= "Elevation (ft)", ylab = "Height (cm)")
abline(v=5500, col="blue")
arrows(BOGR_ht_msd$elevation, (BOGR_ht_msd$ht_mean-BOGR_ht_msd$ht_se), BOGR_ht_msd$elevation, (BOGR_ht_msd$ht_mean+BOGR_ht_msd$ht_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$elevation))

#dist
plot(BOGR_ht_msd$ht_mean ~ BOGR_ht_msd$sld_km, pch = 19, ylim = c(10,30),
     main = "BOGR population mean height by source distance from CG", xlab= "Distance (km)", ylab = "Height (cm)")
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
plot(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_Annual_Ppt, pch = 19, ylim = c(0,100),
     main = "BOGR number of inflorescences by annual precipitation", xlab= "Mean annual precipitation (mm)", ylab = "number of inflorescences")
abline(v=443.7064, col="blue")
arrows(BOGR_inf_msd$Mean_Annual_Ppt, (BOGR_inf_msd$inf_mean-BOGR_inf_msd$inf_se), BOGR_inf_msd$Mean_Annual_Ppt, (BOGR_inf_msd$inf_mean+BOGR_inf_msd$inf_se), length=0.05, angle = 90, code=3, lwd = .75)

summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_Annual_Ppt))

#temp
plot(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_MinWinter_Temp, pch = 19, ylim = c(0,100),
     main = "BOGR number of inflorescences by minimum winter temperature", xlab= "Mean minimum winter temperature (C)", ylab = "number of inflorescences")
abline(v=-7.714918, col="blue")
arrows(BOGR_inf_msd$Mean_MinWinter_Temp, (BOGR_inf_msd$inf_mean-BOGR_inf_msd$inf_se), BOGR_inf_msd$Mean_MinWinter_Temp, (BOGR_inf_msd$inf_mean+BOGR_inf_msd$inf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$Mean_MinWinter_Temp))

#elev
plot(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$elevation, pch = 19, ylim = c(0,100),
     main = "BOGR number of inflorescences by elevation", xlab= "Elevation (ft)", ylab = "number of inflorescences")
abline(v=5500, col="blue")
arrows(BOGR_inf_msd$elevation, (BOGR_inf_msd$inf_mean-BOGR_inf_msd$inf_se), BOGR_inf_msd$elevation, (BOGR_inf_msd$inf_mean+BOGR_inf_msd$inf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$elevation))

#dist
plot(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$sld_km, pch = 19, ylim = c(0,100),
     main = "BOGR number of inflorescences by distance from CG", xlab= "Distance (km)", ylab = "number of inflorescences")
abline(v=0, col="blue")
arrows(BOGR_inf_msd$sld_km, (BOGR_inf_msd$inf_mean-BOGR_inf_msd$inf_se), BOGR_inf_msd$sld_km, (BOGR_inf_msd$inf_mean+BOGR_inf_msd$inf_se), length=0.05, angle = 90, code=3, lwd = .75)
summary(lm(BOGR_inf_msd$inf_mean ~ BOGR_inf_msd$sld_km))

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

#correlation between climate variables
plot(min_wint_temp ~ elev, data = BOGR.crop) #correlated?
plot(Ppt_Annual ~ min_wint_temp, data = BOGR.crop)
plot(elev ~ dist_km, data = BOGR.crop)

#AIC survival - warning "boundary (singular) fit: see help('isSingular')"
BOGR.climate.lm <- glmer(survival_20220927 ~ Ppt_Annual_Z + Ppt_Annual_Z2 + min_wint_temp_Z + min_wint_temp_Z2 + elev_Z + dist_km_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR.temp.precip.lm <- glmer(survival_20220927 ~ Ppt_Annual_Z+ min_wint_temp_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_ppt_lm <- glmer(survival_20220927 ~ Ppt_Annual_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_temp_lm <- glmer(survival_20220927 ~ min_wint_temp_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_elev_lm <- glmer(survival_20220927 ~ elev_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))
BOGR_dist_lm <- glmer(survival_20220927 ~ dist_km_Z + (1|Block), data = BOGR.crop,family = binomial (link ="logit"))


#AIC models ~ ht
BOGR.temp.precip.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z + Ppt_Annual_Z2 + min_wint_temp_Z + min_wint_temp_Z2 + (1|Block), data = BOGR.crop)
BOGR_ppt_lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z + Ppt_Annual_Z2 + (1|Block), data = BOGR.crop)
BOGR_temp_lm <- lmer(log(length_cm_20220801) ~ min_wint_temp_Z + min_wint_temp_Z2 + (1|Block), data = BOGR.crop)
BOGR_elev_lm <- lmer(log(length_cm_20220801) ~ elev_Z + elev_Z2 + (1|Block), data = BOGR.crop)
BOGR_dist_lm <- lmer(log(length_cm_20220801) ~ dist_km_Z + (1|Block), data = BOGR.crop)


models <- list( BOGR.temp.precip.lm, BOGR_ppt_lm, BOGR_temp_lm, BOGR_elev_lm, BOGR_dist_lm)
mod.names <- c('temp.ppt', 'ppt','temp', 'elev', 'dist')
aic.sum <- aictab(cand.set = models, modnames = mod.names )
aic.sum


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

#AIC models ~ days until flowering
BOGR.duf.temp.precip.lm <- glmer.nb(days_until_flowering ~ Ppt_Annual_Z+ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_duf_ppt_lm <- glmer.nb(days_until_flowering ~ Ppt_Annual_Z + (1|Block), data = BOGR.crop)
BOGR_duf_temp_lm <- glmer.nb(days_until_flowering ~ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_duf_elev_lm <- glmer.nb(days_until_flowering ~ elev_Z + (1|Block), data = BOGR.crop)
BOGR_duf_dist_lm <- glmer.nb(days_until_flowering ~ dist_km_Z + (1|Block), data = BOGR.crop)

models <- list(BOGR.duf.climate.lm, BOGR.duf.temp.precip.lm, BOGR.duf.ppt.elev.lm, BOGR.duf.temp.elev.lm, BOGR_duf_ppt_lm, BOGR_duf_temp_lm, BOGR_duf_elev_lm, BOGR_dist_lm,
               BOGR.dist.ppt, BOGR.dist.elev, BOGR.dist.temp, BOGR.dist.ppt.elev)
mod.names <- c('climate', 'temp.ppt', 'ppt.elev', 'temp.elev', 'ppt','temp', 'elev', 'dist', 'dist.ppt', 
               'dist.elev', 'dist.temp', 'dist.ppt.elev')
aictab(cand.set = models, modnames = mod.names )

#AIC chatfield dif height
BOGR.climate_lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_chat + min_wint_temp_chat + elev_chat + dist_km_chat + (1|Block), data = BOGR.crop)
BOGR.temp.precip.lm <- lmer(log(length_cm_20220801)  ~ Ppt_Annual_chat + min_wint_temp_chat + (1|Block), data = BOGR.crop)
BOGR.ppt_lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_chat + (1|Block), data = BOGR.crop)
BOGR.elev_lm <- lmer(log(length_cm_20220801) ~ elev_chat + (1|Block), data = BOGR.crop)
BOGR.temp_lm <- lmer(log(length_cm_20220801) ~ min_wint_temp_chat + (1|Block), data = BOGR.crop)
BOGR.dist_lm <- lmer(log(length_cm_20220801) ~ dist_km_chat + (1|Block), data = BOGR.crop)
BOGR.dist.elev <- lmer(log(length_cm_20220801) ~ elev_chat + dist_km_chat + (1|Block), data = BOGR.crop)

models <- list(BOGR.climate_lm, BOGR.temp.precip.lm ,BOGR.ppt_lm, BOGR.elev_lm, BOGR.temp_lm, BOGR.dist_lm,BOGR.dist.elev)
mod.names <- c('climate', 'temp.ppt', 'ppt', 'elev', 'temp', 'dist', 'dist.elev')
aictab(cand.set = models, modnames = mod.names )

#AIC chatfield dif num inf
BOGR.climate_lm <- glmer(num_inf_20220927 ~ Ppt_Annual_chat + min_wint_temp_chat + elev_chat + dist_km_chat + (1|Block), data = BOGR.crop,family = poisson (link ="log"))
BOGR.temp.precip.lm <- glmer(num_inf_20220927 ~ Ppt_Annual_chat + min_wint_temp_chat + (1|Block), data = BOGR.crop,family = poisson (link ="log"))
BOGR.ppt_lm <- glmer( num_inf_20220927 ~ Ppt_Annual_chat + (1|Block), data = BOGR.crop,family = poisson (link ="log"))
BOGR.elev_lm <- glmer(num_inf_20220927 ~ elev_chat + (1|Block), data = BOGR.crop,family = poisson (link ="log"))
BOGR.temp_lm <- glmer(num_inf_20220927 ~ min_wint_temp_chat + (1|Block), data = BOGR.crop,family = poisson (link ="log"))
BOGR.dist_lm <- glmer(num_inf_20220927 ~ dist_km_chat + (1|Block), data = BOGR.crop,family = poisson (link ="log"))
BOGR.dist.elev <- glmer(num_inf_20220927 ~ elev_chat + dist_km_chat + (1|Block), data = BOGR.crop,family = poisson (link ="log"))

AIC(BOGR.climate_lm)
AIC(BOGR.temp.precip.lm) 
AIC(BOGR.ppt_lm)
AIC(BOGR.elev_lm)
AIC(BOGR.temp_lm)
AIC(BOGR.dist_lm)
AIC(BOGR.dist.elev)

plot(BOGR.crop$length_cm_20220915 ~ BOGR.crop$elev_Z)

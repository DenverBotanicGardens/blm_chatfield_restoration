BOGR <- read.csv(file = 'BOGR_data.csv') 
BOGR.crop <- BOGR[1:1135,]

BOGR.pop.list <- unique(as.character(BOGR.crop$Population))
BOGR.pop.list.df <- as.data.frame(unique(BOGR.crop$Population))
colnames(BOGR.pop.list.df) <- "Population"

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


#emmeans comparisons
BOGR.days.flower.lm <- lmer(days_until_flowering ~ Population + (1|Block), data=BOGR.crop)
BOGR.days.flower.pair <- emmeans(BOGR.days.flower.lm, specs = pairwise ~ Population)
BOGR.days.flower.pair

BOGR.ht.pop.lm <- lmer(log(length_cm_20220801) ~ Population + (1|Block), data = BOGR.crop)
BOGR.pair.ht.pop.lm <- emmeans(BOGR.ht.pop.lm, specs = pairwise ~ Population)
summary(BOGR.pair.ht.pop.lm)

BOGR.ht.sz.lm <- lmer(log(length_cm_20220801) ~ seed_zone + (1|Block), data = BOGR.crop)
BOGR.pair.ht.sz.lm <- emmeans(BOGR.ht.sz.lm, specs = pairwise ~ seed_zone)
summary(BOGR.pair.ht.sz.lm)

BOGR.inf.days.pop.lm

BOGR_inf_glm <- glmer( num_inf_20220927 ~ Population + (1|Block), 
                       data = BOGR.crop, family = poisson (link ="log"))
BOGR_pair_inf <- emmeans(BOGR_inf_glm, specs = pairwise ~ Population)
summary(BOGR_pair_inf)

BOGR_inf_sz_glm <- glmer( num_inf_20220927 ~ seed_zone + (1|Block), 
                          data = BOGR.crop, family = poisson (link ="log"))
BOGR_pair_sz_inf <- emmeans(BOGR_inf_sz_glm, specs = pairwise ~ seed_zone)
summary(BOGR_pair_sz_inf)

plot(num_inf_20220927 ~ length_cm_20220801, data = BOGR.crop, main = "BOGR number of inf x height")
abline(lm(num_inf_20220927 ~ length_cm_20220801, data = BOGR.crop))

#emmeans table

unique(BOGR.crop$Population)

#flowering proportion
BOGR.flower.prop <- BOGR.crop %>% group_by(Population) %>% summarise(BOGR.flowering.prop=sum(flowering_Y_N_20220801, na.rm = TRUE)) %>% print(n=21)

BOGR.crop %>%
  group_by(Population) %>%
  tally()%>% print(n=21)
unique(BOGR.crop$Population)

BOGR_flower_glm <- glm(flowering_Y_N_20220801 ~ Population, 
                  data = BOGR.crop, family = binomial (link ="logit"))
BOGR.flower.pred <- predict(BOGR_flower_glm, BOGR.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
BOGR.flower.pred
BOGR_flower_mat <- matrix(data = BOGR.flower.pred$fit, nrow = 1, ncol = 20)
BOGR_flower_mat

#survival proportion
BOGR.survival.prop <- BOGR.crop %>% group_by(Population) %>% summarise(BOGR.survival.prop=sum(survival_20220801, na.rm = TRUE)) %>% print(n=21)
BOGR.survival.prop
BOGR.crop %>%
  group_by(Population) %>%
  tally()%>% print(n=21)
BOGR.survival.glm <- glm(survival_20221108 ~ Population, 
                    data = ERNA, family = binomial (link ="logit"))

BOGR.survival.pred <- predict(BOGR.survival.glm, BOGR.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
BOGR.survival.pred
BOGR_survival_mat <- matrix(data = survival.pred$fit, nrow = 1, ncol = 20)
BOGR_survival_mat
barplot(survival_mat, ylim = c(0,1), xlab = "Population", ylab = "Survival Rate", main = "Survival Rate by Population")


#rescale climate variables

BOGR.crop$Ppt_Annual_Z <- (BOGR.crop$Ppt_Annual - mean(BOGR.crop$Ppt_Annual)) / sd(BOGR.crop$Ppt_Annual)
BOGR.crop$min_wint_temp_Z <- (BOGR.crop$min_wint_temp - mean(BOGR.crop$min_wint_temp)) / sd(BOGR.crop$min_wint_temp)
BOGR.crop$elev_Z <- (BOGR.crop$elev - mean(BOGR.crop$elev)) / sd(BOGR.crop$elev)
BOGR.crop$dist_km_Z <- (BOGR.crop$dist_km - mean(BOGR.crop$dist_km)) / sd(BOGR.crop$dist_km)

#AIC models ~ ht
BOGR.climate.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z+ min_wint_temp_Z + elev_Z + dist_km_Z + (1|Block), data = BOGR.crop)
BOGR.temp.precip.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z+ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR.ppt.elev.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z+ elev_Z + (1|Block), data = BOGR.crop)
BOGR.temp.elev.lm <- lmer(log(length_cm_20220801) ~ elev_Z + min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_ppt_lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z + (1|Block), data = BOGR.crop)
BOGR_temp_lm <- lmer(log(length_cm_20220801) ~ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_elev_lm <- lmer(log(length_cm_20220801) ~ elev_Z + (1|Block), data = BOGR.crop)
BOGR_dist_lm <- lmer(log(length_cm_20220801) ~ dist_km_Z + (1|Block), data = BOGR.crop)
BOGR.dist.ppt <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z+  dist_km_Z + (1|Block), data = BOGR.crop)
BOGR.dist.elev <- lmer(log(length_cm_20220801) ~ elev_Z + dist_km_Z + (1|Block), data = BOGR.crop)
BOGR.dist.temp <- lmer(log(length_cm_20220801) ~ dist_km_Z + min_wint_temp_Z + (1|Block), data= BOGR.crop)
BOGR.dist.ppt.elev <- lmer(log(length_cm_20220801) ~ dist_km_Z+ Ppt_Annual_Z + elev_Z + (1|Block), data=BOGR.crop)


models <- list(BOGR.climate.lm, BOGR.temp.precip.lm, BOGR.ppt.elev.lm, BOGR.temp.elev.lm, BOGR_ppt_lm, BOGR_temp_lm, BOGR_elev_lm, BOGR_dist_lm,
               BOGR.dist.ppt, BOGR.dist.elev, BOGR.dist.temp, BOGR.dist.ppt.elev)
mod.names <- c('climate', 'temp.ppt', 'ppt.elev', 'temp.elev', 'ppt','temp', 'elev', 'dist', 'dist.ppt', 
'dist.elev', 'dist.temp', 'dist.ppt.elev')
aictab(cand.set = models, modnames = mod.names )



#AIC models ~inf
BOGR.inf.climate.lm <- glmer( num_inf_20220927 ~ Ppt_Annual_Z + min_wint_temp_Z + elev_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.temp.precip.lm <- glmer( num_inf_20220927 ~ Ppt_Annual_Z + min_wint_temp_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.ppt.elev.lm <- glmer( num_inf_20220927 ~ Ppt_Annual_Z +  elev_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.temp.elev.lm <- glmer( num_inf_20220927 ~  min_wint_temp_Z + elev_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.temp.lm <- glmer( num_inf_20220927 ~ min_wint_temp_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.ppt.lm <- glmer( num_inf_20220927 ~ Ppt_Annual_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.elev.lm <- glmer( num_inf_20220927~ elev_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.dist.lm <- glmer( num_inf_20220927~ dist_km_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.dist.ppt <- glmer( num_inf_20220927 ~ Ppt_Annual_Z +  dist_km_Z + (1|Block), data = BOGR.crop, family = poisson(link = 'log'))
BOGR.inf.dist.elev <- glmer( num_inf_20220927 ~ elev_Z + dist_km_Z + (1|Block), data = BOGR.crop, family = poisson (link ="log"))
BOGR.inf.dist.temp <- glmer( num_inf_20220927 ~ dist_km_Z + min_wint_temp_Z + (1|Block), data= BOGR.crop, family = poisson (link ="log"))
BOGR.inf.dist.ppt.elev <- glmer( num_inf_20220927 ~ dist_km_Z+ Ppt_Annual_Z + elev_Z + (1|Block), data=BOGR.crop, family = poisson (link ="log"))

AIC(BOGR.inf.climate.lm)
AIC(BOGR.inf.temp.precip.lm)
AIC(BOGR.inf.ppt.elev.lm) 
AIC(BOGR.inf.temp.elev.lm) 
AIC(BOGR.inf.temp.lm) 
AIC(BOGR.inf.ppt.lm) 
AIC(BOGR.inf.elev.lm) 
AIC(BOGR.inf.dist.lm) 
AIC(BOGR.inf.dist.ppt) 
AIC(BOGR.inf.dist.elev) 
AIC(BOGR.inf.dist.temp) 
AIC(BOGR.inf.dist.ppt.elev) 

hist(BOGR.crop$days_until_flowering)

#AIC models ~ days until flowering
BOGR.duf.climate.lm <- lmer(days_until_flowering ~ Ppt_Annual_Z+ min_wint_temp_Z + elev_Z + dist_km_Z + (1|Block), data = BOGR.crop)
BOGR.duf.temp.precip.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z+ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR.duf.ppt.elev.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z+ elev_Z + (1|Block), data = BOGR.crop)
BOGR.duf.temp.elev.lm <- lmer(log(length_cm_20220801) ~ elev_Z + min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_duf_ppt_lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z + (1|Block), data = BOGR.crop)
BOGR_duf_temp_lm <- lmer(log(length_cm_20220801) ~ min_wint_temp_Z + (1|Block), data = BOGR.crop)
BOGR_duf_elev_lm <- lmer(log(length_cm_20220801) ~ elev_Z + (1|Block), data = BOGR.crop)
BOGR_duf_dist_lm <- lmer(log(length_cm_20220801) ~ dist_km_Z + (1|Block), data = BOGR.crop)
BOGR.duf.dist.ppt <- lmer(log(length_cm_20220801) ~ Ppt_Annual_Z+  dist_km_Z + (1|Block), data = BOGR.crop)
BOGR.duf.dist.elev <- lmer(log(length_cm_20220801) ~ elev_Z + dist_km_Z + (1|Block), data = BOGR.crop)
BOGR.duf.dist.temp <- lmer(log(length_cm_20220801) ~ dist_km_Z + min_wint_temp_Z + (1|Block), data= BOGR.crop)
BOGR.duf.dist.ppt.elev <- lmer(log(length_cm_20220801) ~ dist_km_Z+ Ppt_Annual_Z + elev_Z + (1|Block), data=BOGR.crop)
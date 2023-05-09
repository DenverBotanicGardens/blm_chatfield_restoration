BOGR <- read.csv(file = 'BOGR_data.csv') 

BOGR.crop <- BOGR[1:1135,]


#exclude data subset (e.g. replaced plants)
BOGR.ex.rep <- BOGR.crop[BOGR.crop$replaced_Y_N !="Y",]

BOGR.wet <- BOGR.crop[BOGR.crop$Treatment !="dry" ,]

BOGR.dry <- BOGR.crop[BOGR.crop$Treatment != "wet" ,]

boxplot(BOGR.wet$length_cm_20220801 ~ BOGR.wet$Population,main = "B.gracilis Height (Wet)", 
        ylab = "days until flower", xlab= "",col = "blue", cex.axis=0.4, las=2)
boxplot(BOGR.dry$length_cm_20220801 ~ BOGR.dry$Population,main = "B.gracilis Height (Dry)", 
        ylab = "days until flower", xlab= "",col = "darkorange", cex.axis=0.4, las=2)
summary(aov(length_cm_20220801 ~ Population + Treatment, data = BOGR.crop))
summary(aov(num_inf_20220927 ~ Population + Treatment, data = BOGR.crop))
summary(aov(days_until_flowering ~ Population + Treatment, data = BOGR.crop))



#Aim 1 variation
BOGR_ht_pop_anova <- aov(length_cm_20220801 ~ Population, data = BOGR.crop)
summary(height_pop_anova)

plot(BOGR_ht_pop_anova)

BOGR.ht.pop.lm <- lmer(log(length_cm_20220801) ~ Population + (1|Block), data = BOGR.crop)
BOGR.pair.ht.pop.lm <- emmeans(BOGR.ht.pop.lm, specs = pairwise ~ Population)
summary(BOGR.pair.ht.pop.lm)

BOGR.ht.sz.lm <- lmer(log(length_cm_20220801) ~ seed_zone + (1|Block), data = BOGR.crop)
BOGR.pair.ht.sz.lm <- emmeans(BOGR.ht.sz.lm, specs = pairwise ~ seed_zone)
summary(BOGR.pair.ht.sz.lm)

BOGR_inf_glm <- glmer( num_inf_20220927 ~ Population + (1|Block), 
                       data = BOGR.crop, family = poisson (link ="log"))
BOGR_pair_inf <- emmeans(BOGR_inf_glm, specs = pairwise ~ Population)
summary(BOGR_pair_inf)

BOGR_inf_sz_glm <- glmer( num_inf_20220927 ~ seed_zone + (1|Block), 
                       data = BOGR.crop, family = poisson (link ="log"))
BOGR_pair_sz_inf <- emmeans(BOGR_inf_sz_glm, specs = pairwise ~ seed_zone)
summary(BOGR_pair_sz_inf)



BOGR_ht_sz_anova <- aov(length_cm_20220801 ~ seed_zone, data = BOGR.crop)
summary(BOGR_ht_sz_anova)

BOGR_num_inf_pop_an <- aov( num_inf_20220927 ~ Population, data = BOGR.crop)
summary(BOGR_num_inf_pop_an)

BOGR_num_inf_sz_an <- aov( num_inf_20220927 ~ seed_zone, data = BOGR.crop)
summary(BOGR_num_inf_sz_an)

days_mort_pop_an <- aov( days_until_mortality ~ Population, data=BOGR.crop)
summary(days_mort_pop_an)

days_mort_sz_an <- aov( days_until_mortality ~ seed_zone, data=BOGR.crop)
summary(days_mort_pop_an)

new_order_duf <- with(BOGR.crop, reorder(Pop_code, days_until_flowering, median,  na.rm=T))
boxplot(days_until_flowering ~ new_order_duf, data = BOGR.crop, main = "B.gracilis flowering time x Population", 
        xlab = "Population", ylab = "days until flower",col = "cadetblue1", cex.axis=0.6, las=2)

new_order_sz_duf <- with(BOGR.crop, reorder(seed_zone, days_until_flowering, median,  na.rm=T))
boxplot(days_until_flowering ~ new_order_sz_duf, data = BOGR.crop, main = "B.gracilis flowering time x Seed Zone", 
         ylab = "days until flower", xlab= "",col = "chartreuse", cex.axis=0.4, las=2)

new_order <- with(BOGR.crop, reorder(Population, length_cm_20220801, median,  na.rm=T))
boxplot(length_cm_20220801 ~ new_order,data=BOGR.crop, main="B. gracilis Height x Population", 
        xlab="Population", ylab="Height(cm)",col = "cadetblue1", cex.axis=0.6, las=2)

new_order_sz <- with(BOGR.crop, reorder(seed_zone, length_cm_20220801, median,  na.rm=T)) 
boxplot(length_cm_20220801 ~ new_order_sz,data=BOGR, main="B. gracilis Height x Seed Zone", 
        xlab="", ylab="Height(cm)",col = "chartreuse", cex.axis=0.4, las=2)

new_order_inf <- with(BOGR.crop, reorder(Pop_code, num_inf_20220927, median,  na.rm=T))
boxplot(num_inf_20220927 ~ new_order_inf, data = BOGR.crop, main = "B. gracilis Inflorescenses x Population", xlab = "", 
        ylab = "Number of Inflorescenses",col = "cadetblue1", cex.axis=0.4, las=2 )

new_order_inf_sz <- with(BOGR.crop, reorder(seed_zone, num_inf_20220927, median,  na.rm=T))
boxplot(num_inf_20220927 ~ new_order_inf_sz, data = BOGR.crop, main = "B. gracilis Inflorescenses x Seed Zone", xlab = "", 
        ylab = "Number of Inflorescenses",col = "chartreuse", cex.axis=0.4, las=2 )
hist(BOGR.crop$num_inf_20220927)
unique(BOGR.crop$seed_zone)

#infxseedzone
compare_means(num_inf_20220927 ~ seed_zone, data = BOGR, method = "anova")
boxplot(num_inf_20220927 ~ seed_zone_code, data = BOGR, 
        main = "BOGR Number of Inflorescenses by Seed Zone", xlab = "Seed Zone", 
        ylab = "Num of Inflorescenses",cex.axis=0.6, las=2 ) 

boxplot(days_until_mortality ~ Population, data=BOGR, main = "BOGR Days until mortality by Population", 
        xlab = "Population",cex.axis=0.25, las=2, ylab = "Days until mortality")

boxplot(days_until_mortality ~ seed_zone, data=BOGR, main = "BOGR Days until mortality by Seed Zone", 
        xlab = "Seed Zone", ylab = "Days until mortality",cex.axis=0.25, las=2)


BOGR_inf_glm
summary(BOGR_inf_glm)
plot(BOGR_inf_glm)

hist(BOGR$num_inf_20220927)

colnames(BOGR.crop)

BOGR_survival_glm <- glm(survival_20220927 ~ Population, 
                    data = BOGR.crop, family = binomial (link ="logit"))

summary(survival_glm)
str(BOGR)


BOGR.pop.list.df <- as.data.frame(unique(BOGR.crop$Population))
colnames(BOGR.pop.list.df) <- "Population"

survival.pred <- predict(BOGR_survival_glm, BOGR.pop.list.df, se.fit = TRUE, type = "response", interval = "confidence" )
survival.pred
survival_mat <- matrix(data = survival.pred$fit, nrow = 1, ncol = 21)
barplot(survival_mat, ylim = c(0,1), xlab = "Population", ylab = "Survival Rate", main = "Survival Rate by Population")



boxplot(length_cm_20220801 ~ Population,data=BOGR.wet, main="BOGR Plant Height by Population - Wet", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)

boxplot(length_cm_20220801 ~ Population,data=BOGR.dry, main="BOGR Plant Height by Population - Dry", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)

boxplot(num_inf_20220927 ~ Population, data = BOGR.wet, 
        main = "BOGR Number of Inflorescenses by Population - Wet", xlab = "Population", 
        ylab = "Num of Inflorescenses",cex.axis=0.25, las=2 )

boxplot(num_inf_20220927 ~ Population, data = BOGR.dry, 
        main = "BOGR Number of Inflorescenses by Population - Dry", xlab = "Population", 
        ylab = "Num of Inflorescenses",cex.axis=0.25, las=2 )

BOGR_ht_pop_anova_tx <- aov(length_cm_20220801 ~ Population*Treatment, data = BOGR.crop)
summary(BOGR_ht_pop_anova_tx)

BOGR_inf_pop_anova_tx <- aov(num_inf_20220927 ~ Population*Treatment, data = BOGR.crop)
summary(BOGR_inf_pop_anova_tx)
        
#add precip
        
BOGR_precip <- read.csv("20221129_BOGR_pptAnnual.csv", header = TRUE)
pop.list
unique(BOGR.crop$Population)

BOGR.pop.list <- unique(as.character(BOGR.crop$Population))
        
BOGR.crop[  , 'Ppt_Annual'] <- NA
        
for (dd in 1:length(BOGR.pop.list)) { 
  BOGR.crop$Ppt_Annual[grepl(BOGR.pop.list[dd], BOGR.crop$Population)] <- BOGR_precip$Mean_Annual_Ppt[dd] 
        }
BOGR.crop$Ppt_Annual

head(BOGR.crop)

BOGR_ppt_lm <- lmer(length_cm_20220801 ~ Ppt_Annual + (1|Block), data = BOGR.crop)
plot(BOGR_ppt_lm)

BOGR_ppt_inf_lm <- lmer(num_inf_20220927 ~ Ppt_Annual + (1|Block), data = BOGR.crop)
plot(BOGR_ppt_inf_lm)


boxplot(length_cm_20220801 ~ Ppt_Annual, data = BOGR.crop)
boxplot(num_inf_20220927 ~ Ppt_Annual, data = BOGR.crop)


#add elevation

BOGR_elev <- read.csv("20230125_BOGR_elev.csv", header = TRUE)
BOGR.crop[ , 'elev'] <- NA

for (dd in 1:length(BOGR.pop.list)) { 
  BOGR.crop$elev[grepl(BOGR.pop.list[dd], BOGR.crop$Population)] <- BOGR_elev$elevation[dd] 
}

BOGR.crop$elev

BOGR_elev_lm <- lmer(length_cm_20220801 ~ elev + (1|Block), data = BOGR.crop)
plot(BOGR_elev_lm)

plot(length_cm_20220801 ~ elev, data = BOGR.crop)
plot(num_inf_20220927 ~ elev, data = BOGR.crop)

#add min winter temp

BOGR_temp <- read.csv("20230118_BOGR_tminWinter.csv", header = TRUE)

BOGR.crop[ , 'min_wint_temp'] <- NA

for (dd in 1:length(BOGR.pop.list)) { 
  BOGR.crop$min_wint_temp[grepl(BOGR.pop.list[dd], BOGR.crop$Population)] <- BOGR_temp$Mean_MinWinter_Temp[dd] 
}
BOGR.crop$min_wint_temp

BOGR_temp_lm <- lmer(length_cm_20220801 ~ min_wint_temp + (1|Block), data = BOGR.crop)
plot(BOGR_temp_lm)

plot(length_cm_20220801 ~ min_wint_temp, data = BOGR.crop)
plot(num_inf_20220927 ~ min_wint_temp, data = BOGR.crop)

#add geog distance

#AIC models ~ ht
BOGR.climate.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual+ min_wint_temp + elev + (1|Block), data = BOGR.crop)
BOGR.temp.precip.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual+ min_wint_temp + (1|Block), data = BOGR.crop)
BOGR.ppt.elev.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual+ elev + (1|Block), data = BOGR.crop)
BOGR.temp.elev.lm <- lmer(log(length_cm_20220801) ~ elev + min_wint_temp + (1|Block), data = BOGR.crop)
BOGR_ppt_lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual + (1|Block), data = BOGR.crop)
BOGR_temp_lm <- lmer(log(length_cm_20220801) ~ min_wint_temp + (1|Block), data = BOGR.crop)
BOGR_elev_lm <- lmer(log(length_cm_20220801) ~ elev + (1|Block), data = BOGR.crop)


models <- list(BOGR.climate.lm, BOGR.temp.precip.lm, BOGR.ppt.elev.lm, BOGR.temp.elev.lm, BOGR_ppt_lm, BOGR_temp_lm, BOGR_elev_lm)
mod.names <- c('climate', 'temp.ppt', 'ppt.elev', 'temp.elev', 'ppt','temp', 'elev')
aictab(cand.set = models, modnames = mod.names )

#AIC models ~inf
BOGR.inf.climate.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual+ min_wint_temp + elev + (1|Block), data = BOGR.crop)
BOGR.inf.temp.precip.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual+ min_wint_temp + (1|Block), data = BOGR.crop)
BOGR.inf.ppt.elev.lm <- lmer(log(length_cm_20220801) ~ Ppt_Annual+ elev + (1|Block), data = BOGR.crop)
BOGR.inf.temp.elev.lm <- lmer(log(length_cm_20220801) ~ elev + min_wint_temp + (1|Block), data = BOGR.crop)



models <- list(BOGR.climate.lm, BOGR.temp.precip.lm, BOGR.ppt.elev.lm, BOGR.temp.elev.lm, BOGR_ppt_lm, BOGR_temp_lm, BOGR_elev_lm)
mod.names <- c('climate', 'temp.ppt', 'ppt.elev', 'temp.elev', 'ppt','temp', 'elev')
aictab(cand.set = models, modnames = mod.names )
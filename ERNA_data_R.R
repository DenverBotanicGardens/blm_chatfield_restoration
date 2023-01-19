ERNA <- read.csv("ERNA_data.csv", header = TRUE)

ERNA.crop <- ERNA[1:1124, ]

ERNA.ex.rep <- ERNA.crop[ERNA.crop$replaced_YorN !="Y",]

ERNA.wet <- ERNA.crop[ERNA.crop$treatment !="dry" ,]

ERNA.dry <- ERNA.crop[ERNA.crop$treatment != "wet" ,]

height_pop_anova <- aov(length_cm_20220915 ~ Population, data = ERNA.wet)
summary(height_pop_anova)

height_pop_anova <- aov(length_cm_20220915 ~ Population*treatment, data = ERNA.crop)

height_sz_anova <- aov(length_cm_20220915 ~ seed_zone, data = ERNA)
summary(height_sz_anova)

days_mort_pop_an <- aov( days_until_mortality ~ Population, data=ERNA)
summary(days_mort_pop_an)

days_mort_sz_an <- aov( days_until_mortality ~ seed_zone, data=ERNA)
summary(days_mort_pop_an)


boxplot(length_cm_20220915 ~ Population*treatment,data=ERNA, main="ERNA Plant Height by Population", 
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
str(ERNA)


pop.list <- as.data.frame(unique(ERNA.crop$Population))
pop.list
colnames(pop.list) <- "Population"
survival.pred <- predict(survival_glm, pop.list, se.fit = TRUE, type = "response", interval = "confidence" )
survival.pred
survival_mat <- matrix(data = survival.pred$fit, nrow = 1, ncol = 20)
barplot(survival_mat, ylim = c(0,1), xlab = "Population", ylab = "Survival Rate", main = "Survival Rate by Population")
#zoom x axis from 0 to 1
#error bars

#add elevation
ERNA_elev <- read.csv("20230118_ERNA_elev.csv", header = TRUE)

ERNA.crop[ , 'elev'] <- NA

for (dd in 1:length(pop.list2)) { 
  ERNA.crop$elev[grepl(pop.list2[dd], ERNA.crop$Population)] <- ERNA_elev$elevation[dd] 
}

#add precip
ERNA_precip <- read.csv("20221129_ERNA_pptAnnual.csv", header = TRUE)

pop.list2 <- unique(as.character(ERNA.crop$Population))

ERNA.crop[ , 'Ppt_Annual'] <- NA

for (dd in 1:length(pop.list2)) { 
  ERNA.crop$Ppt_Annual[grepl(pop.list2[dd], ERNA.crop$Population)] <- ERNA_precip$Mean_Annual_Ppt[dd] 
  }

pop.list2
dd
ERNA_precip$Population
ERNA_elev$Population

#run lm height ~ ppt
hist(ERNA.crop$length_cm_20220915)

plot(length_cm_20220915 ~ Ppt_Annual, data = ERNA.crop)

ppt_elev_lm <- lmer(length_cm_20220915 ~ Ppt_Annual + elev + (1|block), data = ERNA.crop)
summary(ppt_lm)

library(car)
Anova(ppt_elev_lm)

boxplot(length_cm_20220915 ~ Ppt_Annual,data=ERNA.crop, main="ERNA Plant Height by Elevation", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)


#run lm ht ~ elev
plot(length_cm_20220915 ~ elev, data = ERNA.crop)

library(lme4)
elev_lm <- lmer(length_cm_20220915 ~ elev + (1|block), data = ERNA.crop)
summary(elev_lm)

#check if column added correctly 
elev.list <-unique(ERNA.crop$elev)
elev.list
ppt.list <- unique(ERNA.crop$Ppt_Annual)
ppt.list

elev_check <- cbind.data.frame(pop.list2, elev.list)
colnames(elev_check) <- colnames(ERNA_elev)
identical(elev_check, ERNA_elev)

ppt_check <- cbind.data.frame(pop.list2,ppt.list)
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




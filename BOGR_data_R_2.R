BOGR <- read.csv(file = 'BOGR_data.csv') 
BOGR.crop <- BOGR[1:1135,]


#exclude data subset (e.g. replaced plants)
BOGR.ex.rep <- BOGR.crop[BOGR.crop$replaced_Y_N !="Y",]

BOGR.wet <- BOGR.crop[BOGR.crop$Treatment !="dry" ,]

BOGR.dry <- BOGR.crop[BOGR.crop$Treatment != "wet" ,]

#Aim 1 variation
height_pop_anova <- aov(length_cm_20220801 ~ Population, data = BOGR.crop)
summary(height_pop_anova)

height_sz_anova <- aov(length_cm_20220801 ~ seed_zone, data = BOGR)
summary(height_sz_anova)

num_inf_pop_an <- aov( num_inf_20220927 ~ Population, data = BOGR.crop)
summary(num_inf_pop_an)

num_inf_sz_an <- aov( num_inf_20220927 ~ seed_zone, data = BOGR.crop)
summary(num_inf_sz_an)

days_mort_pop_an <- aov( days_until_mortality ~ Population, data=BOGR.crop)
summary(days_mort_pop_an)

days_mort_sz_an <- aov( days_until_mortality ~ seed_zone, data=BOGR.crop)
summary(days_mort_pop_an)


boxplot(length_cm_20220801 ~ Population,data=BOGR, main="BOGR Plant Height by Population", 
        xlab="Population", ylab="Height(cm)",cex.axis=0.25, las=2)
 

boxplot(length_cm_20220801 ~ seed_zone,data=BOGR, main="BOGR Plant Height by Seed Zone", 
        xlab="Seed Zone", ylab="Height(cm)", cex.axis=0.25, las=2)

boxplot(num_inf_20220927 ~ Population, data = BOGR, 
        main = "BOGR Number of Inflorescenses by Population", xlab = "Population", 
        ylab = "Num of Inflorescenses",cex.axis=0.25, las=2 )

hist(BOGR.crop$num_inf_20220927)

#infxseedzone
compare_means(num_inf_20220927 ~ seed_zone, data = BOGR, method = "anova")
boxplot(num_inf_20220927 ~ seed_zone, data = BOGR, 
        main = "BOGR Number of Inflorescenses by Seed Zone", xlab = "Seed Zone", 
        ylab = "Num of Inflorescenses",cex.axis=0.25, las=2 ) 

boxplot(days_until_mortality ~ Population, data=BOGR, main = "BOGR Days until mortality by Population", 
        xlab = "Population",cex.axis=0.25, las=2, ylab = "Days until mortality")

boxplot(days_until_mortality ~ seed_zone, data=BOGR, main = "BOGR Days until mortality by Seed Zone", 
        xlab = "Seed Zone", ylab = "Days until mortality",cex.axis=0.25, las=2)

survival_glm <- glm(survival_20220927 ~ Population, 
                    data = BOGR, family = binomial (link ="logit"))

summary(survival_glm)
str(BOGR)


pop.list <- as.data.frame(unique(BOGR.crop$Population))
pop.list
colnames(pop.list) <- "Population"
survival.pred <- predict(survival_glm, pop.list, se.fit = TRUE, type = "response", interval = "confidence" )
survival.pred
survival_mat <- matrix(data = survival.pred$fit, nrow = 1, ncol = 21)
barplot(survival_mat, ylim = c(0,1), xlab = "Population", ylab = "Survival Rate", main = "Survival Rate by Population")
#zoom x axis from 0 to 1
#error bars

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

height_pop_anova_tx <- aov(length_cm_20220801 ~ Population*Treatment, data = BOGR.crop)
summary(height_pop_anova_tx)

inf_pop_anova_tx <- aov(num_inf_20220927 ~ Population*Treatment, data = BOGR.crop)
summary(inf_pop_anova_tx)
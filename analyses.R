#salt-vs-bp
# Analyses will be shown here once the article is published 

# Linear regression analyses
regtesti <- lm(data00v$MeanSBP~data00v$T00.U_Na)
plot(data00v$T00.U_Na, data00v$MeanSBP, 
     xlab = "Urinary sodium (mg)", ylab = "Systolic BP at baseline")
abline(regtesti, col="red", lwd=2)
summary(regtesti)

regtesti2 <- lm(data00v$MeanSBP~data00v$T00.U_Na+data00v$T00.IKA2+data00v$T00.SP2+data00v$T00.RAVI_alc+data00v$T00.BMII_BMI)
summary(regtesti2)

regtesti3 <- lm(data00v$MeanDBP~data00v$T00.U_Na)
plot(data00v$T00.U_Na, data00v$MeanDBP, 
     xlab = "Urinary sodium (mg)", ylab = "Diastolic BP at baseline")
abline(regtesti3, col="blue", lwd=2)
summary(regtesti3)

regtesti4 <- lm(data00v$MeanDBP~data00v$T00.U_Na+data00v$T00.IKA2+data00v$T00.SP2+data00v$T00.RAVI_alc+data00v$T00.BMII_BMI)
summary(regtesti4)

regtesti5 <- lm(data00v$MeanSBP~data00v$T00.RAVI_SALT)
plot(data00v$T00.RAVI_SALT, data00v$MeanSBP, 
     xlab = "Dietary salt intake (g)", ylab = "Systolic BP at baseline")
abline(regtesti5, col="red", lwd=2)
summary(regtesti5)

regtesti6 <- lm(data00v$MeanSBP~data00v$T00.RAVI_SALT+data00v$T00.IKA2+data00v$T00.SP2+data00v$T00.RAVI_alc+data00v$T00.BMII_BMI)
summary(regtesti6)

regtesti7 <- lm(data00v$MeanDBP~data00v$T00.RAVI_SALT)
plot(data00v$T00.RAVI_SALT, data00v$MeanDBP, 
     xlab = "Dietary salt intake (g)", ylab = "Diastolic BP at baseline")
abline(regtesti7, col="blue", lwd=2)
summary(regtesti7)

regtesti8 <- lm(data00v$MeanDBP~data00v$T00.RAVI_SALT+data00v$T00.IKA2+data00v$T00.SP2+data00v$T00.RAVI_alc+data00v$T00.BMII_BMI)
summary(regtesti8)

regtesti9 <- lm(HTA$followup_modified_SBP~data11y$T00.U_Na)
plot(data11y$T00.U_Na, HTA$followup_modified_SBP, 
     xlab = "Urinary sodium (mg)", ylab = "Corrected systolic BP at follow-up")
abline(regtesti9, col="red", lwd=2)
summary(regtesti9)

regtesti10 <- lm(HTA$followup_modified_SBP~data11y$T00.U_Na+data11y$T11.IKA2+data11y$T11.SP2+data11y$T00.RAVI_alc+data11y$T11.BMII_BMI)
summary(regtesti10)

regtesti11 <- lm(HTA$followup_modified_DBP~data11y$T00.U_Na)
plot(data11y$T00.U_Na, HTA$followup_modified_DBP, 
     xlab = "Urinary sodium (mg)", ylab = "Corrected diastolic BP at follow-up")
abline(regtesti11, col="blue", lwd=2)
summary(regtesti11)

regtesti12 <- lm(HTA$followup_modified_DBP~data11y$T00.U_Na+data11y$T11.IKA2+data11y$T11.SP2+data11y$T00.RAVI_alc+data11y$T11.BMII_BMI)
summary(regtesti12)

regtesti13 <- lm(HTA$followup_modified_SBP~data11y$T00.RAVI_SALT)
plot(data11y$T00.RAVI_SALT, HTA$followup_modified_SBP, 
     xlab = "Dietary salt intake (g)", ylab = "Corrected systolic BP at follow-up")
abline(regtesti13, col="red", lwd=2)
summary(regtesti13)

regtesti14 <- lm(HTA$followup_modified_SBP~data11y$T00.RAVI_SALT+data11y$T11.IKA2+data11y$T11.SP2+data11y$T00.RAVI_alc+data11y$T11.BMII_BMI)
summary(regtesti14)

regtesti15 <- lm(HTA$followup_modified_DBP~data11y$T00.RAVI_SALT)
plot(data11y$T00.RAVI_SALT, HTA$followup_modified_DBP, 
     xlab = "Dietary salt intake (g)", ylab = "Corrected diastolic BP at follow-up")
abline(regtesti15, col="blue", lwd=2)
summary(regtesti15)

regtesti16 <- lm(HTA$followup_modified_DBP~data11y$T00.RAVI_SALT+data11y$T11.IKA2+data11y$T11.SP2+data11y$T00.RAVI_alc+data11y$T11.BMII_BMI)
summary(regtesti16)

#################################################################################
# Logistic regression analyses
# U-Na vs baseline prevalent hypertension crude
Lm1 <- glm(data00v$HA00 ~ data00v$T00.U_Na, family="binomial")
summary(Lm1)
confint(Lm1)
exp(Lm1$coefficients)

# U-Na vs prevalent HT adjusted
Lm2 <- glm(data00v$HA00 ~ data00v$T00.U_Na + data00v$T00.IKA2 + data00v$T00.SP2 + data00v$T00.BMII_BMI + data00v$T00.RAVI_alc, family="binomial")
summary(Lm2)

# D-salt vs prevalent HT crude
Lm3 <- glm(data00v$HA00 ~ data00v$T00.RAVI_SALT, family="binomial")
summary(Lm3)

# D-salt + prevalent HT adjusted 
Lm4 <- glm(data00v$HA00 ~ data00v$T00.RAVI_SALT + data00v$T00.IKA2 + data00v$T00.SP2 + data00v$T00.BMII_BMI + data00v$T00.RAVI_alc, family="binomial")
summary(Lm4)

# U-Na + incident HT crude
Lm5 <- glm(HTA$HA11 ~ HTA$data11y.T00.U_Na, family="binomial")
summary(Lm5)
confint(Lm5)
exp(Lm5$coefficients)

# U-Na + incident HT adjusted
Lm6 <- glm(HTA$HA11 ~ HTA$data11y.T00.U_Na + HTA$data11y.T00.BMII_BMI + HTA$data11y.T00.RAVI_alc + HTA$data11y.T11.IKA2 + HTA$data11y.T11.SP2, family="binomial")
summary(Lm6)

# D-salt + incident HT crude
Lm7 <- glm(HTA$HA11 ~ HTA$data11y.T00.RAVI_SALT, family="binomial")
summary(Lm7)

# D-salt + incident HT adjusted
Lm8 <- glm(HTA$HA11 ~ HTA$data11y.T00.RAVI_SALT + HTA$data11y.T00.RAVI_alc + HTA$data11y.T00.BMII_BMI + HTA$data11y.T11.SP2 + HTA$data11y.T11.IKA2, family="binomial")
summary(Lm8)

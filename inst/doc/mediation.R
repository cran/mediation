### R code from vignette source 'mediation.Rnw'

###################################################
### code chunk number 1: mediation.Rnw:218-219 (eval = FALSE)
###################################################
## install.packages("mediation")


###################################################
### code chunk number 2: mediation.Rnw:419-423
###################################################
## load the package
library(mediation)
## load the framing data
data(framing)


###################################################
### code chunk number 3: mediation.Rnw:427-428 (eval = FALSE)
###################################################
## ?framing


###################################################
### code chunk number 4: mediation.Rnw:451-456
###################################################
## Mediator Model
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
## Outcome Model
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))


###################################################
### code chunk number 5: mediation.Rnw:475-479
###################################################
library(sandwich)
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE)
summary(med.out)


###################################################
### code chunk number 6: mediation.Rnw:496-499
###################################################
med.out <- mediate(med.fit, out.fit, boot = TRUE, treat = "treat",
                   mediator = "emo")
summary(med.out)


###################################################
### code chunk number 7: mediation.Rnw:516-517
###################################################
plot(med.out)


###################################################
### code chunk number 8: mediation.Rnw:531-537
###################################################
med.fit <- lm(emo ~ treat + age + educ + gender + income, data=framing)
out.fit <- glm(cong_mesg ~ emo * treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE)
summary(med.out)


###################################################
### code chunk number 9: mediation.Rnw:558-561
###################################################
med.fit <- lm(emo ~ treat * age + educ + gender + income, data=framing)
out.fit <- glm(cong_mesg ~ emo + treat * age + emo * age + educ + gender
               + income, data = framing, family = binomial("probit"))


###################################################
### code chunk number 10: mediation.Rnw:572-578
###################################################
med.age20 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 20))
med.age60 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 60))
summary(med.age20)
summary(med.age60)


###################################################
### code chunk number 11: mediation.Rnw:605-611
###################################################
med.fit <- lm(emo ~ cond + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + cond + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med23.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 2, treat.value = 3)
summary(med23.out)


###################################################
### code chunk number 12: mediation.Rnw:616-619
###################################################
med14.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 1, treat.value = 4)
summary(med14.out)


###################################################
### code chunk number 13: mediation.Rnw:687-699
###################################################
## Mediation model
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
## Outcome model
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))
## Computing the ACME etc.
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE)
## Sensitivity analysis
sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect")
## Summary output
summary(sens.out)


###################################################
### code chunk number 14: mediation.Rnw:733-734 (eval = FALSE)
###################################################
## plot(sens.out, sens.par = "rho", main = "Anxiety", ylim = c(-0.2, 0.2))


###################################################
### code chunk number 15: mediation.Rnw:739-741
###################################################
par(mfrow = c(2,2))
plot(sens.out, sens.par = "rho", main = "Anxiety", ylim = c(-.2,.2))


###################################################
### code chunk number 16: mediation.Rnw:762-763 (eval = FALSE)
###################################################
## plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")


###################################################
### code chunk number 17: mediation.Rnw:768-770
###################################################
par(mfrow = c(2,2))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")


###################################################
### code chunk number 18: mediation.Rnw:840-845
###################################################
framing$english <- as.numeric(framing$english)
framing$anx <- as.numeric(framing$anx)
sed.est <- mediate.sed("english", "anx", "treat", data = framing, SI = TRUE,
                       boot = TRUE)
summary(sed.est)


###################################################
### code chunk number 19: mediation.Rnw:903-909
###################################################
data(boundsdata)
pd <- mediate.pd("out", "med", "ttt", "manip", boundsdata,
                 NINT = TRUE, sims = 1000, conf.level = 0.95)
summary(pd)
pd1 <- mediate.pd("out", "med", "ttt", "manip", boundsdata, NINT = FALSE)
summary(pd1)


###################################################
### code chunk number 20: mediation.Rnw:944-947
###################################################
data(boundsdata)
ped <- mediate.ped("out.enc", "med.enc", "ttt", "enc", boundsdata)
summary(ped)


###################################################
### code chunk number 21: mediation.Rnw:980-983
###################################################
data(CEDdata)
ced <- mediate.ced("Y2", "M1", "M2", "T1", "Z", CEDdata, sims = 100)
summary(ced)


###################################################
### code chunk number 22: mediation.Rnw:1132-1137
###################################################
Xnames <- c("age", "educ", "gender", "income")
m.med <- multimed(outcome = "immigr", med.main = "emo", med.alt = "p_harm", 
                  treat = "treat", covariates = Xnames,
                  data = framing, sims = 1000)
summary(m.med)


###################################################
### code chunk number 23: mediation.Rnw:1159-1160 (eval = FALSE)
###################################################
## plot(m.med, type = "point")


###################################################
### code chunk number 24: mediation.Rnw:1165-1166
###################################################
plot(m.med, type = "point")


###################################################
### code chunk number 25: mediation.Rnw:1184-1187 (eval = FALSE)
###################################################
## oldpar <- par(mfrow = c(2,2))
## plot(m.med, type = c("sigma", "R2-total"), tgroup = c("treated", "control"))
## par(oldpar)


###################################################
### code chunk number 26: mediation.Rnw:1192-1195
###################################################
oldpar <- par(mfrow = c(2,2))
plot(m.med, type = c("sigma", "R2-total"), tgroup = c("treated", "control"))
par(oldpar)


###################################################
### code chunk number 27: mediation.Rnw:1214-1218
###################################################
m.med.para <- multimed(outcome = "out", med.main = "med", treat = "ttt", 
                       experiment = "manip", design = "parallel", 
                       data = boundsdata, sims = 1000)
summary(m.med.para)



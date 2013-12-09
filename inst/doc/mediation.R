### R code from vignette source 'mediation.Rnw'

###################################################
### code chunk number 1: mediation.Rnw:229-230 (eval = FALSE)
###################################################
## install.packages("mediation")


###################################################
### code chunk number 2: mediation.Rnw:433-435
###################################################
library("mediation")
data("framing")


###################################################
### code chunk number 3: mediation.Rnw:439-440 (eval = FALSE)
###################################################
## ?framing


###################################################
### code chunk number 4: mediation.Rnw:462-465
###################################################
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))


###################################################
### code chunk number 5: mediation.Rnw:493-497
###################################################
library("sandwich")
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)
summary(med.out)


###################################################
### code chunk number 6: mediation.Rnw:514-517
###################################################
med.out <- mediate(med.fit, out.fit, boot = TRUE, treat = "treat",
                   mediator = "emo", sims = 100)
summary(med.out)


###################################################
### code chunk number 7: mediation.Rnw:542-543
###################################################
plot(med.out)


###################################################
### code chunk number 8: mediation.Rnw:562-568
###################################################
med.fit <- lm(emo ~ treat + age + educ + gender + income, data=framing)
out.fit <- glm(cong_mesg ~ emo * treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)
summary(med.out)


###################################################
### code chunk number 9: mediation.Rnw:572-573
###################################################
test.TMint(med.out, conf.level = .95)


###################################################
### code chunk number 10: mediation.Rnw:599-602
###################################################
med.fit <- lm(emo ~ treat * age + educ + gender + income, data=framing)
out.fit <- glm(cong_mesg ~ emo + treat * age + emo * age + educ + gender
               + income, data = framing, family = binomial("probit"))


###################################################
### code chunk number 11: mediation.Rnw:613-619
###################################################
med.age20 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 20), sims = 100)
med.age60 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 60), sims = 100)
summary(med.age20)
summary(med.age60)


###################################################
### code chunk number 12: mediation.Rnw:632-635
###################################################
med.init <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo", sims=2)
test.modmed(med.init, covariates.1 = list(age = 20),
                      covariates.2 = list(age = 60), sims = 100)


###################################################
### code chunk number 13: mediation.Rnw:664-670
###################################################
med.fit <- lm(emo ~ cond + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + cond + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med23.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 2, treat.value = 3, sims = 100)
summary(med23.out)


###################################################
### code chunk number 14: mediation.Rnw:675-678
###################################################
med14.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 1, treat.value = 4, sims = 100)
summary(med14.out)


###################################################
### code chunk number 15: mediation.Rnw:746-753
###################################################
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)
sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)


###################################################
### code chunk number 16: mediation.Rnw:787-788 (eval = FALSE)
###################################################
## plot(sens.out, sens.par = "rho", main = "Anxiety", ylim = c(-0.2, 0.2))


###################################################
### code chunk number 17: mediation.Rnw:793-795
###################################################
par(mfrow = c(2,2))
plot(sens.out, sens.par = "rho", main = "Anxiety", ylim = c(-.2,.2))


###################################################
### code chunk number 18: mediation.Rnw:817-818 (eval = FALSE)
###################################################
## plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")


###################################################
### code chunk number 19: mediation.Rnw:823-825
###################################################
par(mfrow = c(2,2))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")


###################################################
### code chunk number 20: mediation.Rnw:857-858
###################################################
data(student)


###################################################
### code chunk number 21: mediation.Rnw:884-890
###################################################
library(arm)  # also loads lme4
med.fit <- glmer(attachment ~ catholic + gender + income + pared + (1|SCH_ID),
                 family = binomial(link = "logit"), data = student)
out.fit <- glmer(fight ~ catholic*attachment +
                         gender + income + pared + (1 + attachment|SCH_ID),
                 family = binomial(link = "logit"), data = student)


###################################################
### code chunk number 22: mediation.Rnw:893-896
###################################################
med.out <- mediate(med.fit, out.fit, treat = "catholic", mediator = "attachment",
                   sims = 100)
summary(med.out)


###################################################
### code chunk number 23: mediation.Rnw:909-910
###################################################
data(school)


###################################################
### code chunk number 24: mediation.Rnw:923-924
###################################################
med.fit <- lm(smorale ~  free + catholic + coed, data = school)


###################################################
### code chunk number 25: mediation.Rnw:930-933
###################################################
out.fit <- lmer(late ~ free + smorale + catholic + coed +
                       gender + income + pared + (1|SCH_ID),
                data = student)


###################################################
### code chunk number 26: mediation.Rnw:939-942
###################################################
med.out <- mediate(med.fit, out.fit, treat = "free", mediator = "smorale",
                   control.value = 3, treat.value = 4, sims = 100)
summary(med.out)


###################################################
### code chunk number 27: mediation.Rnw:1016-1021
###################################################
framing$english <- as.numeric(framing$english)
framing$anx <- as.numeric(framing$anx)
sed.est <- mediate.sed("english", "anx", "treat", data = framing, SI = TRUE,
                       boot = TRUE, sims = 100)
summary(sed.est)


###################################################
### code chunk number 28: mediation.Rnw:1079-1086
###################################################
data("boundsdata")
pd <- mediate.pd("out", "med", "ttt", "manip", boundsdata,
                 NINT = TRUE, sims = 100, conf.level = 0.95)
summary(pd)
pd1 <- mediate.pd("out", "med", "ttt", "manip", boundsdata,
                  NINT = FALSE)
summary(pd1)


###################################################
### code chunk number 29: mediation.Rnw:1121-1124
###################################################
data("boundsdata")
ped <- mediate.ped("out.enc", "med.enc", "ttt", "enc", boundsdata)
summary(ped)


###################################################
### code chunk number 30: mediation.Rnw:1157-1160
###################################################
data("CEDdata")
ced <- mediate.ced("Y2", "M1", "M2", "T1", "Z", CEDdata, sims = 100)
summary(ced)


###################################################
### code chunk number 31: mediation.Rnw:1324-1329
###################################################
Xnames <- c("age", "educ", "gender", "income")
m.med <- multimed(outcome = "immigr", med.main = "emo", med.alt = "p_harm",
                  treat = "treat", covariates = Xnames,
                  data = framing, sims = 100)
summary(m.med)


###################################################
### code chunk number 32: mediation.Rnw:1351-1352 (eval = FALSE)
###################################################
## plot(m.med, type = "point")


###################################################
### code chunk number 33: mediation.Rnw:1357-1358
###################################################
plot(m.med, type = "point")


###################################################
### code chunk number 34: mediation.Rnw:1376-1377 (eval = FALSE)
###################################################
## plot(m.med, type = c("sigma", "R2-total"), tgroup = c("treated", "control"))


###################################################
### code chunk number 35: mediation.Rnw:1382-1385
###################################################
oldpar <- par(mfrow = c(2,2))
plot(m.med, type = c("sigma", "R2-total"), tgroup = c("treated", "control"))
par(oldpar)


###################################################
### code chunk number 36: mediation.Rnw:1404-1408
###################################################
m.med.para <- multimed(outcome = "out", med.main = "med", treat = "ttt",
                       experiment = "manip", design = "parallel",
                       data = boundsdata, sims = 100)
summary(m.med.para)



### R code from vignette source 'mediation.Rnw'

###################################################
### code chunk number 1: mediation.Rnw:226-227
###################################################
options(prompt = "R> ")


###################################################
### code chunk number 2: mediation.Rnw:229-230 (eval = FALSE)
###################################################
## install.packages("mediation")


###################################################
### code chunk number 3: mediation.Rnw:432-434
###################################################
library("mediation")
data("framing", package = "mediation")


###################################################
### code chunk number 4: mediation.Rnw:438-439 (eval = FALSE)
###################################################
## ?framing


###################################################
### code chunk number 5: mediation.Rnw:464-468
###################################################
set.seed(2014)
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))


###################################################
### code chunk number 6: mediation.Rnw:500-505
###################################################
library("sandwich")
set.seed(2014) 
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)
summary(med.out)


###################################################
### code chunk number 7: mediation.Rnw:522-526
###################################################
set.seed(2014)
med.out <- mediate(med.fit, out.fit, boot = TRUE, treat = "treat",
                   mediator = "emo", sims = 100)
summary(med.out)


###################################################
### code chunk number 8: mediation.Rnw:551-552
###################################################
plot(med.out)


###################################################
### code chunk number 9: mediation.Rnw:572-579
###################################################
set.seed(2014)
med.fit <- lm(emo ~ treat + age + educ + gender + income, data=framing)
out.fit <- glm(cong_mesg ~ emo * treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)
summary(med.out)


###################################################
### code chunk number 10: mediation.Rnw:583-584
###################################################
test.TMint(med.out, conf.level = .95)


###################################################
### code chunk number 11: mediation.Rnw:632-636
###################################################
set.seed(2014)
med.fit <- lm(emo ~ treat * age + educ + gender + income, data=framing)
out.fit <- glm(cong_mesg ~ emo + treat * age + emo * age + educ + gender
               + income, data = framing, family = binomial("probit"))


###################################################
### code chunk number 12: mediation.Rnw:651-658
###################################################
set.seed(2014)
med.age20 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 20), sims = 100)
med.age60 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 60), sims = 100)
summary(med.age20)
summary(med.age60)


###################################################
### code chunk number 13: mediation.Rnw:671-675
###################################################
set.seed(2014)
med.init <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo", sims=2)
test.modmed(med.init, covariates.1 = list(age = 20),
                      covariates.2 = list(age = 60), sims = 100)


###################################################
### code chunk number 14: mediation.Rnw:704-711
###################################################
set.seed(2014)
med.fit <- lm(emo ~ cond + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + cond + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med23.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 2, treat.value = 3, sims = 100)
summary(med23.out)


###################################################
### code chunk number 15: mediation.Rnw:716-720
###################################################
set.seed(2014)
med14.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 1, treat.value = 4, sims = 100)
summary(med14.out)


###################################################
### code chunk number 16: mediation.Rnw:788-796
###################################################
set.seed(2014)
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)
sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)


###################################################
### code chunk number 17: mediation.Rnw:824-826
###################################################
par(mfrow = c(2,2))
plot(sens.out, sens.par = "rho", main = "Anxiety", ylim = c(-.2,.2))


###################################################
### code chunk number 18: mediation.Rnw:841-842 (eval = FALSE)
###################################################
## plot(sens.out, sens.par = "rho", main = "Anxiety", ylim = c(-0.2, 0.2))


###################################################
### code chunk number 19: mediation.Rnw:858-859 (eval = FALSE)
###################################################
## plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")


###################################################
### code chunk number 20: mediation.Rnw:865-867
###################################################
par(mfrow = c(2,2))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")


###################################################
### code chunk number 21: mediation.Rnw:931-932
###################################################
data("student", package = "mediation")


###################################################
### code chunk number 22: mediation.Rnw:961-968
###################################################
library(lme4)  
set.seed(2014)
med.fit <- glmer(attachment ~ catholic + gender + income + pared + (1|SCH_ID),
                 family = binomial(link = "logit"), data = student)
out.fit <- glmer(fight ~ catholic*attachment +
                         gender + income + pared + (1 + attachment|SCH_ID),
                 family = binomial(link = "logit"), data = student)


###################################################
### code chunk number 23: mediation.Rnw:971-975
###################################################
set.seed(2014)
med.out <- mediate(med.fit, out.fit, treat = "catholic", mediator = "attachment",
                   sims = 100)
summary(med.out)


###################################################
### code chunk number 24: mediation.Rnw:991-992
###################################################
data("school", package = "mediation")


###################################################
### code chunk number 25: mediation.Rnw:1027-1029
###################################################
set.seed(2014)
med.fit <- lm(smorale ~  free + catholic + coed, data = school)


###################################################
### code chunk number 26: mediation.Rnw:1035-1039
###################################################
set.seed(2014)
out.fit <- lmer(late ~ free + smorale + catholic + coed +
                       gender + income + pared + (1|SCH_ID),
                data = student)


###################################################
### code chunk number 27: mediation.Rnw:1046-1050
###################################################
set.seed(2014)
med.out <- mediate(med.fit, out.fit, treat = "free", mediator = "smorale",
                   control.value = 3, treat.value = 4, sims = 100)
summary(med.out)


###################################################
### code chunk number 28: mediation.Rnw:1154-1160
###################################################
framing$english <- as.numeric(framing$english)
framing$anx <- as.numeric(framing$anx)
set.seed(2014)
sed.est <- mediate.sed("english", "anx", "treat", data = framing, SI = TRUE,
                       boot = TRUE, sims = 100)
summary(sed.est)


###################################################
### code chunk number 29: mediation.Rnw:1217-1226
###################################################
data("boundsdata", package = "mediation")
set.seed(2014)
pd <- mediate.pd("out", "med", "ttt", "manip", boundsdata,
                 NINT = TRUE, sims = 100, conf.level = 0.95)
summary(pd)
set.seed(2014)
pd1 <- mediate.pd("out", "med", "ttt", "manip", boundsdata,
                  NINT = FALSE)
summary(pd1)


###################################################
### code chunk number 30: mediation.Rnw:1261-1265
###################################################
data("boundsdata", package = "mediation")
set.seed(2014)
ped <- mediate.ped("out.enc", "med.enc", "ttt", "enc", boundsdata)
summary(ped)


###################################################
### code chunk number 31: mediation.Rnw:1298-1302
###################################################
data("CEDdata", package = "mediation")
set.seed(2014)
ced <- mediate.ced("Y2", "M1", "M2", "T1", "Z", CEDdata, sims = 100)
summary(ced)


###################################################
### code chunk number 32: mediation.Rnw:1466-1472
###################################################
Xnames <- c("age", "educ", "gender", "income")
set.seed(2014)
m.med <- multimed(outcome = "immigr", med.main = "emo", med.alt = "p_harm",
                  treat = "treat", covariates = Xnames,
                  data = framing, sims = 100)
summary(m.med)


###################################################
### code chunk number 33: mediation.Rnw:1494-1495 (eval = FALSE)
###################################################
## plot(m.med, type = "point")


###################################################
### code chunk number 34: mediation.Rnw:1500-1501
###################################################
plot(m.med, type = "point")


###################################################
### code chunk number 35: mediation.Rnw:1519-1520 (eval = FALSE)
###################################################
## plot(m.med, type = c("sigma", "R2-total"), tgroup = c("treated", "control"))


###################################################
### code chunk number 36: mediation.Rnw:1525-1528
###################################################
oldpar <- par(mfrow = c(2,2))
plot(m.med, type = c("sigma", "R2-total"), tgroup = c("treated", "control"))
par(oldpar)


###################################################
### code chunk number 37: mediation.Rnw:1569-1574
###################################################
set.seed(2014)
m.med.para <- multimed(outcome = "out", med.main = "med", treat = "ttt",
                       experiment = "manip", design = "parallel",
                       data = boundsdata, sims = 100)
summary(m.med.para)


###################################################
### code chunk number 38: mediation.Rnw:1736-1737
###################################################
data("jobs", package = "mediation")


###################################################
### code chunk number 39: mediation.Rnw:1745-1748
###################################################
set.seed(2014)
a <- lm(comply ~ treat + sex + age + marital + nonwhite + educ + income, 
        data = jobs)


###################################################
### code chunk number 40: mediation.Rnw:1757-1762
###################################################
set.seed(2014)
b <- glm(job_dich ~ comply + treat + sex + age + marital + 
          nonwhite + educ + income, data = jobs, family = binomial)
c <- lm(depress2 ~ job_dich * (comply + treat) + sex + age + marital + 
          nonwhite + educ + income, data = jobs)


###################################################
### code chunk number 41: mediation.Rnw:1782-1785
###################################################
set.seed(2014)
out <- ivmediate(a, b, c, sims = 100, boot = FALSE, 
                 enc = "treat", treat = "comply", mediator = "job_dich")


###################################################
### code chunk number 42: mediation.Rnw:1821-1822
###################################################
summary(out)


###################################################
### code chunk number 43: mediation.Rnw:1842-1843 (eval = FALSE)
###################################################
## plot(out)


###################################################
### code chunk number 44: mediation.Rnw:1848-1849
###################################################
plot(out)



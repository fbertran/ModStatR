#' ---
#' title: "Mod\u00e9lisation statistique par la pratique avec R"
#' subtitle: "Chapitre 4 : Analyse de r\u00e9gression"
#' author: "Fr\u00e9d\u00e9ric Bertrand, Emmanuelle Claeys, Myriam Maumy-Bertrand"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---

#Chapitre 4

#page 248
#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require("BioStatR")){install.packages("BioStatR")}
library(BioStatR)
str(Mesures)

summary(Mesures)

#page 249
#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require("ISLR")){install.packages("ISLR")}
library(ISLR)
summary(Hitters[,17:20])

Mes.B = subset(Mesures,Mesures$espece=="bignone")
model<-lm(masse~taille,data=Mes.B)
summary(model)

Mes.B$masse

#page 250
round(fitted(model), 2)

round(residuals(model), 2)

#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require("broom")){install.packages("broom")}
library(broom)
(model.diag.metrics<-augment(model))

#page 252
tidy(model)

glance(model)

with(Mes.B,plot(taille,masse,pch=20,xlab="Taille",ylab="Masse"))
abline(model, lwd=2, col="red")
with(Mes.B,lines(lowess(taille, masse), lty=2, lwd=2, col="blue"))
with(Mes.B,lines(smooth.spline(taille,masse),col="orange",lwd=2, 
                 lty=4))
with(Mes.B,segments(taille, masse, taille, fitted(model), lty=2, 
                    col="red"))
legend("topleft",lty=c(1,2,4),legend = c("lm","lowess", 
                                         "smooth.spline"),lwd=2,col=c("red","blue","orange"))

#page 253
#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require("ggiraphExtra")){install.packages("ggiraphExtra")}
library(ggiraphExtra)
ggPredict(model)

shapiro.test(residuals(model))

with(Mes.B,plot(fitted(model),masse-fitted(model),pch=20,xlab= 
                  "Valeurs pr\u00e9dites",main="R\u00e9sidus en fonction de la valeur pr\u00e9dite", 
                ylab="R\u00e9sidus = valeurs observ\u00e9es - valeurs pr\u00e9dites"))
abline(h=0)
with(Mes.B,lines(lowess(fitted(model),masse-fitted(model)),col= 
                   "red",lwd=2))

#page 254
library(MASS)
model.r <- lqs(masse ~ taille, data=Mes.B)
summary(model.r)

#page 255
coefficients(model.r)

model.r$bestone

coef(lm(masse ~ taille, data=Mes.B[model.r$bestone,]))

with(Mes.B,1-sum(residuals(model.r)^2)/sum((masse-mean(masse))^2))

#page 256
with(Mes.B,1-sum(residuals(model)^2)/sum((masse-mean(masse))^2))

plot(masse ~ taille, data=Mes.B, xlab="Taille", ylab="Masse")
abline(model.r, lty=1)
abline(model, lty=2)
with(Mes.B,points(taille[model.r$bestone],masse[model.r$bestone],
                  pch=19,col="red"))
abline(lm(masse ~ taille, data=Mes.B[model.r$bestone,]), lty=3)
legend("topleft", legend=c("R\u00e9sistante : moindres carr\u00e9s tronqu\u00e9s",
       "Moindres carr\u00e9s ordinaires",
       "Droite ajust\u00e9e entre les deux observations"), lty=1:3)

shapiro.test(residuals(model.r))

#page 257
model2<-lm(masse~taille+I(taille^2),data=Mes.B)

summary(model2)

plot(masse~taille,data=Mes.B,xlab="Taille",ylab="Masse",pch=20)
with(Mes.B,lines(lowess(taille,masse),lty=3,lwd=2,col="blue"))
with(Mes.B,points(sort(taille),fitted(model2)[order(taille)] 
                  ,col="red"))
with(Mes.B,lines(sort(taille),fitted(model2)[order(taille)],lty=1, 
                 col="red",lwd=2))
with(Mes.B,segments(taille,masse,taille,fitted(model2),lty=2, 
                    col="red"))
legend("topleft",legend=c("Moindres carr\u00e9s ordinaires", 
       "Ajustement local"),lty=c(1,3),lwd=2,col=c("red","blue"))

#page 258
shapiro.test(residuals(model2))

#page 259
confint(model2)

anova(model2)

library(ModStatR)
my.confidence.region(model2, which=1)
my.confidence.region(model2, which=2)
my.confidence.region(model2, which=3)

#page 260
set.seed(314)
model2.r<-lqs(masse~taille+I(taille^2),data=Mes.B)
rbind(coefficients(model2),coefficients(model2.r))

#page 261
shapiro.test(residuals(model2.r))

plot(masse~taille,data=Mes.B,xlab="Taille",ylab="Masse",pch=20)
with(Mes.B,lines(lowess(taille, masse), lty=3, lwd=2, col="blue"))
with(Mes.B,points(sort(taille),fitted(model2.r)[order(taille)], 
                  col="green"))
with(Mes.B,lines(sort(taille),fitted(model2.r)[order(taille)], 
                 lty=2,col="green",lwd=2))
with(Mes.B,segments(taille,masse,taille,fitted(model2.r),lty=2, 
                    col="green"))
legend("topleft",legend=c("R\u00e9sistante : moindres carr\u00e9s trim\u00e9s", 
        "Ajustement local"),lty=c(2,3),lwd=2,col=c("green","blue"))

#page 261
plot(masse~taille,data=Mes.B,xlab="Taille",ylab="Masse",pch=20)
with(Mes.B,lines(lowess(taille,masse),lty=3,lwd=2,col="blue"))
with(Mes.B,points(sort(taille),fitted(model2)[order(taille)], 
                  col="red"))
with(Mes.B,lines(sort(taille),fitted(model2)[order(taille)], 
                 col="red",lwd=2,lty=1))
with(Mes.B,points(sort(taille),fitted(model2.r)[order(taille)], 
                  col="green"))
with(Mes.B,lines(sort(taille),fitted(model2.r)[order(taille)], 
                 col="green",lwd=2,lty=2))
legend("topleft",legend=c("Moindres carr\u00e9s ordinaires", 
       "R\u00e9sistante : moindres carr\u00e9s trim\u00e9s","Ajustement local"),lty=1:3 
       ,lwd=2,col=c("red","green","blue"))

#page 262
data("mtcars")
View(mtcars)

help(mtcars)

#page 263
head(mtcars,n=10)
?mtcars
str(mtcars)

#page 264
mtcars2 <- within(mtcars, { 
  vs <- factor(vs, labels = c("V", "S")) 
  am <- factor(am, labels = c("automatic", "manual")) 
  cyl <- ordered(cyl) 
  gear <- ordered(gear) 
  carb <- ordered(carb) 
  })
summary(mtcars2)

subsetmtcars<-mtcars[,c(1,3,4,5,6,7)]

#page 265
library(MVN)
set.seed(1133)
result1 = mvn(data = subsetmtcars, 
              mvnTest = "mardia", 
              univariateTest = "SW", univariatePlot = "histogram", 
              multivariatePlot = "qq", 
              multivariateOutlierMethod = "adj", 
              showOutliers = TRUE, showNewData = TRUE)
result1$multivariateNormality

result1$multivariateOutliers

#page 266
library(corrplot); set.seed(1133)
permmtcars <- perm.cor.mtest(subsetmtcars)
permmtcars$p<.05/choose(ncol(subsetmtcars),2)
corrplot(permmtcars$cor,p.mat=permmtcars$p,pch.col="white",insig= 
           "label_sig",sig.level=.05/choose(ncol(subsetmtcars),2))

#page 267
fit.mtcars<-lm(mpg~.,data=subsetmtcars)
fit.mtcars

shapiro.test(residuals(fit.mtcars))

fit2.mtcars<-lm(mpg~.+disp:hp,data=subsetmtcars)
round(coef(fit2.mtcars), 3)

shapiro.test(residuals(fit2.mtcars))

covratio(fit2.mtcars)
dffits(fit2.mtcars)
dfbetas(fit2.mtcars)
library(car)
vif(fit2.mtcars)

#page 268
library(perturb)
colldiag(fit2.mtcars)
plot(fit2.mtcars)
influence.measures(fit2.mtcars)
influencePlot(fit2.mtcars)
influenceIndexPlot(fit2.mtcars)

summary(fit2.mtcars)
anova(fit2.mtcars)

#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require("ISLR")){install.packages("ISLR")}
library(ISLR)
data(Hitters)
#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require("mice")){install.packages("mice")}
library(mice)
md.pattern(Hitters)

#page 269
md.pairs(Hitters)

library(dplyr)
#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require("finalfit")){install.packages("finalfit")}
library(finalfit)
Hitters %>% 
  missing_plot()

explanatory = setdiff(colnames(Hitters),"Salary")
dependent = "Salary"
Hitters %>% 
  missing_pattern(dependent, explanatory)

#page 270
#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require("naniar")){install.packages("naniar")}
library(naniar);library(ggplot2)
Hitters %>% 
  bind_shadow() %>% 
  ggplot(aes(x = Hits, 
             fill = Salary_NA)) + 
  geom_density(alpha = 0.5)

#page 271
gg_miss_var(Hitters)
try(gg_miss_upset(Hitters))

#page 272
#Exercice 4.1
data(anscombe)
str(anscombe)

#page 273
#Exercice 4.2
Hitters = na.omit(Hitters)

#q4
#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require(leaps)){install.packages("leaps")}
library(leaps)
regfit.full = regsubsets(Salary ~ ., data = Hitters)
summary(regfit.full)
regfit19.full = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary = summary(regfit19.full)
names(reg.summary)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], pch = 20, col = "red")
plot(regfit19.full, scale = "Cp")
coef(regfit19.full, 10)

#page 274
#q5
regfit.fwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, 
                        method = "forward")
summary(regfit.fwd)
plot(regfit.fwd, scale = "Cp")

#q6
set.seed(314)
train = sample(seq(263), 180, replace = FALSE)
regfit.fwd = regsubsets(Salary ~ ., data = Hitters[train, ], 
                        nvmax = 19, method = "forward", nbest=9)
n.vars=NULL
val.errors=NULL
x.test = model.matrix(Salary ~ ., data = Hitters[-train, ])
for (i in 1:length(summary(regfit.fwd)$rss)) {
  coefi = coef(regfit.fwd, id = i)
  pred = x.test[, names(coefi)] %*% coefi
  n.vars = c(n.vars,length(names(coefi))-1)
  val.errors = c(val.errors,mean((Hitters$Salary[-train]-pred)^2))
}

#page 275
#q6 (suite)
mod.ordre=as.numeric(unlist(sapply(table(n.vars), 
                                   function(x){1:x})))
plot(n.vars,sqrt(val.errors), ylab ="Root MSE",pch =as.character( 
  mod.ordre),col=n.vars)
lines(n.vars[mod.ordre==1],sqrt(val.errors)[mod.ordre==1], 
      lwd=2,lty=2)

#page 276
#q7
predict.regsubsets = function(object, newdata, id, ...) { 
  form = as.formula(object$call[[2]]) 
  mat = model.matrix(form, newdata) 
  coefi = coef(object, id = id) 
  mat[, names(coefi)] %*% coefi 
}

#q8
library(glmnet)
x=model.matrix(Salary~.-1,data=Hitters)
y=Hitters$Salary
lasso_model_cv=cv.glmnet(x,y)
plot(lasso_model_cv)
n_best_m=which(lasso_model_cv$lambda==lasso_model_cv$lambda.min)
lasso_model_cv$glmnet.fit$beta[,n_best_m]

#page 277
#Exercice 4.3
#q1
CancerSein <- read.csv("https://tinyurl.com/y3l6sh59")

#page 279
#Exercice 4.4
#q1
SidaChat <- read.csv("https://tinyurl.com/yxe6yxem")

#page 282
#Exercice 4.5
#q1
Vitamines <- read.csv("https://tinyurl.com/y3shxcsd")

#page 283
#Exercice 4.6
#q1
Beton <- read.csv("https://tinyurl.com/y4w2qv9t")

#page 284
#Exercice 4.7
#q1
chal <- read.csv("https://tinyurl.com/yyb3cztf")
#q2
cdplot(as.factor(Defaillance)~Temperature, data=chal, 
       ylab="Defaillance")

#page 285
#q3
chal.glm <- glm(Defaillance~Temperature,data=chal,
                family="binomial")

#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require(hnp)){install.packages("hnp")}
library(hnp)
hnp(chal.glm, sim = 99, conf = 0.95)

#page 286
#q6
#Si le package n'est pas installe, enlever le commentaire
#puis executer la commande ci-dessous.
#if(!require(rms)){install.packages("rms")}
library(rms)
chal.lrm <- lrm(Defaillance~Temperature, data=chal, x=TRUE, y=TRUE)
print(chal.lrm)
residuals(chal.lrm, "gof")

#Exercice 4.8
#q1
Cypermethrine <- read.csv("https://tinyurl.com/y4deakfd")

#page 288
#Exercice 4.9
#q1
poly <- read.csv("https://tinyurl.com/yyhhcw37")

#q2
poly_glm1 <- glm(nombre~traitement+age, family=poisson(),data=poly)
library(hnp)
hnp(poly_glm1, sim = 99, conf = 0.95)
summary(poly_glm1)
confint(poly_glm1)
poly_glm2 <- glm(nombre~traitement+age, family=quasipoisson(), 
                 data=poly)
library(hnp)
hnp(poly_glm2, sim = 99, conf = 0.95)
summary(poly_glm2)
confint(poly_glm2)
poly_glm3 <- glm.nb(nombre~traitement+age,data=poly)
library(hnp)
hnp(poly_glm3, sim = 99, conf = 0.95)
summary(poly_glm3)
confint(poly_glm3)

#page 289
with(poly,plot(nombre~age,type="n",ylab="Nombre de polypes", 
               xlab="\u00c2ge"))
with(poly,points(age[traitement=="placebo"],fitted(poly_glm2)[ 
  traitement=="placebo"],pch="P",col="red"))
xv1<-seq(0,50,.05)
yv1<-predict(poly_glm2,list(traitement=as.factor(rep("placebo", 
                                                     length(xv1))),age=xv1))
lines(xv1,exp(yv1),col="red")
with(poly,points(age[traitement=="placebo"], 
                 nombre[traitement=="placebo"],pch="p"))
with(poly,points(age[traitement=="medicament"],fitted( 
  poly_glm2)[traitement=="medicament"],pch="D",col="blue"))
with(poly,points(age[traitement=="medicament"], 
                 nombre[traitement=="medicament"],pch="d"))
yv2<-predict(poly_glm2,list(traitement=as.factor(rep("medicament", 
                                                     length(xv1))),age=xv1))
lines(xv1,exp(yv2),col="blue")

with(poly,plot(nombre~age,type="n",ylab="Nombre de polypes", 
               xlab="\u00c2ge"))
with(poly,points(age[traitement=="placebo"],fitted(poly_glm3)[ 
  traitement=="placebo"],pch="P",col="red"))
xv1<-seq(0,50,.05)
yv1<-predict(poly_glm3,list(traitement=as.factor(rep("placebo", 
                                                     length(xv1))),age=xv1))
lines(xv1,exp(yv1),col="red")
with(poly,points(age[traitement=="placebo"], 
                 nombre[traitement=="placebo"],pch="p"))
with(poly,points(age[traitement=="medicament"],fitted( 
  poly_glm3)[traitement=="medicament"],pch="D",col="blue"))
with(poly,points(age[traitement=="medicament"], 
                 nombre[traitement=="medicament"],pch="d"))
yv2<-predict(poly_glm3,list(traitement=as.factor(rep("medicament", 
                                                     length(xv1))),age=xv1))
lines(xv1,exp(yv2),col="blue")







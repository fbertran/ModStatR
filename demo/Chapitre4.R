#' ---
#' title: "Mod\u00e9lisation statistique par la pratique avec R"
#' subtitle: "Chapitre 4 : Analyse de r\u00e9gression"
#' author: "Fr\u00e9d\u00e9ric Bertrand, Emmanuelle Claeys, Myriam Maumy-Bertrand"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---

#Chapitre 4

#page 266
if(!require("BioStatR")){install.packages("BioStatR")}
library(BioStatR)
str(Mesures)

summary(Mesures)

#page 267
if(!require("ISLR")){install.packages("ISLR")}
library(ISLR)
summary(Hitters[,17:20])

Mes.B = subset(Mesures,Mesures$espece=="bignone")
model<-lm(masse~taille,data=Mes.B)
summary(model)

Mes.B$masse

#page 268
round(fitted(model), 2)

round(residuals(model), 2)

if(!require("broom")){install.packages("broom")}
library(broom)
(model.diag.metrics<-augment(model))

#page 270
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

#page 271
if(!require("ggiraphExtra")){install.packages("ggiraphExtra")}
library(ggiraphExtra)
ggPredict(model)

shapiro.test(residuals(model))

with(Mes.B,plot(fitted(model),masse-fitted(model),pch=20,xlab= 
                  "Valeurs pr\u00e9dites",main="R\u00e9sidus en fonction de la valeur pr\u00e9dite", 
                ylab="R\u00e9sidus = valeurs observ\u00e9es - valeurs pr\u00e9dites"))
abline(h=0)
with(Mes.B,lines(lowess(fitted(model),masse-fitted(model)),col= 
                   "red",lwd=2))

#page 272
library(MASS)
model.r <- lqs(masse ~ taille, data=Mes.B)
summary(model.r)

#page 273
coefficients(model.r)

model.r$bestone

coef(lm(masse ~ taille, data=Mes.B[model.r$bestone,]))

with(Mes.B,1-sum(residuals(model.r)^2)/sum((masse-mean(masse))^2))

#page 274
with(Mes.B,1-sum(residuals(model)^2)/sum((masse-mean(masse))^2))

plot(masse ~ taille, data=Mes.B, xlab="Taille", ylab="Masse")
abline(model.r, lty=1)
abline(model, lty=2)
legend("topleft", legend=c("Robuste","Moindres carr\u00e9s"),lty=1:2)

shapiro.test(residuals(model.r))

#page 275
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

#page 276
shapiro.test(residuals(model2))

#page 277
confint(model2)

anova(model2)

library(ModStatR)
my.confidence.region(model2, which=1)
my.confidence.region(model2, which=2)
my.confidence.region(model2, which=3)

#page 278
set.seed(314)
model2.r<-lqs(masse~taille+I(taille^2),data=Mes.B)
rbind(coefficients(model2),coefficients(model2.r))

#page 279
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

#page 279-280
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

#page 280
data("mtcars")
View(mtcars)

help(mtcars)

#page 281
head(mtcars,n=10)
?mtcars
str(mtcars)

#page 282
mtcars2 <- within(mtcars, { 
  vs <- factor(vs, labels = c("V", "S")) 
  am <- factor(am, labels = c("automatic", "manual")) 
  cyl <- ordered(cyl) 
  gear <- ordered(gear) 
  carb <- ordered(carb) 
  })
summary(mtcars2)

subsetmtcars<-mtcars[,c(1,3,4,5,6,7)]

#page 283
library(MVN)
result1 = mvn(data = subsetmtcars, 
              mvnTest = "mardia", 
              univariateTest = "SW", univariatePlot = "histogram", 
              multivariatePlot = "qq", 
              multivariateOutlierMethod = "adj", 
              showOutliers = TRUE, showNewData = TRUE)
result1$multivariateNormality

result1$multivariateOutliers

#page 

#page 

#page 

#page 

#page 

#page 

#page 




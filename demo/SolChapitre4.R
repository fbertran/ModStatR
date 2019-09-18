#' ---
#' title: "Mod\u00e9lisation statistique par la pratique avec R"
#' subtitle: "Chapitre 4 : Analyse de r\u00e9gression - solution des exercices"
#' author: "Fr\u00e9d\u00e9ric Bertrand, Emmanuelle Claeys, Myriam Maumy-Bertrand"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---

#Chapitre 4 : solution des exercices

#Complement en ligne
#Exercice 4.4
#q1
CancerSein <- read.csv("https://tinyurl.com/y3l6sh59")

#q2
layout(c(1,2))
plot(Survie~Traitement, data=CancerSein)
plot(Age~Traitement,data=CancerSein)
layout(1)
tapply(CancerSein$Age,CancerSein$Traitement,mean)
aov1 <- aov(Survie~Traitement,data=CancerSein)
shapiro.test(residuals(aov1))
library(car)
bartlett.test(residuals(aov1),CancerSein$Traitement)
summary(aov1)
               
#Exercice 4.5
#q1
SidaChat <- read.csv("https://tinyurl.com/yxe6yxem")

#q2
layout(matrix(c(1,1,2,3),byrow=T,nrow=2))
plot(LnT4~Jours, data=SidaChat)
plot(LnT4~Sexe, data=SidaChat)
plot(Jours~Sexe, data=SidaChat)
layout(1)

#Exercice 4.6
#q1
chal <- read.csv("https://tinyurl.com/yyb3cztf")

#q2
plot(Defaillance~Temperature,data=chal)
cdplot(as.factor(Defaillance)~Temperature, data=chal, 
       ylab="Defaillance")

#q3
chal.glm<-glm(Defaillance~Temperature,data=chal,family="binomial")
if(!require("hnp")){install.packages("hnp")}
hnp(chal.glm, sim = 99, conf = 0.95)
summary(chal.glm)
anova(chal.glm,test = "Chisq")
ggiraphExtra::ggPredict(chal.glm)

#q4
confint(chal.glm, parm="Temperature")
exp(coef(chal.glm))["Temperature"]
exp(confint(chal.glm, parm="Temperature"))

#q5
predict(chal.glm,newdata = list(Temperature=31),type = "response")

#q6
if(!require(rms)){install.packages("rms")}
library(rms)
chal.lrm <- lrm(Defaillance~Temperature,data=chal,x=TRUE,y=TRUE)
print(chal.lrm)
residuals(chal.lrm, "gof")

#Exercice 4.7
#q1
read.csv("https://tinyurl.com/y3shxcsd")

#Exercice 4.8
#q1
read.csv("https://tinyurl.com/y4w2qv9t")

#Exercice 4.9
#q1
read.csv("https://tinyurl.com/y4deakfd")

#Exercice 4.10
#q1
poly <- read.csv("https://tinyurl.com/yyhhcw37")

#q1
poly_glm1 <- glm(nombre~traitement+age,family=poisson(),data=poly)
library(hnp)
hnp(poly_glm1, sim = 99, conf = 0.95)
summary(poly_glm1)
confint(poly_glm1)
poly_glm2 <- glm(nombre~traitement+age,family=quasipoisson(), 
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


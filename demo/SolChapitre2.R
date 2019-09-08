#' ---
#' title: "Mod\u00e9lisation statistique par la pratique avec R"
#' subtitle: "Chapitre 2 : Mesures de liaison - solution des exercices"
#' author: "Fr\u00e9d\u00e9ric Bertrand, Emmanuelle Claeys, Myriam Maumy-Bertrand"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---

#Chapitre 2 : solution des exercices

library(BioStatR)

data("Quetelet")
Quetelet$BMI=with(Quetelet,poids/(taille/100)^2)

library(MVN)
mvn(Quetelet[,-1], mvnTest = "mardia",
    univariateTest = "SW", univariatePlot = "histogram",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj", 
    showOutliers = TRUE, showNewData = TRUE)

Quetelet_H=subset(Quetelet,subset=Quetelet$sexe=="h")
Quetelet_F=subset(Quetelet,subset=Quetelet$sexe=="f")

mvn(Quetelet_H[,-1], mvnTest = "mardia",
    univariateTest = "SW", univariatePlot = "histogram",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj", 
    showOutliers = TRUE, showNewData = TRUE)
mvn(Quetelet_F[,-1], mvnTest = "mardia",
    univariateTest = "SW", univariatePlot = "histogram",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj", 
    showOutliers = TRUE, showNewData = TRUE)

cor(Quetelet[,-1], method="spearman")

library(corrplot)
cor.mtest(Quetelet[,-1], method="spearman")
#d'apres la formule du BMI, mauvais signe pour cor BMI et taille

library(ppcor)
pcor(Quetelet[,-1], method = "spearman")
#d'apres la formule du BMI, bon signe pour cor BMI et taille


cor(Quetelet[,-1], method="kendall")
cor.mtest(Quetelet[,-1], method="kendall")
#d'apres la formule du BMI, mauvais signe pour cor BMI et taille
pcor(Quetelet[,-1], method = "kendall")
#d'apres la formule du BMI, bon signe pour cor BMI et taille


cor(Quetelet_H[,-1], method="spearman")
cor.mtest(Quetelet_H[,-1], method="spearman")
#d'apres la formule du BMI, mauvais signe pour cor BMI et taille
pcor(Quetelet_H[,-1], method = "spearman")
#d'apres la formule du BMI, bon signe pour cor BMI et taille


cor(Quetelet_H[,-1], method="kendall")
cor.mtest(Quetelet_H[,-1], method="kendall")
#d'apres la formule du BMI, mauvais signe pour cor BMI et taille
pcor(Quetelet_H[,-1], method = "kendall")
#d'apres la formule du BMI, bon signe pour cor BMI et taille


cor(Quetelet_F[,-1], method="spearman")
cor.mtest(Quetelet_F[,-1], method="spearman")
#d'apres la formule du BMI, mauvais signe pour cor BMI et taille
pcor(Quetelet_F[,-1], method = "spearman")
#d'apres la formule du BMI, bon signe pour cor BMI et taille


cor(Quetelet_F[,-1], method="kendall")
cor.mtest(Quetelet_F[,-1], method="kendall")
#d'apres la formule du BMI, mauvais signe pour cor BMI et taille
pcor(Quetelet_F[,-1], method = "kendall")
#d'apres la formule du BMI, bon signe pour cor BMI et taille

library(GGally)
ggpairs(Quetelet)
#Pour sauvegarder le graphique enlever les commentaires des trois commandes ci-dessous
# pdf("ggpairsBMI.pdf")
# print(ggpairs(Quetelet))
# dev.off()

#generation de donnees de type Ecole
library(mvtnorm)
sigma <- matrix(c(4,-.5,2.5,-.5,4,3,2.5,3,4),byrow=TRUE, ncol=3)
sigma

library(corpcor)
pcor2cor(cov2cor(sigma))
aaa=rmvnorm(n=119, mean=c(11,11,14), sigma=pcor2cor(cov2cor(sigma)))
cor(aaa[,2],(aaa[,3]-5)/10)
bbb=aaa
bbb[,3]<-(aaa[,3]-1)
bbb[,1]<-((aaa[,1])-7)/6*17
bbb[,2]<-((aaa[,2])-7)/6*17

round(aaa,digits = 2)
round(bbb,digits = 2)
boxplot(bbb)

colnames(bbb) <- c("Maths","Sport","Age")
bbb <- data.frame(bbb)

ccc <- round(bbb, 2)
ccc <- data.frame(ccc)
#Pour sauvegarder les jeux de donnees enlever les commentaires des deux commandes ci-dessous
# write.csv(ccc,file="Ecole3.csv",row.names = FALSE)
# write.csv(ccc[,1:2],file="Ecole2.csv",row.names = FALSE)

mvn(ccc, mvnTest = "mardia",
    univariateTest = "SW", univariatePlot = "histogram",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj", 
    showOutliers = TRUE, showNewData = TRUE)
mvn(ccc[,1:2], mvnTest = "mardia",
    univariateTest = "SW", univariatePlot = "histogram",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj", 
    showOutliers = TRUE, showNewData = TRUE)


mvn(read.csv("https://tinyurl.com/y2c68uvw"), mvnTest = "mardia",
    univariateTest = "SW", univariatePlot = "histogram",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj", 
    showOutliers = TRUE, showNewData = TRUE)

mvn(read.csv("https://tinyurl.com/y2asrzgk"), mvnTest = "mardia",
    univariateTest = "SW", univariatePlot = "histogram",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj", 
    showOutliers = TRUE, showNewData = TRUE)

cor(ccc)
cor.mtest(ccc)

pcor(ccc)

ggpairs(ccc)
res1=residuals(lm(Maths~Age,data=ccc))
res2=residuals(lm(Sport~Age,data=ccc))
plot(res1,res2)


cor(ccc, method="spearman")
cor.mtest(ccc, method="spearman")

pcor(ccc, method = "spearman")


cor(ccc, method="kendall")
cor.mtest(ccc, method="kendall")

pcor(ccc, method = "kendall")


ggpairs(ccc)

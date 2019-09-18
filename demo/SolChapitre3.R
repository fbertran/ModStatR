#' ---
#' title: "Mod\u00e9lisation statistique par la pratique avec R"
#' subtitle: "Chapitre 3 : Analyse exploratoire des donn\u00e9es - solution des exercices"
#' author: "Fr\u00e9d\u00e9ric Bertrand, Emmanuelle Claeys, Myriam Maumy-Bertrand"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---

#Chapitre 3 : solution des exercices

#Complement en ligne
#Exercice 3.1
d_hotels <- read.csv("https://tinyurl.com/y3rxbxoo")
head(d_hotels)
library(GGally)
ggparcoord(d_hotels, columns = 3:ncol(d_hotels), groupColumn=2, 
           scale = "std", boxplot = TRUE, alphaLines = 0.5)+ facet_grid(
             rows=vars(PAYS))+ theme(axis.text.x=element_text(angle=45,hjust=1,
                                                              vjust=1),legend.position="")

#Exercice 3.2
d_pres2002<-read.csv("https://tinyurl.com/yyoowvkl",row.names=1)
head(d_pres2002[,1:3])
mosaicplot(d_pres2002, type = "pearson", shade = TRUE, las = 2, 
           main = "Associations et r\u00e9sidus du test du chi2")

d_pres2007 <- read.csv("https://tinyurl.com/yyolq665",row.names=1)
head(d_pres2007[,1:8])
mosaicplot(d_pres2007, type = "pearson", shade = TRUE, las = 2, 
           main = "Associations et r\u00e9sidus du test du chi2")

#q1
data(UCBAdmissions)
str(UCBAdmissions)
mosaicplot(UCBAdmissions)
library(vcd)
assoc(UCBAdmissions)

#q2
library(FactoMineR) 
try(MCA(UCBAdmissions))
?MCA

#q3
library(DescTools)
UCBA.df <- Untable(UCBAdmissions)
head(UCBA.df)

str(UCBA.df)

MCA(UCBA.df, graph=FALSE)

#Exercice 3.4
d_wow <- read.csv("https://tinyurl.com/y5gffvsb", row.names =1)
head(d_wow[,1:3])
d.d_wow <- dist(d_wow[,8:12])
wow.cah.ward <- hclust(d.d_wow, method="ward.D2")
library(ggdendro)
ggdendrogram(wow.cah.ward, labels = FALSE)

#Exercice 3.5
d_hotels <- read.csv("https://tinyurl.com/y3rxbxoo", row.names=1)
head(d_hotels)
d.d_hotels <- dist(d_hotels[,2:7])
hotels.cah.ward <- hclust(d.d_hotels, method="ward.D2")
ggdendrogram(hotels.cah.ward)

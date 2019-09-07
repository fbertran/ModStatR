#' ---
#' title: "Mod\u00e9lisation statistique par la pratique avec R"
#' subtitle: "Chapitre 3 : Analyse exploratoire des donn\u00e9es"
#' author: "Fr\u00e9d\u00e9ric Bertrand, Emmanuelle Claeys, Myriam Maumy-Bertrand"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---

#Chapitre 3

#Analyse en composantes principales
#page 141
#q1
d_macdo<-read.csv("https://tinyurl.com/y3qobgsd")

#q2
str(d_macdo)

#q3
summary(d_macdo)

#page 143
library(GGally)

ggparcoord(d_macdo, columns = 3:ncol(d_macdo), groupColumn=1, 
             scale = "std", boxplot = TRUE, alphaLines = 0.5)+facet_grid( 
               rows=vars(Category))+ theme(axis.text.x=element_text(angle=45, 
             hjust=1,vjust=1),legend.position="top")

if(!require(ggiraphExtra)){install.packages("ggiraphExtra")}
library(ggiraphExtra)
ggRadar(d_macdo[,-c(2,3)],aes(facet=Category))

#page 145
#q4
cor(d_macdo[,-c(1,2,3)])[1:3,1:3]

#page 146
#q5
tmp_d_macdo <- d_macdo
p_ <- GGally::print_if_interactive
plotList <- list()
plotList[[1]] <- ggally_smooth_lm(tmp_d_macdo, mapping = 
                                    ggplot2::aes(x = Calories, y = Total.Fat))
plotList[[2]] <- ggally_smooth_loess(tmp_d_macdo, mapping = 
                                       ggplot2::aes(x = Calories, y = Total.Fat))
pm <- ggmatrix(plotList, 1, 2, c("Ajustement lin\u00e9aire", 
                                 "R\u00e9gression polynomiale locale"), byrow = TRUE)
p_(pm)

ggduo(tmp_d_macdo, c("Calories"), c("Total.Fat"), 
      types = list(continuous = "smooth_loess"))

#page 147
pdf("ggduo.pdf")
ggduo(tmp_d_macdo, c("Calories"), c("Total.Fat"), 
      types = list(continuous = "smooth_loess"))
dev.off()

#q6
library(MVN)
result = mvn(data = d_macdo[,c("Calories","Total.Fat")], 
             mvnTest = "mardia", 
             univariateTest = "SW", univariatePlot = "histogram", 
             multivariatePlot = "qq", 
             multivariateOutlierMethod = "adj", 
             showOutliers = TRUE, showNewData = TRUE)
result$multivariateNormality

#page 148
d_macdo[83,c("Item","Calories","Total.Fat")]

sort(d_macdo$Calories,decreasing = TRUE)[1:5]

#page 149
sort(d_macdo$Total.Fat,decreasing = TRUE)[1:5]

stem(d_macdo$Total.Fat)

d_macdo$Item[as.numeric(rownames(result$multivariateOutliers))]

#page 150
library(GGally)
tmp_d_macdo <- d_macdo
tmp_d_macdo$indic_outlier <- as.factor(1:nrow(d_macdo) 
      %in% as.numeric(rownames(result$multivariateOutliers)))
colnumbers <- which(colnames(tmp_d_macdo) %in% 
                      c("Calories", "Total.Fat"))
ggscatmat(tmp_d_macdo, columns = colnumbers, color= 
            "indic_outlier")

#page 151
d_macdo1 <- d_macdo[-(83),]
dim(d_macdo1)

result1 = mvn(data = d_macdo1[,c("Calories","Total.Fat")], 
              mvnTest = "mardia", 
              univariateTest = "SW", univariatePlot = "histogram", 
              multivariatePlot = "qq", 
              multivariateOutlierMethod = "adj", 
              showOutliers = TRUE, showNewData = TRUE)
result1$multivariateNormality

#q7
library(pspearman)
spearman.test(d_macdo[,"Calories"],d_macdo[,"Total.Fat"])

#page 152
library(coin)
set.seed(1133)
spearman_test(d_macdo[,"Calories"]~d_macdo[,"Total.Fat"], 
              distribution=approximate(999999))

cor.test(d_macdo[,"Calories"],d_macdo[,"Total.Fat"], 
         method = "kendall")

#page 153
#q8
library(jmuOutlier)
set.seed(1133)
perm.cor.test(d_macdo[,"Calories"],d_macdo[,"Total.Fat"])

#q9
library(ModStatR)
set.seed(1133)
r_c_mdo <- perm.cor.mtest(d_macdo[,c("Calories","Total.Fat", 
                                     "Cholesterol","Sodium","Sugars","Protein")], num.sim = 50000)
lapply(r_c_mdo, round, 4)

#page 154
r_c_mdo$p < .05/(6*5/2)

source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(d_macdo[,c("Calories","Total.Fat", 
                         "Cholesterol","Sodium","Sugars","Protein")])$r

#page 155
#q10
library(rgl)

#q11
plot3d(d_macdo$Calories,d_macdo$Total.Fat, d_macdo$Cholesterol,type="s")

#page 156
#q12
list <- c("Calories", "Total.Fat", "Cholesterol")
d_macdo.cr <- scale(d_macdo[, list])
lims <- c(min(d_macdo.cr),max(d_macdo.cr))
plot3d(d_macdo.cr, type = "s", xlim = lims, 
       ylim = lims,zlim = lims)

#page 157
#q13
d_macdo.cr_df <- as.data.frame(d_macdo.cr)

plot3d(d_macdo.cr, type = "s", xlim = lims, 
       ylim = lims, zlim = lims)

plot3d(ellipse3d(cor(cbind(d_macdo.cr_df$Calories, 
       d_macdo.cr_df$Total.Fat,d_macdo.cr_df$Cholesterol))), col="grey",add=TRUE)

#page 159
#q 16
if(!require("ade4")){install.packages("ade4")}
library(ade4)
list <- setdiff(colnames(d_macdo), c("Category", "Item", 
                                     "Serving.Size"))
macdo.acp <- dudi.pca(d_macdo[, list], center=TRUE, 
                      scale=TRUE, scannf = FALSE, nf = 3)
names(macdo.acp)

#q17
macdo.acp$cw

head(macdo.acp$lw)

#q18
round(macdo.acp$eig,3)

sum(macdo.acp$eig)

#page 161
#q19
round(pve <- 100*macdo.acp$eig/sum(macdo.acp$eig),3)
round(cumsum(pve),2)
round(cumsum(pve),2)[3:4]

#page 162
#q20
screeplot(macdo.acp)
if(!require("factoextra")){install.packages("factoextra")}
library("factoextra")
fviz_eig(macdo.acp)

inertia.dudi(macdo.acp)

#page 163
#q21
s.corcircle(macdo.acp$co, xax=1, yax=2)

#q22
round(inertia.dudi(macdo.acp, col.inertia = TRUE)$col.abs,4)

#page 164
round(inertia.dudi(macdo.acp, col.inertia = TRUE)$col.rel, 4)

#page 165
round(macdo.acp$co,4)

#page 166
#q25
round(get_pca_var(macdo.acp)$cos2, 4)

#q26
round(sort(rowSums(get_pca_var(macdo.acp)$cos2[,1:2])), 4)

#page 167
fviz_pca_var(macdo.acp, col.var="contrib", gradient.cols= 
               c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#page 168
fviz_pca_var(macdo.acp, col.var = "cos2", gradient.cols = 
               c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#page 169
fviz_pca_var(macdo.acp, axes = c(1, 3), col.var="contrib", 
             gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(macdo.acp, axes = c(1, 3), col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#q27
s.label(macdo.acp$li, xax = 1, yax = 2)

#page 170
s.label(macdo.acp$li, label=as.character(d_macdo$Item), clabel=0.5)

library(yarrr)
cont_ind <- get_pca_ind(macdo.acp)$contrib
pirateplot(data=cont_ind,point.o=.75, ylab="Contribution",xlab="")

#page 171
rownames(cont_ind) <- tmp_d_macdo$Item
round(sort(cont_ind[,1],decreasing=TRUE)[1:20], 4)

round(sort(cont_ind[,2],decreasing=TRUE)[1:20], 4)

round(sort(cont_ind[,3],decreasing=TRUE)[1:20], 4)

names(sort(cont_ind[cont_ind[,1]>100/260*5,1],decreasing=TRUE))

#page 172
names(sort(cont_ind[cont_ind[,2]>100/260*5,2],decreasing=TRUE))

names(sort(cont_ind[cont_ind[,3]>100/260*5,3],decreasing=TRUE))

fviz_pca_ind(macdo.acp, geom = c("point"), col.ind = 
               "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_pca_ind(macdo.acp, geom = c("point"), col.ind = 
               "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#page 174
fviz_pca_ind(macdo.acp, geom = c("point"), alpha.ind = "contrib")
fviz_pca_ind(macdo.acp, geom = c("point"), alpha.ind = "cos2")

#q(28)
scatter(macdo.acp)

#q29
gcol <- hcl.colors(9, palette = "Dynamic")
s.class(dfxy = macdo.acp$li, fac = d_macdo$Category, 
        col = gcol, xax = 1, yax = 2)

#page 176
#q30
fviz_pca_biplot(macdo.acp, col.ind ="contrib", col.var ="contrib")
fviz_pca_biplot(macdo.acp, col.ind = "cos2", col.var = "cos2")

if(!require("adegraphics")){install.packages("adegraphics")}
list <- setdiff(colnames(d_macdo), c("Category", "Item", 
                                     "Serving.Size"))
tmp_macdo=list()
tmp_macdo$Category=d_macdo[, "Category"]
tmp_macdo$tab=d_macdo[, list]
macdo.acp.dudi <- dudi.pca(tmp_macdo$tab, center = TRUE, scale = 
                             FALSE, scan = FALSE, nf = 3)

#page 177
macdo.acp.dudi1 <- dudi.pca(tmp_macdo$tab, center = TRUE, scale = 
                              TRUE, scan = FALSE, nf = 3)
g1 <- s.class(macdo.acp.dudi$li, tmp_macdo$Category, plot = FALSE)
g2 <- s.arrow(macdo.acp.dudi$c1, lab = names(macdo.acp$tab), 
              plot = FALSE)
g3 <- s.class(macdo.acp.dudi1$li, tmp_macdo$Category, plot = FALSE)
g4 <- s.corcircle(macdo.acp.dudi1$co, lab = names(macdo.acp$tab), 
                  full = FALSE, plot = FALSE)
G1 <- rbindADEg(cbindADEg(g1, g2, plot = FALSE), cbindADEg(g3, g4, 
                  plot = FALSE), plot = TRUE)

#page 178
head(sort(apply(d_macdo[,-(1:3)],2,var),decreasing=TRUE))



#Analyse factorielle des correspondances
#page 178
d_vac<-read.csv2("https://tinyurl.com/y3emuylu", row.names = 1)
rownames(d_vac)

#page 179
str(d_vac)

barplot(t(d_vac),beside=TRUE, col = hcl.colors(8, palette = 
        "Dynamic"), legend.text = colnames(d_vac), args.legend = 
        list(bg = "white", x = "topleft"))

#page 180
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)

my_cols <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF", 
             "#E16462FF", "#FCA636FF", "#F0F921FF")
ggballoonplot(d_vac, fill = "value") + scale_fill_gradientn( 
  colors = my_cols)

library(ade4)
table.cont(d_vac)

#page 181
sum(d_vac)

khi.test.d_vac <- chisq.test(d_vac)
round(khi.test.d_vac$expected, 2)

khi.test.d_vac

#page 182
set.seed(1133)
chisq.test(d_vac, simulate.p.value = TRUE, B=50000)

mosaicplot(d_vac, type = "pearson", shade = TRUE, las = 2, 
           main = "Associations et r\u00e9sidus du test du chi2")

mosaicplot(t(d_vac), type = "pearson", shade = TRUE, las = 2, 
           main = "Associations et r\u00e9sidus du test du chi2")

#page 183
if(!require("vcd")){install.packages("vcd")}
library(vcd)

d_vactable <- as.table(as.matrix(d_vac))
assoc(d_vactable, shade=TRUE, las=2, varnames=F, rot_labels=90, 
main="Associations et r\u00e9sidus du test du test du chi2", 
labeling_args=list(abbreviate=c(A=TRUE)))


#page 184
if(!require(FactoMineR)){install.packages("FactoMineR")}
library(FactoMineR)

(res.ca.d_vac<-CA(d_vac, ncp=4, graph=FALSE))

#page 185
library(factoextra)
round(eig.val <- get_eigenvalue(res.ca.d_vac), 3)

#page 186
as.numeric(colSums(eig.val)[1])

sqrt(as.numeric(khi.test.d_vac$statistic)/sum(d_vac)/ 
       (min(nrow(d_vac),ncol(d_vac))-1))

if(!require(questionr)){install.packages("questionr")}
library(questionr)
cramer.v(d_vac)

if(!require(DescTools)){install.packages("DescTools")}
library(DescTools)
CramerV(d_vac)

Phi(d_vac)

#page 187
CramerV(d_vac, conf.level = .95)

d_vac.tab <- as.table(as.matrix(d_vac))
d_vac.frm <- Untable(d_vac.tab)
head(d_vac.frm)

n <- 10000
set.seed(1133)
idx <- matrix(sample(nrow(d_vac.frm), size=nrow(d_vac.frm) * n, 
                     replace=TRUE), ncol=n, byrow=FALSE)
v <- apply(idx, 2, function(x) CramerV(d_vac.frm[x,1], 
                                       d_vac.frm[x,2]))
if(!require(lattice)){install.packages("lattice")}
lattice::bwplot(v)

quantile(v, probs=c(0.025,0.975))

#page 189
set.seed(1133)
idx.perm <- replicate(n,sample(nrow(d_vac.frm), replace=FALSE))
v.perm <- apply(idx.perm, 2, function(x) CramerV(d_vac.frm[,1], 
                                                 d_vac.frm[x,2]))
hist(v.perm, xlim=c(0,CramerV(d_vac)))
abline(v=CramerV(d_vac), lwd=2, col="red")
mean(v.perm>=CramerV(d_vac))

library(factoextra)
fviz_eig(res.ca.d_vac)

#page 190
mean(res.ca.d_vac$eig[,1])
res.ca.d_vac$eig[,2]>1/(ncol(d_vac)-1)*100

fviz_ca_row(res.ca.d_vac)
fviz_ca_col(res.ca.d_vac) 
fviz_ca_biplot(res.ca.d_vac)

#page 191
fviz_ca_row(res.ca.d_vac)
fviz_ca_row(res.ca.d_vac, col.row="contrib")
fviz_ca_row(res.ca.d_vac, col.row="cos2")
fviz_ca_row(res.ca.d_vac, alpha.row="contrib")
fviz_ca_row(res.ca.d_vac, alpha.row="cos2")
fviz_ca_col(res.ca.d_vac)
fviz_ca_col(res.ca.d_vac, col.col="contrib")
fviz_ca_col(res.ca.d_vac, col.col="cos2")
fviz_ca_col(res.ca.d_vac, alpha.col="contrib")
fviz_ca_col(res.ca.d_vac, alpha.col="cos2")
fviz_ca_biplot(res.ca.d_vac)
fviz_ca_biplot(res.ca.d_vac, col.row="contrib", col.col="contrib")
fviz_ca_biplot(res.ca.d_vac, col.row="cos2", col.col="cos2")

#page 192
rowpr <- fviz_ca_biplot(res.ca.d_vac, map="rowprincipal", arrow = 
                          c(TRUE, TRUE), repel=TRUE)
colpr <- fviz_ca_biplot(res.ca.d_vac, map="colprincipal", arrow = 
                          c(TRUE, TRUE), repel=TRUE)
library(GGally)
ggmatrix(list(rowpr, colpr),1,2)


#Analyse non symetrique des correspondances
#page 193
d_TM<-read.csv2("https://tinyurl.com/y55e3k9y", row.names = 1)
rownames(d_TM)

str(d_TM)

#page 194
barplot(t(d_TM), beside=TRUE, names=d_TM$Task, col = c("red", 
        "green","blue","purple"), legend.text = colnames(d_TM), 
        args.legend = list(bg = "white", x = "top"))

#page 195
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
my_cols <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF", 
             "#E16462FF", "#FCA636FF", "#F0F921FF")
ggballoonplot(d_TM, fill = "value") + scale_fill_gradientn(
  colors = my_cols)

library(ade4)
table.cont(d_TM)

sum(d_TM)

#page 196
if(!require(DescTools)){install.packages("DescTools")}
library(DescTools)
Lambda(d_TM)

Lambda(d_TM, conf.level=0.95)

#page 197
Lambda(d_TM, direction="row", conf.level=0.95)

Lambda(d_TM, direction="column", conf.level=0.95)

d_TM.tab <- as.table(as.matrix(d_TM))
d_TM.frm <- Untable(d_TM.tab)
head(d_TM.frm)

n <- 10000
set.seed(1133)
idx <- matrix(sample(nrow(d_TM.frm), size=nrow(d_TM.frm) * n, 
                     replace=TRUE), ncol=n, byrow=FALSE)
lR.d_TM <- apply(idx, 2, function(x) Lambda(d_TM.frm[x,1], 
                            d_TM.frm[x,2],direction = "row"))
lC.d_TM <- apply(idx, 2, function(x) Lambda(d_TM.frm[x,1], 
                            d_TM.frm[x,2],direction = "column"))
l.d_TM <- apply(idx, 2, function(x) Lambda(d_TM.frm[x,1], 
                            d_TM.frm[x,2]))
lattice::bwplot(lR.d_TM)
lattice::bwplot(lC.d_TM)
lattice::bwplot(l.d_TM)

#page 198
quantile(lR.d_TM, probs=c(0.025,0.975))

quantile(lC.d_TM, probs=c(0.025,0.975))

quantile(l.d_TM, probs=c(0.025,0.975))

#page 198-199
n <- 10000; set.seed(1133)
idx.perm <- replicate(n,sample(nrow(d_TM.frm), replace=FALSE))
lR.perm.d_TM <- apply(idx.perm, 2, function(x) Lambda(d_TM.frm[,1], 
                      d_TM.frm[x,2], direction = "row"))
lC.perm.d_TM <- apply(idx.perm, 2, function(x) Lambda(d_TM.frm[,1], 
                      d_TM.frm[x,2], direction = "column"))
l.perm.d_TM <- apply(idx.perm, 2, function(x) Lambda(d_TM.frm[,1], 
                      d_TM.frm[x,2]))
hist(lR.perm.d_TM, xlim=c(0,Lambda(d_TM, direction = "row")))
abline(v=Lambda(d_TM, direction = "row"), lwd=2, col="red")
hist(lC.perm.d_TM, xlim=c(0,Lambda(d_TM, direction = "column")))
abline(v=Lambda(d_TM, direction = "column"), lwd=2, col="red")
hist(l.perm.d_TM, xlim=c(0,Lambda(d_TM)))
abline(v=Lambda(d_TM), lwd=2, col="red")
mean(lR.perm.d_TM>=Lambda(d_TM))
mean(lC.perm.d_TM>=Lambda(d_TM))
mean(l.perm.d_TM>=Lambda(d_TM))
                                                        
GoodmanKruskalTau(d_TM.tab, direction="column", conf.level=0.95)

#page 200
GoodmanKruskalTau(d_TM.tab, direction="row", conf.level=0.95)

library(ade4)
(res.nsc.d_TM <- dudi.nsc(d_TM, scan = FALSE))

#page 201
library(adegraphics)
g1 <- s.label(res.nsc.d_TM$c1, plab.cex = 1.25)
g2 <- s.arrow(res.nsc.d_TM$li, add = TRUE, plab.cex = 0.75)

#Analyse des correspondances multiples
#page 202
poke<-read.csv("https://tinyurl.com/y4y6a86m",na.strings= c("","NA"))
poke<-as.data.frame(poke)
poke$Generation<-as.factor(poke$Generation)
summary(poke)
poke.x<-poke[,c(3,12,13)]

#page 
library(ade4); library(adegraphics)
res.acm.poke<-dudi.acm(poke.x,scannf=FALSE)

min(nrow(poke.x) - 1, nlevels(poke$Type.1) + nlevels(
  poke$Generation) + nlevels(poke$Legendary) - ncol(poke.x))

fviz_screeplot(res.acm.poke)

get_eig(res.acm.poke)

#page 204
res.acm.poke$cr

#page 205
score(res.acm.poke, xax=1)
score(res.acm.poke, xax=1, type = "boxplot")
boxplot(res.acm.poke)
ade4::s.corcircle(res.acm.poke$co, clabel = 0.7)

#page 206
library(devtools)
if(!require(JLutils)){install_github("larmarange/JLutils")}
library(JLutils)
s.freq(res.acm.poke$li)

library(factoextra)
fviz_mca_biplot(res.acm.poke)
scatter(res.acm.poke)

#Analyse factorielle des donnees mixtes
#page 208
if(!require("PCAmixdata")){install.packages("PCAmixdata")}
library(PCAmixdata)

round(cor(poke[,c(7,8,9,10,11)]),2)

mix.poke<-PCAmix(subset(poke,select=7:11),subset(poke,select=3))

#page 209
round(mix.poke$eig, 2)

round(mix.poke$categ.coord, 2)

#Classification ascendante hierarchique et methode des K-moyennes
#page 211
data_event <- read.csv("https://tinyurl.com/y2k7mwbr")
head(data_event)

#page 212
list_col <- c("sort_order","time","event_team","fthg","odd_h")
data_event.x <- data_event[,list_col]

#page 213
data_event.c<-aggregate(.~event_team,data=data_event.x,FUN=mean)
str(data_event.c)

rownames(data_event.c) <- data_event.c$event_team
data_event.c$event_team <- NULL
head(data_event.c)

library(GGally)
ggpairs(data_event.c)

#page 215
d.data_event <- dist(data_event.c)
cah.ward <- hclust(d.data_event, method="ward.D2")
plot(cah.ward, xlab="\u00e9quipe de football", ylab="", 
     main="Dendrogramme", sub="", axes=TRUE, cex=0.5)
if(!require(ggdendro)){install.packages("ggdendro")}
library(ggdendro)
ggdendrogram(cah.ward, rotate = FALSE, size = 2)

#page 216
library(JLutils)
best.cutree(cah.ward, min = 3, graph = TRUE, xlab = 
              "Nombre de classes", ylab = "Inertie relative")

#page 217
(groupes.cah <- cutree(cah.ward,k=5))
table(groupes.cah)
plot(cah.ward, xlab="\u00e9quipe de football", ylab="", 
     main="Dendrogramme", sub="", axes=TRUE, cex=0.5)
rect.hclust(cah.ward, 5)

#page 218
library(factoextra)
hc.cut <- hcut(d.data_event, k = 5, hc_method = "complete")
fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex",data=d.data_event)

#page 220
if(!require("vegan")){install.packages("vegan")}
library(vegan)
k.event.cal <- cascadeKM(data_event.c, 3, 10, iter = 100, 
                         criterion = "calinski")
plot(k.event.cal)

#page 221
set.seed(1133)
groupes.kmeans <- kmeans(data_event.c,centers=5,nstart=1000)
print(groupes.kmeans)

#page 222
print(table(groupes.cah,groupes.kmeans=groupes.kmeans$cluster))

#page 223
library(FactoMineR)
res.pca <- PCA(data_event.c, ncp = 3, graph = FALSE)
get_eig(res.pca)
fviz_screeplot(res.pca)

res.hcpc <- HCPC(res.pca, graph = FALSE)
fviz_dend(res.hcpc, cex = 0.7, palette = "jco", rect = TRUE, 
          rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)

#page 224
fviz_cluster(res.hcpc, repel = TRUE, show.clust.cent = TRUE, 
             palette = "jco", ggtheme = theme_minimal(), main = "Factor map")

plot(res.hcpc, choice = "3D.map")

#page 225
#Exercice 3.1
read.csv("https://tinyurl.com/y3rxbxoo")

#Exercice 3.2
read.csv("https://tinyurl.com/yyoowvkl")
read.csv("https://tinyurl.com/yyolq665")

#page 226
#Exercice 3.4
read.csv("https://tinyurl.com/y5gffvsb")

#Exercice 3.5
read.csv("https://tinyurl.com/y3rxbxoo")











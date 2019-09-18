#' @title Fonction hypergéométrique de Gauss (hypergeo)
#'
#' @param a Paramètre pour Gauss2F1
#' @param b Paramètre pour Gauss2F1
#' @param c Paramètre pour Gauss2F1
#' @param x Argument principal, nombre réel ou complexe
#'
#' @return Valeur numérique, l'évaluation de la fonction hypergéométrique de Gauss pour les paramètres \code{a}, \code{b}, \code{c} et \code{x}
#' @export
#'
#' @examples
#' Gauss2F1(1/2,1/2,(40-2)/2,1-.75^2)
#' 
Gauss2F1 <- function(a,b,c,x){
  if(x>=0 & x<1){
  Re(hypergeo::hypergeo(a,b,c,x))
  }else{
      Re(hypergeo::hypergeo(c-a,b,c,1-1/(1-x))/(1-x)^b)
  }
}
Gauss2F1<-Vectorize(Gauss2F1, vectorize.args = "x")

#' @title Fonction hypergéométrique de Gauss (gsl)
#'
#' @param a Paramètre pour Gauss2F1
#' @param b Paramètre pour Gauss2F1
#' @param c Paramètre pour Gauss2F1
#' @param x Argument principal, nombre réel ou complexe
#'
#' @return Valeur numérique, l'évaluation de la fonction hypergéométrique de Gauss pour les paramètres \code{a}, \code{b}, \code{c} et \code{x}
#' @export
#'
#' @examples
#' Gauss2F1gsl(1/2,1/2,(40-2)/2,1-.75^2)
#' 
Gauss2F1gsl <- function(a,b,c,x){
  if(x>=0 & x<1){
    gsl::hyperg_2F1(a,b,c,x)
  }else{
    gsl::hyperg_2F1(c-a,b,c,1-1/(1-x))/(1-x)^b
  }
  }
Gauss2F1gsl<-Vectorize(Gauss2F1gsl, vectorize.args = "x")

#' @title Corrélation de Bravais-Pearson pour bootstrap ou permutation
#'
#' @param x un vecteur numérique
#' @param y un vecteur numérique
#' @param indices un vecteur d'indices de même longueur que x et y
#'
#' @return Valeur numérique, le coefficient de corrélation de Bravais-Pearson calculé pour la permutation des vecteurs \code{x} et \code{y} spécifiée par le vecteur \code{indices}
#' @export
#'
#' @examples
#' set.seed(1133)
#' rho(rnorm(30),rnorm(30),sample(30))
#' 
rho <- function(x, y, indices){
  restest <- cor.test(x[indices], y[indices],
                      method = c("pearson"))
  return(restest$estimate)
}

#' @title Densité de rho chapeau, estimateur du coefficient de corrélation de Bravais-Pearson
#'
#' @param rho Valeur en laquelle la densité est évaluée
#' @param rho_0 Valeur de référence pour le coefficient de corrélation de Bravais-Pearson
#' @param n Effectif de l'échantillon
#'
#' @return Valeur numérique, densité au point \code{rho} de l'estimateur, construit à partir d'un échantillon de taille \code{n}, du coefficient de corrélation de Bravais-Pearson, de valeur théorique égale à \code{rho0}, sous hypothèse de normalité multivariée
#' @export
#'
#' @examples
#' corrdist(.7,.8,30)
#' 
corrdist <- function (rho, rho_0, n) {
  y = (n-2)*gamma(n-1)*(1-rho_0^2)^((n-1)/2)*(1-rho^2)^((n-4)/2)
  y = y/(sqrt(2*pi)*gamma(n-1/2)*(1-rho_0*rho)^(n-3/2))
  y = y*Gauss2F1(1/2,1/2,(2*n-1)/2,(rho_0*rho+1)/2)
  return(y)
}

#' @title Approximation de la densité de rho chapeau, estimateur du coefficient de corrélation de Bravais-Pearson
#'
#' @param rho Valeur en laquelle la densité est évaluée
#' @param rho_0 Valeur de référence pour le coefficient de corrélation de Bravais-Pearson
#' @param n Effectif de l'échantillon
#'
#' @return Valeur numérique, approximation de la densité au point \code{rho} de l'estimateur, construit à partir d'un échantillon de taille \code{n}, du coefficient de corrélation de Bravais-Pearson, de valeur théorique égale à \code{rho0}, sous hypothèse de normalité multivariée
#' @export
#'
#' @examples
#' corrdistapprox(.7,.8,30)
#' 
corrdistapprox <- function (rho, rho_0, n) {
  y = (n-2)*gamma(n-1)*(1-rho_0^2)^((n-1)/2)*(1-rho^2)^((n-4)/2)
  y = y/(sqrt(2*pi)*gamma(n-1/2)*(1-rho_0*rho)^(n-3/2))
  y = y*(1+1/4*(rho_0*rho+1)/(2*n-1)+9/32*(rho_0*rho+1)^2/
           (2*n-1)/(2*n+1))
  return(y)
}

#' @title Meilleure approximation de la densité de rho chapeau, estimateur du coefficient de corrélation de Bravais-Pearson
#'
#' @param rho Valeur en laquelle la densité est évaluée
#' @param rho_0 Valeur de référence pour le coefficient de corrélation de Bravais-Pearson
#' @param n Effectif de l'échantillon
#'
#' @return Valeur numérique, approximation de la densité au point \code{rho} de l'estimateur, construit à partir d'un échantillon de taille \code{n}, du coefficient de corrélation de Bravais-Pearson, de valeur théorique égale à \code{rho0}, sous hypothèse de normalité multivariée
#' @export
#'
#' @examples
#' corrdistapprox2(.7,.8,30)
#' 
corrdistapprox2 <- function (rho, rho_0, n) {
  y = (n-2)*gamma(n-1)*(1-rho_0^2)^((n-1)/2)*(1-rho^2)^((n-4)/2)
  y = y/(sqrt(2*pi)*gamma(n-1/2)*(1-rho_0*rho)^(n-3/2))
  y = y*(
    1+1/4*(rho_0*rho+1)/(2*n-1)+ 
    9/32*(rho_0*rho+1)^2/(2*n+1)/(2*n-1)+ 
    75/128*(rho_0*rho+1)^3/((2*n+3)*(4*n^2-1))+ 
    3675/2048*(rho_0*rho+1)^4/((2*n+3)*(2*n+5)*(4*n^2-1))+ 
    59535/8192*(rho_0*rho+1)^5/((2*n+3)*(2*n+5)*(2*n + 7)*(4*n^2-1))
  )
  return(y)
}

#' @title Test exact du coefficient de corrélation de Bravais-Pearson avec une référence non nécessairement nulle
#' 
#' @param corobs Valeur observée du coefficient de corrélation de Bravais-Pearson
#' @param rho_0 Valeur de référence pour le coefficient de corrélation de Bravais-Pearson
#' @param n Effectif de l'échantillon
#'
#' @return Valeur numérique, p$valeur calculée de manière exacte du test avec la référence \code{rho0} du coefficient de corrélation de Bravais-Pearson sous hypothèse de normalité multivariée
#' @export
#'
#' @examples
#' ref.cor.test(corobs=.7,rho_0=.8,n=30)
#' 
ref.cor.test <- function(corobs, rho_0, n){
  if(corobs<rho_0){
    return(2*integrate(corrdist, lower=-1, upper=corobs, 
                       rho_0=rho_0, n=n)$value)
  }
  if(corobs>rho_0){
    return(2*integrate(corrdist, lower=corobs, upper=1,
                       rho_0=rho_0, n=n)$value)
  }
  if(corobs==rho_0){
    return(1)
  }
}


#' @title Test exact matriciel du corrélation de Bravais-Pearson avec une référence non nécessairement nulle
#' 
#' @param mat Matrice des données
#' @param matrho_0 Matrice des valeurs de référence pour chacun des coefficients de corrélation de Bravais-Pearson
#'
#' @return Liste comportant trois matrices : la matrice des p-valeurs, la matrice des coefficients de corrélations observés et la matrice des effectifs ayant servis au calcul de ces coefficients de corrélation
#' @export
#'
#' @examples
#' data(Mesures5,package="BioStatR")
#' Mes5_red_lr = subset(Mesures5[,-5],subset=Mesures5$espece=="laurier rose")
#' ref.cor.mtest(Mes5_red_lr[,c("masse","taille","masse_sec")],0.7)
#' 
ref.cor.mtest = function (mat, matrho_0) 
{ 
  mat <- as.matrix(mat)
  n <- ncol(mat)
  if(is.vector(matrho_0) & length(matrho_0)==1){matrho_0= 
    matrix(matrho_0,nrow=n,ncol=n)} 
  p.mat <- cor.mat <- nval.mat <- matrix(NA, n, n) 
  diag(p.mat) <- 0 
  diag(cor.mat) <- 1 
  diag(nval.mat) <- colSums(!is.na(mat)) 
  for (i in 1:(n-1)) { 
    for (j in (i+1):n) { 
      cor.mat[i,j] <- cor.mat[j,i] <- cor(x = mat[,i], 
                                          y = mat[,j],method="pearson",use="pairwise.complete.obs") 
      nval.mat[i,j]<- nval.mat[j,i]<- nrow(na.omit(mat[,c(i,j)])) 
      p.mat[i,j]<- p.mat[j,i]<- ref.cor.test(corobs=cor.mat[i,j], 
                                             rho_0=matrho_0[i,j], n=nval.mat[i,j])
    }
  }
  list(p = p.mat, cor = cor.mat, n=nval.mat)
}

#' @title Test par permutation d'une matrice de corrélations de Bravais-Pearson
#'
#' @param mat Matrice des données
#' @param alternative Type d'hypothèses bilatéral, unilatéral inférieur ou supérieur
#' @param method Méthode de calcul de corrélation Pearson ou Spearman
#' @param num.sim Nombre de simulations
#' @param ... Paramètre suplémentaires transmis à la fonction cor
#'
#' @return Liste de deux éléments : matrice p.mat (matrice des p-valeurs des tests) et matrice cor.mat (matrice des valeurs observées des coefficients de corrélation de Bravais-Pearson)
#' @export
#'
#' @examples
#' data(Mesures5,package="BioStatR")
#' Mes5_red_gv = subset(Mesures5[,-5],subset=Mesures5$espece=="glycine violette")
#' perm.cor.mtest(Mes5_red_gv,num.sim=100)
#' 
perm.cor.mtest = function(mat, alternative= "two.sided", 
                          method= "pearson", num.sim= 20000, ...) 
  {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- cor.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(cor.mat) <- 1
  for (i in 1:(n-1)) {
    for (j in (i + 1):n) {
      tmp <- jmuOutlier::perm.cor.test(x = mat[, i], y = mat[, j], 
                           alternative=alternative, method=method, 
                           num.sim=num.sim)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      cor.mat[i, j] <- cor.mat[j, i] <- cor(x = mat[, i],
                                            y = mat[, j], method=method, ...)
    }
  }
  list(p = p.mat, cor = cor.mat)
}

#' @title Matrice de corrélation de Bravais-Pearson, bootstrap ou permutation
#'
#' @param mat Matrice des données
#' @param indices Vecteur d'indices dont la longueur est égale au nombre de lignes de la matrice
#'
#' @return Matrice des corrélations de Bravais-Pearson des données permutées
#' @export
#'
#' @examples
#' data(Mesures5,package="BioStatR")
#' Mes5_red_gv = subset(Mesures5[,-5],subset=Mesures5$espece=="glycine violette")
#' set.seed(1133)
#' rho.mult(Mes5_red_gv[,c("masse","taille","masse_sec")],sample(nrow(Mes5_red_gv)))
#' 
rho.mult <- function(mat, indices){
  tempcor <- as.vector(cor(x = mat[indices, ], method="pearson", 
                           use="pairwise.complete.obs"))[outer(1:ncol(mat),1:ncol(mat),">")]
  return(tempcor)
}

#' @title Intervalles de confiance bootstrap pour une matrice de corrélation de Bravais-Pearson
#'
#' @param mat Matrice des données
#' @param boot.mcor.res Résultat du bootstrap de la matrice de corrélation de Bravais-Pearson
#' @param conflevel Niveau de confiance pour les intervalles
#'
#' @return Liste de quatre éléments : matrice des limites inférieures des intervalles de confiance bootstrap percentile, matrice des limites supérieures des intervalles de confiance bootstrap percentile, matrice des limites inférieures des intervalles de confiance bootstrap BCa, matrice des limites supérieures des intervalles de confiance bootstrap BCa
#' @export
#'
#' @examples
#' data(Mesures5,package="BioStatR")
#' Mes5_red_lr = subset(Mesures5[,-5],subset=Mesures5$espece=="laurier rose")
#' library(boot)
#' boot.mcor <- boot(Mes5_red_lr[,c("masse","taille","masse_sec")], rho.mult, R=1000)
#' boot.mcor
#' boot.mcor.ic.res <- boot.mcor.ic(Mes5_red_lr[,c("masse", "taille","masse_sec")],boot.mcor)
#' boot.mcor.ic.res
#' 
boot.mcor.ic <- function(mat, boot.mcor.res, conflevel = 0.95){
  indboot.ci=function(i,type, conf = 0.95){
    if(type=="perc") 
      return(boot.ci(boot.mcor.res,index=i,type=type,conf=conf)$perc)
    if(type=="bca")
      return(boot.ci(boot.mcor.res,index=i,type=type,conf=conf)$bca)
  }
  bootperc <- matrix(unlist(lapply(1:ncol(mat),indboot.ci,type= 
                                     "perc",conf=conflevel)),ncol=5,byrow=TRUE) 
  bootabc <- matrix(unlist(lapply(1:ncol(mat),indboot.ci,type= 
                                    "bca",conf=conflevel)),ncol=5,byrow=TRUE) 
  cor.ic.percentile.low=matrix(NA,ncol(mat),ncol(mat)) 
  diag(cor.ic.percentile.low) <- 1 
  cor.ic.percentile.low[outer(1:ncol(mat),1:ncol(mat),">")]<-bootperc[,4] 
  cor.ic.percentile.low[outer(1:ncol(mat),1:ncol(mat),"<")]<-bootperc[,4] 
  cor.ic.percentile.up=matrix(NA,ncol(mat),ncol(mat)) 
  diag(cor.ic.percentile.up) <- 1 
  cor.ic.percentile.up[outer(1:ncol(mat),1:ncol(mat),">")]<-bootperc[,5] 
  cor.ic.percentile.up[outer(1:ncol(mat),1:ncol(mat),"<")]<-bootperc[,5] 
  cor.ic.BCa.low=matrix(NA,ncol(mat),ncol(mat)) 
  diag(cor.ic.BCa.low) <- 1 
  cor.ic.BCa.low[outer(1:ncol(mat),1:ncol(mat),">")]<-bootabc[,4] 
  cor.ic.BCa.low[outer(1:ncol(mat),1:ncol(mat),"<")]<-bootabc[,4] 
  cor.ic.BCa.up=matrix(NA,ncol(mat),ncol(mat)) 
  diag(cor.ic.BCa.up) <- 1 
  cor.ic.BCa.up[outer(1:ncol(mat),1:ncol(mat),">")]<- bootabc[,5] 
  cor.ic.BCa.up[outer(1:ncol(mat),1:ncol(mat),"<")]<-bootabc[,5] 
  return(list(cor.ic.percentile.low=cor.ic.percentile.low, 
              cor.ic.percentile.up=cor.ic.percentile.up,cor.ic.BCa.low= 
                cor.ic.BCa.low,cor.ic.BCa.up=cor.ic.BCa.up)) 
}


#' @title Ellipse ou intervalles de confiance pour une paire de paramètres d'un modèle linéaire
#'
#' @param g Modele linéaire
#' @param a Premier paramètre de l'ellipse
#' @param b Second paramtère de l'ellipse
#' @param which Type de région de confiance : 1 (ellipse autour des deux paramètres), 2 (rectangle autour du premier paramètre, axe des x) et 3 (rectangle autour du second paramètre, axes des y)
#' @param col Couleur de remplissage de la région de confiance
#'
#' @return NULL
#' @export
#'
#' @examples
#' data(Mesures,package="BioStatR")
#' Mes.B = subset(Mesures,Mesures$espece=="bignone")
#' model2<-lm(masse~taille+I(taille^2),data=Mes.B)
#' my.confidence.region(model2, which=1)
#' my.confidence.region(model2, which=2)
#' my.confidence.region(model2, which=3)
#' 
my.confidence.region <- function(g,a=2,b=3,which=0,col="pink"){ 
  e <- ellipse::ellipse(g,c(a,b)) 
  x <- g$coef[a]; y <- g$coef[b] 
  cf <- summary(g)$coefficients 
  ia <- cf[a,2]*stats::qt(.975,g$df.residual) 
  ib <- cf[b,2]*stats::qt(.975,g$df.residual) 
  xmin <- min(e[,1]); xmax <- max(e[,1])
  ymin <- min(e[,2]); ymax <- max(e[,2]) 
  graphics::plot(e, type="l", xlim=c(xmin,xmax), ylim=c(ymin,ymax)) 
  if(which==1){graphics::polygon(e,col=col) } 
  else if(which==2){graphics::rect(x-ia,par("usr")[3],x+ia,par("usr")[4], 
                          col=col,border=col)} 
  else if(which==3){graphics::rect(par("usr")[1],y-ib,par("usr")[2],y+ib, 
                          col=col,border=col)} 
  lines(e); points(x,y,pch=18) 
  graphics::abline(v=c(x+ia,x-ia),lty=2); graphics::abline(h=c(y+ib,y-ib),lty=2) 
  graphics::points(0,0); graphics::abline(v=0,lty="F848"); graphics::abline(h=0,lty="F848") 
}


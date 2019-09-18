#' ---
#' title: "Mod\u00e9lisation statistique par la pratique avec R"
#' subtitle: "Chapitre 1 : G\u00e9n\u00e9ralit\u00e9s sur le langage R"
#' author: "Fr\u00e9d\u00e9ric Bertrand, Emmanuelle Claeys, Myriam Maumy-Bertrand"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---

#Chapitre 1
#page 9

#q()

#page 10
help(read.table)

#help(package="package")
# par exemple
help(package="MASS")

example(plot)

# page 11
help("read.table",help_type="html")
help("read.table",help_type="text")
help.start()

#page 12
n<-28
N<-20

m=1973
m

N+n

# page 13
rm(m)

rm(n,N)

2 + 8

# page 14

120:155
sqrt(4)

# page 15
# source(file="C://chemin//vers//nomdefichier// + fichier.R",echo=T)
# source(file=".../repertoire/fichier.R",echo=T)
# source("fichier.R",echo=T)

# page 19, exercice 1.3 
f <- function(x){ 
  return (3*x^2) 
  }

# page 19, exercice 1.4 
a <- 3
if (a > 5) {
  print("a est plus grand que 5") 
} else { 
  print ("a est plus petit que 5") 
}



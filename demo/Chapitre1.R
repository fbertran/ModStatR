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

# page 19, exercice 1.4 
f <- function(x){ 
  return (3*x^2) 
  }

# page 20, exercice 1.5 
a <- 3
if (a > 5) {
  print("a est plus grand que 5") 
} else { 
  print ("a est plus petit que 5") 
}


# page 20, solution exercice 1.2
# q1
maVariable <- 10
maVariable
# q2
str(maVariable)
# q3
maVariable * 2
# q4
maVariable * maVariable
# q5
maVariable <- maVariable + 1
maVariable
# page 21, solution exercice 1.2
# q6
maVariable2 <- NULL 
maVariable2
# q7
str(maVariable2)
# q8
maVariable3 <- "Hello World"
maVariable3
# q9
str(maVariable3)

# page 21, solution exercice 1.3
# q1
1:10
# q2
a <- 1:10
a
# q3, avec RStudio ou avec la fonction str
str(a)
# page 22, q4
a+a
# q5. Oui car a est un vecteur contenant des nombres.
10*a
# q6
a>5
# q7
a[a>5]
# q8
b <- 5:8
# q9
b[1]
b[4]
b[5]
# Ou d'un seul coup
b[c(1,4,5)]
# Comme il n'y a pas de cinquieme element dans b, R affiche NA pour Not Available, ce qui est logique puisqu'il n'y a rien comme valeur.
# q10
b[7]<-9
b
# Comme b n'a que quatre elements, R affecte la valeur NA aux cinquiemes et sixiemes elements de b puis la valeur 9 au septieme element de b.
# page 23, q11
seq(5,30,by=5)

# page 23, solution exercice 1.4
# q1
f <- function(x){return ((5*x^5)/4)}
f
str(f)
# q2
f(3)
# q3
# Avec la fonction curve
curve(f,from=-10,to=10)
# Avec la fonction plot qui fait appel \u00e0 la fonction curve lorsqu'elle est appliquee \u00e0 un objet de classe fonction
plot(f,xlim=(c(-10,10)))

# q4
g <- function(x,a) { return (a*x^2) }
g

# page 23, solution exercice 1.5
for (i in 1:5) {print("Hello world")}


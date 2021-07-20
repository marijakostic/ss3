#           TRECI ZADATAK

#install.packages("plotrix")
library(plotrix)
library(imager)

# Ucitavamo obe slike
setwd("C:/Users/Korisnik/Desktop/seminarski/treci_zadatak")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T)
slike <- lapply(files, load.image) 

slika1 <- slike[[1]]
slika2 <- slike[[2]]
par(mfrow=c(1,2))
plot(slika1)
plot(slika2)

d1 <- dim(slika1)
d2 <- dim(slika2)
if ((d1[1]!=d2[1])||(d1[2]!=d2[2]))
  print("Slike nisu iste dimenzije.")


# Ne postoji jedinstveni nacin za zapisivanje slike u racunaru
# Primenom funkcije renorm() trebalo bi da se dobije normalizovani oblik
# Sve slike i svi objekti koji se drugacije zapisuju u R-u, ali izgledaju isto,
# trebalo bi da posle primene funkcije renorm() postanu (skoro) isti

razlike <- function(slika1,slika2)
{
  # Uzimamo dva prazna vektora u koja cemo smestati piksele na x odnsono y osi
  # gde se slike razlikuju
  
  X <- c()
  Y <- c()
  
  slika1 <- rm.alpha(slika1) # dobijamo da je slika samo u RGB spektru
  slika2 <- rm.alpha(slika2)
  # Proveravamo da li su iste, zato moramo da koristimo "renorm()"
  # Primenom funkcije "renorm()" trebalo bi da e dobije normalizovani oblik
  # svake slike i svi objekti koji se drugacije zapisuju u R-u, ali izgledaju 
  # isto bi trebalo posle primene te funkcije da postanu (skoro) iste
  
  slika <- renorm(slika1)-renorm(slika2)
  n1 <- dim(slika)[1]
  n2 <- dim(slika)[2]
  
  # prolazimo kroz sve piksele
  for (i in 1:n1)
  {
    for (j in 1:n2)
    {
      if(!(all(slika[i,j,1,]==c(0,0,0))))  # ako nisu nule, onda ima razlike
      {
        X <- c(X,i) # dodajemo taj piksel gde postoji razlika
        Y <- c(Y,j)
      }
      
    }
  }
 
  plot(slika1)
  for(i in seq(1,length(X),length(X)/50))
    draw.ellipse(X[i],Y[i],10,10,border = "red")
 plot(slika2)
 for(i in seq(1,length(X),length(X)/50))
   draw.ellipse(X[i],Y[i],10,10,border = "red")
 
 razlika <-cbind(X,Y)
 return(razlika)
}

razlike(slika1,slika2)


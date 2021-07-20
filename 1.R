#           PRVI ZADATAK

# Paket "imager" sadrzi veliki broj funkcija pomocu kojih mozemo da radimo sa slikama
library(imager)
require(imager)

# Prvo cemo ucitati sve slike iz foldera prvi_zadatak

setwd("C:/Users/Korisnik/Desktop/seminarski/prvi_zadatak")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T)
slike <- lapply(files, load.image)

# Nacrtacemo svih 7 slika 
slika1 <- slike[[1]]
plot(slika1)

slika2 <- slike[[2]]
plot(slika2)

slika3 <- slike[[3]]
plot(slika3)

slika4 <- slike[[4]]
plot(slika4)

slika5 <- slike[[5]]
plot(slika5)

slika6 <- slike[[6]]
plot(slika6)

slika7 <- slike[[7]]
plot(slika7)

# Pravimo funkciju

f <- function(slika,n)
{
  # n nam predstavlja kanal boje
  slika <- renorm(slika) # primenom funkcije "renorm" dobijamo normalizovani oblik slike
  
  q <- slika[,,1,n]
  dimenzija <- dim(q)
  # q je matrica dimenzije 256x256
  srednja_vrednost <- mean(q) # srednja vrednost boje
  indikator <-(q>srednja_vrednost) # gledamo samo one vrednosti koje su vece od srednje vrednosti
  
  levo <-dimenzija # tacka skroz levo
  gore <-dimenzija # najvisa tacka
  desno <-c(0,0) # tacka skroz desno
  dole <- c(0,0) # najniza tacka

  for(i in 1:dimenzija[1])
  {
    for(j in 1:dimenzija[2])
    {
      if(indikator[i,j]!=indikator[1,1])
      {
        if(i<levo[1]) levo <- c(i,j)
        if(i>desno[1]) desno <- c(i,j)
        if(j<gore[2]) gore <- c(i,j)
        if(j>dole[2]) dole <- c(i,j)
      }
    }
  }
  # razlika od pozadine je tacka koja nam treba
  
  C <- (gore+dole+levo+desno)/4 #centar
  xy<- abs(dole-desno)
  ugao_rotacije <- atan2(xy[2],xy[1])*180/pi
  return(c(ugao_rotacije,C))

}


# Pravimo folder u kome cemo sacuvati novodobijene izrotirane slike
dir.create(file.path(getwd(),"slike"))

f(slika1,2)[1] #ugao rotacije 
rotirana_slika1 <- imrotate(slika1,f(slika1,2)[1],round(f(slika1,2)[2]),round(f(slika1,2)[3]))
plot(rotirana_slika1)
save.image(rotirana_slika1,paste("slike\\",strsplit(files[1],".",fixed = T)[[1]][1],".png",sep=""))

f(slika2,2)[1]
rotirana_slika2 <- imrotate(slika2,f(slika2,2)[1],round(f(slika2,2)[2]),round(f(slika2,2)[3]))
plot(rotirana_slika2)
save.image(rotirana_slika2,paste("slike\\",strsplit(files[2],".",fixed = T)[[1]][1],".png",sep=""))

f(slika3,2)[1]
rotirana_slika3 <- imrotate(slika3,f(slika3,2)[1],round(f(slika3,2)[2]),round(f(slika3,2)[3]))
plot(rotirana_slika3)
save.image(rotirana_slika3,paste("slike\\",strsplit(files[3],".",fixed = T)[[1]][1],".png",sep=""))

f(slika4,2)[1]
rotirana_slika4 <- imrotate(slika4,f(slika4,2)[1],round(f(slika4,2)[2]),round(f(slika4,2)[3]))
plot(rotirana_slika4)
save.image(rotirana_slika4,paste("slike\\",strsplit(files[4],".",fixed = T)[[1]][1],".png",sep=""))

f(slika5,2)[1]
rotirana_slika5 <- imrotate(slika5,f(slika5,2)[1],round(f(slika5,2)[2]),round(f(slika5,2)[3]))
plot(rotirana_slika5)
save.image(rotirana_slika5,paste("slike\\",strsplit(files[5],".",fixed = T)[[1]][1],".png",sep=""))

f(slika6,2)[1]
rotirana_slika6 <- imrotate(slika6,f(slika6,2)[1],round(f(slika6,2)[2]),round(f(slika6,2)[3]))
plot(rotirana_slika6)
save.image(rotirana_slika6,paste("slike\\",strsplit(files[6],".",fixed = T)[[1]][1],".png",sep=""))

f(slika7,2)[1]
rotirana_slika7 <- imrotate(slika7,f(slika7,2)[1],round(f(slika7,2)[2]),round(f(slika7,2)[3]))
plot(rotirana_slika7)
save.image(rotirana_slika7,paste("slike\\",strsplit(files[7],".",fixed = T)[[1]][1],".png",sep=""))


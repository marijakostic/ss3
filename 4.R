#           CETVRTI ZADATAK

# Instaliracemo i ukljucimo paket "imager" 
library(imager)
# Odmah cemo i instalirati i ukljuciti pakete "MASS" i "nnet"
library(MASS) # U MASS-u su "lda()" i "qda()" koje ce nam trebati
library(nnet) # U "nnet" se nalazi "multinom()" koja ce nam trebati

# Ucitavamo sve slike iz foldera za obucavanje u listu

setwd("C:/Users/Korisnik/Desktop/seminarski/cetvrti_zadatak_obucavanje")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T)
# Primenjujemo funkciju load.image da ucitamo sve slike iz foldera
slike <- lapply(files, load.image) 

# Odedjujemo granice polja


granice <- function(matrica, epsilon, prob, by.row=T)
{
  if(by.row)
  {
    Mean <- rowMeans #za granice po vrstama, koristimo f-ju rowMeans
  }
  else
  {
    Mean <- colMeans #za granice po kolonama colMeans
  }
  
  vec <- Mean(matrica < epsilon) >= prob
  return(which(vec!=F))
}

sredjivanje <- function(vektor)
{
  novi_vektor <- c()
  for(i in 1:(length(vektor)-1))
  {
    if(vektor[i+1]-vektor[i]>5)
    {
      novi_vektor <- c(novi_vektor, vektor[i])
    }
  }
  for(j in length(vektor):2)
  {
    if(vektor[j]-vektor[j-1]>5)
    {
      novi_vektor <- c(novi_vektor, vektor[j])
    }
  }
  return(sort(novi_vektor))
}


# Pravimo prediktor-procenat u koliko piksela je najveca vrednost plave
# (u odgovarajucim matrcama)

prediktor <- function(slika, vrste, kolone)
{
  prosek_plave <- c()
  
  l <- 1
  for(j in 1:9)
  {
    for(i in 1:9)
    {
      #izdvojimo polje
      s <- array(slika[vrste[2*j-1]:vrste[2*j], kolone[2*i-1]:kolone[2*i], 1, 1:3],
                c(vrste[2*j]-vrste[2*j-1]+1,kolone[2*i]-kolone[2*i-1]+1, 1, 3))
      nova <- cimg(s)
      crvena <- as.vector(nova[ , , 1, 1])
      zelena <- as.vector(nova[ , , 1, 2])
      plava <- as.vector(nova[ , , 1, 3])
      
      brojac_plave <- mean(plava/(crvena+zelena+plava),na.rm = TRUE)
      prosek_plave[l] <-brojac_plave
      
      l=l+1
    }
  }
  
  return(prosek_plave)
}

# Disperzija polja u crno-belom spektru

prediktor2 <- function(slika, vrste, kolone)
{
  X <- c()
  l <- 1
  # r sirina odsecanja
  r <- ceiling((vrste[2]-vrste[1])/16)
  for(j in 1:9)
  {
    for(i in 1:9)
    {
      s <-array(slika[(vrste[2*j-1]+r):(vrste[2*j]-r), (kolone[2*i-1]+r):(kolone[2*i]-r), 1, 1:3],
              c(vrste[2*j]-vrste[2*j-1]-2*r+1,kolone[2*i]-kolone[2*i-1]-2*r+1, 1, 3))
      X[l] <- var(as.vector(grayscale(cimg(s))[,,,1]))
      l <-l+1
    }
  }
  return(X)
}

# Sada pravimo vektore sa vrednostima za kategorijsku promenljivu Y, sto moramo rucno.
# Uz unapred obavljen dogovor o dodeljivanju brojeva u zavisnosti od toga sta imamo u polju

# Zatvoreno polje : 0
# Broj 1 : 10
# Broj 2 : 20
# Broj 3 : 30
# Broj 4 : 40
# Broj 5 : 50
# Broj 6 : 60
# Broj 7 : 70
# Broj 8 : 80
# Mina : -100
# Prazno polje : 100

Y1 <-c(0,0,0,0,0,0,0,0,0,
       0,0,0,20,10,10,-100,10,0,
       0,0,30,-100,10,10,10,10,0,
       20,30,-100,20,10,100,10,10,0,
       -100,20,10,10,100,100,10,-100,0,
       10,10,100,100,10,10,20,10,0,
       100,10,10,10,10,-100,20,10,0,
       100,10,-100,0,0,0,0,0,0,
       100,10,0,0,0,0,0,0,0)

Y2 <-c(0,0,0,10,100,10,10,10,100,
       0,20,0,20,10,10,-100,10,100,
       0,0,30,-100,10,10,10,10,100,
       20,30,-100,20,10,100,10,10,10,
       -100,20,10,10,100,100,10,-100,10,
       10,10,100,100,10,10,20,10,10,
       100,10,10,10,10,-100,20,10,100,
       100,10,-100,10,10,20,0,10,100,
       100,10,10,10,100,10,0,10,100)

Y3 <-c(100,100,100,100,100,100,10,0,0,
       10,10,100,100,100,10,20,0,0,
       0,10,100,100,100,20,0,0,0,
       0,10,100,100,100,20,0,30,10,
       0,10,10,10,100,10,10,10,100,
       0,0,0,10,100,100,100,100,100,
       0,0,0,10,100,10,10,10,100,
       0,0,0,10,10,10,0,20,10,
       0,0,0,0,0,0,0,0,0)

Y4 <-c(100,100,100,100,100,100,10,10,10,
       10,10,100,100,100,10,20,-100,20,
       -100,10,100,100,100,20,-100,40,-100,
       10,10,100,100,100,20,-100,30,10,
       100,10,10,10,100,10,10,10,100,
       100,10,-100,10,100,100,100,100,100,
       100,10,10,10,100,10,10,10,100,
       10,10,20,10,10,10,-100,20,10,
       10,-100,20,-100,10,10,10,20,-100)
       
Y5 <-c(100,100,100,100,100,100,100,10,0,
       10,10,100,100,100,100,100,10,0,
       0,10,100,100,10,20,30,30,0,
       20,20,100,100,10,-100,-100,-100,10,
       0,10,100,100,10,20,30,20,10,
       0,10,10,10,10,100,100,100,100,
       10,10,10,0,20,10,10,100,100,
       0,0,20,10,20,0,10,100,100,
       0,0,10,100,10,0,10,100,100)

Y6 <-c(100,100,100,100,100,100,100,100,100,
       100,100,100,100,100,10,10,10,100,
       100,100,100,100,100,20,0,30,10,
       10,10,100,100,100,20,0,0,0,
       0,10,100,10,20,30,30,30,0,
       10,10,100,10,0,0,10,10,0,
       100,10,10,20,20,20,10,10,10,
       10,20,0,10,100,100,100,10,0,
       0,20,10,10,100,100,100,10,0)

Y7 <-c(100,100,100,100,100,100,10,20,0,
       20,20,20,10,10,100,10,0,20,
       -100,-100,0,0,30,10,10,10,10,
       20,20,30,0,0,10,100,100,100,
       100,100,10,20,20,10,100,100,100,
       100,10,10,10,100,100,100,100,100,
       100,10,0,10,100,100,100,100,100,
       100,20,20,30,10,10,100,100,100,
       100,10,0,20,0,10,100,100,100)


Y8 <-c(100,10,10,10,100,100,100,100,100,
       100,10,0,10,100,100,10,20,20,
       10,20,10,10,10,20,30,0,0,
       0,10,100,100,10,0,0,40,30,
       10,10,10,10,20,20,20,20,0,
       100,100,10,0,20,10,100,10,10,
       100,100,10,20,0,10,100,100,100,
       100,100,100,10,10,10,10,10,10,
       100,100,100,100,100,100,10,0,0)


Y9 <-c(0,0,0,0,0,0,0,0,0,
        0,30,10,30,0,0,0,0,0,
        0,20,100,10,20,20,0,0,0,
        10,10,100,100,100,10,0,0,0,
        100,100,100,10,20,40,0,0,0,
        10,20,10,20,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,40,0,
        0,0,0,0,0,0,0,0,0)


# Spojicemo sve u jedan vektor koji ce predstavljati zavisnu promenljivu
Y <- c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9)


# Vektori u koje cemo smestiti vrednosti prediktora za svaku tablu minolovca
X1 <- c()
X2 <- c()

# Sad odredimo granice za svaku sliku iz skupa za obucavanje,
# izracunavamo vrednost prediktora i sve smestamo u vektore

n <- length(slike)
for(i in 1:n) {
  slika <- rm.alpha(slike[[i]]) #da slika bude samo u RGB spektru
  
  q11 <-slika [ , , 1, 1]
  q12 <-slika [ , , 1, 2]
  q13 <-slika [ , , 1, 3]
  M <- pmax(q11, q12, q13) #pmax dodeljuje maksimalnu vrednost po elementima matrica/vektora
  
  m_vrsta <- granice(M,0.2, 0.3)
  m_kolona <-granice(M,0.2, 0.3,FALSE)
  
  v <-sredjivanje(m_vrsta)
  k <-sredjivanje(m_kolona)
  
  X1 <- c(X1, prediktor(slika, v, k)) #dodajemo vrednost prediktora i prediktora2 za ovu sliku u vektor
  X2 <- c(X2, prediktor2(slika, v, k))
}
X1
X2


# Ucitavamo sada slike za testiranje modela

setwd("C:/Users/Korisnik/Desktop/seminarski/cetvrti_zadatak_kontrolni")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T)
slike2 <- lapply(files, load.image) 

# Ovo ce biti prediktori na osnovu cije vrednosti predvidjamo
X12 = c()
X22 = c()

# Racunamo na isti nacin 

for(i in 1:length(slike2)) {
  slika <- rm.alpha(slike2[[i]]) #da slika bude samo u RGB spektru
  
  q11 <-slika [ , , 1, 1]
  q12 <-slika [ , , 1, 2]
  q13 <-slika [ , , 1, 3]
  M <- pmax(q11, q12, q13) #pmax dodeljuje maksimalnu vrednost po elementima matrica/vektora
  
  m_vrsta2 <- granice(M,0.2, 0.3)
  m_kolona2<-granice(M,0.2, 0.3,FALSE)
  
  v2 <-sredjivanje(m_vrsta2)
  k2 <-sredjivanje(m_kolona2)
  
  X12 <- c(X12, prediktor(slika, v2, k2)) #dodajemo vrednost prediktora i prediktora2 za ovu sliku u vektor
  X22 <- c(X22, prediktor2(slika, v2, k2))
}

# Sada pravimo vektore sa vrednostima za kategorijsku promenljivu Y za kontrolu

Y_kon1 <- c(10,10,20,-100,10,100,100,100,100,
            20,-100,30,10,10,100,100,100,100,
            20,-100,20,100,100,10,10,10,100,
            0,20,10,100,100,10,-100,20,10,
            -100,10,100,10,10,20,20,0,0,
            10,10,100,10,-100,10,10,0,0,
            100,100,100,10,10,10,10,20,0,
            100,100,100,100,10,10,20,0,0,
            100,100,100,100,10,0,20,0,0)

Y_kon2 <- c(0,0,10,0,0,-100,10,100,100,
            10,10,10,10,20,20,10,10,10,
            100,100,100,100,100,100,100,10,0,
            100,100,100,100,100,100,100,10,10,
            100,100,100,100,100,10,10,20,10,
            100,100,100,100,100,20,-100,30,-100,
            100,100,100,10,10,40,-100,40,10,
            100,100,100,20,-100,40,-100,20,100,
            100,100,100,20,-100,30,10,10,100)

# Spojicemo ih u jedan vektor koji ce nam sluziti da proverimo model
Y_kontolni <- c(Y_kon1, Y_kon2)

# Sada zelimo malo ozbiljnije to da testiramo i proverimo koliko dobro detektuje elemente skupa za obucavanje
# pomocu ova dva prediktora i metodama "multinom", "lda", "qda"

# Multinomni logisticki
# U paketu "nnet" se nalazi "multinom()"

model.multinom <- multinom(Y ~ X1+X2)
summary(model.multinom)

# Predvidjamo vrednosti zavisne promenljive za vrednosti prediktora slika iz kontrolnog skupa
# koristeci model napravljen na osnovu skupa za obucavanje 

model.mulpred <- predict(model.multinom, newdata = data.frame(X1=X12, X2=X22))
matrix(model.mulpred, ncol=9)
table(model.mulpred, Y_kontolni)
mean(model.mulpred == Y_kontolni)


# LDA
# U paketu "MASS" se nalazi "lda()"

model.lda <- lda(Y~X1+X2)
summary(model.lda)
model.ldapred <- predict(model.lda, newdata = data.frame(X1=X12, X2=X22))
table(model.ldapred$class, Y_kontolni)
mean(model.ldapred$class == Y_kontolni)


# QDA
# U paketu "MASS" se nalazi "qda()"

model.qda <- qda(Y~X1+X2)
summary(model.qda)
model.qdapred <- predict(model.qda, newdata = data.frame(X1=X12, X2=X22))
table(model.qdapred$class, Y_kontolni)
mean(model.qdapred$class == Y_kontolni)


  







  
  

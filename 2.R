#           DRUGI ZADATAK

# Paket "imager" sadrzi veliki broj funkcija pomocu kojih mozemo da radimo sa slikama
library(imager)
require(imager)

# Prvo cemo ucitati sve slike iz foldera drugi_zadatak

# Ucitavamo pravougaonike

setwd("C:/Users/Korisnik/Desktop/seminarski/drugi_zadatak/pravougaonici")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T)
pravougaonici <- lapply(files, load.image)


# (a,b) #

# Tacka dole levo
T1 <- function(slika,k)
{
  n1 = dim(slika)[1] # broj piksela na x-osi
  n2 = dim(slika)[2] # broj piksela na y-osi
  for(j in n2:1)
  {
    # Iduci odozdo ka gore, trazimo prvu tacku koja se razlikuje vise od 0.1% od pozadine(k)
    if(mean(slika[,j]!=k) > 0.001) 
    {
      for(i in 1:n1) # duz x-ose trazimo tacku koja se ralikuje
      {
        if(slika[i,j]!=k)
        {
          tacka <- c(i,j) #Prva tacka koja se razlikuje od pozadine je ona koja nam treba
          return(tacka)
        }
      }
    }
  }
}

# Tacka dole desno
T2 <- function(slika,k)
{
  n1 = dim(slika)[1]
  n2 = dim(slika)[2]
  for(i in n1:1) # Idemo x-osom od kraja do pocetka
  {
    if(mean(slika[i,]!=k) > 0.001)
    {
      for(j in n2:1) # Odozdo trazimo prvu tacku koja se razlikuje od pozadine
      {
        if(slika[i,j]!=k)
        {
          tacka <- c(i,j)
          return(tacka)
        }
      }
    }
  }
}

# Tacka gore desno
T3 <- function(slika,k)
{
  n1 = dim(slika)[1]
  n2 = dim(slika)[2]
  for(j in 1:n2) # Idemo y-osom Odozgo na dole 
  {
    if(mean(slika[,j]!=k) > 0.001)
    {
      for(i in n1:1) # Od desnog kraja x-ose trazimo prvu tacku koja se razlikuje od pozadine
      {
        if(slika[i,j]!=k)
        {
          tacka <- c(i,j)
          return(tacka)
        }
      }
    }
  }
}

# Tacka gore levo
T4 <- function(slika,k)
{
  n1 = dim(slika)[1]
  n2 = dim(slika)[2]
  for(i in 1:n1) # Idemo sleva duz x-ose
  {
    if(mean(slika[i,]!=k) > 0.001)
    {
      for(j in 1:n2) # Odozgo trazimo prvu tacku koja se razlikuje od pozadine
      {
        if(slika[i,j]!=k)
        {
          tacka <- c(i,j)
          return(tacka)
        }
      }
    }
  }
}

pravougaonik <- function(slika,i)
{
  slika1 <- rm.alpha(slika) # uklanjamo cetvrti spektar boje i dobijamo sliku u RGB spektru
  slika <- grayscale(slika1) # pretvaramo sliku u crno belu
  slika <- slika[,,1,1]
  n1 <- dim(slika)[1]
  n2 <- dim(slika)[2]
  pozadina <- slika[1,1]
  
  A <- T1(slika,pozadina)
  B <- T2(slika,pozadina)
  C <- T3(slika,pozadina)
  D <- T4(slika,pozadina)
  
  koordinate <- cbind(A,B,C,D)
  koordinate <-as.matrix(koordinate)
  colnames(koordinate) <- c('A','B','C','D')
  rownames(koordinate) <- c('x','y')
  # Izdvajamo deo slike koji je ogranicen tackama A,B,C,D
  
  x1 <- min(A[1],B[1],C[1],D[1])
  x2 <- max(A[1],B[1],C[1],D[1])
  y1 <- min(A[2],B[2],C[2],D[2])
  y2 <- max(A[2],B[2],C[2],D[2])
  
  pravougaonik <- array(slika[x1:x2,y1:y2],c(x2-x1+1,y2-y1+1))
  pravougaonik <- renorm(as.cimg(pravougaonik))
  pravougaonik # izdvojili smo samo pravougaonik sa slike
 #plot(pravougaonik) # slika izdvojenog pravougaonika
  
  pravougaonik <- pravougaonik[,,1,1]
  n12 <- dim(pravougaonik)[1]
  n22 <- dim(pravougaonik)[2]
  
  # Kako bi proverili da li se na slici nalazi jedan ili vise pravougaonika,trazicemo prelaze boja
  # Ako ima najvise dva prelaza, onda je na slici sigurno jedan pravougaonik
  # Ako ima vise od dva prelaza, onda na slici ima vise od jednog pravougaonika
  
  # Brojimo prelaze
  
  l <-1
  for(i in 1:n12)
  {
    l1 <- l
    l <- 1
    
    for(j in 2:n22)
    {
      if(pravougaonik[i,j]!=pravougaonik[i,j-1])
        l <- l+1
    }
    
    if(l1>l && l==1)
    {
      print("Na slici se nalazi vise od jednog pravougaonika.")
      return()
    }
  }
  
  
  # Ako je na slici jedan pravougaonik, treba da proverimo da li je crne, bele ili neke trece boje
  # Najlakse je naci centar pravougaonika i proveriti koje je boje
  
  # Koordinate centra pravougaonika racunamo pomocu formula 
  s <- 2*(D[1]*(B[2]-C[2])+B[1]*(C[2]-D[2])+C[1]*(D[2]-B[2]))
  s1 <- (D[1]^2+D[2]^2)*(B[2]-C[2])+(B[1]^2+B[2]^2)*(C[2]-D[2])+(C[1]^2+C[2]^2)*(D[2]-B[2])
  s2 <- (D[1]^2+D[2]^2)*(C[1]-B[1])+(B[1]^2+B[2]^2)*(D[1]-C[1])+(C[1]^2+C[2]^2)*(B[1]-D[1])
  centar <- c(s1/s,s2/s)
  
  # Proveravamo koje je boje centar i ako je crne ili bele boje
  
  if(all(slika1[s1/s,s2/s,1,]==c(1,1,1)))
  {
    print("Na slici je jedan beli pravougaonik.Koordinate njegovih temena su:")
    return(koordinate)
    
  }
  else if(all(slika1[s1/s,s2/s,1,]==c(0,0,0)))
  {
    print("Na slici je jedan crni pravougaonik.Koordinate njegovih temena su:")
    return(koordinate)
  }
  else print("Na slici nije pravougaonik potrebne boje.")
  
}


# Za svaku sliku iz foldera proveravamo da li se na njoj nalazi tacno jedan crni 
# ili jedan beli pravougaonik

length(pravougaonici)

# ima 9 slika
slika1 <- pravougaonici[[1]]
plot(slika1)
pravougaonik(slika1,1)

slika2 <- pravougaonici[[2]]
plot(slika2)
pravougaonik(slika2,2)

slika3 <- pravougaonici[[3]]
plot(slika3)
pravougaonik(slika3,3)

slika4 <- pravougaonici[[4]]
plot(slika4)
pravougaonik(slika4,4)

slika5 <- pravougaonici[[5]]
plot(slika5)
pravougaonik(slika5,5)

slika6 <- pravougaonici[[6]]
plot(slika6)
pravougaonik(slika6,6)

slika7 <- pravougaonici[[7]]
plot(slika7)
pravougaonik(slika7,7)

slika8 <- pravougaonici[[8]]
plot(slika8)
pravougaonik(slika8,8)

slika9 <- pravougaonici[[9]]
plot(slika9)
pravougaonik(slika9,9)

# (v) #

# Ucitavamo krugove

setwd("C:/Users/Korisnik/Desktop/seminarski/drugi_zadatak/krugovi")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T)
krugovi <- lapply(files, load.image)

krug <- function(slika)
{
  slika1 <- rm.alpha(slika) # uklanjamo cetvrti spektar boje i dobijamo sliku u RGB spektru
  slika <- grayscale(slika1) # pretvaramo sliku u crno belu
  slika <- slika[,,1,1]
  n1 <- dim(slika)[1]
  n2 <- dim(slika)[2]
  pozadina <- slika[1,1]
  
  A <- T1(slika,pozadina)
  B <- T2(slika,pozadina)
  C <- T3(slika,pozadina)
  D <- T4(slika,pozadina)
  
  x1 <- min(A[1],B[1],C[1],D[1])
  x2 <- max(A[1],B[1],C[1],D[1])
  y1 <- min(A[2],B[2],C[2],D[2])
  y2 <- max(A[2],B[2],C[2],D[2])
  
  krug <- array(slika[x1:x2,y1:y2],c(x2-x1+1,y2-y1+1))
  krug <- renorm(as.cimg(krug))
  krug 
  # Na ovaj nacin smo izdvojili kvadrat, na polovini svake stranice nalazi se jedna od tacaka A,B,C,D
  #plot(krug) # slika izdvojenog kvadrata
  
  krug <- krug[,,1,1]
  n12 <- dim(krug)[1]
  n22 <- dim(krug)[2]
  
  l <- 0
  # Idemo duz x-ose, za fikciranu y koordinatu koja je bas sredina stranice kvadrata
  # Trazimo one tacke koje se razlikuju od (1,centar) u vise od 5% slucajeva
  
  for(i in 1:n12)
  {
    if(mean(krug[i,(y2-y1)/2]!=krug[1,(y2-y1)/2]) > 0.05) 
      l <-l+1
  }
  
  # Sada idemo duz y-ose, za fikciranu x koordinatu koja je bas sredina stranice kvadrata
  # Trazimo one tacke koje se razlikuju od (centar,n22) u vise od 5% slucajeva  
  for(i in n22:1)
  {
    if(mean(krug[(x2-x1)/2,i]!=krug[(x2-x1)/2,n22]) > 0.05) 
      l <-l+1
  }
  
  # Ako smo dobili da postoje tecke koje se razlikuju, ima smisla da gledamo da li se na toj slici 
  # nalazi vise krugova
  # Ako ima vise od 2 prelaza, imamo vise krugova
  
  k=1
  if (l>0)
  {
       for (i in 1:n12)
    {
      for (j in 2:n22)
      {
        if (krug[i,j]!=krug[i,j-1])
        k <-k+1
      }
      
      
       }
    if(k>3)
    {
      print("Na slici se nalazi vise od jednog kruga.")
      return()
    }
  }
  
  
  # Ako je na slici jedan krug, treba da proverimo da li je crne, bele ili neke trece boje
  # Najlakse je naci centar kruga i proveriti koje je boje
  
  # Koordinate centra  racunamo pomocu formula 
  s <- 2*(D[1]*(B[2]-C[2])+B[1]*(C[2]-D[2])+C[1]*(D[2]-B[2]))
  s1 <- (D[1]^2+D[2]^2)*(B[2]-C[2])+(B[1]^2+B[2]^2)*(C[2]-D[2])+(C[1]^2+C[2]^2)*(D[2]-B[2])
  s2 <- (D[1]^2+D[2]^2)*(C[1]-B[1])+(B[1]^2+B[2]^2)*(D[1]-C[1])+(C[1]^2+C[2]^2)*(B[1]-D[1])
  centar <- c(s1/s,s2/s)
  
  # Poluprecnik kruga racunamo pomocu formule
  R <- sqrt((B[1]-D[1])^2+(B[2]-D[2])^2)/2
  
  # Proveravamo koje je boje centar i ako je crne ili bele boje
  
  rezultat <-list(koordinate_centra=centar,poluprecnik=R)
  
  if(all(slika1[s1/s,s2/s,1,]==c(1,1,1)))
  {
    print("Na slici je jedan beli krug.Koordinate centra i poluprecnik su redom:")
    return(rezultat)
    
  }
  else if(all(slika1[s1/s,s2/s,1,]==c(0,0,0)))
  {
    print("Na slici je jedan crni krug.Koordinate centra i poluprecnik su :")
    return(rezultat)
  }
  else print("Na slici nije krug potrebne boje.")
  
}

length(krugovi)
# Imamo 4 slike

slika1 <- krugovi[[1]]
plot(slika1)
krug(slika1)

slika2 <- krugovi[[2]]
plot(slika2)
krug(slika2)

slika3 <- krugovi[[3]]
plot(slika3)
krug(slika3)

slika4 <- krugovi[[4]]
plot(slika4)
krug(slika4)


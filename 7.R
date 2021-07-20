#               SEDMI ZADATAK

## (a) 


# Pravimo funkciju koja ce za prosledjenu vec popunjenu do kraja matricu tabele Minolovca  
# da vrati odgovor da li je ovakva matrica jedna moguca tabela Minolovca

# Da bi matrica bila tabla minolovca mora da zadovoljava uslove:
#     1) sva polja moraju biti otvorena
#     2) broj mina mora biti jednak ukupnom broju mina
#     3) ako je prazno polje, u okolini ne sme biti mina
#     4) oko polja sa brojem ima tacno toliko mina

prava_matrica <- function(matrica,dimenzija,broj_mina)
{
  M <- matrica
  M <- cbind(rep(1, dimenzija), M, rep(1, dimenzija))
  M <- rbind(rep(1, dimenzija+2), M, rep(1, dimenzija+2))
  
  # Broj mina mora biti jednak ukupnom broju mina
  if(sum(matrica==-100)!=broj_mina)
    return(FALSE)
  
  for(i in 2:(dimenzija+1))
  {
    for(j in 2:(dimenzija+1))
    {
      vektor <- c(M[i-1,j-1], M[i-1, j], M[i-1, j+1], M[i, j-1], M[i, j+1], M[i+1, j-1], M[i+1, j], M[i+1, j+1])
      
      # Sva polja moraju biti otvorena
      if(M[i,j] == 0)
      {
        print("Matrica ima zatvorenih polja!")
        return(FALSE)
      }
      
      # Ako je prazno polje, u okolini ne sme biti mina
      if(M[i,j]==100)
      {
        if(sum(vektor==0)!=0)
        return(FALSE)
      }
      
      # Oko polja sa brojem ima tacno toliko mina
      for (k in 1:8) 
      {
        if(M[i,j]==k*10)
        {
          if(sum(vektor==-100)!=k)
          return(FALSE)
        }
      }
    }
    
  }
  
}

## (b) 

# Pravimo funkciju sa ulaznim argumentima dimenzije table i brojem mina
# Funkcija vraca matricu - jednu gotovu i potpuno popunjenju tablu minolovca

generator_table <- function(n1,n2, broj_mina)
{
  # matrica ce predstavljati tablu minolovca
  matrica <- rep(0,n1*n2)
  
  # Slucajno izaberemo pozicije gde cemo postaviti mine
  pozicija_mina <- sort(sample(1:length(matrica), broj_mina))
  matrica[pozicija_mina] <- -100
  matrica <- matrix(matrica, nrow=n1)
  
  # Pravimo matricu koja ima u prvoj i poslednjoj vrsti(koloni)sve jedinice
  M <- matrica
  M <- cbind(rep(1, n1), M, rep(1, n1))
  M <- rbind(rep(1, n2+2), M, rep(1, n2+2))
  
  # Pravimo petlju koja prolazi kroz sva polja table minolovca
  for(i in 2:(n1+1))
  {
    for(j in 2:(n2+1)) 
    {
      # Okolina polja matrica[i,j]
      vektor <- c(M[i-1,j-1], M[i-1, j], M[i-1, j+1], M[i, j-1], M[i, j+1], M[i+1, j-1], M[i+1, j], M[i+1, j+1])
      
      # Ako nije mina, mora biti ili broj ili prazno polje
      if (M[i,j]!=-100)
      {
        M[i,j] <- sum(vektor==-100)*10 
        # Broj mina u okolini odredjuje koji je broj
        # Ako dobijemo da je ovo 0, nema mina u okolini, znaci prazno polje
        if (M[i,j]==0)
        M[i,j] <- 100
      }
    }
  }
  
  # Moramo da skinemo dodate kolone i vrste
  
  M <- M[-1, -1]
  M <- M[-(n1+1), -(n2+1)]
  return(M)
}

## (v) 


# Pravimo funkciju koja za proizvoljnu matricu- tablu minolovca
# zatvori slucajno odabranih broj_polja koje nisu mine i sve mine

sakrivanje_polja <- function(matrica, broj_polja)
{
  M <- as.vector(matrica)
  dimenzija <- length(matrica[,1])
  
  # Prvo cemo zatvoriti sve mine
  M[M==-100] <- 0
  
  # Izbacujemo pozicije na kojima se nalaze mine(odnosno polje koje smo zatvoruli)
  # Ostavimo samo pozicije na kojima se nalazi broj ili prazno polje
  vektor <- seq(1:length(M))
  vektor <- vektor[-which(M == 0)]
  
  # Slucajno odaberemo polja gde nisu mine koja cemo da zatvorimo
  s <- sample(vektor,broj_polja)
  M[s] <- 0
  
  M <- matrix(M, nrow = dimenzija)
  return(M)
}
  
## (g) 

# Prvo cemo napraviti pomocnu funkciju resi_prosto koja ce za prosledjenu matricu table Minolovca,
# za sva polja, za koje je moguce sigurno odluciti da li je mina ili ne,da vrati tacan odgovor

resi_prosto <- function(M,broj_mina)
{
  dimenzija <- length(M[1,])
  # Pravimo matricu m koja ima u prvoj i poslednjoj vrsti(koloni)sve jedinice,
  # da ne bismo imali problem sa for petljom i inedksiranjem
  m <- M
  m <- cbind(rep(1, dimenzija), m, rep(1, dimenzija))
  m <- rbind(rep(1, dimenzija+2), m, rep(1, dimenzija+2))
  
  # Prolazimo kroz sva polja table minolovca
  for(i in 2:(dimenzija+1))
  {
    for(j in 2:(dimenzija+1))
    {
      for(k in 1:8)
      {
        # Ako je broj u polju m[i,j] (u okolini mora biti tacno toliko mina)
        if(m[i, j]==k*10)
        {
          # Okolina polja m[i,j]
          vektor <- c(m[i-1,j-1], m[i-1, j], m[i-1, j+1], m[i, j-1], m[i, j+1], m[i+1, j-1], m[i+1, j], m[i+1, j+1])
          # Ako je tacno k mina u okolini onda zatvorena polja ne kriju mine, pa mozemo da ih otvorimo
          # Ovde to manifestujemo povecanjem za 1
          
          if(sum(vektor==-100)==k)
          {
            if(m[i-1, j-1]<9 & m[i-1, j-1]!=-100)
              m[i-1, j-1] <- m[i-1, j-1]+1
            if(m[i-1, j]<9 & m[i-1,j]!=-100)
              m[i-1, j] <- m[i-1, j]+1
            if(m[i-1, j+1]<9 & m[i-1,j+1]!=-100)
              m[i-1, j+1] <- m[i-1, j+1]+1
            if(m[i, j-1]<9 & m[i,j-1]!=-100)
              m[i, j-1] <- m[i, j-1]+1
            if(m[i, j+1]<9 & m[i,j+1]!=-100)
              m[i, j+1] <- m[i, j+1]+1
            if(m[i+1, j-1]<9 & m[i+1,j-1]!=-100)
              m[i+1, j-1] <- m[i+1, j-1]+1
            if(m[i+1, j]<9 & m[i+1,j]!=-100)
              m[i+1, j] <- m[i+1, j]+1
            if(m[i+1, j+1]<9 & m[i+1,j+1]!=-100)
              m[i+1, j+1] <- m[i+1, j+1]+1
          }
        }
      }
      
      # Ako je polje prazno, onda u okolini nema mina, mozemo da otvorimo sva polja u okolini
      if(m[i,j]==100)
      {
        if(m[i-1, j-1]==0)
          m[i-1, j-1] <- m[i-1, j-1]+1
        if(m[i-1, j]==0)
          m[i-1, j] <- m[i-1, j]+1
        if(m[i-1, j+1]==0)
          m[i-1, j+1] <- m[i-1,j+1]+1
        if(m[i, j-1]==0)
          m[i, j-1] <- m[i,j-1]+1
        if(m[i, j+1]==0)
          m[i, j+1] <- m[i,j+1]+1
        if(m[i+1, j-1]==0)
          m[i+1, j-1] <- m[i+1,j-1]+1
        if(m[i+1, j]==0)
          m[i+1, j] <- m[i+1,j]+1
        if(m[i+1, j+1]==0)
          m[i+1, j+1] <- m[i+1,j+1]+1
      }
    }
  }
  
  for(i in 2:(dimenzija+1))
  {
    for(j in 2:(dimenzija+1))
    {
      for(k in 1:8)
      {
        if(m[i,j]==k*10)
        {
          vektor <- c(m[i-1,j-1], m[i-1, j], m[i-1, j+1], m[i, j-1], m[i, j+1], m[i+1, j-1], m[i+1, j], m[i+1, j+1])
          # Ako je ukupno polja sa minama i zatvorenih polja k, onda sva zatvorena kriju mine
          if(sum(vektor==-100)+sum(vektor==0)==k)
          {
            if(m[i-1, j-1]==0)
              m[i-1, j-1] <- -100
            if(m[i-1, j]==0)
              m[i-1, j] <- -100
            if(m[i-1, j+1]==0)
              m[i-1, j+1] <- -100
            if(m[i, j-1]==0)
              m[i, j-1] <- -100
            if(m[i, j+1]==0)
              m[i, j+1] <- -100
            if(m[i+1, j-1]==0)
              m[i+1, j-1] <- -100
            if(m[i+1, j]==0)
              m[i+1, j] <- -100
            if(m[i+1, j+1]==0)
              m[i+1, j+1] <- -100
          }
        }
      }
    }
  }
  
  # Ostalo je jos da odredimo koji su brojevi u poljima koja smo otvorili
  for (i in 2:(dimenzija+1))
  {
    for (j in 2:(dimenzija+1)) 
    {
      if (m[i,j]<10 & m[i,j]>0)
      {
        vektor <- c(m[i-1,j-1], m[i-1, j], m[i-1, j+1], m[i, j-1], m[i, j+1], m[i+1, j-1], m[i+1, j], m[i+1, j+1])
        # Broj u polju je broj mina u njegovoj okolini
        m[i,j] <- sum(vektor==-100)*10
      }
    }
  }
  
  # Skidamo dodate vrste i kolone
  m <- m[-1, -1]
  m <- m[-(dimenzija+1), -(dimenzija+1) ]
  
  # Ako je broj mina bas jednak trazenom, nema vise mina
  if(sum(m==-100)==broj_mina)
  {
    return(m)
  }
  
  # A ako je broj mina manji od navedenog, i ako je manji bas za broj zatvorenih polja, onda sigurno znamo 
  # da se iza tih zatvorenih polja kriju mine
  if(sum(m==-100)<broj_mina)
  {
    if(sum(m==0)+sum(m==-100)==broj_mina)
    {
      for(i in 1:dimenzija)
      {
        for(j in 1:dimenzija)
        {
          if(m[i,j]==0)
            m[i,j] <- -100
        }
      }
    }
  }
  
  return(m)
}

# Pravimo funkciju koja za prosledjenu matricu tabele Minolovca, treba da pomocu Monte Karlo simulacije
# odredi,koje od preostalih polja ima najvecu verovatnocu da sadrzi minu,i obrnuto,koje polje ima najmanju
# verovatnocu da sadrzi minu

MK_simulacija <- function(M,broj_mina) 
{
  dimenzija <- length(M[1,])
  # Prvo otvorimo sva polja za koja sa sigurnoscu znamo sta kriju
  M <- resi_prosto(M,broj_mina) 
  
  # Lista u kojoj cemo cuvati pozicije(u matrici minolovca) zatvorenih polja
  zatvorena <- list()
  
  k <- 1
  # Prolazimo kroz tabelu minolovca da pokupimo pozicije zatvorenih polja
  for (i in 1:dimenzija)
  {
    for (j in 1:dimenzija)
    {
      if (M[i,j]==0)
      {
        zatvorena[[k]] <- c(i,j)
        k <- k + 1
      }
    }
  }
  
  k <-length(zatvorena) # Ovoliko imamo  zatvorenih polja
  z_polja <- rep(0,k) # Ovde cemo beleziti pojavljivanja mina iza svakog od zatvorenih polja iz liste
  
  # Radimo 1000 simulacija, brojimo pojavljivanja mina iza svakog od zatvorenih polja, ali tako da
  # nam je takva tabla prava tabla minolovca
  
  for (t in 1:1000)
  {
    m <- M
    n <- broj_mina - sum(as.vector(M)==-100) # n je broj zatvorenih polja koja kriju mine
    s = sort(sample(1:k,n)) # Biramo n mesta slucajno, od ukupnog broja zatvorenih polja
    
    # Iz liste zatvorenih polja nadjemo pozicije mesta koja smo odabrali, i postavimo da budu mine
    for (i in 1:n)
      m[zatvorena[[s[i]]][1],zatvorena[[s[i]]][2]] <- -100
    
    # Opet dodajemo vrste i kolone da ne bismo imali problema
    m <- cbind(rep(1, dimenzija), m, rep(1, dimenzija))
    m <- rbind(rep(1, dimenzija+2), m, rep(1, dimenzija+2))
    
    # Sve mine smo rasporedili, ostala  zatvorena polja su brojevi ili prazna, 
    # sto vidimo iz toga koliko je mina u okolini
    for (i in 2:(dimenzija+1))
    {
      for (j in 2:(dimenzija+1))
      {
        if (m[i,j]==0)
        {
          vektor <- c(m[i-1,j-1], m[i-1, j], m[i-1, j+1], m[i, j-1], m[i, j+1], m[i+1, j-1], m[i+1, j], m[i+1, j+1])
          m[i,j] <- sum(vektor==-100)*10 
          # Uklapa se i prazno, onda je sum(vektor==-100)==0, nema mina u okolini
        }
      }
    }
    
    # Skinemo dodate kolone i vrste
    m <- m[-1, -1]
    m <- m[-(dimenzija+1), -(dimenzija+1)]
    
    # Ako smo dobili pravu tablu minolovca, belezimo u vektoru z_polja iza kojih zatvorenih polja su nam mine
    # tj. povecavamo broj pojavljivanja polja na odgovarajucim pozicijama za 1
    if (prava_matrica(m,dimenzija,broj_mina))
    {
      for (i in 1:n)
        z_polja[s[i]] <- z_polja[s[i]]+1
    }
  }
  
  # Iza njega je najcesce bila mina
  v1 <- zatvorena[which.max(z_polja)]
  # Iza njega je najmanje puta bila mina
  v2 <- zatvorena[which.min(z_polja)]
  
  return (c(v1,v2))
}



#           OSMI ZADATAK

# Argumenti funkcije:
# N- koliko ponavljanja Monte Karlo metode zahtevamo
# matrica - trenutna matrica koja je na raspolaganju u igri
# moguci_brojevi-* brojevi koji jos nisu iskorisceni
# ind_prvog - indikator da li prvi igrac na potezu ili ne(uzima vrednosti 0 i 1)

igra_determinanta <- function(N,matrica,moguci_brojevi,ind_prvog)
{
  # Od date matrice pravimo vektor
  vektorizacija <- as.vector(matrica)
  
  # M je velika matrica u koju cemo da smestamo nase verovatnoce
  M <- matrix(rep(0,81),ncol=9,nrow=9)
  
  # Moguca mesta su mesta koja jos nisu popunjena u matrici
  moguca_mesta <- which(vektorizacija==0)
  
  for(i in moguci_brojevi)
  {
    for(j in moguca_mesta)
    {
      # Kako smo fiksirali jedno mesto i jedan broj, moramo ih izbaciti sa raspolaganja
      moguca_m <- moguca_mesta[moguca_mesta!=j]
      moguci_b <- moguci_brojevi[moguci_brojevi!=i]
      
      for(k in 1:N)
      {
        # Dobijemo neku slucajnu permutaciju mogucih mesta i samim tim i brojeva.
        if(length(moguca_m) > 1)
        {
          x <- sample(moguca_m, length(moguca_m), replace=FALSE)
        }
        else
        {
          x <- moguca_m 
        }
        y <- moguci_b # pravljenje samo jednog 'sample'-a dvostruko ubzava kod
        
        # Ubacimo sve u nasu vektorizovanu matricu.
        for(l in 1:length(moguci_b))
        {
          vektorizacija[x[l]]=y[l]
        }        
        vektorizacija[j] <- i
        matr <- vektorizacija
        dim(matr) <- c(3,3)
        D <- det(matr)
        if(D>0 & ind_prvog==1)
        {
          M[i, j] = M[i, j]+1
        }
        if(D<0 & ind_prvog==0)
        {
          M[i, j] = M[i, j]+1
        }
      }
    }
  }
  
  M <- M/N
  return(M)
  # vraca verovatnocu kada je detminanta pozitivna, za broj i stavljen na j-oto mesto (na mestu (i,j) u  matrici)
}

# Napravimo praznu matricu
matrica <- matrix(c(0,0,0,0,4,0,0,0,0), ncol=3)
matrica

# m - matrica koju vraca funkcija
# Odabir gde da se postavi prva cifra, mada svejedno je:
m <- igra_determinanta(100, matrica, c(1,2,3,5,6,7,8,9), T)

simulacija_igre <- function(N, dimenzija)
{
  matrica <- matrix(0, dimenzija[1], dimenzija[2])
  n <- prod(dimenzija)
  for(i in 1:n)
  {
    message("Pocinje ", i, ". potez.")
    
    m <- igra_determinanta(N = i * 3000, matrica = matrica,moguci_brojevi = 1:n, ind_prvog = i %% 2)
    
    
    verovatnoca <- m[which.max(m)]
    broj_pozicija <- which(verovatnoca == m, arr.ind = TRUE)
    broj <- broj_pozicija[1, 1]
    pozicija <- broj_pozicija[1, 2]
    if(verovatnoca == 0)
    {
      if(i %% 2 == 0)
      {
        igrac_koji_pobedjuje = 'prvi'
      }
      else
      {
        igrac_koji_pobedjuje = 'drugi'
      }
      message('Nema smisla nastavljati, pobedio je: ', igrac_koji_pobedjuje, ' igrac!')
      return()
    }
    # upisemo trazeni broj
    matrica[pozicija] <- broj
    message("Posle ", i, "-tog poteza:")
    print(matrica)
  }
  det(matrica)
}

simulacija_igre(1000, c(3,3))







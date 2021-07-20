#           PETI ZADATAK


# U paketu "ISLR" se nalaze skupovi podataka koje cemo koristiti kao primer za stabla odlucivanja,
# tako da cemo prvo instalirati i ucitati te pakete
# install.packages("tree")
# install.packages("ISLR")

library(tree)
library(ISLR)

# Pogledacemo bazu sa kojoj radimo
?Carseats
head(Carseats)
summary(Carseats)
attach(Carseats)

hist(Sales)

# Pravimo binarnu promenljivu koja odredjuje da li je prodaja sedista bila velika tj.
# da li je Sales sedista bila High(>8)

High <- ifelse(Sales<=8,"No","Yes")
High

# Dobijenu promenljivu spajamo sa bazom podataka

Carseats <- data.frame(Carseats, High)
head(Carseats)

# Koristimo funkciju tree() za predvidjanje High koristeci sve promenljive baze osim Sales

tree.carseats <- tree(High ~.-Sales, Carseats)
summary(tree.carseats)

# Crtamo grafik

plot(tree.carseats)
text(tree.carseats, pretty = 0)

# Pogledajmo regije i slomove promenljive High na stablu 
tree.carseats

# Skup za obucavanje i kontrolni skup
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train ,]
High.test <- High[-train]
tree.carseats <- tree(High ~.-Sales, Carseats, subset=train)

# Pogledajmo prediktore na kontrolnom skupu 
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test)

# Unakrsna validacija
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
# pogledajmo dobijeni rezultat
cv.carseats

# Crtamo gresku u funkciji od size i k
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# koristimo fju prune.carseats da obelezimo stablo do stabla sa 9 cvorova
par(mfrow = c(1, 1))
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)


# Pravimo predvidjanje na kontrolnom skupu
prune.pred <- predict(prune.carseats, Carseats.test, type = "class")
# Pogledajmo matricu konfuzije
table(prune.pred, High.test)
# Proveravamo koliko je model dobar
mean(prune.pred == High.test)









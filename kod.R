
```{r}
library(tidyverse)
```

```{r}
srednie <- vector()
maksimum <- vector()
minimum <- vector()
l_rund <- vector()
odch <- vector()
#Losowanie pytań
for(proby in 1:30) {
  
baza <- sample(1:100,100)
testowa <- matrix(data = 1,nrow = 100, ncol = 50)

for(i in 2:50){

    
  losowe <- sample(1:100,100)

  baza <- cbind(baza, losowe)

}

#Sprawdzanie, czy pytanie już bylo

for(wiersz in 2:100){
  for(kolumna in 1:50) {
   if(baza[wiersz,kolumna] %in% baza[1:wiersz-1,]){testowa[wiersz,kolumna] = 0} 
  }
}


#Statystki dla proby 1

oblicz <- vector()

for(kolumna in 1:50){
  oblicz[kolumna] <- sum(testowa[,kolumna])
}
srednie[proby] <- mean(oblicz)
maksimum[proby] <- max(oblicz)
minimum[proby] <- min(oblicz)
odch[proby] <- sd(oblicz)


#Statystki dla proby 2

war_wier <- 1
y <- 1

while(war_wier != 0) {
  y <- y+1
  war_wier <- sum(testowa[y,])
}
l_rund[proby] <- y

}

print(paste("Średnio każdy odpowie na",round(mean(srednie),0),
            "pytania, 70% z was odpowie na od",round(mean(srednie)-mean(odch),0),"do",round(mean(srednie)+mean(odch),0),
            "pytań, a 95% z was na od",round(mean(srednie)-2*mean(odch),0),"do",round(mean(srednie)+2*mean(odch),0),"pytań"))
print(paste("Maksymalnie ktos odpowie na",max(maksimum),"pytań"))
print(paste("Minimalnie ktos odpowie na",min(minimum),"pytań"))
print(paste("Średnio po ",round(mean(l_rund),0),"rundach bedziecie znali odpowiedz na wszystkie pytania"))
```




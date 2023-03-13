library(dplyr)
csv <- read.csv("C:/Users/dansd/OneDrive/Pulpit/Studia/Magisterka/Rok 2/SAS enterprise miner - Szymkowiak/SAS EM/projekt/winequality-red.csv")


csv <- csv %>% mutate(quality_rec = ifelse(quality >= 6, "Good", "Bad"))


is.null(csv)

csv %>% group_by(quality_rec) %>% count()

csv %>% group_by(quality) %>% count()

write.csv(csv, file = "wine.csv")



is.null(csv$density)

mean(csv$density)

frequency(csv$density)



cor(csv$alcohol, csv$res)


wynik <- lm(density~alcohol+residual.sugar, data = csv)

summary(wynik)


mean(abs((csv$alcohol*-0.0009068+csv$residual.sugar*0.0005044+1.0049179 -csv$density)/csv$density))


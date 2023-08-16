library(readr)
daten_aufgabe2 <- read_csv("daten2.csv")


# daten_aufgabe2$x2 <- daten_aufgabe2$x2/10
# daten_aufgabe2$x2 <- daten_aufgabe2$x2/10
# daten_aufgabe2$y <- daten_aufgabe2$y -150 
# daten_aufgabe2$y <- daten_aufgabe2$y * 0.8

hist(daten_aufgabe2$y)

summary(lm(y ~ x1 + x2, data = daten_aufgabe2))
write.csv(daten_aufgabe2, "daten2.csv", row.names=FALSE)

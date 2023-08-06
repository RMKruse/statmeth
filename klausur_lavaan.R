library(lavaan)
set.seed(42)

data("PoliticalDemocracy")
summary(PoliticalDemocracy)

m_gleichung <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ y1 + y2 + y3 + y4
                Var2 ~ Var1' 
m_c <- sem(m_gleichung, data=PoliticalDemocracy)

m_gleichung <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ y1 + y2 + y3 + y4
                Var3 =~ y5+ y6 + y7 + y8
                Var3 ~ Var1 + Var2' 
m_d <- sem(m_gleichung, data=PoliticalDemocracy)

m_gleichung <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ y1 + y2 + y3 + y4
                Var3 =~ y5+ y6 + y7 + y8
                Var3 ~ Var1 + Var2
                y1 ~~ y5
                y2 ~~ y4 + y6
                y3 ~~ y7
                y4 ~~ y8
                y5 ~~ y8'
m_e <- sem(m_gleichung, data=PoliticalDemocracy)

summary(m_d, fit.measures=T)
summary(m_e, fit.measures=T)

AIC(m_d, m_e)

#------ Alternativ Modelle

# einfaches Modell
m_1         <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ y1 + y2 + y3 + y4
                Var3 =~ y5+ y6 + y7 + y8
                Var3 ~ Var1 + Var2' 

m_1 = sem(m_1, data=PoliticalDemocracy)
## Interpretation der Regression? 

# Komplexes Modell
m_2         <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ y1 + y2 + y3 + y4
                Var3 =~ y5+ y6 + y7 + y8
                Var3 ~ Var1 + Var2' 

m_2 <- sem(m_2, data=PoliticalDemocracy)
## Interpretation

# Vergleiche Modelle 1 und 2
## modell wahl
AIC(m_1, m_2)
summary(m_1, fit.measures=T)
summary(m_2, fit.measures=T)
# Covarianzen entfernen
## Covarianz entfernen

## Model Wahl
## Was bedeutet es`?


#### Rename Daten
# dat = PoliticalDemocracy
# names(dat) = c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4", "x5", "x6", "x7")
# dat2 = data.frame(cbind(dat$x1, dat$x2, dat$x3, dat$x4, dat$x5, dat$x6, dat$x7, dat$y1, dat$y2, dat$y3, dat$y4))
# names(dat2) = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "y1", "y2", "y3", "y4")
# write.csv(dat2, "daten2.csv", row.names=FALSE)


# Aufgabe 3)

# a)
# Lade zunächst den Datensatz daten2.csv ein.
# Du hast mehrere Variablen im Datensatz. Die x-Variablen sind erklärende Variablen, die y-Variable sind zu erklärende Variablen.
# Das Problem ist hierbei du bist interessiert an latenten Variablen.

# Erstelle aus den Variablen x1, x2, x3 eine Latente Variable mit den Namen `Var1` und erstelle im selben Modell eine neue Variable mit dem Namen `Var2` aus den Variablen x4, x5, x6, x7.
daten2 <- read_csv("daten2.csv")

m_gleichung <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ x4 + x5 + x6 + x7'
m_3a <- sem(m_gleichung, data=daten2)

# b)
# Erkläre was es bedeutet eine latente Variable wie in Teilaufgabe a) zu bauen.  
# Erkläre dabei was der Abschnitt 'Latent Variables' in der Summary() des SEM Modell aus Teilaufgabe dabei aussagt.

# c)
# Erstelle nun ein Modell, welches ebenfalls die beiden Variablen erstellt. 
# Darüberhinaus erstellst du nun eine latente Variable mit dem Namen `Var3` aus den Variablen `y1`, `y2`,`y3`, `y4`.
# Füge dem Modell noch eine Regressions Komponente hinzu, bei der du `Var1` und `Var2` nutzt um `Var3` zu erklären.
# Interpretiere Ergebnisse für die Regressionskomponente in der Summary.
m_gleichung <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ x4 + x5 + x6 + x7
                Var3 =~ y1 + y2 + y3 + y4
                Var3 ~ Var1 + Var2'
m_3c <- sem(m_gleichung, data=daten2)
summary(m_3c)

# d)
# Wir erweitern nun das letzte Modell, um noch eine explizite residuale Korrelationsstruktur.
# Unterstellte hierbei die folgende Korrelationsstruktur zwischen:
#   
# - `x4` und `y1`
# - KEINE zwischen `x5` und `x7`
# - KEINE zwischen `Var1` und `Var2`
# Verändern sich die Ergebnisse des Regressionsparts?
m_gleichung <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ x4 + x5 + x6 + x7
                Var3 =~ y1 + y2 + y3 + y4
                Var3 ~ Var1 + Var2
                x4 ~~ y1
                x5 ~~ 0*x7
                Var1 ~~ 0*Var2'
m_3d <- sem(m_gleichung, data=daten2)
summary(m_3d)

## 3)
# Vergleiche die Modelle aus c) und d). Wenn du dich für ein Modell entscheiden müsstest, welches würdest du nehmen? 
#   Begründe wieso?
AIC(m_3c, m_3d)
summary(m_3c, fit.measures=T)
summary(m_3d, fit.measures=T)
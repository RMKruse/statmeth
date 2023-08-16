library(lavaan)
set.seed(42)

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
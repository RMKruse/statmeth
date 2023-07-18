# Aufgabe 2)
## a)
data("PoliticalDemocracy")

## b)
m_gleichung <- 'Var1 =~ x1 + x2 + x3' 
m_b <- sem(m_gleichung, data=PoliticalDemocracy)

## c)
m_gleichung <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ y1 + y2 + y3 + y4
                Var2 ~ Var1' 
m_c <- sem(m_gleichung, data=PoliticalDemocracy)

## d)
m_gleichung <- 'Var1 =~ x1 + x2 + x3
                Var2 =~ y1 + y2 + y3 + y4
                Var3 =~ y5+ y6 + y7 + y8
                Var3 ~ Var1 + Var2' 
m_d <- sem(m_gleichung, data=PoliticalDemocracy)

## e)
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


## f)
summary(m_d, fit.measures=T)
summary(m_e, fit.measures=T)

AIC(m_d, m_e)

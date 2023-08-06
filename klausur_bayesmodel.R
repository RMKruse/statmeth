library(readr)
daten_aufgabe2 <- read_csv("~/Downloads/daten_aufgabe2.csv")

hist(daten_aufgabe2$y)

sd_werte = aggregate(data$Reaction, list(data$Subject), FUN=sd) 
sd(sd_werte$x)

data =  lme4::sleepstudy
summary(data)

# library(dplyr)
# 
# data %>%
#   group_by(Subject) %>%
#   summarise_at(vars(Reaction), list(name=sd))  %>%


m3 = brm(Reaction ~ 1 + Days + (1 | Subject) ,
         family = gaussian(),
         data = data,
         prior = c(
           prior(normal(300, 100), class=Intercept),
           prior(normal(0, 60), class = sigma),
           prior(normal(4.5, 3), class = b, coef = Days),
           prior(normal(0, 30), class = sd, coef=Intercept , group = Subject)
           ),
         cores = 10)
summary(m3)

summary(lme4::lmer(Reaction ~ 1 + Days + (1 | Subject), data = data))

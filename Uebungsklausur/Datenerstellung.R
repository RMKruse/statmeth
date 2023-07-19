library(bcogsci)
data(df_pupil)

datensatz1 = c()
datensatz1$x1 = df_pupil$load
datensatz1$x2 = rnorm(41, mean = 100.5, sd = 200)
datensatz1$y = rnorm(41, 640, 100) + datensatz1$x1 * 30 + datensatz1$x2*0 + rnorm(41, 0, 100)
hist(datensatz1$y)


fit_pupil <- brm(y ~ 1 + x1 + x2,
                 data = datensatz1,
                 family = gaussian(),
                 prior = c(
                   prior(normal(640, 100), class = Intercept),
                   prior(normal(0, 150), class = sigma),
                   prior(normal(0, 100), class = b, coef = x1),
                   prior(normal(1.5, 2), class = b, coef = x2)
                 )
)
model_c <-brm(y ~ 1 + x1 + x2,
              data = datensatz1,
              family = gaussian(),
              prior = c(
                prior(normal(675, 150), class = Intercept),
                prior(normal(0, 150), class = sigma),
                prior(normal(2.5, 1.8), class = b, coef = x1),
                prior(normal(125, 200), class = b, coef = x2)
              )
)

summary(model_c)
write.csv(datensatz1, file = "datensatz1.csv", row.names = FALSE)

summary(fit_pupil)
plot(fit_pupil)



# Daten erstellen für die SEM Aufgabe 
library(lavaan)
tosim <- '

#structural component
y ~ .5*f1 + .7*f2  #strength of regression with external criterion


#measurement component
f1 =~ .8*x1 + .6*x2 + .7*x3 + .8*x4 + .75*x5  #definition of factor f with loadings on 5 items

x1 ~~ (1-.8^2)*x1 #residual variances. Note that by using 1-squared loading, we achieve a total variability of 1.0 in each indicator (standardized)
x2 ~~ (1-.6^2)*x2
x3 ~~ (1-.7^2)*x3
x4 ~~ (1-.8^2)*x4
x5 ~~ (1-.75^2)*x5

f2 =~ .8*x6 + .9*x7
x6~~(1-.8^2)*x6*x4
x7~~(1-.9^2)*x7

f1~~.2*f2
'

# generate data; note, standardized lv is default
sim_df <- simulateData(tosim, sample.nobs=200)
write.csv(sim_df, file = "datensatz2.csv", row.names = FALSE)

# Model bauen

m_gleichung <- '#structural
                y ~ f1+ f2
                #measurement
                f1 =~ x1 + x2 + x3 + x4 + x5 
                f2 =~ x6 + x7
                #correlation
                x6 ~~ x4
                '
sem_model <- sem(m_gleichung, sim_df)
summary(sem_model, fit.measures = TRUE)

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


data = PoliticalDemocracy
data = subset(data, select = c(y1, y3, y4, x1, x2, x3))
write.csv(data, file = "datensatz2.csv", row.names = FALSE)

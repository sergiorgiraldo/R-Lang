data <- read.csv("c:/users/sgiraldo/src/R-lang/tapStats/sales.csv")
data.lm <- lm(sales ~ clients, data=data)

plot(data$sales ~ data$clients, pch=15)
coefficients(data.lm)

data <- read.csv("c:/Users/Sergio/source/R-Lang/tapStats/sales.csv")
data.lm <- lm(sales ~ clients, data=data)

plot(data$sales ~ data$clients, pch=15)
coefficients(data.lm)

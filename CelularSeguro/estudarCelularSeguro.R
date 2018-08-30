library(dplyr)
data <- read.csv("fonesSegurosMaisQue1Conta.csv")

summary(data)

data %>%
  group_by(COUNT...) %>%
  orde

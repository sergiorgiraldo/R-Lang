library(ggplot2)
library(stringr)
library(lubridate)

trans <- read.csv("C:\\Users\\sgiraldo\\source\\R\\transacoesJun05-06-07.csv")

trans$time <- as.numeric(str_split_fixed(trans$time, ":", Inf)[,1]) * 60 + as.numeric(str_split_fixed(trans$time, ":", Inf)[,2])

# trans$date <- str_split_fixed(trans$timestamp, "_", Inf)[,1] 
# trans$minute <- str_split_fixed(trans$timestamp, "_", Inf)[,2]
# trans$timestamp <- NULL
# trans <- trans[trans$date == "20180605",]

ggplot(data=trans, aes(x=trans$time, y=trans$qtyTrans, group=1)) +
  geom_line()+
  geom_point()

model <- lm(trans$qtyTrans ~ poly(trans$time, 3))
summary(model)


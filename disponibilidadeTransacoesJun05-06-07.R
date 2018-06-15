library(ggplot2)
library(stringr)

trans <- read.csv("C:\\Users\\sgiraldo\\source\\R\\transacoesJun05-06-07.csv")

trans$date <- str_split_fixed(trans$timestamp, "_", Inf)[,1]
trans$minute <- str_split_fixed(trans$timestamp, "_", Inf)[,2]
trans$timestamp <- NULL
trans <- trans[trans$date == "20180605",]

plot(trans, x = trans$minutes, y = trans$total)


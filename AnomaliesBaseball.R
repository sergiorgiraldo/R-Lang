library(tidyverse)
library(anomalize)

#*******************************************************
#anomalies in baseball
mantle <- read.csv("C:\\temp\\mantle.csv")
mantle$Age = as.Date(paste(mantle$Age, 1, 1, sep = "-"))
mantleDf <- tbl_df(mantle)
mantleDf2 %>%
  time_decompose(HR, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

#*******************************************************
#anomalies in baseball

Batting <- read_csv("C:/Users/sgiraldo/source/R/baseball/data/lahman/Batting.csv", 
col_types = cols(`2B` = col_skip(), `3B` = col_skip(), 
                 AB = col_skip(), BB = col_skip(), 
                 CS = col_skip(), G = col_skip(), 
                 GIDP = col_skip(), HBP = col_skip(), 
                 HR = col_skip(), IBB = col_skip(), 
                 R = col_skip(), RBI = col_skip(), 
                 SB = col_skip(), SF = col_skip(), 
                 SH = col_skip(), SO = col_skip(), 
                 lgID = col_skip(), stint = col_skip(), 
                 teamID = col_skip(), playerID= col_skip()))
BattingDf <- tbl_df(Batting)
BattingDf %>%
  time_decompose(H, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

#************************************

aggdata <-aggregate(Batting, by=list(Batting$yearID), FUN=sum)
aggdata$Group.1 = as.Date(paste(aggdata$Group.1, 1, 1, sep = "-"))
x <- tbl_df(aggdata)
x %>%
   time_decompose(H, method = "stl", frequency = "auto", trend = "auto") %>%
   anomalize(remainder) %>%
   time_recompose() %>%
   plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)



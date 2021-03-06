#Install the devtools package then github packages
install.packages("devtools")
install.packages("Rcpp")
library(devtools)
install_github("petermeissner/wikipediatrend")
install_github("twitter/AnomalyDetection")

#Loading the libraries
library(Rcpp)
library(wikipediatrend)
library(AnomalyDetection)

#*******************************************#
#     FIFA ARTICLES IN WIKIPEDIA            #
#*******************************************#

#Download wikipedia webpage "fifa" 
fifa_data_wikipedia <- wp_trend("fifa", from="2013-03-18", lang = "en")

library(ggplot2)
ggplot(fifa_data_wikipedia, aes(x=date, y=views, color=views)) + geom_line()

# Keep only date & page views and discard all other variables
columns_to_keep <- c("date","views")
fifa_data_wikipedia <- fifa_data_wikipedia[,columns_to_keep]

#Apply anomaly detection and plot the results
anomalies <- AnomalyDetectionTs(fifa_data_wikipedia, direction="pos", plot=TRUE)
anomalies$plot
anomalies$anoms

#*******************************************#
#     BITCOIN PRICES                        #
#*******************************************#

#Installing anomalize
install.packages('anomalize')
#Update from github
library(devtools)
install_github("business-science/anomalize")
#Load the package
library(anomalize)
# We will also use tidyverse package for processing and coindeskr to get bitcoin data
library(tidyverse)
install_github("amrrs/coindeskr")

#Get bitcoin data from 1st January 2017
bitcoin_data <- coindeskr::get_historic_price(start = "2018-01-01", end = "2018-06-01")

#Convert bitcoin data to a time series
bitcoin_data_ts <- bitcoin_data %>% rownames_to_column() %>% as.tibble() %>% mutate(date = as.Date(rowname)) %>% select(-one_of('rowname'))

#Decompose data using time_decompose() function in anomalize package. We will use stl method which extracts seasonality
bitcoin_data_ts %>% time_decompose(Price, method = "stl", frequency = "auto", trend = "auto") %>%  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% plot_anomaly_decomposition()

bitcoin_data_ts %>% time_decompose(Price) %>% anomalize(remainder) %>% time_recompose() %>%  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

#Extract the anomalies
anomalies <- bitcoin_data_ts %>% time_decompose(Price) %>%  anomalize(remainder) %>%  time_recompose() %>%  filter(anomaly == 'Yes')

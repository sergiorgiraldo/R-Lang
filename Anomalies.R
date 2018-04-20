library(tidyverse)
library(anomalize)

dataset <- tidyverse_cran_downloads

tidyverse_cran_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

# time series decomposition
tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto")

# anomaly detection of reminder
tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2)

#just a single package
  tidyverse_cran_downloads %>%
  
  # Select a single time series
  filter(package == "lubridate") %>%
  ungroup() %>%
  
  # Anomalize
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  
  # Plot Anomaly Decomposition
  plot_anomaly_decomposition() +
  ggtitle("Lubridate Downloads: Anomaly Decomposition")
  
#anomaly lower and upper bounds
tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose()  

# bounds in lubridate
tidyverse_cran_downloads %>%
  # Select single time series
  filter(package == "lubridate") %>%
  ungroup() %>%
  # Anomalize
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  # Plot Anomaly Decomposition
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("Lubridate Downloads: Anomalies Detected")


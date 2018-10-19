library(dplyr)
data <- read.csv("data.csv")
colnames(data) <- c("giberish", "count")

summary(data)

data %>%
  group_by(gr=cut(count, breaks= c(0,1,2,3,4,5,10, 100,1000,7867))) %>% 
  summarise(n= n()) %>%
  arrange(as.numeric(gr))

data %>%
  group_by(gr=cut(count, breaks= c(0,1,2,3,4,5,10, 100,1000,7867))) %>% 
  summarise(n= n()) %>%
  mutate(per=paste0(round(n/sum(n)*100, 2), "%")) %>%
  arrange(as.numeric(gr))

library(dplyr)
library(lubridate)

summary(lakers)

lakers %>% 
  mutate(asTime <- ms(time)) %>% 
  mutate(asDate <- ymd(date)) %>% 
  mutate(asDateTime <- ymd_hm(paste(date, time, sep = ""))) %>% 
  mutate(dayOfGame <- wday(ymd(date), TRUE)) %>% 
  mutate(monthOfGame <- month(ymd(date), TRUE)) %>%
  mutate(yearOfGame <- year(ymd(date)))

lakers %>% 
  filter(etype == "shot", team == "LAL", period == "1", result == "made") %>% 
  group_by(date) %>% 
  filter(time == max(time)) %>% 
  ungroup() %>% 
  summarise(meanTimeOfShot = mean(seconds(ms("12:00") - ms(time))))

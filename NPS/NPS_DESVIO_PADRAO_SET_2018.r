library("dplyr")
library("stringr")

#SETEMBRO

setembro <- read.csv2("./NOTAS_setembro.csv")

# TODOS LONGTAIL
TODOSLONGTAIL <-
  setembro %>% 
  filter(str_detect(GRUPO, "LONG TAIL")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  group_by(GRUPO) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  ) %>%
  as.data.frame()

colnames(TODOSLONGTAIL) <- c("NOTA", "DESVIO")

TODOSLONGTAIL

# LONGTAIL
LONGTAIL <-
  setembro %>% 
  filter(str_detect(GRUPO, "LONG TAIL")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  ) %>%
  as.data.frame()

colnames(LONGTAIL) <- c("NOTA", "DESVIO")

LONGTAIL

# TODOS GV
TODOSGV <-
  setembro %>% 
  filter(str_detect(GRUPO, "GV")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  group_by(GRUPO) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  ) %>%
  as.data.frame()

colnames(TODOSGV) <- c("GRUPO", "NOTA", "DESVIO")

TODOSGV

#GV

GV <-
  setembro %>% 
  filter(str_detect(GRUPO, "GV")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  ) %>%
  as.data.frame()

colnames(GV) <- c("NOTA", "DESVIO")

GV


# PF LT
PFLT <-
  setembro %>% 
  filter(str_detect(GRUPO, "PF")) %>%
  filter(str_detect(GRUPO, "LONG TAIL")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(PFLT) <- c("NOTA", "DESVIO")

PFLT

# PF GV
PFGV <-
  setembro %>% 
  filter(str_detect(GRUPO, "PF")) %>%
  filter(str_detect(GRUPO, "GV")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(PFGV) <- c("NOTA", "DESVIO")

PFGV

# PJLT
PJLT <-
  setembro %>% 
  filter(str_detect(GRUPO, "PJ")) %>%
  filter(str_detect(GRUPO, "LONG TAIL")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(PJLT) <- c("NOTA", "DESVIO")

PJLT

# PJGV
PJGV <-
  setembro %>% 
  filter(str_detect(GRUPO, "PJ")) %>%
  filter(str_detect(GRUPO, "GV")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(PJGV) <- c("NOTA", "DESVIO")

PJGV


# WEB
WEB <-
  setembro %>% 
  filter(str_detect(GRUPO, "WEB")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
          (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(WEB) <- c("NOTA", "DESVIO")

WEB

# WEBLT
WEBLT <-
  setembro %>% 
  filter(str_detect(GRUPO, "WEB")) %>%
  filter(str_detect(GRUPO, "LONG TAIL")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(WEBLT) <- c("NOTA", "DESVIO")

WEBLT

# WEBGV
WEBGV <-
  setembro %>% 
  filter(str_detect(GRUPO, "WEB")) %>%
  filter(str_detect(GRUPO, "GV")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(WEBGV) <- c("NOTA", "DESVIO")

WEBGV


# MOBILE
MOBILE <-
  setembro %>% 
  filter(str_detect(GRUPO, "MOBILE")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(MOBILE) <- c("NOTA", "DESVIO")

MOBILE

# MOBILELT

MOBILELT <-
  setembro %>% 
  filter(str_detect(GRUPO, "MOBILE")) %>%
  filter(str_detect(GRUPO, "LONG TAIL")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(MOBILELT) <- c("NOTA", "DESVIO")

MOBILELT

# MOBILEGV

MOBILEGV <-
  setembro %>% 
  filter(str_detect(GRUPO, "MOBILE")) %>%
  filter(str_detect(GRUPO, "GV")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( 
      (100^2 / n()) * ( 
        (sum(PROMOTOR)/n()) * (1 - (sum(PROMOTOR)/n())) + 
          (sum(DETRATOR)/n()) * (1 - (sum(DETRATOR)/n())) + 
          (2 * (sum(PROMOTOR)/n()) * (sum(DETRATOR)/n()))  
      ) ))
  )

colnames(MOBILEGV) <- c("NOTA", "DESVIO")

MOBILEGV

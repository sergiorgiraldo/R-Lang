library("dplyr")
library("stringr")

#OUTUBRO

outubro <- read.csv2("./NOTAS_outubro.csv")

colnames(outubro) <- c('NOTA', 'GRUPO')

outubro <- na.omit(outubro)

# TODOS LONGTAIL
TODOSLONGTAIL <-
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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
  outubro %>% 
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

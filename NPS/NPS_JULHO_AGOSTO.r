library("dplyr")
library("stringr")

#JULHO

julho <- read.csv2("./NOTAS_JULHO.csv")

# TODOS LONGTAIL
LONGTAIL <-
  julho %>% 
    na.omit() %>%
    mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
    group_by(GRUPO) %>%
    summarise( 
      ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
      ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
      ) %>%
    as.data.frame()

colnames(LONGTAIL) <- c("GRUPO", "NOTA", "DESVIO")

LONGTAIL

LONGTAIL %>%
  summarise(sqrt(sum((DESVIO^2))))


# PF
PF <-
  julho %>% 
    filter(str_detect(GRUPO, "PF")) %>%
    na.omit() %>%
    mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
    group_by(GRUPO) %>%
    summarise( 
      ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
      ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
    )

colnames(PF) <- c("GRUPO", "NOTA", "DESVIO")

PF

PF %>%
  summarise(sqrt(sum((DESVIO^2))))

# PJ
PJ <-
  julho %>% 
    filter(str_detect(GRUPO, "PJ")) %>%
    na.omit() %>%
    mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
    group_by(GRUPO) %>%
    summarise( 
      ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
      ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
    )

colnames(PJ) <- c("GRUPO", "NOTA", "DESVIO")

PJ

PJ %>%
  summarise(sqrt(sum((DESVIO^2))))

# WEB
WEB <-
  julho %>% 
    filter(str_detect(GRUPO, "WEB")) %>%
    na.omit() %>%
    mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
    group_by(GRUPO) %>%
    summarise( 
      ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
      ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
    )

colnames(WEB) <- c("GRUPO", "NOTA", "DESVIO")

WEB

WEB %>%
  summarise(sqrt(sum((DESVIO^2))))


# MOBILE
MOBILE <-
  julho %>% 
    filter(str_detect(GRUPO, "MOBILE")) %>%
    na.omit() %>%
    mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
    group_by(GRUPO) %>%
    summarise( 
      ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
      ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
    )

colnames(MOBILE) <- c("GRUPO", "NOTA", "DESVIO")

MOBILE

MOBILE %>%
  summarise(sqrt(sum((DESVIO^2))))

#AGOSTO

agosto <- read.csv2("./NOTAS_agosto.csv")

# TODOS LONGTAIL
LONGTAIL <-
  agosto %>% 
  filter(str_detect(GRUPO, "LONG TAIL")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  group_by(GRUPO) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
  ) %>%
  as.data.frame()

colnames(LONGTAIL) <- c("GRUPO", "NOTA", "DESVIO")

LONGTAIL

LONGTAIL %>%
  summarise(sqrt(sum((DESVIO^2))))

# TODOS GV
GV <-
  agosto %>% 
  filter(str_detect(GRUPO, "GV")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  group_by(GRUPO) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
  ) %>%
  as.data.frame()

colnames(GV) <- c("GRUPO", "NOTA", "DESVIO")

GV

GV %>%
  summarise(sqrt(sum((DESVIO^2))))

# PF
PF <-
  agosto %>% 
  filter(str_detect(GRUPO, "PF")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  group_by(GRUPO) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
  )

colnames(PF) <- c("GRUPO", "NOTA", "DESVIO")

PF

PF %>%
  summarise(sqrt(sum((DESVIO^2))))

# PJ
PJ <-
  agosto %>% 
  filter(str_detect(GRUPO, "PJ")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  group_by(GRUPO) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
  )

colnames(PJ) <- c("GRUPO", "NOTA", "DESVIO")

PJ

PJ %>%
  summarise(sqrt(sum((DESVIO^2))))

# WEB
WEB <-
  agosto %>% 
  filter(str_detect(GRUPO, "WEB")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  group_by(GRUPO) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
  )

colnames(WEB) <- c("GRUPO", "NOTA", "DESVIO")

WEB

WEB %>%
  summarise(sqrt(sum((DESVIO^2))))


# MOBILE
MOBILE <-
  agosto %>% 
  filter(str_detect(GRUPO, "MOBILE")) %>%
  na.omit() %>%
  mutate(PROMOTOR = (NOTA > 8), DETRATOR = (NOTA < 7)) %>%
  group_by(GRUPO) %>%
  summarise( 
    ceiling(((sum(PROMOTOR) - sum(DETRATOR)) / n()) * 100),
    ceiling(sqrt( (var(PROMOTOR)^2 + var(DETRATOR)^2)/ var(NOTA)^2 ) * 100)
  )

colnames(MOBILE) <- c("GRUPO", "NOTA", "DESVIO")

MOBILE

MOBILE %>%
  summarise(sqrt(sum((DESVIO^2))))


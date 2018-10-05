library("dplyr")
library("stringr")

#JUNHO

junho <- read.csv2("./NOTAS_JUNHO.csv")

# TODOS LONGTAIL
TODOSLONGTAIL <-
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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
  junho %>% 
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

#JULHO

julho <- read.csv2("./NOTAS_JULHO.csv")

# TODOS LONGTAIL
TODOSLONGTAIL <-
  julho %>% 
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

colnames(TODOSLONGTAIL) <- c("GRUPO", "NOTA", "DESVIO")

TODOSLONGTAIL

# LONGTAIL

LONGTAIL <-
  julho %>% 
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


# PF
PF <-
  julho %>% 
    filter(str_detect(GRUPO, "PF")) %>%
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

colnames(PF) <- c("NOTA", "DESVIO")

PF

# PJ
PJ <-
  julho %>% 
    filter(str_detect(GRUPO, "PJ")) %>%
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

colnames(PJ) <- c("NOTA", "DESVIO")

PJ

# WEB
WEB <-
  julho %>% 
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


# MOBILE
MOBILE <-
  julho %>% 
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


#AGOSTO

agosto <- read.csv2("./NOTAS_agosto.csv")

# TODOS LONGTAIL
TODOSLONGTAIL <-
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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
  agosto %>% 
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

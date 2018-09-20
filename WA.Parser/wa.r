  library("dplyr")
  library("stringr")
  library("lubridate")
  library("DT")
  library("ggplot2")
  
  history <-  "WhatsAppChat.txt"
  
  # import data ---------------------------
  chat <- readr::read_lines(history, locale = readr::locale(encoding = "UTF-8"))
  nrows <- length(chat)

  # initialise result dataframe ---------------------------
  chat.df <- data.frame(day = rep("d.na", nrows),
                        time = rep("t.na", nrows),
                        author = rep("a.na", nrows),
                        content = rep("c.na", nrows),
                        stringsAsFactors = FALSE)
  
  # data processing ---------------------------
  beginMsgPattern <- "^(\\d{1,2}/\\d{1,2}/\\d{2}),\\s(\\d{2}:\\d{2})\\s-\\s(.*?):\\s(.*)"
  auxIndex <- 0
  auxContent <- ""
  
  for(i in 1:nrows){
    matches <- str_match(chat[i], beginMsgPattern)
    if (length(matches[!is.na(matches)]) > 0){
      if (auxIndex != 0){
        chat.df[auxIndex,4] <- auxContent 
      }
      auxIndex <- auxIndex + 1
      chat.df[auxIndex,1] <- matches[2]
      chat.df[auxIndex,2] <- matches[3] 
      chat.df[auxIndex,3] <- matches[4] 
      auxContent <- matches[5]
    } 
    else{
      auxContent <- paste(auxContent, chat[i], sep="\n")
    }
  }
  chat.df[auxIndex,4] <- auxContent
  chat.df <- subset(chat.df, content != "c.na")
  
  # data transformation ---------------------------
  chat.df[,1] <- mdy(chat.df[,1])
  chat.df[,2] <- hm(chat.df[,2])
  chat.df$periodOfTheDay <- with(chat.df,  
                              ifelse(hour(time) >= 0 & hour(time) <= 5, "madrugada",
                              ifelse(hour(time) > 5 & hour(time) <= 12, "manhã", 
                              ifelse(hour(time) > 12 & hour(time) <= 18, "tarde", 
                              "noite"))))
  
  # data insights ---------------------------
  
  #autores - total
  chat.df %>% 
    summarise(n_distinct(author))
  
  #autores - saíram
  sum(str_count(chat.df$content, "left"))
  
  
  #posts por autor
  chat.df %>% 
    group_by(author) %>% 
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    datatable(caption="POsts por autor")

  #posts por dia
  chat.df %>% 
    group_by(day) %>% 
    summarize(count= n()) %>%
    datatable(caption="Posts por dia")
  
  #media de posts por dia
  ceiling(nrow(chat.df) / as.numeric(today() - chat.df[1,1])) 

  #melhor e pior dia em número de posts
  chat.df %>% 
    group_by(day) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    filter(row_number()==1 | row_number()==n()) %>% 
    datatable(caption="Melhor e pior dia")
  
  chat.df %>%
    mutate(daymn = as.factor(day)) %>%
    group_by(daymn) %>%
    summarise(cnt = n()) %>%
    ggplot(aes(x = (daymn), y = cnt)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
  
  #tempo que o autor escreve
  chat.df %>% 
    group_by(author) %>%
    summarise(DaysInWA= difftime(today(), first(day), unit='days')) %>% 
    datatable(caption="Tempo do autor no chat")

  #posts por período
  chat.df %>% 
    group_by(periodOfTheDay) %>%
    summarize(count = n()) %>% 
    mutate(percentage = paste0(round(count/sum(count)*100, 2), "%"))

  #posts por dia da semana
  chat.df %>% 
    group_by(wday(day)) %>%
    summarize(count = n()) %>% 
    mutate(percentage = paste0(round(count/sum(count)*100, 2), "%"))
  
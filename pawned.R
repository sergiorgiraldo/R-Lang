library(httr)
library(jsonlite)

url  <- "https://haveibeenpwned.com"
path <- "api/v2/breachedaccount/"

clients <- read.csv(file="C:/temp/mailFull.txt",sep = ";",header = TRUE)

for (email in clients$email){
  raw.result <- GET(url = url, path = paste(path,email,sep=""))
  statusCode <- raw.result$status_code
  if (statusCode == 200){
    contents <- rawToChar(raw.result$content)
    if (contents != ""){
      write(email, file = "C:/temp/FoundInPawned.txt", append=TRUE)
      print(email)
    }
  }
  else{
    print(statusCode)
  }
}

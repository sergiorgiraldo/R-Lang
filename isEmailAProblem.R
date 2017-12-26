library("httr")

mustBlockInPs <- function (email, dateOfLeak) { #yyyymmdd
  isAProblem <- FALSE
  
  requestEmail <- GET(paste("http://udp-service-api.gl.mesos.pagseguro.intranet/users/email/", email, sep=""))
  
  if (requestEmail$status_code == 200){ #404 user does not exist
    id <- content(requestEmail)$person_id
    requestPwd <- GET(paste("http://udp-service-api.gl.mesos.pagseguro.intranet/users/", id,"/changed/password/",dateOfLeak,sep=""))
    if (requestPwd$status_code == 200){
      isAProblem <- !content(requestPwd)
    }
  }            
  
  return(isAProblem)
}


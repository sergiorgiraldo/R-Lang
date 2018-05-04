library(genalg)
teams <- read.csv("C:\\Users\\sgiraldo\\source\\python\\juan\\teams.csv",sep=";")
floorCapacity <- 10
chromosome <-  rbinom(nrow(teams), 1, 0.5) 
teams[chromosome == 1, ]

evalFunc <- function(x) {
  current_solution_capacity <- x %*% teams$total
  
  if (current_solution_capacity > floorCapacity) 
    return(0) else return(-current_solution_capacity)
}

iter = 100
GAmodel <- rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)
cat(summary(GAmodel))
solution<-GAmodel$population[which.min(GAmodel$evaluations),]
teams[solution == 1, ]
cat(solution %*% teams$total)


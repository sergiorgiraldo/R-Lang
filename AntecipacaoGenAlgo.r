library("genalg")
#get transactions from db
values <- c(10,12,20,30,40,12,3,12,15,15,30,15,41,10,12) 
howMuchToAntecipate <- 82
evalFunc <- function(chromosome) {
  currentSum <- sum(chromosome %*% values)
  if (currentSum > howMuchToAntecipate ) 
    return(sum(values) + 1) else return(-currentSum)
}
iter = 50
GAmodel <- rbga.bin(size = length(values), popSize = 200, iters = iter, mutationChance = 0.01, 
                    elitism = T, evalFunc = evalFunc)
solution<-GAmodel$population[which.min(GAmodel$evaluations),]
values[solution == 1]
sum(values[solution == 1])
#plot(GAmodel)

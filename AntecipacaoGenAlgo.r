library("genalg")

transactions <- sample.int(1000, sample.int(300, 1))
howMuchToAntecipate <- 3000

evalFunc <- function(chromossome) {
  currentSum <- sum(chromossome %*% transactions)
  if (currentSum > howMuchToAntecipate )
    return(currentSum)
  else
    return(-currentSum)
}

numberOfIterations = 100
sizeOfPopulation = 100
GAmodel <- rbga.bin(size = length(transactions),
                    popSize = sizeOfPopulation,
                    iters = numberOfIterations,
                    mutationChance = 0.01,
                    elitism = T,
                    evalFunc = evalFunc)
solution<-GAmodel$population[which.min(GAmodel$evaluations),]

print("transactions")
print(transactions)
print("solution")
print(solution)
print(transactions[solution == 1])
paste("calculated/given : ",sum(transactions[solution == 1]), "/", howMuchToAntecipate)

plot(GAmodel)


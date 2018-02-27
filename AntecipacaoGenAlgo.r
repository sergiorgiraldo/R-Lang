
library("genalg")

transactions <- sample.int(1000, sample.int(300, 1))

howMuchToAntecipate <- 3000

evalFunc <- function(chromossome) {
  currentSum <- sum(chromossome %*% transactions)
  if (currentSum > howMuchToAntecipate )
    return(sum(transactions) + 1)
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

transactions
transactions[solution == 1]
sum(transactions[solution == 1])

plot(GAmodel)

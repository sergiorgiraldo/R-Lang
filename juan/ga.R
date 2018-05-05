library(genalg)

teams <- read.csv("C:\\Users\\sgiraldo\\source\\python\\juan\\teams.csv",sep=";")
floorCapacity <- 10

#********************************************************#
#Fitness function
evalFunc <- function(chromosome) {
  
  #aux variables
  mondayTeams    <- teams[chromosome[(nrow(teams) * 0 + 1) : (1 * nrow(teams))] == 1, ]
  tuesdayTeams   <- teams[chromosome[(nrow(teams) * 1 + 1) : (2 * nrow(teams))] == 1, ]
  wednesdayTeams <- teams[chromosome[(nrow(teams) * 2 + 1) : (3 * nrow(teams))] == 1, ]
  thursdayTeams  <- teams[chromosome[(nrow(teams) * 3 + 1) : (4 * nrow(teams))] == 1, ]
  fridayTeams    <- teams[chromosome[(nrow(teams) * 4 + 1) : (5 * nrow(teams))] == 1, ]
  
  mondayValues    <- chromosome[(nrow(teams) * 0 + 1) : (1 * nrow(teams))]
  tuesdayValues   <- chromosome[(nrow(teams) * 1 + 1) : (2 * nrow(teams))]
  wednesdayValues <- chromosome[(nrow(teams) * 2 + 1) : (3 * nrow(teams))]
  thursdayValues  <- chromosome[(nrow(teams) * 3 + 1) : (4 * nrow(teams))]
  fridayValues    <- chromosome[(nrow(teams) * 4 + 1) : (5 * nrow(teams))]
  
  #********************************************************#
  #restriction 1: available seats
  currCapacityMonday	 <- (floorCapacity - sum(mondayTeams$total))
  ifelse(currCapacityMonday	 < 0, currCapacityMonday <- 999, currCapacityMonday	 <- currCapacityMonday/floorCapacity)
  
  currCapacityTuesday <- (floorCapacity - sum(tuesdayTeams$total))
  ifelse(currCapacityTuesday < 0, currCapacityTuesday <- 999, currCapacityTuesday <- currCapacityTuesday/floorCapacity)
  
  currCapacityWednesday <- (floorCapacity - sum(wednesdayTeams$total))
  ifelse(currCapacityWednesday < 0, currCapacityWednesday <- 999, currCapacityWednesday <- currCapacityWednesday/floorCapacity)
  
  currCapacityThursday <- (floorCapacity - sum(thursdayTeams$total))
  ifelse(currCapacityThursday < 0, currCapacityThursday <- 999, currCapacityThursday <- currCapacityThursday/floorCapacity)
  
  currCapacityFriday <- (floorCapacity - sum(fridayTeams$total))
  ifelse(currCapacityFriday < 0, currCapacityFriday <- 999, currCapacityFriday <- currCapacityFriday/floorCapacity)
  
  #********************************************************#
  #restriction 2: every team must not appear all days in a row. 
  currViolationsConsecutiveDaysAll <- 0 
  currViolationsConsecutiveDaysNone <- 0
  for (idx in 1: nrow(teams)){
    #teams at ps all days
    if (sum(mondayValues[idx], tuesdayValues[idx], wednesdayValues[idx], thursdayValues[idx], fridayValues[idx]) == 5){
      currViolationsConsecutiveDaysAll <- 999
      break
    }

    #teams at home all days
    if (sum(mondayValues[idx], tuesdayValues[idx], wednesdayValues[idx], thursdayValues[idx], fridayValues[idx]) == 0){
      currViolationsConsecutiveDaysNone <- 999
      break
    }
  }
  
  #if this restriction is not a blocker
  #currViolationsConsecutiveDaysAll <- currViolationsConsecutiveDaysAll / nrow(teams) #normalization
  #currViolationsConsecutiveDaysNone <- currViolationsConsecutiveDaysNone / nrow(teams) #normalization
  
  #********************************************************#
  #restriction 3: dependencies must be enforced

  checkDependencies <- function(dayOfWeekTeams){
    violations <-0
    for (dependencies in dayOfWeekTeams$dependsOn){
      if (dependencies != ""){
        for (dependency in unlist(strsplit(dependencies,","))){
          if (!dependency %in% dayOfWeekTeams$name){
            violations <- violations + 1
          }
        }
      }
    }  
    return(violations)
  }

  totalDependencies <- 0 
  for (dependencies in teams$dependsOn){
    if (dependencies != ""){
      totalDependencies <- totalDependencies + length(unlist(strsplit(dependencies,",")))
    }
  }  

  currViolationsDependenciesMonday	   <- checkDependencies(mondayTeams)/totalDependencies #normalization
  currViolationsDependenciesTuesday    <- checkDependencies(tuesdayTeams)/totalDependencies #normalization
  currViolationsDependenciesWednesday  <- checkDependencies(wednesdayTeams)/totalDependencies #normalization
  currViolationsDependenciesThursday   <- checkDependencies(thursdayTeams)/totalDependencies #normalization
  currViolationsDependenciesFriday     <- checkDependencies(fridayTeams)/totalDependencies #normalization

  return(sum(
      currCapacityMonday,
      currCapacityTuesday,
      currCapacityWednesday,
      currCapacityThursday,
      currCapacityFriday,
      currViolationsConsecutiveDaysAll,
      currViolationsConsecutiveDaysNone,
      currViolationsDependenciesMonday	,
      currViolationsDependenciesTuesday,
      currViolationsDependenciesWednesday,
      currViolationsDependenciesThursday,
      currViolationsDependenciesFriday
      ))
}

GAmodel <- rbga.bin(size = 5 * nrow(teams), popSize = 200, iters = 200, mutationChance = 0.01, elitism = T, evalFunc = evalFunc)
cat(summary(GAmodel))
chromosome<-GAmodel$population[which.min(GAmodel$evaluations),]

#solution is distribution of teams alongside the week
print("Chromosome") 
chromosome
print("Monday") 
teams[chromosome[(nrow(teams) * 0 + 1) :(1 *nrow(teams))] == 1, ]
print("Tuesday") 
teams[chromosome[(nrow(teams) * 1 + 1) :(2 *nrow(teams))] == 1, ]
print("Wednesday") 
teams[chromosome[(nrow(teams) * 2 + 1) :(3 *nrow(teams))] == 1, ]
print("Thursday") 
teams[chromosome[(nrow(teams) * 3 + 1) :(4 *nrow(teams))] == 1, ]
print("Friday") 
teams[chromosome[(nrow(teams) * 4 + 1) :(5 *nrow(teams))] == 1, ]

plot(GAmodel)


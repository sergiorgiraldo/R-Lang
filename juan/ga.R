library(genalg)
teams <- read.csv("C:\\Users\\sgiraldo\\source\\python\\juan\\teams.csv",sep=";")
floorCapacity <- 10

#********************************************************#
#Fitness function
evalFunc <- function(x) {
  
  mondayTeams    <- teams[x[(nrow(teams) * 0 + 1) :(1 *nrow(teams))] == 1, ]
  tuesdayTeams   <- teams[x[(nrow(teams) * 1 + 1) :(2 *nrow(teams))] == 1, ]
  wednesdayTeams <- teams[x[(nrow(teams) * 2 + 1) :(3 *nrow(teams))] == 1, ]
  thursdayTeams  <- teams[x[(nrow(teams) * 3 + 1) :(4 *nrow(teams))] == 1, ]
  fridayTeams    <- teams[x[(nrow(teams) * 4 + 1) :(5 *nrow(teams))] == 1, ]
  
  mondayValues    <- x[(nrow(teams) * 0 + 1) :(1 *nrow(teams))]
  tuesdayValues   <- x[(nrow(teams) * 1 + 1) :(2 *nrow(teams))]
  wednesdayValues <- x[(nrow(teams) * 2 + 1) :(3 *nrow(teams))]
  thursdayValues  <- x[(nrow(teams) * 3 + 1) :(4 *nrow(teams))]
  fridayValues    <- x[(nrow(teams) * 4 + 1) :(5 *nrow(teams))]
  
  #********************************************************#
  #restriction 1: available seats
  curr_capacity_monday <- (floorCapacity - sum(mondayTeams$total))
  ifelse(curr_capacity_monday < 0, curr_capacity_monday <- 999, curr_capacity_monday <- curr_capacity_monday/floorCapacity)
  
  curr_capacity_tuesday <- (floorCapacity - sum(tuesdayTeams$total))
  ifelse(curr_capacity_tuesday < 0, curr_capacity_tuesday <- 999, curr_capacity_tuesday <- curr_capacity_tuesday/floorCapacity)
  
  curr_capacity_wednesday <- (floorCapacity - sum(wednesdayTeams$total))
  ifelse(curr_capacity_wednesday < 0, curr_capacity_wednesday <- 999, curr_capacity_wednesday <- curr_capacity_wednesday/floorCapacity)
  
  curr_capacity_thursday <- (floorCapacity - sum(thursdayTeams$total))
  ifelse(curr_capacity_thursday < 0, curr_capacity_thursday <- 999, curr_capacity_thursday <- curr_capacity_thursday/floorCapacity)
  
  curr_capacity_friday <- (floorCapacity - sum(fridayTeams$total))
  ifelse(curr_capacity_friday < 0, curr_capacity_friday <- 999, curr_capacity_friday <- curr_capacity_friday/floorCapacity)
  
  #********************************************************#
  #restriction 2: every team must not appear all days in a row. 
  curr_violations_consecutiveDays_all <- 0 
  curr_violations_consecutiveDays_none <- 0
  for (idx in 1: nrow(teams)){
    #teams at ps all days
    if (sum(mondayValues[idx], tuesdayValues[idx], wednesdayValues[idx], thursdayValues[idx], fridayValues[idx]) == 5){
      curr_violations_consecutiveDays_all <- 999
    }
    #teams at home all days
    if (sum(mondayValues[idx], tuesdayValues[idx], wednesdayValues[idx], thursdayValues[idx], fridayValues[idx]) == 0){
      curr_violations_consecutiveDays_none <- 999
    }
  }
  
  #if this restriction must not be met
  #curr_violations_consecutiveDays_all <- curr_violations_consecutiveDays_all / nrow(teams) #normalization
  #curr_violations_consecutiveDays_none <- curr_violations_consecutiveDays_none / nrow(teams) #normalization
  
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

  curr_violations_dependencies_monday     <- checkDependencies(mondayTeams)/totalDependencies #normalization
  curr_violations_dependencies_tuesday    <- checkDependencies(tuesdayTeams)/totalDependencies #normalization
  curr_violations_dependencies_wednesday  <- checkDependencies(wednesdayTeams)/totalDependencies #normalization
  curr_violations_dependencies_thursday   <- checkDependencies(thursdayTeams)/totalDependencies #normalization
  curr_violations_dependencies_friday     <- checkDependencies(fridayTeams)/totalDependencies #normalization

  return(sum(
      curr_capacity_monday,
      curr_capacity_tuesday,
      curr_capacity_wednesday,
      curr_capacity_thursday,
      curr_capacity_friday,
      curr_violations_consecutiveDays_all,
      curr_violations_consecutiveDays_none,
      curr_violations_dependencies_monday,
      curr_violations_dependencies_tuesday,
      curr_violations_dependencies_wednesday,
      curr_violations_dependencies_thursday,
      curr_violations_dependencies_friday
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


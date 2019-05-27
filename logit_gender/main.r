library(readr)
library(caret)
library(dplyr)
library(MLmetrics)
library(ggplot2)
library(jtools)

heights_weights <- read_csv("heights_weights.csv")

ggplot(heights_weights,aes(Height,Weight)) +
  geom_point(aes(colour = Male), 
             show.legend = F, size = 1)

train <- heights_weights[1:8000,]
test <- heights_weights[8001:10000,]

train$Male <- as.factor(as.numeric(as.character(train$Male)))

model <- glm(Male ~.,
              family=binomial,
              data=train)

print(summary(model))

plot(model)



featureImportance <- varImp(model, scale = FALSE)
featureImportanceDf <- data.frame(FEATURE = row.names(featureImportance),
                                  IMPORTANCE = featureImportance$Overall)

featureImportanceDf %>% 
  mutate(IMPORTANCE = round(IMPORTANCE, 4)) %>% 
  arrange(-IMPORTANCE)

trainingPred <- predict(model,
                        train,
                        type = "response")

trainingPred <- data.frame(trainingPred) %>% 
  mutate(gender = as.factor(ifelse(trainingPred > 0.3, 1, 0)))

confusionMatrixCalculated <- confusionMatrix(data = trainingPred$gender,
                                             reference = train$Male)

print(confusionMatrixCalculated)

print(paste("GINI: ",
            Gini(as.integer(as.character(trainingPred$gender)),
                 as.integer(as.character(train$Male)))))


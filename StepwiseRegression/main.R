#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

library(tidyverse)
library(caret)
library(leaps)
library(MASS)

summary(swiss)

print("########METHOD 1: stepAIC - MASS package #############")
# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "backward", 
                      trace = FALSE)
summary(step.model)


print("########METHOD 2: regsubsets - leaps package #############")
models <- regsubsets(Fertility~., data = swiss, nvmax = 5,
                     method = "seqrep")
summary(models)

print("########METHOD 3: train - caret package #############")
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Fertility ~., data = swiss,
                    method = "leapBackward", #"lmStepAIC"
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)

coef(step.model$finalModel, 4)

lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, 
   data = swiss)
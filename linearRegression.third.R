#http://r-statistics.co/Linear-Regression.html

years <- 11:18
quantities <- c(1004,1122,1082,1364,1415,1438,1118,1156)
funds <- data.frame(years, quantities)

#scatter plot
scatter.smooth(x=funds$quantities, y=funds$years, main="years ~ quantities")

#density plot
plot(density(funds$quantities), main="Density Plot: Quantities", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(funds$quantities), 2)))  
polygon(density(funds$quantities), col="red")

#correlation
cor(funds$quantities, funds$years)

#linear regression
linearMod <- lm(quantities ~ years, data=funds)  
modelSummary <- summary(linearMod)
modelSummary

plot(years, quantities, pch = 16, cex = 1.3, col = "blue")
abline(lm(quantities ~ years, data=funds))


#t statistic and p-values
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["quantities", "Estimate"]  # get beta estimate for quantities
std.error <- modelCoeffs["quantities", "Std. Error"]  # get std.error for quantities
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(funds)-ncol(funds))  # calc p Value

paste("t_value", t_value)
paste("p_value", p_value)

##predict

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(funds), 0.8*nrow(funds))  # row indices for training data
trainingData <- funds[trainingRowIndex, ]  # model training data
testData  <- funds[-trainingRowIndex, ]   # test data
# Build the model on training data -
lmMod <- lm(quantities ~ years, data=trainingData)  # build the model
quantitiesPred <- predict(lmMod, testData)  # predict yearsance

actuals_preds <- data.frame(cbind(actuals=testData$quantities, predicteds=quantitiesPred))
head(actuals_preds)
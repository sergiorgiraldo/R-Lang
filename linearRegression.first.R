#eruptions = a + b*waiting + epsilon

eruption.lm = lm(eruptions ~ waiting, data=faithful)

plot(faithful$eruptions ~ faithful$waiting)

#solution 1
coeffs = coefficients(eruption.lm)
waiting = 80
duration = coeffs[1] + coeffs[2]*waiting

#solution 2
newdata = data.frame(waiting=80)
predict(eruption.lm, newdata, interval="confidence") #95% of confidence

#r²
summary(eruption.lm)

#plots
eruption.res = resid(eruption.lm)
eruption.stdres = rstandard(eruption.lm)
plot(faithful$waiting, eruption.stdres,ylab="Residuals", 
     xlab="Waiting Time",
     main="Old Faithful Eruptions") 
abline(0, 0)

qqnorm(eruption.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Old Faithful Eruptions") 
qqline(eruption.stdres)


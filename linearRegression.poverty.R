mydata = read.table("c:/users/sgiraldo/source/R/poverty.txt", header = TRUE, stringsAsFactors = FALSE )
poverty.lm = lm(Brth15to17 ~ PovPct, data=mydata)
summary(poverty.lm)
coeffs = coefficients(poverty.lm); coeffs

newdata = data.frame(PovPct=7.1)
predict(poverty.lm, newdata, interval="confidence") #95% of confidence

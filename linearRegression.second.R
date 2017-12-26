#customerSatisfaction.txt
#rows are
#      I,  the index;
#      A1, variable 1;
#      A2, variable 2;
#      A3, variable 3;
#      A4, variable 4;
#      A5, variable 5;
#      A6, variable 6;
#      B,  the satisfaction rating.
#
#      B = A1 * X1 + A2 * X2 + A3 * X3 + A4 * X4 + A5 * X5 + A6 * X6
mydata = read.table("c:/users/sgiraldo/source/R/customerSatisfaction.txt")
satisfaction.lm = lm (V8 ~ V2 + V3 + V4 + V5 + V6 + V7, data=mydata)
newdata = data.frame(V2=82,V3=39,V4=59,V5=64,V6=78,V7=39)
predict(satisfaction.lm, newdata) 

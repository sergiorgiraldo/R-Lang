set.seed(101)
acq.freq <- 200
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)
trajectory <- 3*rnorm(101) + 3*sin(3*w*ts)
plot(trajectory, type="l")

install.packages("TSA")
library(TSA)

p <- periodogram(ts)

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)

# display the 2 highest "power" frequencies
top2

# convert frequency to time periods
time = 1/top2$f
time

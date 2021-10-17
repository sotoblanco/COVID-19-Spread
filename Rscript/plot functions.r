
confirmed_cases <- read_csv("covid.csv")
confirmed_cases$date1 <- as.Date(confirmed_cases$date, format = "%d/%m/%Y")


expo_model <- function(x,p, N0=3, x0=1) {
  N0*(1+p)^(x-x0)
}

logistic <- function(x,a, b, c){
  c/(1+exp(-(x-b)/a))
}

tt <- seq(0,100, length.out = 100)
ggplot(confirmed_cases, aes(date1, total_cases)) +
  geom_point(col = "red") +
  geom_line(data = logistic())
  #geom_smooth(method =  "expo_model", se = TRUE)+
  ylab("Cumulative confirmed cases")+
  geom_line(aes(tt, logistic(tt,4.8,50,100000))+
  scale_x_date(confirmed_cases$date1, labels=date_format("%Y-%m-%d"))

line(confirmed_cases$date1,confirmed_cases$total_cases)
plot(date, cummulative, pch=19, ylab="Total number of confirmed", ylim=c(0,100000))
plot(tt, logistic(tt,4.8,50,100000),ylim=c(0,140000), type = "l",col= "blue",lwd=2, xlab="Time", ylab="Cases")
lines(tt, expo_model(tt,0.23), col = "green", lwd = 2)
legend(80,140000, c("Logistic", "Exponential"), lwd=c(2,2),cex=0.5,col =c("blue", "green"))



#Logistic model for COVID-19
library(MASS)
library(nnet)
library(readr)
library(investr)
library(ggplot2)


options(scipen = 999) #This allow to R to show the values without scientific notation

################################
file_path = "Data"
confirmed_cases <- read_csv(file.path(file_path, "covid.csv"))
confirmed_cases$date <- as.Date(confirmed_cases$date, format = "%d/%m/%Y")
a=confirmed_cases
a$x=seq(length(confirmed_cases$date))
a$y=a$total_cases

##Logistic model
log_model <- nls(y~SSlogis(x, phi1, phi2, phi3), data = a) #nls to choose the best fit parameters
summary(log_model)
xnew <- 174:350 #prediction range

#create logistic model
y.predict.log_model <- predict(log_model, newdata = list(x=xnew))

#predict.log_model
## with CI

#range of the graph
xnew <- 1:350

#create the confidence interval for the logaritmic model
y.pred.conf.log_model <- investr::predFit(log_model, newdata=list(x=xnew), interval = 'confidence')
tail(y.pred.conf.log_model)

#Plot both models with the real data
date1 <- seq(as.Date("2020-02-22"), as.Date("2021-02-05"), by="1 day")

y.pred.conf.log_model <- data.frame(y.pred.conf.log_model, date1)

ggplot(y.pred.conf.log_model, aes(date1,fit))+
  geom_line()+
  geom_point(data=a, aes(date,y, color = "red"))+
  xlab("")+
  ylab("Total number of confimed cases")+
  labs(color = "Logis Model")+
  scale_x_date(date_labels = "%Y-%m-%d")+
  geom_linerange(aes(ymin=lwr, ymax=upr))



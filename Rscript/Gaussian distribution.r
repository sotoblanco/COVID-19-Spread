#untitle 7
##this model is good with logistic it left exponential
library(MASS)
library(nnet)
library(readr)
library(investr)
library(ggplot2)
#install.packages("BiocManager"); BiocManager::install("xcms")
library(xcms)

options(scipen = 999)

################################
file_path = "Data"
confirmed_cases <- read_csv(file.path(file_path, "covid.csv"))
confirmed_cases$date <- as.Date(confirmed_cases$date, format = "%d/%m/%Y")
a=confirmed_cases
a$x=seq(length(confirmed_cases$date))
a$y=a$total_cases

gauss_model <- nls(new~SSgauss(x, mu, sigma, h), data = a) #nls to choose the best fit parameters


summary(gauss_model)
xnew <- 174:300 #prediction range

#create logistic model
y.predict.gauss <- predict(gauss_model, newdata = list(x=xnew))

#predict.gaus_model
## with CI

#range of the graph
xnew <- 1:300

#create the confidence interval for the logaritmic model
y.pred.conf.gauss_model <- investr::predFit(gauss_model, newdata=list(x=xnew), interval = 'confidence')
tail(y.pred.conf.gauss_model)

#Plot both models with the real data
date1 <- seq(as.Date("2020-02-22"), as.Date("2020-12-17"), by="1 day")
date1

y.pred.conf.gauss_model <- data.frame(y.pred.conf.gauss_model, date1)

ggplot(y.pred.conf.gauss_model, aes(date1,fit))+
  geom_line()+
  geom_point(data=a, aes(date,new,colour="red"))+
  xlab("")+
  ylab("New cases daily")+
  scale_x_date(date_labels = "%Y-%m-%d")+
  geom_linerange(aes(ymin=lwr, ymax=upr))
#untitle 7
##this model is good with logistic it left exponential
library(MASS)
library(nnet)
library(readr)
library(investr)

options(scipen = 999)

file_path = "Data"
################################
confirmed_cases <- read_csv(file.path(file_path, "covid.csv"))
confirmed_cases$date <- as.Date(confirmed_cases$date, format = "%d/%m/%Y")
a=confirmed_cases
a$x=seq(length(confirmed_cases$date))
a$y=a$total_cases

#exponential_model
#select parameters
theta.0 <- min(a$y)*0.5
model.0 <- lm(log(a$y - theta.0) ~ a$x, data=a)
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters for exponential model
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
mymodel <- nls(y ~ alpha * exp(beta * x) + theta , data = a, start = start)

# Parameters estimated

alpha  =  mymodel$coefficients[1]
beta   =  mymodel$coefficients[2]
theta = mymodel$coefficients[3]


nlsmodel <- nls(formula = y ~ alpha * exp(beta * x) + theta,
                data=a, start=list(alpha = alpha.0 , beta = beta.0, theta = theta.0))

xnew <- 174:350 ##
y.pred <- predict(nlsmodel, newdata=list(x=xnew))
y.pred.conf <- investr::predFit(nlsmodel, newdata=list(x=xnew), interval = 'confidence')
y.pred.conf <- investr::predFit(nlsmodel, newdata=list(x=xnew), interval = 'confidence')


tail(y.pred.conf)
xnew <- 1:350
y.pred.conf <- investr::predFit(nlsmodel, newdata=list(x=xnew), interval = 'confidence')
tail(y.pred.conf)

##Logistic model
log_model <- nls(y~SSlogis(x, phi1, phi2, phi3), data = a) #nls to choose the best fit parameters
summary(log_model)
xnew <- 174:350 #prediction range

#create logistic model
predict.log_model <- predict(log_model, newdata = list(x=xnew))

#predict.log_model
## with CI

#range of the graph
xnew <- 1:350

#create the confidence interval for the logaritmic model
y.pred.conf.log_model <- investr::predFit(log_model, newdata=list(x=xnew), interval = 'confidence')
tail(y.pred.conf.log_model)

#Plot both models with the real data
#plot(y.pred.conf[,1],"dashed", col="green", type = "l", ylab ="Total number of confirmed" ) #exponential_model
plot(y.pred.conf[,1],lty ="dashed", col="green", type = "l", lwd = 1,ylab = "Total number of confirmed")#exponential
lines(a$y~a$x, col="black",type = "p", pch = 20) #real data
polygon(c(rev(xnew), xnew), c(rev(y.pred.conf.log_model[ ,3]), y.pred.conf.log_model[ ,2]), col = 'green', border = NA) #confidence interval
plot(y.pred.conf.log_model[,1], col="darkgreen", type = "l", lwd = 2)#logistic model

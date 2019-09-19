# Single variable linear fit
x = rnorm(25) # 25 x values from normal distibution
y = 1+x+rnorm(25)/2 # linear relationship: w0=1,w1=1
plot(x,y,pch=1)
abline(1,1,col='blue') # True relationship
fit = lm(y ~ x) # Fitting a linear model
abline(fit$coefficients[1],fit$coefficients[2],col='red') # Plotting the linear fit

# 100 times
stddev = 1/2 # Choose the residual variance here
N = 100
plot(NULL,NULL,xlab='x',ylab='y',main='',xlim=c(-3,3),ylim=c(-2,4))
for (i in 1:100) {
  x = rnorm(N)
  y = 1+x+rnorm(N)*stddev
  fit = lm(y ~ x)
  abline(fit$coefficients[1],fit$coefficients[2],col='red')
}
abline(1,1,col='blue',lwd=2)

# Non-constant variance.
stddev = 1/2 # Choose the standard deviation
N = 100 # How many data points
coef = sd(rnorm(1000000)*(rnorm(1000000)^2)) # normalize so that standard deviation is correct
x = rnorm(N) # Simulate x values from normal distribution
y = 1+x+rnorm(N)*(x^2)/coef*stddev # Linear relationship with residual standard deviation proportional to x^2
plot(x,y,pch=1)
abline(1,1,col='blue')
fit = lm(y ~ x) # Standard least squares fit
abline(fit$coefficients[1],fit$coefficients[2],col='red')

# Do it 100 times
plot(NULL,NULL,xlab='x',ylab='y',main='',xlim=c(-3,3),ylim=c(-2,4))
for (i in 1:100) {
  x = rnorm(N)
  y = 1+x+rnorm(N)*x^2/coef*stddev
  fit = lm(y ~ x)
  abline(fit$coefficients[1],fit$coefficients[2],col='red')
}
abline(1,1,col='blue',lwd=2)

# Now with weighted least squares
stddev = 1/2
N = 100
x = rnorm(N)
y = 1+x+rnorm(N)*(x^2)*stddev
plot(x,y,pch=1)
abline(1,1,col='blue')
fit = lm(y ~ x,weights=1/x^4) # weighted least squares. Weights should be proportional to variance
abline(fit$coefficients[1],fit$coefficients[2],col='red')

# Do it 100 times
plot(NULL,NULL,xlab='x',ylab='y',main='',xlim=c(-3,3),ylim=c(-2,4))
for (i in 1:100) {
  x = rnorm(N)
  y = 1+x+rnorm(N)*x^2*stddev
  fit = lm(y ~ x,weights=1/x^4)
  abline(fit$coefficients[1],fit$coefficients[2],col='red')
}
abline(1,1,col='blue',lwd=2)

# Linear fit with an outlier/leverage point
x = rnorm(25) # Simulate linear relationship as before
y = 1+x+rnorm(25)/2

x = c(x,2) # add (2,-2) - an outlier point
y = c(y,-2)
plot(x,y,pch=1)
abline(1,1,col='blue')
fit = lm(y ~ x) # fit with least squares
abline(fit$coefficients[1],fit$coefficients[2],col='red')

# Fitting non linear data
xgrid = data.frame(seq(from=-10,to=10,by=0.01)) # Make a grid of points for plotting
colnames(xgrid) = c('x')

x = rnorm(100)*2 # samples from normal
y = sin(x)+rnorm(100)/4 # a sine relationship
data = data.frame(cbind(x,y)) # R wants a data.frame for poly fit
plot(x,y)
fit = lm(y ~ x, data) # Linear fit from data.frame
lines(xgrid[,1],predict(fit,xgrid)) # Draw it
plot(x,y)
fit = lm(y ~ poly(x,2), data) # Degree 2 fit
lines(xgrid[,1],predict(fit,xgrid)) # Draw it
plot(x,y)
fit = lm(y ~ poly(x,3), data) # Degree 3 fit
lines(xgrid[,1],predict(fit,xgrid))
plot(x,y)
fit = lm(y ~ poly(x,4), data) # Degree 4 fit
lines(xgrid[,1],predict(fit,xgrid))
plot(x,y)
fit = lm(y ~ poly(x,5), data) # Degree 5 fit
lines(xgrid[,1],predict(fit,xgrid))
plot(x,y)
fit = lm(y ~ poly(x,10), data) # Degree 10 fit
lines(xgrid[,1],predict(fit,xgrid))

# Polynomial fit to linear data
xgrid = data.frame(seq(from=-5,to=5,by=0.01))
colnames(xgrid) = c('x')

x = rnorm(25) # Same linear simulation
y = 1+x+rnorm(25)/2
data = data.frame(cbind(x,y))
plot(x,y)
fit = lm(y ~ x, data)
lines(xgrid[,1],predict(fit,xgrid))

plot(NULL,NULL,xlab='x',ylab='y',xlim=c(-3,3),ylim=c(-2,4))
for (i in 1:10) {
  x = rnorm(25)
  y = 1+x+rnorm(25)/2
  data = data.frame(cbind(x,y))
  fit = lm(y ~ x, data)
  lines(xgrid[,1],predict(fit,xgrid))
}

# Fitting degree 5 polynomial
x = rnorm(25)
y = 1+x+rnorm(25)/2
data = data.frame(cbind(x,y))
plot(x,y)
fit = lm(y ~ poly(x,5), data)
lines(xgrid[,1],predict(fit,xgrid))

# Fitting degree 10 polynomial
x = rnorm(25)
y = 1+x+rnorm(25)/2
data = data.frame(cbind(x,y))
plot(x,y)
fit = lm(y ~ poly(x,10), data)
lines(xgrid[,1],predict(fit,xgrid))

plot(NULL,NULL,xlab='x',ylab='y',xlim=c(-3,3),ylim=c(-2,4))
for (i in 1:10) {
  x = rnorm(25)
  y = 1+x+rnorm(25)/2
  data = data.frame(cbind(x,y))
  fit = lm(y ~ poly(x,10), data)
  lines(xgrid[,1],predict(fit,xgrid))
}




# Load packages/datasets
  library(MASS) 
  library(ISLR)

# Simple linear regression
  # Variables
    names(Boston)
    ?Boston # codebook
  
  # Plot
    plot(medv ~ lstat, Boston)
    abline(fit1, col = "red")
  
  
  # Linear model
    fit1 = lm(medv ~ lstat, data = Boston)
    fit1
    summary(fit1) # more detailed

    # confidence interval for the fit 
      confint(fit1)
      
    # query a linear model fit at specific values
      predict(fit1, data.frame(lstat = c(5,10,15)), interval = "confidence")

  
# Multiple linear regression

    fit2 = lm(medv ~ lstat + age, data = Boston)
    summary(fit2)

    fit3 = lm(medv ~., Boston) # use all variables
    summary(fit3)

    # plot
      par(mfrow = c(2,2)) # mulitple plts
      plot(fit3)
      
      fit4 = update(fit3,~.-age-indus) # remove nonsignificant variables
      summary(fit4)



# Nonlinear terms and Interactions
  fit5 = lm(medv ~ lstat*age, Boston)
  summary(fit5)

  fit6 = lm(medv ~ lstat + I(lstat^2), Boston) # protect with identity function
  summary(fit6)
  
  # Plot
    attach(Boston)
    par(mfrow = c(1,1)) # go back to single pane
    plot(medv ~ lstat)
    points(lstat, fitted(fit6), col = "red", pch = 20) # use this instead of abline because nonlinear

    # better to fit polynomals
    fit7 = lm(medv ~ poly(lstat, 4)) #4th degree polynomial
    points(lstat,fitted(fit7),col = "blue",pch = 20)

# Characters available for plotting!    
  plot(1:20, 1:20, pch = 1:20, cex = 2)
    
# Qualitative predictors
  names(Carseats)
  summary(Carseats)

  fit1 = lm(Sales ~ . + Income:Advertising + Age:Price, Carseats)
  summary(fit1)

  contrasts(Carseats$ShelveLoc)
  
# Writing R functions: Fit a regression model and fit a plot
  # Function 1
    regplot = function(x,y) 
      {                     # indicates starts giving the code in our function
      fit = lm(y ~ x)       # put x and y into a linear model
      plot(x,y)             # plot x, y
      abline(fit,col="red") # add the model line to the plot
      }
    
    # Execute the function
      attach(Carseats)
      regplot(Price, Sales)

  # Function 2
    regplot = function(x,y,...) # unnamed arguments, available to add
      {
        fit = lm(y~x)
        plot(x,y,...)
        abline(fit,col = "red")
      }

    regplot(Price, Sales, xlab = "Price", ylab = "Sales", col = "blue", pch = 20)





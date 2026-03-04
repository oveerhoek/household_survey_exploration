# Load the mtcars dataset
data(mtcars)

# Create a histogram of the hp variable
hist(mtcars$hp, 
     main = "Histogram van de horsepower variabele", 
     xlab = "Horsepower", 
     ylab = "Frequentie")

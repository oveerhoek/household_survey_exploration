# Load the mtcars dataset
data(mtcars)

# Set up the PNG device to save the plot
png("src/aider/histogram_hp.png", width = 800, height = 600)

# Create a histogram of the hp variable
hist(mtcars$hp, 
     main = "Histogram van de horsepower variabele", 
     xlab = "Horsepower", 
     ylab = "Frequentie")

# Close the PNG device
dev.off()

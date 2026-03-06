# Load the mtcars dataset
data(mtcars)

# Define output path
out_path <- file.path("src", "aider", "histogram_hp.png")

# Ensure the output directory exists
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

# Set up the PNG device to save the plot
png(out_path, width = 800, height = 600)
on.exit(dev.off(), add = TRUE)

# Create a histogram of the hp variable
hist(
  mtcars$hp,
  main = "Histogram van de horsepower variabele",
  xlab = "Horsepower",
  ylab = "Frequentie",
  col = "steelblue",
  border = "white"
)

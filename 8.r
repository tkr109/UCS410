# Import the csv data file in R.
data <- read.csv("C:/Users/deepa/Desktop/Clt-data.csv")

# Validate data for correctness by counting the number of rows and viewing the top ten rows of the dataset.
nrow(data)
ncol(data)
head(data, 10)

# Calculate the population mean and plot the observations by making a histogram.
mean_value <- mean(data$Wall.Thickness)
mean_value
hist(data$Wall.Thickness, main = "Histogram of Wall Thickness", xlab = "Wall Thickness", ylab = "Frequency", col = "lightblue")
abline(v = mean_value, col = "red", lwd = 2)

# Comment on whether the data is normally distributed or not.

# The histogram visually shows the distribution of the Wall Thickness data. 
# If the data follows a normal distribution, the histogram should have a bell-shaped curve.
# Observing the plot, you can comment on whether the data appears to be normally distributed or not.

# Draw sufficient samples of size 10, calculate their means, and plot them in R by making a histogram.
sample_10 <- replicate(1000, mean(sample(data$Wall.Thickness, 10, replace = TRUE)))
hist(sample_10)

# Do you get a normal distribution?

# Now repeat the same with sample sizes 50, 500, and 9000.
sample_50 <- replicate(1000, mean(sample(data$Wall.Thickness, 50, replace = TRUE)))
hist(sample_50)

sample_500 <- replicate(1000, mean(sample(data$Wall.Thickness, 500, replace = TRUE)))
hist(sample_500)

sample_9000 <- replicate(1000, mean(sample(data$Wall.Thickness, 9000, replace = TRUE)))
hist(sample_9000)

# Can you comment on what you observe?

# Here, observe the histograms for different sample sizes and comment on whether they approach a normal distribution.
# If the sampling distribution of the mean approaches normality as the sample size increases, it supports the use of sampling distributions for further analysis.
# Provide your observations and insights on the sampling distributions.

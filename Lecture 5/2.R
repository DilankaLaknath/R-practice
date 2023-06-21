# Original sample data
ants <- c(43, 59, 22, 25, 36, 47, 19, 21)

# Set seed for reproducibility
set.seed(1)

# Number of bootstrap samples
num_samples <- 1000

# Empty vector to store means of bootstrap samples
bootstrap_means <- numeric(num_samples)

# Generate bootstrap samples and calculate means
for (i in 1:num_samples) {
  bootstrap_sample <- sample(ants, size = length(ants), replace = TRUE)
  bootstrap_means[i] <- mean(bootstrap_sample)
}

# Plot histogram of bootstrap sample means
hist(bootstrap_means, breaks = 20, col = "skyblue", main = "Histogram of Bootstrap Sample Means")


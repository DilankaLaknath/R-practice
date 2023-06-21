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

# Calculate mean of the means
mean_of_means <- mean(bootstrap_means)

# Calculate standard error
standard_error <- sd(bootstrap_means) / sqrt(num_samples)

# Output the results
print(mean_of_means)
print(standard_error)

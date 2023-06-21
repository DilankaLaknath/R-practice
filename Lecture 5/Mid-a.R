# Original sample
original_sample <- c(43, 59, 22, 25, 36, 47, 19, 21)

# Number of bootstrap samples
num_bootstraps <- 100000

# Empty vectors to store bootstrap means and standard deviations
bootstrap_means <- numeric(num_bootstraps)
bootstrap_stdevs <- numeric(num_bootstraps)

# Perform bootstrapping
for (i in 1:num_bootstraps) {
  # Generate bootstrap sample by sampling with replacement
  bootstrap_sample <- sample(original_sample, replace = TRUE)
  
  # Calculate mean and standard deviation of the bootstrap sample
  bootstrap_means[i] <- mean(bootstrap_sample)
  bootstrap_stdevs[i] <- sd(bootstrap_sample)
}

# Mean and standard deviation of the original sample
original_mean <- mean(original_sample)
original_stdev <- sd(original_sample)

# Print the results
cat("Original Sample:\n")
cat("Mean: ", original_mean, "\n")
cat("Standard Deviation: ", original_stdev, "\n\n")

cat("Bootstrap Sample Results:\n")
cat("Mean (Bootstrap): ", mean(bootstrap_means), "\n")
cat("Standard Deviation (Bootstrap): ", mean(bootstrap_stdevs), "\n")

# Step 1: Generate samples from two normal distributions
set.seed(123)  # Set seed for reproducibility

# Generate sample 1 from N(5, 0.5)
sample1 <- rnorm(10, mean = 5, sd = 0.5)

# Generate sample 2 from N(6, 0.5)
sample2 <- rnorm(10, mean = 6, sd = 0.5)

# Step 2: Calculate the mean difference
mean_diff <- mean(sample1) - mean(sample2)

# Step 3: Generate bootstrap population of mean difference
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})

# Step 4: Plot histogram of bootstrap population
hist(bootstrap_population, main = "Bootstrap Distribution of Mean Difference", xlab = "Mean Difference")

# Step 5: Conduct hypothesis test
# H0: Mu = 0
# Ha: Mud != 0
p_value <- 2 * min(sum(bootstrap_population <= mean_diff), sum(bootstrap_population >= mean_diff))

# Step 6: State p-value and conclusion
cat("p-value:", p_value, "\n")
if (p_value < 0.05) {
  cat("Reject H0: There is a significant difference in means.\n")
} else {
  cat("Fail to reject H0: There is no significant difference in means.\n")
}

# Step 7: Repeat experiment for different sample sizes
sample_sizes <- c(10, 100, 1000)

par(mfrow = c(1, 3))  # Create subplots
for (size in sample_sizes) {
  # Generate samples
  sample1 <- rnorm(size, mean = 5, sd = 0.5)
  sample2 <- rnorm(size, mean = 6, sd = 0.5)
  
  # Generate bootstrap population
  bootstrap_population <- replicate(1000, {
    bootstrap_sample1 <- sample(sample1, replace = TRUE)
    bootstrap_sample2 <- sample(sample2, replace = TRUE)
    mean(bootstrap_sample1) - mean(bootstrap_sample2)
  })
  
  # Plot bootstrap distribution
  hist(bootstrap_population, main = paste("Sample Size:", size), xlab = "Mean Difference")
}


# Step 8: Repeat experiment for different pairs of distributions and sample sizes

# Define the pairs of distributions and sample sizes
distribution_pairs <- list(
  list(mean1 = 5.5, sd1 = 0.1, mean2 = 6.5, sd2 = 0.1),
  list(mean1 = 5.5, sd1 = 0.5, mean2 = 6.5, sd2 = 0.5),
  list(mean1 = 5.5, sd1 = 1.0, mean2 = 6.5, sd2 = 1.0),
  list(mean1 = 5.5, sd1 = 5.0, mean2 = 6.5, sd2 = 5.0)
)
sample_sizes <- c(10, 100, 1000)

par(mfrow = c(length(distribution_pairs), length(sample_sizes)))  # Create subplots

for (i in 1:length(distribution_pairs)) {
  for (j in 1:length(sample_sizes)) {
    # Generate samples
    sample1 <- rnorm(sample_sizes[j], mean = distribution_pairs[[i]]$mean1, sd = distribution_pairs[[i]]$sd1)
    sample2 <- rnorm(sample_sizes[j], mean = distribution_pairs[[i]]$mean2, sd = distribution_pairs[[i]]$sd2)
    
    # Generate bootstrap population
    bootstrap_population <- replicate(1000, {
      bootstrap_sample1 <- sample(sample1, replace = TRUE)
      bootstrap_sample2 <- sample(sample2, replace = TRUE)
      mean(bootstrap_sample1) - mean(bootstrap_sample2)
    })
    
    # Plot bootstrap distribution
    hist(bootstrap_population, main = paste("Distribution Pair:", i, "| Sample Size:", sample_sizes[j]), xlab = "Mean Difference")
  }
}

# Step 10: Hypothesis test for mean difference at 90% significance level
alpha <- 0.10  # Significance level

# Pair 1: N~(5.5,0.1) & N ~ (6.5,0.1)
sample1 <- rnorm(1000, mean = 5.5, sd = 0.1)
sample2 <- rnorm(1000, mean = 6.5, sd = 0.1)
mean_diff <- mean(sample1) - mean(sample2)
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})
p_value_pair1 <- mean(abs(bootstrap_population) >= abs(mean_diff))

# Pair 2: N~(5.5,0.5) & N ~ (6.5,0.5)
sample1 <- rnorm(1000, mean = 5.5, sd = 0.5)
sample2 <- rnorm(1000, mean = 6.5, sd = 0.5)
mean_diff <- mean(sample1) - mean(sample2)
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})
p_value_pair2 <- mean(abs(bootstrap_population) >= abs(mean_diff))

# Pair 3: N~(5.5,1.0) & N ~ (6.5,1.0)
sample1 <- rnorm(1000, mean = 5.5, sd = 1.0)
sample2 <- rnorm(1000, mean = 6.5, sd = 1.0)
mean_diff <- mean(sample1) - mean(sample2)
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})
p_value_pair3 <- mean(abs(bootstrap_population) >= abs(mean_diff))

# Pair 4: N~(5.5,5.0) & N ~ (6.5,5.0)
sample1 <- rnorm(1000, mean = 5.5, sd = 5.0)
sample2 <- rnorm(1000, mean = 6.5, sd = 5.0)
mean_diff <- mean(sample1) - mean(sample2)
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})
p_value_pair4 <- mean(abs(bootstrap_population) >= abs(mean_diff))

# Print calculated p-values
cat("Pair 1 p-value:", p_value_pair1, "\n")
cat("Pair 2 p-value:", p_value_pair2, "\n")
cat("Pair 3 p-value:", p_value_pair3, "\n")
cat("Pair 4 p-value:", p_value_pair4, "\n")


# Step 11: Hypothesis test for mean difference at 95% significance level
alpha <- 0.05  # Significance level

# Pair 1: N~(5.5,0.1) & N ~ (6.5,0.1)
sample1 <- rnorm(1000, mean = 5.5, sd = 0.1)
sample2 <- rnorm(1000, mean = 6.5, sd = 0.1)
mean_diff <- mean(sample1) - mean(sample2)
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})
p_value_pair1 <- mean(abs(bootstrap_population) >= abs(mean_diff))

# Pair 2: N~(5.5,0.5) & N ~ (6.5,0.5)
sample1 <- rnorm(1000, mean = 5.5, sd = 0.5)
sample2 <- rnorm(1000, mean = 6.5, sd = 0.5)
mean_diff <- mean(sample1) - mean(sample2)
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})
p_value_pair2 <- mean(abs(bootstrap_population) >= abs(mean_diff))

# Pair 3: N~(5.5,1.0) & N ~ (6.5,1.0)
sample1 <- rnorm(1000, mean = 5.5, sd = 1.0)
sample2 <- rnorm(1000, mean = 6.5, sd = 1.0)
mean_diff <- mean(sample1) - mean(sample2)
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})
p_value_pair3 <- mean(abs(bootstrap_population) >= abs(mean_diff))

# Pair 4: N~(5.5,5.0) & N ~ (6.5,5.0)
sample1 <- rnorm(1000, mean = 5.5, sd = 5.0)
sample2 <- rnorm(1000, mean = 6.5, sd = 5.0)
mean_diff <- mean(sample1) - mean(sample2)
bootstrap_population <- replicate(1000, {
  bootstrap_sample1 <- sample(sample1, replace = TRUE)
  bootstrap_sample2 <- sample(sample2, replace = TRUE)
  mean(bootstrap_sample1) - mean(bootstrap_sample2)
})
p_value_pair4 <- mean(abs(bootstrap_population) >= abs(mean_diff))

# Print calculated p-values
cat("Pair 1 p-value:", p_value_pair1, "\n")
cat("Pair 2 p-value:", p_value_pair2, "\n")
cat("Pair 3 p-value:", p_value_pair3, "\n")
cat("Pair 4 p-value:", p_value_pair4, "\n")

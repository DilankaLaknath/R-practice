# Original sample data
ants <- c(43, 59, 22, 25, 36, 47, 19, 21)

# Set seed for reproducibility
set.seed(1)

# Bootstrap sampling with replacement
bootstrap_sample <- sample(ants, size = length(ants), replace = TRUE)

# Calculate mean and standard deviation of the bootstrap sample
mean_ants <- mean(bootstrap_sample)
sd_ants <- sd(bootstrap_sample)

# Output the results
print(mean_ants)
print(sd_ants)

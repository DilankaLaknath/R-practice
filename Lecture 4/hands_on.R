# Generate a random sample with a 50/50 proportion using binomial sampling
sample_size <- 100000

# Generate a random sample using binomial sampling
random_sample <- rbinom(sample_size, 8, 0.5)

# Convert the binary values to meaningful groups
# normalized_random_sample <- ifelse(random_sample == 0, "Head", "Tail")

# View the resulting sample
random_sample

# normalized_random_sample

hist(random_sample, main = "observations = 100, size = 8, prob = 0.5")


# Count the number of occurrences of 8 in the random sample
count_8 <- sum(random_sample == 8)

# View the count
count_8/sample_size
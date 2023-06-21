  # Define a population (Hight of SriLankans)
p_mean <- 1.6
p_sd <- 0.2

# Sampling
sample_size <- 100
sample <- rnorm(sample_size, p_mean, p_sd)
sample_mean <- mean(sample)

#Creating a array of 100 size where sample means are there
nu_of_samples <- 10000
sample_means <- numeric(nu_of_samples)

for (i in 1:nu_of_samples){
  sample <- rnorm(sample_size, p_mean, p_sd)
  sample_means[i] <- mean(sample)
}
sample_means

hist(sample_means, main = "mean = 1.6, sd = 0.2, sample_size = 10, nu_of_samples = 100")


# Bootstrap sampling
boot_sample_means <- numeric(nu_of_samples)

for (i in 1:nu_of_samples){
  boot_sample_means[i] <- mean(sample(sample, sample_size, replace=TRUE))
}

hist(boot_sample_means, main = "mean = 1.6, sd = 0.2, sample_size = 10, nu_of_samples = 100")

sd(sample_means)
sd(boot_sample_means)
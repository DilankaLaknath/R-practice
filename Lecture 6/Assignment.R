# Parameters
n_values <- c(1, 10, 30, 50, 100)
p_values <- c(0.5, 0.7, 0.1)

# Plotting
par(mfrow = c(length(p_values), length(n_values)), mar = c(2, 4, 2, 2))

# Generate and plot proportion distributions
for (p in p_values) {
  for (n in n_values) {
    # Simulate binomial distribution and calculate proportions
    x <- rbinom(1000, n, p) / n
    
    # Plot proportion distribution
    hist(x, main = paste0("p = ", p, ", n = ", n), ylab = "Proportion",
         xlim = c(0, 1), ylim = c(0, 30), col = "skyblue", border = "white", horiz = TRUE)
  }
}

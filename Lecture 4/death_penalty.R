# Set the seed for reproducibility
set.seed(123)

# Define the observed proportions
p1980 <- 0.66
p2010 <- 0.64

# Define the sample sizes
n1980 <- 1000  # Replace with the actual sample size from 1980
n2010 <- 1000  # Replace with the actual sample size from 2010

# Calculate the standard errors
se1980 <- sqrt(p1980*(1-p1980)/n1980)
se2010 <- sqrt(p2010*(1-p2010)/n2010)

# Calculate the observed difference in proportions
obs_diff <- p1980 - p2010

# Calculate the pooled standard error
pooled_se <- sqrt(se1980^2 + se2010^2)

# Calculate the z-score
z_score <- obs_diff / pooled_se

# Calculate the p-value
p_value <- 1 - pnorm(z_score)

# View the p-value
p_value

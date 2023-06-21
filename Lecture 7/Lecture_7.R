# Set the significance level
alpha <- 0.05

# Set the mean and standard deviation
mean <- 3.44
standard_deviation <- 4.70

# Calculate the standard error
standard_error <- standard_deviation / sqrt(18)

# Calculate the quantile for the upper tail
upper_quantile <- qt(1 - alpha/2, df = 18 - 1)

# Calculate the quantile for the lower tail
lower_quantile <- qt(alpha/2, df = 18 - 1)

# Calculate the endpoints
upper_endpoint <- mean + upper_quantile * standard_error
lower_endpoint <- mean - lower_quantile * standard_error

# Print the endpoints
print(upper_endpoint)
print(lower_endpoint)

# Set the value
value <- 2.30

# Set the degrees of freedom
df <- 15 - 1

# Calculate the area to the right
# area_right <- 1 - pt(value, df)
area_right <- pt(value, df, lower.tail = FALSE)

# Print the result
print(area_right)


p_1 <- 0.2
p_2 <- 0.36

n_1 <- 167
n_2 <- 189

nu_of_samples <- 1000

sample_1 <- c(rep(1, 33),rep(0, 167-33))
sample_2 <- c(rep(1, 68),rep(0, 189-68))


sample_dist <- numeric(1000)


for (i in 1:1000){
  ratio_1 <- sum(sample(sample_1, n_1, replace=T))/n_1
  ratio_2 <- sum(sample(sample_2, n_2, replace=T))/n_2
  sample_dist[i] <- (ratio_1 - ratio_2)
}

sample_dist

hist(sample_dist)
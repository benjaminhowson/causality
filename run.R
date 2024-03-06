# experiments with small number of observations
results <- experiment(n = 1000, niters = 100, propensity = TRUE, response = TRUE)
write.csv(results, "all-correct.csv")

results <- experiment(n = 1000, niters = 100, propensity = TRUE, response = FALSE)
write.csv(results, "prop-correct.csv")

results <- experiment(n = 1000, niters = 100, propensity = FALSE, response = TRUE)
write.csv(results, "resp-correct.csv")

results <- experiment(n = 1000, niters = 100, propensity = FALSE, response = FALSE)
write.csv(results, "none-correct.csv")
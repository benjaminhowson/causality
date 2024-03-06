library(ggplot2)

plot <- function(data, ate = 1){
  # visualise the bias of estimator
  data$values <- data$values - ate
  
  ggplot(data, aes(x = values, color = method)) + 
    labs(x = "Bias", y = "Density") + 
    geom_density()
}

# visualise results via density plot
results <- read.csv("results/all-correct.csv")
plot(results)

# visualise results via density plot
results <- read.csv("results/prop-correct.csv")
plot(results)


# visualise results via density plot
results <- read.csv("results/resp-correct.csv")
plot(results)

# visualise results via density plot
results <- read.csv("results/none-correct.csv")
plot(results)

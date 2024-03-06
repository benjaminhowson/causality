library(ggplot2)

plot <- function(data, ate = 0){
  # visualise the bias of estimator
  data$values <- data$values - ate
  
  ggplot(data, aes(x = values, color = method)) + 
    labs(x = "Bias", y = "Density") + 
    geom_density()
}

# visualise results via density plot
results <- read.csv("all-correct.csv")
plot(results)

# visualise results via density plot
results <- read.csv("prop-correct.csv")
plot(results)


# visualise results via density plot
results <- read.csv("resp-correct.csv")
plot(results)

# visualise results via density plot
results <- read.csv("none-correct.csv")
plot(results)

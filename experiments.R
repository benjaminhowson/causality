install.packages("mlr3")
install.packages("xgboost")
install.packages("DoubleML")
install.packages("Matching")

library(mlr3)
library(xgboost)
library(DoubleML)
library(Matching)

logger = lgr::get_logger("mlr3")
logger$set_threshold("warn")

generate <- function(n){
  # size variable for binomial distribution
  size <- 1
  
  # generate covariates for nuisance functions
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  X3 <- rnorm(n)
  
  # compute treatment probabilities
  p <- 1/(1 + exp(-(X1 + X2 + 0.5 * X1 * X2)))
  
  # create treatment indicators
  A <- rbinom(n, size, p)
  
  # compute mean vector for responses
  mu <- (1 - A) * (X2 + X3) + A*(1 + X2 + X3)
  
  # create response variables
  Y <- rnorm(n, mu)
  
  # create data.frame using the variables
  data <- data.frame(X1, X2, X3, A, Y)
  
  return(data)
}

matching <- function(data, propensity = TRUE){
  if (propensity) {
    # fit logistic regression for treatment probabilities
    prop <- glm(A ~ X1 + X2 + X1*X2, data = data, 
                family = binomial(link = "logit"))
  } else {
    # fit logistic regression for treatment probabilities
    prop <- glm(A ~ X1, data = data, 
                family = binomial(link = "logit"))
  }
  
  # predict treatment probabilities using logistic model
  phat <- predict(prop, type = "response")
  
  # compute the average treatment effect
  value <- Match(data$Y, data$A, phat, estimand = "ATE", version = "fast")$est[1, 1]
  
  return(value)
}

# method for augmented inverse propensity weighting
augmented <- function(data, propensity = TRUE, response = TRUE){
  
  if (propensity){
    # fit logistic regression for treatment probabilities
    prop <- glm(A ~ X1 + X2 + X1*X2, data = data, 
                family = binomial(link = "logit"))
  } else {
    # fit logistic regression for treatment probabilities
    prop <- glm(A ~ X1, data = data, 
                family = binomial(link = "logit"))
  }
  
  # predict treatment probabilities using logistic model
  phat <- predict(prop, type = "response")
  
  if (response){
    # model adjusting for treatment and covariates
    resp <- lm(Y ~ A + X2, data = data)
  } else {
    # model adjusting for treatment and covariates
    resp <- lm(Y ~ A + X3, data = data)
  }
  
  # partition dataset into control and treated
  control <- data
  treated <- data
  
  # create fake world with everyone control or treated
  control$A <- 0
  treated$A <- 1
  
  # compute mean response in the world with everyone treated
  mutreat <- mean(data$A * (data$Y - predict(resp))/phat + predict(resp, treated))
  
  # compute mean response in world with everyone in control
  mucontr <- mean((1 - data$A) * (data$Y - predict(resp))/(1 - phat) + predict(resp, control))
  
  return(mutreat - mucontr)
}

doubleml <- function(data, method){
  # create dataframe compatible with the software package
  df <- DoubleML::double_ml_data_from_data_frame(data, 
                                                 y_col = c("Y"), 
                                                 d_cols = c("A"), 
                                                 x_cols = c("X1", "X2", "X3"))

  if (method == "xgboost"){
    # use xgboost for modelling nuissance parameters
    xgboost <- DoubleML::DoubleMLPLR$new(df, n_folds = 10, n_rep = 10,
                                         ml_g = mlr3::lrn("regr.xgboost"), 
                                         ml_m = mlr3::lrn("classif.xgboost", predict_type = "prob"))
    xgboost$fit()
    return(xgboost$coef)
  }
  
  if (method == "svm"){
    svm <- DoubleML::DoubleMLPLR$new(df, n_folds = 10, n_rep = 10, 
                                     ml_g = mlr3::lrn("regr.svm"), 
                                     ml_m = mlr3::lrn("classif.svm", predict_type = "prob"))
    svm$fit()
    return(svm$coef)
  }
}

experiment <- function(n, niters, response = TRUE, propensity = TRUE){
  values <- c()
  method <- c()
  
  for (it in 1:niters){
    print(it/niters)
    # create data for this iteration
    data <- generate(n)
    
    # compute average treatment effect with matching
    values <- c(values, matching(data, propensity))
    method <- c(method, "PSM")
    
    # compute average treatment effect with augmented prop weighting
    values <- c(values, augmented(data, propensity, response))
    method <- c(method, "AIPW")
    
    # compute average treatment effect with double machine learning
    values <- c(values, doubleml(data, method = "xgboost"))
    method <- c(method, "DML (XGBoost)")

    # compute average treatment effect with double machine learning
    values <- c(values, doubleml(data, method = "svm"))
    method <- c(method, "DML (SVM)")
    
  }
  
  results <- data.frame(values, method)
  
  return(results)
}

plot <- function(data, ate = 1){
  # visualise the bias of estimator
  data$values <- data$values - ate
  
  ggplot(data, aes(x = values, color = method)) + 
    labs(x = "Bias", y = "Density") + 
    geom_density()
}


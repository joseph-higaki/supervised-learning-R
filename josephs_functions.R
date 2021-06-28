library(stats)

# found this idempotent way to install / load library
check_install_package <- function(package_name) {
  if (length(find.package(package_name, quiet=TRUE)) <= 0){  
    install.packages(package_name)
  }
  library(package_name, character.only=TRUE)
  #eval(paste("library(",package_name,")"))
}


#' linear_model_prediction_result
#' 
#' @param formula Language formula to feed the model
#' @param training_set 
#' @param testing_set 
#' @param original_set_column The column that holds real observation values
#'
#' @return a vector with model indicators
linear_model_prediction_result <- function(
  formula,
  training_set,
  testing_set,
  original_set_column)
{
  linear_model <- lm(formula = formula, data = training_set)
  prediction <- predict(linear_model, newdata = testing_set)
  mse <- mean((original_set_column - prediction)^2)
  #return (c(deparse(formula), mse, prediction))
  return (c(deparse(formula), mse, 0))
}


#' Title split_train_test_data
#' Returns array of Train test data
#' @param observations_dataset 
#' @param train_sample_percentage  number from 0 to 1, to represent how much from the observation dataset will be splitted into the train dataset
#'
#' @return An array of dataset. First element: Train. Second element: Test
split_train_test_data <- function(
  observations_dataset,
  train_sample_percentage,
  sampling_random_seed
)
{
  train_sample_size <- floor(train_sample_percentage * nrow(observations_dataset))
  set.seed(sampling_random_seed)
  train_indexes <- sample(seq_len(nrow(observations_dataset)), size = train_sample_size)
  return (
    list("train_data" = observations_dataset[train_indexes, ], 
         "test_data" = observations_dataset[-train_indexes, ]))
  #train_data <-
  #test_data <- observations_dataset[-train_indexes, ]
}

#setNames(result_models, c("formula_name","kernel function","polynomial_degree", "gamma", "cost", "accuracy"))
svm_model_prediction_result <- function(
  formula,
  kernel_function,
  polynomial_degree,
  gamma,
  cost,
  training_set,
  testing_set,
  original_set_column)
{
  svm_model <- svm(formula, 
                   kernel = kernel_function, 
                   data = training_set, 
                   degree = polynomial_degree, 
                   gamma = gamma, 
                   cost = cost)
  #prediction <- 
  confusion_matrix <- table(predict(svm_model, testing_set), original_set_column)
  return (
    c(
      deparse(formula), 
      kernel_function, 
      polynomial_degree, 
      gamma, 
      cost, 
      sum(diag(confusion_matrix)) / sum(confusion_matrix)
      )
    )
  #"formula_name","kernel function","polynomial_degree", "gamma", "cost", "accuracy"
}
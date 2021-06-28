source("josephs_functions.R")

#load packages
check_install_package(package_name = "MASS")
check_install_package(package_name = "ISLR")

#load data
data(Carseats)

# *********************************************************************
# ************************** understand data **************************
# *********************************************************************
names(Carseats)
# data dictionary:
# https://rdrr.io/cran/ISLR/man/Carseats.html  

# I have 10 variables, that means I would have 2^10 = 1024 ways to arrange possible predicting variables
summary(Carseats)
levels(Carseats$ShelveLoc)
levels(Carseats$US)
levels(Carseats$Urban)
# TOO MANY VARIABLES TO REVIEW 
pairs(Carseats, pch = ".")
#REVIEW IN SETS
pairs( Sales ~ Income, Carseats)
pairs( Sales ~ Income + Advertising, Carseats)
pairs(~ Sales + Income + Advertising + CompPrice + Price, Carseats)
pairs( Sales ~ Population + Age + Education, Carseats)
# *********************************************************************
# ******************* high level insights from looking at pairs ********
# *********************************************************************
# CompPrice does not seem to correlate with Sales
# Income does not seem to correlate with Sales
# Advertising DOES seem to correlate with Sales
# Population does not seem to correlate with Sales
# Age DOES seem to correlate somehow with Sales, looks like areas where people are older have more sales

# *********************************************************************
#hyper params
# *********************************************************************
train_sample_percentage <- 0.75
sampling_random_seed <- 666

# *********************************************************************
#split data set in train and test data 
# *********************************************************************
splitted_data <- split_train_test_data(Carseats, train_sample_percentage, sampling_random_seed)
train_data <- splitted_data$train_data # train data
test_data <- splitted_data$test_data  # test data


# *********************************************************************
# create a data frame to see several models using candidate variable  combinations
result_models <- data.frame(matrix(nrow = 0, ncol = 3))
setNames(result_models, c("formula_name","mse","prediction_vector"))
summary(result_models)

# Add example from pdf lab
result_models[nrow(result_models) + 1, ] = linear_model_prediction_result(Sales ~ Income + Advertising + ShelveLoc, train_data, test_data, Carseats$Sales)

result_models[nrow(result_models) + 1, ] = 
  linear_model_prediction_result(Sales ~ Income,
                                 train_data, test_data, Carseats$Sales)

result_models[nrow(result_models) + 1, ] = 
  linear_model_prediction_result(Sales ~ Income + Advertising,
                                 train_data, test_data, Carseats$Sales)

result_models[nrow(result_models) + 1, ] = 
  linear_model_prediction_result(Sales ~ Advertising,
                                 train_data, test_data, Carseats$Sales)

result_models[nrow(result_models) + 1, ] = 
  linear_model_prediction_result(Sales ~ Age,
                                 train_data, test_data, Carseats$Sales)

result_models[nrow(result_models) + 1, ] = 
  linear_model_prediction_result(Sales ~ Income + Age,
                                 train_data, test_data, Carseats$Sales)


result_models[nrow(result_models) + 1, ] = 
  linear_model_prediction_result(Sales ~ Advertising + Age,
                                 train_data, test_data, Carseats$Sales)

result_models[nrow(result_models) + 1, ] = 
  linear_model_prediction_result(Sales ~ Population + Advertising,
                                 train_data, test_data, Carseats$Sales)

result_models[nrow(result_models) + 1, ] = 
  linear_model_prediction_result(Sales ~ Income + Population + Advertising,
                                 train_data, test_data, Carseats$Sales)

print(result_models)
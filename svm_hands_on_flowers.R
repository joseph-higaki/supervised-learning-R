source("josephs_functions.R")

# Library containing the SVM model
check_install_package("e1071")
check_install_package("caret")

#load data
data(iris)

# *********************************************************************
# ************************** understand data **************************
# *********************************************************************
head(iris, 5)
summary(iris)
nrow(iris)
names(iris)
levels(iris$Species)

# *********************************************************************
# ************************** check vazriables combinations *********************
# *********************************************************************
# choosing 2 variables from 4
# 4 choose 2 (n!)/(k!(n-k)!) = 6 combinations
plot(iris$Sepal.Length, iris$Sepal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])
plot(iris$Sepal.Length, iris$Petal.Length, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])
plot(iris$Sepal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])
plot(iris$Sepal.Width, iris$Petal.Length, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])
plot(iris$Sepal.Width, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])

#plot(1:150, iris$Sepal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])

# *********************************************************************
#hyper params
# *********************************************************************
train_sample_percentage <- 0.75
sampling_random_seed <- 123

# *********************************************************************
#split data set in train and test data 
# *********************************************************************
splitted_data <- split_train_test_data(iris, train_sample_percentage, sampling_random_seed)
train_data <- splitted_data$train_data # train data
test_data <- splitted_data$test_data  # test data

# fit the model
svm_model <- svm(Species ~ Sepal.Width + Sepal.Length, kernel="linear", data=train_data)
summary(svm_model)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Species)

obj <- confusionMatrix(confusion_matrix)
obj$overall["Accuracy"]
(accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix))

#plot(x = svm_model, data = train_data, formula = Species ~ Sepal.Width + Sepal.Length)
     #,   iris$Sepal.Length, iris$Sepal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])
#plot

# *********************************************************************
# create a data frame to see several models using candidate variable  combinations
# *********************************************************************
result_models <- data.frame(matrix(nrow = 0, ncol = 7))
result_models <- setNames(result_models, c("formula_name","kernel_function","polynomial_degree", "gamma", "coef0", "cost", "accuracy"))
names(result_models)
# *********************************************************************
#  Sepal.Width + Sepal.Length - linear
# *********************************************************************
formula <- Species ~ Sepal.Width + Sepal.Length
kernel_function <- "linear"
svm_model <- svm(formula, kernel = kernel_function, data=train_data)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Species)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    deparse(formula),
    kernel_function,
    "N/A",
    "N/A",
    "N/A",
    "N/A",
    accuracy)

# *********************************************************************
#  Sepal.Width + Sepal.Length - polynomial
# degree = 3
# *********************************************************************
formula <- Species ~ Sepal.Width + Sepal.Length
kernel_function <- "polynomial"
degree <- 3
svm_model <- svm(formula, kernel = kernel_function, degree = degree, data=train_data)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Species)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    deparse(formula),
    kernel_function,
    degree,
    "N/A",
    "N/A",
    "N/A",
    accuracy)

# *********************************************************************
#  Sepal.Width + Sepal.Length - polynomial 
# degree = 3, gamma 2.5
# *********************************************************************
formula <- Species ~ Sepal.Width + Sepal.Length
kernel_function <- "polynomial"
degree <- 3
gamma <- 2.5
svm_model <- svm(formula, kernel = kernel_function, degree = degree, data=train_data, gamma = gamma)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Species)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    deparse(formula),
    kernel_function,
    degree,
    gamma,
    "N/A",
    "N/A",
    accuracy)
plot(svm_model, data = iris, Sepal.Width ~ Sepal.Length, fill = TRUE, grid = 10,
     svSymbol = "🚀",
     symbolPalette = c("red","green4","blue"),
     color.palette =  hsv_palette()
     )


# *********************************************************************
#  Sepal.Width + Sepal.Length - radial
# *********************************************************************
formula <- Species ~ Sepal.Width + Sepal.Length
kernel_function <- "radial"
gamma <- 100
svm_model <- svm(formula, kernel = kernel_function, data=train_data, gamma = gamma)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Species)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    deparse(formula),
    kernel_function,
    "N/A",
    gamma,
    "N/A",
    "N/A",
    accuracy)


    
result_models


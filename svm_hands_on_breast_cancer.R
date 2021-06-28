source("josephs_functions.R")
check_install_package("mlbench") #BReast|Cncer data
check_install_package("e1071") #ssvm
check_install_package("caret") #confusionmatrix function

# *********************************************************************
# ************************** understand data **************************
# *********************************************************************
data(BreastCancer)
head(BreastCancer, 100)
summary(BreastCancer)
levels(BreastCancer$Class)
str(BreastCancer)

# *********************************************************************
# *********** Noticed that Bare.nuclei had NA values. removing them from set
# *********************************************************************
BreastCancerClean <- na.omit(BreastCancer)
summary(BreastCancerClean)

# *********************************************************************
# ****** make chars categorical values, numeric ***********************
# *********************************************************************
BreastCancerClean[,"Cl.thickness"] <- as.numeric(as.character(BreastCancerClean[,"Cl.thickness"]))
BreastCancerClean[,"Cell.size"] <- as.numeric(as.character(BreastCancerClean[,"Cell.size"]))
BreastCancerClean[,"Cell.shape"] <- as.numeric(as.character(BreastCancerClean[,"Cell.shape"]))
BreastCancerClean[,"Marg.adhesion"] <- as.numeric(as.character(BreastCancerClean[,"Marg.adhesion"]))
BreastCancerClean[,"Epith.c.size"] <- as.numeric(as.character(BreastCancerClean[,"Epith.c.size"]))
BreastCancerClean[,"Bare.nuclei"] <- as.numeric(as.character(BreastCancerClean[,"Bare.nuclei"]))
BreastCancerClean[,"Bl.cromatin"] <- as.numeric(as.character(BreastCancerClean[,"Bl.cromatin"]))
BreastCancerClean[,"Normal.nucleoli"] <- as.numeric(as.character(BreastCancerClean[,"Normal.nucleoli"]))
BreastCancerClean[,"Mitoses"] <- as.numeric(as.character(BreastCancerClean[,"Mitoses"]))
#"Cl.thickness"    "Cell.size"       "Cell.shape"      "Marg.adhesion"  [6] "Epith.c.size"    
#"Bare.nuclei"     "Bl.cromatin"     "Normal.nucleoli" "Mitoses"        [11] "Class"          names(BreastCancerClean)

plot(BreastCancerClean$Cl.thickness, BreastCancerClean$Cell.size, pch=21, bg=c("blue", "red")[unclass(BreastCancerClean$Class)])
plot(BreastCancerClean$Cell.size, BreastCancerClean$Cell.shape, pch=21, bg=c("blue", "red")[unclass(BreastCancerClean$Class)])
plot(BreastCancerClean$Cell.size, BreastCancerClean$Epith.c.size, pch=21, bg=c("blue", "red")[unclass(BreastCancerClean$Class)])
plot(BreastCancerClean$Bare.nuclei, BreastCancerClean$Bl.cromatin, pch=21, bg=c("blue", "red")[unclass(BreastCancerClean$Class)])
plot(BreastCancerClean$Bare.nuclei, BreastCancerClean$Normal.nucleoli, pch=21, bg=c("blue", "red")[unclass(BreastCancerClean$Class)])
plot(BreastCancerClean$Mitoses, BreastCancerClean$Marg.adhesion, pch=21, bg=c("blue", "red")[unclass(BreastCancerClean$Class)])
plot(BreastCancerClean$Marg.adhesion, BreastCancerClean$Epith.c.size, pch=21, bg=c("blue", "red")[unclass(BreastCancerClean$Class)])


# *********************************************************************
# Plot histograms of single variables
# *********************************************************************
par(mfrow=c(3,3))
for(i in 2:10) {
  #barplot(table(BreastCancerClean$Class, BreastCancerClean[,i]), main=names(BreastCancer)[i], legend.text=unique(BreastCancerClean$Class))
  barplot(table(BreastCancerClean$Class, BreastCancerClean[,i]), main=names(BreastCancer)[i])
}


par(mfrow=c(3,3))
for(i in 2:10) {
  hist(BreastCancerClean[,i], main=names(BreastCancer)[i])
}

# *********************************************************************
#hyper params
# *********************************************************************
train_sample_percentage <- 0.75
sampling_random_seed <- 342

# *********************************************************************
# **** Cost matrix assumptions
# **** predict benign cancer on actual malignant tumor = 100
# **** predict malign cancer on actual benign tumor = 2
# *********************************************************************
cost_matrix <- matrix(c(0,100,2, 0), nrow = 2, byrow = TRUE)
cost_matrix

# *********************************************************************
#split data set in train and test data 
# *********************************************************************
splitted_data <- split_train_test_data(BreastCancerClean, train_sample_percentage, sampling_random_seed)
train_data <- splitted_data$train_data # train data
test_data <- splitted_data$test_data  # test data


# *********************************************************************
# create a data frame to see several models using candidate variable  combinations
# *********************************************************************
result_models <- data.frame(matrix(nrow = 0, ncol = 8))
result_models <- setNames(result_models, c("formula_name","kernel_function","polynomial_degree", "gamma", "coef0", "cost_parameter", "accuracy", "prediction_cost"))
names(result_models)

# *********************************************************************
#  Class ~ Cell.size + Cl.thickness - linear C = 10
# *********************************************************************
formula <- Class ~ Cell.size + Cl.thickness
kernel_function <- "linear"
C <- 10
svm_model <- svm(formula, kernel = kernel_function, data=train_data, C = C)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
prediction_cost <- sum(confusion_matrix * cost_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    paste(deparse(formula), collapse = ""),
    kernel_function,
    "N/A",
    "N/A",
    "N/A",
    C,
    accuracy,
    prediction_cost)

# *********************************************************************
#  Class ~ Cell.size + Cl.thickness - linear C = 100
# *********************************************************************
formula <- Class ~ Cell.size + Cl.thickness
kernel_function <- "linear"
C <- 100
svm_model <- svm(formula, kernel = kernel_function, data=train_data, C = C)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
prediction_cost <- sum(confusion_matrix * cost_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    paste(deparse(formula), collapse = ""),
    kernel_function,
    "N/A",
    "N/A",
    "N/A",
    C,
    accuracy,
    prediction_cost)

# *********************************************************************
# LINEAR C = 10
#  Class ~
# Cl.thickness + 
#   Cell.size + 
#   Cell.shape + 
#   Marg.adhesion + 
#   Epith.c.size + 
#   Bare.nuclei + 
#   Bl.cromatin +  
#   Normal.nucleoli + 
#   Mitoses  
# *********************************************************************
formula <- Class ~
  Cl.thickness + 
  Cell.size + 
  Cell.shape + 
  Marg.adhesion + 
  Epith.c.size + 
  Bare.nuclei + 
  Bl.cromatin +  
  Normal.nucleoli + 
  Mitoses 
kernel_function <- "linear"
C <- 10
svm_model <- svm(formula, kernel=kernel_function, data=train_data, C = C)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
prediction_cost <- sum(confusion_matrix * cost_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    paste(deparse(formula), collapse = ""),
    kernel_function,
    "N/A",
    "N/A",
    "N/A",
    C,
    accuracy,
    prediction_cost)

# *********************************************************************
# LINEAR C = 100
#  Class ~
# Cl.thickness + 
#   Cell.size + 
#   Cell.shape + 
#   Marg.adhesion + 
#   Epith.c.size + 
#   Bare.nuclei + 
#   Bl.cromatin +  
#   Normal.nucleoli + 
#   Mitoses  
# *********************************************************************
formula <- Class ~
  Cl.thickness + 
  Cell.size + 
  Cell.shape + 
  Marg.adhesion + 
  Epith.c.size + 
  Bare.nuclei + 
  Bl.cromatin +  
  Normal.nucleoli + 
  Mitoses 
kernel_function <- "linear"
C <- 100
svm_model <- svm(formula, kernel=kernel_function, data=train_data, C = C)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
prediction_cost <- sum(confusion_matrix * cost_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    paste(deparse(formula), collapse = ""),
    kernel_function,
    "N/A",
    "N/A",
    "N/A",
    C,
    accuracy,
    prediction_cost)

# *********************************************************************
# POLYNOMIAL degree=3 gamma=2.5 C = 10
#  Class ~
# Cl.thickness + 
#   Cell.size + 
#   Cell.shape + 
#   Marg.adhesion + 
#   Epith.c.size + 
#   Bare.nuclei + 
#   Bl.cromatin +  
#   Normal.nucleoli + 
#   Mitoses  
# *********************************************************************
formula <- Class ~
  Cl.thickness + 
  Cell.size + 
  Cell.shape + 
  Marg.adhesion + 
  Epith.c.size + 
  Bare.nuclei + 
  Bl.cromatin +  
  Normal.nucleoli + 
  Mitoses 
kernel_function <- "polynomial"
degree <- 3
gamma <- 2.5
C <- 10
svm_model <- svm(formula, kernel=kernel_function, data=train_data,
                 degree = degree,
                 gamma = gamma,
                 C = C)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
prediction_cost <- sum(confusion_matrix * cost_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    paste(deparse(formula), collapse = ""),
    kernel_function,
    degree,
    gamma,
    "N/A",
    C,
    accuracy,
    prediction_cost)

# *********************************************************************
# POLYNOMIAL degree=4 gamma=5 C = 1000
#  Class ~
# Cl.thickness + 
#   Cell.size + 
#   Cell.shape + 
#   Marg.adhesion + 
#   Epith.c.size + 
#   Bare.nuclei + 
#   Bl.cromatin +  
#   Normal.nucleoli + 
#   Mitoses  
# *********************************************************************
formula <- Class ~
  Cl.thickness + 
  Cell.size + 
  Cell.shape + 
  Marg.adhesion + 
  Epith.c.size + 
  Bare.nuclei + 
  Bl.cromatin +  
  Normal.nucleoli + 
  Mitoses 
kernel_function <- "polynomial"
degree <- 4
gamma <- 5
C <- 1000
svm_model <- svm(formula, kernel=kernel_function, data=train_data,
                 degree = degree,
                 gamma = gamma,
                 C = C)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
prediction_cost <- sum(confusion_matrix * cost_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    paste(deparse(formula), collapse = ""),
    kernel_function,
    degree,
    gamma,
    "N/A",
    C,
    accuracy,
    prediction_cost)

# *********************************************************************
# RADIAL   gamma=2 C = 10
#  Class ~
# Cl.thickness + 
#   Cell.size + 
#   Cell.shape + 
#   Marg.adhesion + 
#   Epith.c.size + 
#   Bare.nuclei + 
#   Bl.cromatin +  
#   Normal.nucleoli + 
#   Mitoses  
# *********************************************************************
formula <- Class ~
  Cl.thickness + 
  Cell.size + 
  Cell.shape + 
  Marg.adhesion + 
  Epith.c.size + 
  Bare.nuclei + 
  Bl.cromatin +  
  Normal.nucleoli + 
  Mitoses 
kernel_function <- "radial"
gamma <- 2
C <- 10
svm_model <- svm(formula, kernel=kernel_function, data=train_data,
                 gamma = gamma,
                 C = C)
prediction <- predict(svm_model, test_data)
confusion_matrix <- table(prediction, test_data$Class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
prediction_cost <- sum(confusion_matrix * cost_matrix)
result_models[nrow(result_models) + 1, ] = 
  c(
    paste(deparse(formula), collapse = ""),
    kernel_function,
    "N/A",
    gamma,
    "N/A",
    C,
    accuracy,
    prediction_cost)



result_models
write.csv(result_models, "result_models.csv")
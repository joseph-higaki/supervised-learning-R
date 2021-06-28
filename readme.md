# Supervised Learning Models
This is a course assignment for supervised machine learning models using R.
This is from the Data Science and Advanced Analytics course from the Big Data &amp; Analytics Masters @ [EAE](https://www.eae.es/) class of 2021.
This assignment has three sections.
1. Regression Analysis for Child Carseat Sales
1. Classification Analysis for Breast Cancer 
1. Classification Analysis for Iris Species


## Regression Analysis for Child Carseat Sales
Given a dataset of 400 observations (locations) with 11 variables, we need to predict the sales volume.

[Dataset documentation](https://rdrr.io/cran/ISLR/man/Carseats.html)

**Answer**

I used Linear Regression with 8 different variable combinations.
Model performance was evaluated using Mean Square Error

R script found here: [regression_hands_on.R](https://github.com/joseph-higaki/supervised-learning-R/blob/49a1736775369dd641223a31f2e1a28ad98eabec/regression_hands_on.R)


## Classification Analysis for Breast Cancer 
Given a dataset of 699 observations with 11 variables, of what appears to be imaging from breast tissue. We need to train a model to predict whether the observation corresponds to a **benignant** or **malignant** class.
 
[Dataset documentation](https://cran.r-project.org/web/packages/mlbench/mlbench.pdf)

**Answer**

I used Support Vector Machines models with different kernel functions.
For model evaluation purposes I added a cost matrix based on these assumptions

![Cost Matrix and assumptions](https://user-images.githubusercontent.com/11904085/123662884-27a6ab80-d836-11eb-9137-524c104f9b1d.png)

![Table of multiple kernel functions ](https://user-images.githubusercontent.com/11904085/123662961-3beaa880-d836-11eb-8e30-c3808f2b631e.png)

Conclusion: Use the model #7, as it represents the one with the lower prediction cost. Even though it has an accuracy of ~ 93% even though there are other models at higher accuracies ~ 95%


R script found here [svm_hands_on_breast_cancer.R](https://github.com/joseph-higaki/supervised-learning-R/blob/49a1736775369dd641223a31f2e1a28ad98eabec/svm_hands_on_breast_cancer.R)


## Classification Analysis for Iris Species
Dataset of 150 observations with 4 variables and a class. The purpose isto predict the classification of the Iris species: Setosa, Versicolor, Virginica.

[Dataset documentation](https://archive.ics.uci.edu/ml/datasets/Iris)

**Answer**
I also used Support Vector Machines models. When doing the variable analysis, by eyeballing the distribution of the species in variable pairs, it looks like Sepal Width and Sepal Length are good input variables.
From the different kernel functions tested, I went with the Polynomial Degree 3, Gamma 2.5.
Another interesting takeaway from this assignment was to use the plot feature to visualize observations vs prediction.

![SVM Classification plot](https://user-images.githubusercontent.com/11904085/123663127-63417580-d836-11eb-8474-a7673194b301.png)

R script found here [svm_hands_on_flowers.R](https://github.com/joseph-higaki/supervised-learning-R/blob/49a1736775369dd641223a31f2e1a28ad98eabec/svm_hands_on_flowers.R)


# Professor
* **[Marta Tolós](https://www.linkedin.com/in/martatolos/)**
 
**Professor Assistants**
* [Pere Miquel Brull Borràs](https://www.linkedin.com/in/pmbrull/)
* [Alberto Villa](https://www.linkedin.com/in/avillam/)

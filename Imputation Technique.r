####              Missing Values Imputation Techniques             #####

# 1. 'mice' Package

#Load the dataset
data <- iris
View(data)

#Summary of the dataset
summary(data)

#Installing missForest package

install.packages("missForest")
library(missForest)

#Generate 10% missing values at Random
missing <- prodNA(iris, noNA = 0.1)

#Check missing values introduced in the data
summary(missing)

# Removing categorical data
missing <- subset(missing, select = -c(Species))
summary(missing)

# install mice
install.packages("mice")
library(mice)

#Tabular Representation
md.pattern(missing)
summary(missing)

#No. of missing values
sum(is.na(missing))

install.packages("VIM")
library(VIM)
mice_plot <- aggr(missing, col=c('red','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(missing), cex.axis=.7,
                    gap=3, ylab=c("Missing values","Pattern"))
View(mice_plot)
summary(mice_plot)

#Imputing Data
imputed_Data <- mice(missing, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

#check imputed values
imputed_Data$imp$Sepal.Width

#get complete data (3rd out of 5)
completeData <- complete(imputed_Data,3)
View(completeData)

#build predictive model
fit <- with(data, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))

#combine results of all 5 models
combine <- c(fit)
summary(combine)

# 2. 'Hmisc' Package

#install package and load library
install.packages("Hmisc")
library(Hmisc)

#load data
data <- iris

#seed missing values (10%)
missing <- prodNA(data, noNA = 0.1)
summary(missing)

# impute with mean value
missing$imputed_age <- with(missing, impute(Sepal.Length, mean))

# impute with random value
missing$imputed_age2 <- with(missing, impute(Sepal.Length, 'random'))

#similarly you can use min, max, median to impute missing value
#using argImpute
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                             Species, data = missing, n.impute = 5)
# argImpute() automatically identifies the variable type and treats them accordingly.
impute_arg

#check imputed variable Sepal.Length
impute_arg$imputed$Sepal.Length

# 3. 'Amelia' Package

#install package and load library
install.packages("Amelia")
library(Amelia)

#load data
data("iris")

#seed 10% missing values
missing <- prodNA(data, noNA = 0.1)
summary(missing)

#specify columns and run amelia
fit <- amelia(missing, m=5, parallel = "multicore", noms = "Species")

#access imputed outputs
fit$imputations[[1]]
fit$imputations[[2]]
fit$imputations[[3]]

# To check a particular column in a data set, use the following commands
amelia_fit$imputations[[5]]$Petal.Length

#export the outputs to csv files
write.amelia(fit, file.stem = "imputed_data_set")

# 4. 'missForest' Package
install.packages("missForest")
library(missForest)

#load data
data("iris")

#seed 10% missing values
missing <- prodNA(data, noNA = 0.1)
summary(missing)

#impute missing values, using all parameters as default values
data.imp <- missForest(missing)

#check imputed values
data.imp$ximp

#check imputation error
data.imp$OOBerror

""" NRMSE        PFC 
0.13765654 0.03649635 

NRMSE is normalized mean squared error. It is used to represent error derived from imputing continuous values. PFC (proportion of falsely classified) is used to represent error derived from imputing categorical values.
comparing actual data accuracy """

data.err <- mixError(data.imp$ximp, missing, data)
data.err

""" NRMSE       PFC 
0.1510024 0.0000000  """


# 5. 'mi' Package

#install package and load library
install.packages("mi")
library(mi)

#load data
data("iris")

#seed missing values ( 10% )
missing<- prodNA(data, noNA = 0.1)
summary(missing)

#imputing missing value with mi
mi_data <- mi(missing, seed = 335)

""" I've used default values of parameters namely:
rand.imp.method as "bootstrap"
n.imp (number of multiple imputations) as 3
n.iter ( number of iterations) as 30 """"

summary(mi_data)

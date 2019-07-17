#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("e1071")

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(e1071)
#################################Loading Data##################

train_data<- read.delim("mnist_train.csv",sep = ",", stringsAsFactors = F,header = F)
test_data<- read.delim("mnist_test.csv",sep = ",", stringsAsFactors = F,header = F)

# View(train_data) # Data has no column names
# View(test_data) # Data has no column names

names(train_data)[1] <- "label"
names(test_data)[1] <- "label"

#Structure of the dataset

str(train_data)
str(test_data)

#printing first few rows

head(train_data)
head(test_data)

#Exploring the data

summary(train_data)
summary(test_data)


#checking missing value

sum(is.na(train_data))
sum(is.na(test_data))


# checking duplicate the data

sum(duplicated(train_data))
sum(duplicated(test_data))

#######################Data Understanding###############
str(train_data) # all dependant variables are integers, 60000 observations, 785 variables
str(test_data) # all dependant variables integers, 10000 observations, 785 variables

summary(train_data[ , 2:100]) 
summary(test_data[ , 2:100]) 
## Data needs to be scaled. Some of the columns have only zeros. Not removing those as these are pixel data 
#and can be used in future

###################Data Preparation###############

#Making our target class to factor

train_data$label<-factor(train_data$label)
summary(train_data$label)

test_data$label<-factor(test_data$label)
summary(test_data$label)

#Sampling of training data set as computational time is unacceptable for large amount of data.

dim(train_data)

set.seed(100)
train_sample.indices = sample(1:nrow(train_data), 0.1*nrow(train_data))
train_sample = train_data[train_sample.indices, ]

# Scaling data 

max(train_sample[ ,2:ncol(train_sample)]) # max pixel value is 255, using this to scale data
train_sample[ , 2:ncol(train_sample)] <- train_sample[ , 2:ncol(train_sample)]/255

test_data <- cbind(label = test_data[ ,1], test_data[ , 2:ncol(test_data)]/255)


#####################################Exploratory Data Analysis##################################


## Distribution of digits across all data sets (Full training dataset 
##, sampling training data set and test data set)

plot1 <- ggplot(train_data, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "Full Train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot2 <- ggplot(train_sample, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "train sample dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot3 <- ggplot(test_data, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "test dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

grid.arrange(plot1, plot2, plot3, nrow = 3)

# Similar frequency observed for all data sets

######################################### Model Building & Evaluation ######################################


#Using Linear Kernel Method using default parameters

model1_linear <- ksvm(label ~ ., data = train_sample, scaled = FALSE, kernel = "vanilladot")
print(model1_linear) 

eval1_linear <- predict(model1_linear, newdata = test_data, type = "response")
confusionMatrix(eval1_linear, test_data$label) 

# Accuracy of 92%
# High Sensitivities > 83%
# Very High Specificities > 98%

#---------------Using RBF method : Radial Kernel method-------------------#
#Using RBF Kernel
Model_RBF<- ksvm(label ~ ., data = train_sample, scaled = FALSE, kernel = "rbfdot")
print(Model_RBF)
Eval_RBF<- predict(Model_RBF, newdata = test_data, type = "response")

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test_data$label)

# Accuracy is highest at C = 1 and sigma = 0.01
# Accuracy: 95.2%
# High Sensitivities > 92%
# Very High Specificities > 99%

#------------Optimising C further using Cross validation with Radial Kernel method------------#


grid_rbf <- expand.grid(.sigma=c( 0.01 , 1.01 , 2.01 ), .C=c(1,2,3) )

fit.rbf <- train(label ~ ., data = train_sample, metric = "Accuracy", method = "svmRadial",tuneGrid = grid_rbf,
                  trControl = trainControl(method = "cv", number = 5), preProcess = NULL,scaled=FALSE)

# printing results of cross validation
print(fit.rbf) 
plot(fit.rbf)

eval_cv_rbf <- predict(fit.rbf, newdata = test_data)
confusionMatrix(eval_cv_rbf, test_data$label)

# Accuracy is highest at C = 3 and sigma = 0.01
# Accuracy: 95.73%
# High Sensitivities > 92%
# Very High Specificities > 99%

########################Final Model with Conclusion#################

################################################ Conclusion ################################################

# Final model
final_model = fit.rbf

## SVM using RBF kernel achieved highest accuracy for digit recognition where C = 3, sigma = 0.01

# Used training data set of 6000 instances (extracted using random sampling)
# distribution of the dependent variable (digtits) has been preserved while sampling
# Model performance on validation data set of 10000 instances
# Accuracy = 95.73%
# Sensitivites > 92%
# Specificities > 99%



## Set the working directory.

##install required packages. 
install.packages("splitstackshape")
library(splitstackshape)
install.packages("MASS")
library(MASS)
install.packages("car") 
library(car)
install.packages("dplyr")
library(dplyr)


# Load the data and View the data

GeelyData<- read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)

View(GeelyData)

#See the structure of the data

str(GeelyData)

##Data Preparation####

# Split the CarName column .
GeelyData<- cSplit(GeelyData, "CarName", " ")

#rename CarName_1 column to CarCompany

names(GeelyData)[26]<- "CarCompany"

# remove other CarName_2, CarName_3,CarName_4, CarName_5.
#As it is mentioned the data preparation section that "consider only company name as the 
#independent variable for the model building. " So removing other columns.

GeelyData<- GeelyData[,-c(27:30)]

#Rename some wrong value for column CarCompany

which(GeelyData$CarCompany=="Nissan")
GeelyData$CarCompany[90]<-"nissan"
which(GeelyData$CarCompany=="toyouta")
GeelyData$CarCompany[182]<- "toyota"
which(GeelyData$CarCompany=="vokswagen")
GeelyData$CarCompany[183]<- "volkswagen"
which(GeelyData$CarCompany=="vw")
GeelyData$CarCompany[183]<- "volkswagen"
which(GeelyData$CarCompany=="maxda")
GeelyData$CarCompany[51]<- "mazda"
which(GeelyData$CarCompany=="maxda")
GeelyData$CarCompany[52]<- "mazda"
which(GeelyData$CarCompany=="porcshce")
GeelyData$CarCompany[127]<- "porsche"

#Remove duplicate values 
unique(GeelyData)
#Identify missing values
sum(is.na(GeelyData))
# Find the outliner (based on 1% interval) and then cap all values.
quantile(GeelyData$wheelbase,seq(0,1,0.01))
GeelyData$wheelbase[which(GeelyData$wheelbase>115.544)]<- 115.544

quantile(GeelyData$carlength,seq(0,1,0.01))
GeelyData$carlength[which(GeelyData$carlength>202.480)]<- 202.480

quantile(GeelyData$enginesize,seq(0,1,0.01))
GeelyData$enginesize[which(GeelyData$enginesize>209.00)]<- 209.00

quantile(GeelyData$compressionratio,seq(0,1,0.01))
GeelyData$compressionratio[which(GeelyData$compressionratio>10.9400)]<- 10.9400

quantile(GeelyData$horsepower,seq(0,1,0.01))
GeelyData$horsepower[which(GeelyData$horsepower>184.00)]<- 184.00

#Dummy variable creation for Categorical variable having 2 levels
str(GeelyData$fueltype)
GeelyData$fueltype <- as.factor(GeelyData$fueltype)
levels(GeelyData$fueltype)<-c(1,0)
GeelyData$fueltype<- as.numeric(levels(GeelyData$fueltype))[GeelyData$fueltype]

GeelyData$aspiration <- as.factor(GeelyData$aspiration)
levels(GeelyData$aspiration)<-c(1,0)
GeelyData$aspiration<- as.numeric(levels(GeelyData$aspiration))[GeelyData$aspiration]

GeelyData$doornumber <- as.factor(GeelyData$doornumber)
levels(GeelyData$doornumber)<-c(1,0)
GeelyData$doornumber<- as.numeric(levels(GeelyData$doornumber))[GeelyData$doornumber]

GeelyData$enginelocation <- as.factor(GeelyData$enginelocation)
levels(GeelyData$enginelocation)<-c(1,0)
GeelyData$enginelocation<- as.numeric(levels(GeelyData$enginelocation))[GeelyData$enginelocation]


#Dummy variable creation for Categorical variable having more than 2 levels

GeelyData$carbody <- as.factor(GeelyData$carbody)
dummy_1<- data.frame(model.matrix(~carbody,data=GeelyData))
dummy_1<- dummy_1[,-1]


GeelyData$drivewheel <- as.factor(GeelyData$drivewheel)
dummy_2<- data.frame(model.matrix(~drivewheel,data=GeelyData))
dummy_2<- dummy_2[,-1]

GeelyData$enginetype <- as.factor(GeelyData$enginetype)
dummy_3<- data.frame(model.matrix(~enginetype,data=GeelyData))
dummy_3<- dummy_3[,-1]

GeelyData$cylindernumber <- as.factor(GeelyData$cylindernumber)
dummy_4<- data.frame(model.matrix(~cylindernumber,data=GeelyData))
dummy_4<- dummy_4[,-1]

GeelyData$fuelsystem <- as.factor(GeelyData$fuelsystem)
dummy_5<- data.frame(model.matrix(~fuelsystem,data=GeelyData))
dummy_5<- dummy_5[,-1]

GeelyData$CarCompany <- as.factor(GeelyData$CarCompany)
dummy_6<- data.frame(model.matrix(~CarCompany,data=GeelyData))
dummy_6<- dummy_6[,-1]
## Delete unnessary columns CarCompanymaxda ,CarCompanyNissan,CarCompanyporcshce, CarCompanyvokswagen,CarCompanyvw
dummy_6[c(9,14,17,23,24,27)]<- list(NULL)

GeelyData$symboling <- as.factor(GeelyData$symboling)
dummy_7<- data.frame(model.matrix(~symboling,data=GeelyData))
dummy_7<- dummy_7[,-1]

#Combine the dummy variables to the main data set, 
#after removing the original categorical column

GeelyData_1<- cbind(GeelyData[,-6],dummy_1)
GeelyData_1<- cbind(GeelyData_1[,-6],dummy_2)
GeelyData_1<- cbind(GeelyData_1[,-12],dummy_3)
GeelyData_1<- cbind(GeelyData_1[,-12],dummy_4)
GeelyData_1<- cbind(GeelyData_1[,-13],dummy_5)
GeelyData_1<- cbind(GeelyData_1[,-21],dummy_6)
GeelyData_1<- cbind(GeelyData_1[,-2],dummy_7)

View(GeelyData_1)

# EDA for GeelyData_1 after data preparation.
hist(GeelyData_1$boreratio, main = "Histogram of Boreratio of car", xlab = "Ratio")

boxplot(GeelyData_1$compressionratio,
        main = toupper("Boxplot of compression ratio of car"),
        ylab = "Ratio",
        col = "blue")

d <- density(GeelyData_1$horsepower)
plot(d, main = "Kernel density of horsepower")
polygon(d, col = "red", border = "blue")


# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(GeelyData_1), 0.7*nrow(GeelyData_1))
train = GeelyData_1[trainindices,]
test = GeelyData_1[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

#Building model with stepAIC function
step <- stepAIC(model_1, direction="both")

step

# Let's execute this model here, 
model_2 <- lm(formula = price ~ horsepower           +CarCompanyhonda      +carbodywagon         +symboling2           +CarCompanymercury    +carlength            +symboling1           +enginetypeohc        +curbweight           +enginesize           +aspiration           +CarCompanyrenault    +stroke               +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel          +drivewheelrwd        +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive   +enginetypeohcf       +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_2)

## Let's check for multicollinearity 
vif(model_2)

# we found "curbweight " and "enginesize" have high VIF but low P value (two star) , so not considering these. 
#Then found "horsepower" with high VIF as well as highest P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_3<- lm(formula = price ~ CarCompanyhonda      +carbodywagon         +symboling2           +CarCompanymercury    +carlength            +symboling1           +enginetypeohc        +curbweight           +enginesize           +aspiration           +CarCompanyrenault    +stroke               +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel          +drivewheelrwd        +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive   +enginetypeohcf       +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_3)
## Let's check for multicollinearity 
vif(model_3)

# we found "curbweight " , "enginesize" and cylindernumberfour have high VIF but low P value (two and three star) , so not considering these. 
#Then found "carlength" with high VIF and one star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_4<- lm(formula = price ~ CarCompanyhonda      +carbodywagon         +symboling2           +CarCompanymercury  +symboling1           +enginetypeohc        +curbweight           +enginesize           +aspiration           +CarCompanyrenault    +stroke               +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel          +drivewheelrwd        +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive   +enginetypeohcf       +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_4)
## Let's check for multicollinearity 
vif(model_4)

# we found curbweight , enginesize, carwidth and cylindernumberfour have high VIF but low P value (three star) , so not considering these. 
#Then found "enginetypeohc" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_5<- lm(formula = price ~ CarCompanyhonda      +carbodywagon         +symboling2           +CarCompanymercury  +symboling1  +curbweight           +enginesize           +aspiration           +CarCompanyrenault    +stroke               +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel          +drivewheelrwd        +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive   +enginetypeohcf       +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_5)
## Let's check for multicollinearity 
vif(model_5)

# Found "stroke" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_6<- lm(formula = price ~ CarCompanyhonda      +carbodywagon         +symboling2           +CarCompanymercury  +symboling1  +curbweight           +enginesize           +aspiration           +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel          +drivewheelrwd        +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive   +enginetypeohcf       +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_6)
## Let's check for multicollinearity 
vif(model_6)

# Found "enginesize" with high VIF and one star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_7<- lm(formula = price ~ CarCompanyhonda      +carbodywagon         +symboling2           +CarCompanymercury  +symboling1  +curbweight           +aspiration           +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel          +drivewheelrwd        +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive   +enginetypeohcf       +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_7)
## Let's check for multicollinearity 
vif(model_7)

# Found "drivewheelrwd" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_8<- lm(formula = price ~ CarCompanyhonda      +carbodywagon         +symboling2           +CarCompanymercury  +symboling1  +curbweight           +aspiration           +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive   +enginetypeohcf       +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_8)
## Let's check for multicollinearity 
vif(model_8)

# Found "CarCompanyhonda" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_9<- lm(formula = price ~ carbodywagon         +symboling2           +CarCompanymercury  +symboling1  +curbweight           +aspiration           +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive   +enginetypeohcf       +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_9)
## Let's check for multicollinearity 
vif(model_9)


#-------------------------------------------------
# Found "enginetypeohcf" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_10<- lm(formula = price ~ carbodywagon         +symboling2           +CarCompanymercury  +symboling1  +curbweight           +aspiration           +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_10)
## Let's check for multicollinearity 
vif(model_10)

# Found "symboling1" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_11<- lm(formula = price ~ carbodywagon         +symboling2           +CarCompanymercury  +curbweight           +aspiration           +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_11)
## Let's check for multicollinearity 
vif(model_11)

# Found "aspiration" with high VIF and no star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_12<- lm(formula = price ~ carbodywagon         +symboling2           +CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_12)
## Let's check for multicollinearity 
vif(model_12)

# Found "symboling2" with high VIF and no star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_13<- lm(formula = price ~ carbodywagon   +CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth   +CarCompanynissan     +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_13)
## Let's check for multicollinearity 
vif(model_13)

# Found "CarCompanynissan" with high VIF and no star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_14<- lm(formula = price ~ carbodywagon   +CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_14)
## Let's check for multicollinearity 
vif(model_14)

# Found "carbodywagon" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_15<- lm(formula = price ~ CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth             +CarCompanysaab       +CarCompanyplymouth +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_15)
## Let's check for multicollinearity 
vif(model_15)


# Found "CarCompanysaab" with high VIF and no star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_16<- lm(formula = price ~ CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth    +CarCompanyplymouth +CarCompanydodge      +CarCompanyvolkswagen +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_16)
## Let's check for multicollinearity 
vif(model_16)

# Found "CarCompanyvolkswagen" with high VIF and one star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_17<- lm(formula = price ~ CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth    +CarCompanyplymouth +CarCompanydodge      +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix    +CarCompanymitsubishi +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_17)
## Let's check for multicollinearity 
vif(model_17)

# Found "CarCompanymitsubishi" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_18<- lm(formula = price ~ CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth    +CarCompanyplymouth +CarCompanydodge      +enginetypel   +CarCompanymazda      +CarCompanytoyota     +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix  +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_18)
## Let's check for multicollinearity 
vif(model_18)


# Found "CarCompanytoyota" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_19<- lm(formula = price ~ CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth    +CarCompanyplymouth +CarCompanydodge      +enginetypel   +CarCompanymazda   +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix  +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_19)
## Let's check for multicollinearity 
vif(model_19)

# Found "CarCompanymazda" with high VIF and two star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_20<- lm(formula = price ~ CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth    +CarCompanyplymouth +CarCompanydodge      +enginetypel    +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix  +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_20)
## Let's check for multicollinearity 
vif(model_20)

# Found "CarCompanyplymouth" with high VIF and no star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_21<- lm(formula = price ~ CarCompanymercury  +curbweight   +CarCompanyrenault   +carwidth  +CarCompanydodge      +enginetypel    +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix  +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_21)
## Let's check for multicollinearity 
vif(model_21)

# Found "CarCompanymercury" with high VIF and no star P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_22<- lm(formula = price ~ curbweight   +CarCompanyrenault   +carwidth  +CarCompanydodge      +enginetypel    +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix  +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_22)
## Let's check for multicollinearity 
vif(model_22)

# Found "CarCompanyrenault" with high VIF and dot P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_23<- lm(formula = price ~ curbweight  +carwidth  +CarCompanydodge      +enginetypel    +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix  +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_23)
## Let's check for multicollinearity 
vif(model_23)

# Found "CarCompanydodge" with high VIF and dot P value , so it is insignificant. Removing from the model
# Let's execute this model here, 
model_24<- lm(formula = price ~ curbweight  +carwidth  +enginetypel    +cylindernumberfour   +CarCompanyjaguar     +cylindernumbersix  +CarCompanybuick      +cylindernumberfive +CarCompanybmw        +enginelocation , data = train)
# Let us look at the summary of the model
summary(model_24)
## Let's check for multicollinearity 
vif(model_24)


# Now model_24 is the final model. Becasue it has all variables with three star p value, 
#all VIF is less than 10. Finally R-squared and adjusted R-squared are almost same and near to 0.94


# Predict the prices in the testing dataset
Predict_1 <- predict(model_24,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared



#-----------------------------#
# Global Mart case Study     #
#-----------------------------#
#####################Business Understanding#####################

# A sales store wants to forecast the sales and demand of next 6 months for 2 of their most 
#profitable and consistent segments.
# The store caters to 7 different market segments and in 3 major categories.
# From these 21 segments we need to forecast the sales and demand of most profitable and consistent segment.

#####################Data Understanding#####################

#The data currently has the transaction level data, where each row represents a particular order made 
#on the online store. There are 24 attributes related to each such transaction. 
#The "Market" attribute has 7-factor levels representing the geographical market sector that the customer belongs to. 
#The "Segment" attribute tells which of the 3 segments that customer belongs to.

#####################Data Loading#########################

## Installing and Loading below packages

#install.packages('forecast')
#install.packages('raster')
#install.packages('tseries')
library(ggplot2)
library(forecast)
library(graphics)
library(lubridate)
library(raster)
library(tseries)

##Loading data to the environment 

GlobalMart<- read.csv("Global Superstore.csv")

# checking structure of data set
str(GlobalMart)

# view the data set 
View(GlobalMart)

#Exploring data set 
summary(GlobalMart)

# checking for missing values 
sum(is.na(GlobalMart))
# observed 41296 NA values 

# Finding columns which have NA values
names(which(sapply(GlobalMart, function(x) any(is.na(x))))) 
#Postal.Code column is having NA values corresponding to NON US PinCodes

# checking duplicate the data
sum(duplicated(GlobalMart)) #No Duplicate values present

str(GlobalMart) # 	51290 observation with  24 variables:
summary(GlobalMart)  

#Changing Order.Date and Ship.Date to Date type
GlobalMart$Order.Date <- as.Date(GlobalMart$Order.Date,"%d-%m-%Y")
GlobalMart$Ship.Date <- as.Date(GlobalMart$Ship.Date,"%d-%m-%Y")

#####################Data Preparation#####################

#Checking unique values of Segment and Market
unique(GlobalMart$Segment) #Consumer / Corporate / Home Office
unique(GlobalMart$Market) #US / APAC / EU / Africa / EMEA / LATAM / Canada

#We first need to segment the data into 21 subsets corresponding to each Segment and Market level
GlobalMart$Order_year = year(GlobalMart$Order.Date)
GlobalMart$Order_month = month(GlobalMart$Order.Date)

#To generate the an unique sequence corresponding to each order date and sort based on date
Month_year <- GlobalMart$Order_year*12 + GlobalMart$Order_month
Min_month_year <- min(Month_year)
Month_year_seq <- Month_year - Min_month_year +1
GlobalMart$Month_seq <- Month_year_seq
GlobalMart <- GlobalMart[order(GlobalMart$Month_seq),]

#Based on the requirement, we are summing up Sales, Quantity and Profit based on Market, Segment and Month_seq
GlobalMart_agg <- aggregate(GlobalMart[,c("Sales","Quantity","Profit")],
                            by=list(GlobalMart$Market,GlobalMart$Segment,
                                    GlobalMart$Month_seq),FUN = sum)

#Renaming the columns of the dataframe
colnames(GlobalMart_agg)[1]<-c("Market")
colnames(GlobalMart_agg)[2]<-c("Segment")
colnames(GlobalMart_agg)[3]<-c("Month_Seq")

View(GlobalMart_agg)

#From the different markets and segments, we need to identify 2 of the most profitable and consistently profitable segments
#To find them, we need to calculate sum of profit and coefficient of variance for each market segment.

#Finding Sum of profit for each market segment
GlobalMart_profit_agg <-aggregate(GlobalMart_agg[,c("Profit")],
                                    by=list(GlobalMart_agg$Market,GlobalMart_agg$Segment)
                                    ,FUN = sum) 

#Renaming the columns of the dataframe
colnames(GlobalMart_profit_agg)[1]<-c("Market")
colnames(GlobalMart_profit_agg)[2]<-c("Segment")
colnames(GlobalMart_profit_agg)[3]<-c("Profit_SUM")

View(GlobalMart_profit_agg)

#Finding co-variance of profit for each market segment
GlobalMart_cv_agg <-aggregate(GlobalMart_agg[,c("Profit")],
                                by=list(GlobalMart_agg$Market,GlobalMart_agg$Segment)
                                ,FUN = cv)

#Renaming the columns of the dataframe
colnames(GlobalMart_cv_agg)[1]<-c("Market")
colnames(GlobalMart_cv_agg)[2]<-c("Segment")
colnames(GlobalMart_cv_agg)[3]<-c("Profit_CV")

View(GlobalMart_cv_agg)

#Merging GlobalMart_profit_agg and GlobalMart_cv_agg
GlobalMart_Profit_CV_agg <- merge(GlobalMart_profit_agg,GlobalMart_cv_agg)
View(GlobalMart_Profit_CV_agg)

#Since we need to find 2 most profitable and consistent market and segment: ordering based on highest profit and least CV
head(GlobalMart_Profit_CV_agg[order(-GlobalMart_Profit_CV_agg$Profit_SUM),c(1:2)],2) #Ordering profit on descending order to get max profit at top
head(GlobalMart_Profit_CV_agg[order(GlobalMart_Profit_CV_agg$Profit_CV),c(1:2)],2) #Ordering CV on ascending order to get min CV at top

#Based on the above we are getting APAC Consumer and EU Consumer as most profitable and consistent

#####################EDA#####################

#Performing some EDA to visualize the data

#Plotting Sales data corresponding to different markets. Also checking for profit in the sales
plot1 <- ggplot(GlobalMart_agg,aes(x=GlobalMart_agg$Market,y=GlobalMart_agg$Sales, fill=Profit))
plot1+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Sales")+ggtitle("Region vs Sales Comparison")
#Clearly APAC Market corresponds to maximum sales

#Plotting the Market and Segmentwise Profit verses Coefficient of variation
plot2 <- ggplot(GlobalMart_Profit_CV_agg,aes(x = GlobalMart_Profit_CV_agg$Profit_SUM, y = GlobalMart_Profit_CV_agg$Profit_CV, color = Market, shape = Segment))
plot2+ geom_point(size = 5)+xlab("Profit")+ylab("Coefficient of Variation")+ggtitle("Market and Segmentwise Profit vs CV") 
#This plot shows that APAC and EU Consumer are the most profitable (since it has the highest profit)
#and consistently profitable (since it has the lowest cv) segments

#######################Model building and Model Evaluation###############################
    
# Since we derived above APAC Consumer and EU Consumer as most profitable and consistent, 
# Create subsets for APAC Consumer , EU Consumer.

APAC_Consumer_agg<-subset(GlobalMart_agg,GlobalMart_agg$Market=="APAC" & GlobalMart_agg$Segment=="Consumer")
EU_Consumer_agg<-subset(GlobalMart_agg,GlobalMart_agg$Market=="EU" & GlobalMart_agg$Segment=="Consumer")

View(APAC_Consumer_agg)
View(EU_Consumer_agg)

#Now we will build timeseries on these 2 dataframes

#Train and Test Data for APAC Consumer Sales
#Create subset of 1st 42 months data for training and rest 6 months data for testing purpose 
tseries_apac_cons_sales<-ts(APAC_Consumer_agg$Sales)
APAC_Consumer_agg_sales_train<-APAC_Consumer_agg[1:42,]
#Creating timeseries on the training dataset and plotting the same
tseries_apac_cons_sales_train<-ts(APAC_Consumer_agg_sales_train$Sales)
plot(tseries_apac_cons_sales_train)

#Train and Test Data for EU Consumer Sales
tseries_eu_cons_sales <- ts(EU_Consumer_agg$Sales)
eu_Consumer_agg_sales_train <- EU_Consumer_agg[1:42,]
#Creating timeseries on the training dataset and plotting the same
tseries_eu_cons_sales_train<-ts(eu_Consumer_agg_sales_train$Sales)
plot(tseries_eu_cons_sales_train)

#Train and Test Data for APAC Consumer Quantity
tseries_apac_cons_quantity<-ts(APAC_Consumer_agg$Quantity)
APAC_Consumer_agg_quantity_train<-APAC_Consumer_agg[1:42,]
#Creating timeseries on the training dataset and plotting the same
tseries_apac_cons_quantity_train<-ts(APAC_Consumer_agg_quantity_train$Quantity)
plot(tseries_apac_cons_quantity_train)

#Train and Test Data for EU Consumer Quantity
tseries_eu_cons_quantity <- ts(EU_Consumer_agg$Quantity)
eu_Consumer_agg_quantity_train <- EU_Consumer_agg[1:42,]
#Creating timeseries on the training dataset and plotting the same
tseries_eu_cons_quantity_train<-ts(eu_Consumer_agg_quantity_train$Quantity)
plot(tseries_eu_cons_quantity_train)

###########Model building on APAC Consumer Sales using Classical Decomposition#####################
###################################################################################################

plot(tseries_apac_cons_sales_train)
# After getting the above Trendy time seies , applying smoothing (Moving Average Smoothing)
# Checking for different width values to get the appropriate smoothened series

w<-2 #For w=2, the smoothened series is coming correctly compared to other values
smoothedseries_apac_cons <- filter(tseries_apac_cons_sales_train, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries_apac_cons[w+2] - smoothedseries_apac_cons[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_apac_cons[i] <- smoothedseries_apac_cons[i+1] - diff
}

#Smoothing right end of the time series
n <- length(tseries_apac_cons_sales_train)
diff <- smoothedseries_apac_cons[n-w] - smoothedseries_apac_cons[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_apac_cons[i] <- smoothedseries_apac_cons[i-1] + diff
}

#Plotting the smoothed time series
lines(smoothedseries_apac_cons, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#Changing the timeseries to a dataframe and adding month to it
timevals_in_apac_cons <- APAC_Consumer_agg_sales_train$Month
smootheddf_apac_cons <- as.data.frame(cbind(timevals_in_apac_cons, as.vector(smoothedseries_apac_cons)))
colnames(smootheddf_apac_cons) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf_apac_cons)
global_pred_apac_cons <- predict(lmfit, Month=timevals_in_apac_cons)
lines(timevals_in_apac_cons, global_pred_apac_cons, col='red', lwd=2)

#Now, after predicting the global part, let's look at the locally predictable series
#We will model it as an ARMA series

#Removing the global pred from the APAC Consumer Sales training data
local_pred_apac_cons <- tseries_apac_cons_sales_train-global_pred_apac_cons

#Plotting the local timeseries
plot(local_pred_apac_cons,col="yellow",lwd=2)

#Running ACF on the local timeseries to check whether it is a stationary time series or not
acf(local_pred_apac_cons)
acf(local_pred_apac_cons, type="partial")
#From ACF plot, it looks like a white noise, since only at lag 0, the boundary of confidence interval is crossed

#Running auto.arima again to check whether the series has any auto regressive properties in it
armafit_apac_cons <- auto.arima(local_pred_apac_cons)
par(mar = rep(2,4)) # using this to avoid error message "Error in plot.new() : figure margins too large"
tsdiag(armafit_apac_cons)
armafit_apac_cons #ARIMA(0,0,0) with zero mean 
#AIC=891.33   AICc=891.43   BIC=893.07
#auto.arima also suggests it is pure white noise

#Now We'll check if the residual series is white noise
#From the local series removing the auto.ariam fitted series

resi_apac_cons <- local_pred_apac_cons - fitted(armafit_apac_cons)

#Running Dicky-Fuller Test and KPSS test on residual time series to check whether it is pure white noise or not
adf.test(resi_apac_cons,alternative = "stationary")
#p-value = 0.01
#Null hypothesis of Dicky Fuller test is Time Series is not stationary
#Since p-value which we got is less than threshold, hence we can reject null hypothesis and can say series is stationary

kpss.test(resi_apac_cons)
#p-value = 0.1
#Null hypothesis of KPSS test is Time Series is stationary
#Since p-value is more than threshold, we fail to reject null hypothesis and can say series is stationary

####Model Evaluation####

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 8 months
outdata_apac_cons <- APAC_Consumer_agg[43:48,]

#Extracting the months from the test data
timevals_out_apac_cons <- outdata_apac_cons$Month_Seq

#Predicting the value corresponding to the months of the test data using the global prediction model built earlier
global_pred_out_apac_cons <- predict(lmfit,data.frame(Month =timevals_out_apac_cons))
fcast_apac_cons <- global_pred_out_apac_cons

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_apac_cons_sales<- accuracy(fcast_apac_cons,outdata_apac_cons[,4])[5]
MAPE_class_dec_apac_cons_sales #23.86525

#Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred_apac_cons <- c(ts(global_pred_apac_cons),ts(global_pred_out_apac_cons))
plot(tseries_apac_cons_sales, col = "black")
lines(class_dec_pred_apac_cons, col = "blue", lwd=2)

#So, that was classical decomposition, now let's do an ARIMA fit

####################Model building on APAC Consumer Sales using ARIMA##############################
###################################################################################################

autoarima_apac_cons <- auto.arima(tseries_apac_cons_sales_train)
autoarima_apac_cons
#ARIMA(0,1,1) is fitted on the data
#AIC=898.23   AICc=898.55   BIC=901.66 log likelihood=-447.11

par(mar = rep(2, 4)) # using this to avoid error message "Error in plot.new() : figure margins too large"

#Plotting the auto.arima timeseries with the fitted line
tsdiag(autoarima_apac_cons)
plot(autoarima_apac_cons$x, col="black")
lines(fitted(autoarima_apac_cons), col="red", lwd=2)

#Again, let's check if the residual series is white noise. 
#Removing fitted autoarima model from the training timeseries data

resi_auto_arima_apac_cons <- tseries_apac_cons_sales_train - fitted(autoarima_apac_cons)

adf.test(resi_auto_arima_apac_cons,alternative = "stationary")
#p-value = 0.01
#Null hypothesis of Dicky Fuller test is Time Series is not stationary
#Since p-value which we got is less than threshold, hence we can reject null hypothesis and can say series is stationary

kpss.test(resi_auto_arima_apac_cons)
#p-value = 0.1
#Null hypothesis of KPSS test is Time Series is stationary
#Since p-value is more than threshold, we fail to reject null hypothesis and can say series is stationary

####Model Evaluation####

#let's evaluate the model using MAPE
fcast_auto_arima_apac_cons <- predict(autoarima_apac_cons, n.ahead = 6)

MAPE_auto_arima_apac_cons_sales <- accuracy(fcast_auto_arima_apac_cons$pred,outdata_apac_cons[,4])[5]
MAPE_auto_arima_apac_cons_sales
#27.68952

#let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred_apac_cons <- c(fitted(autoarima_apac_cons),ts(fcast_auto_arima_apac_cons$pred))
plot(tseries_apac_cons_sales, col = "black")
lines(auto_arima_pred_apac_cons, col = "red", lwd=2)

##########################################################################################################

###########Model building on EU Consumer Sales using Classical Decomposition#####################
###################################################################################################

plot(tseries_eu_cons_sales_train) #plotting the timeseries for EU Consumer Sales Training data

# After getting the above Trendy time seies , applying smoothing (Moving Average Smoothing)
# Checking for different width values to get the appropriate smoothened series

w<-2 #For w=2, the smoothened series is coming correctly compared to other values
smoothedseries_eu_cons <- filter(tseries_eu_cons_sales_train, 
                                   filter=rep(1/(2*w+1),(2*w+1)), 
                                   method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries_eu_cons[w+2] - smoothedseries_eu_cons[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_eu_cons[i] <- smoothedseries_eu_cons[i+1] - diff
}

#Smoothing right end of the time series
n <- length(tseries_eu_cons_sales_train)
diff <- smoothedseries_eu_cons[n-w] - smoothedseries_eu_cons[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_eu_cons[i] <- smoothedseries_eu_cons[i-1] + diff
}

#Plotting the smoothed time series
lines(smoothedseries_eu_cons, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#Changing the timeseries to a dataframe and adding month to it
timevals_in_eu_cons <- eu_Consumer_agg_sales_train$Month
smootheddf_eu_cons <- as.data.frame(cbind(timevals_in_eu_cons, as.vector(smoothedseries_eu_cons)))
colnames(smootheddf_eu_cons) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_1 <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf_eu_cons)
global_pred_eu_cons <- predict(lmfit_1, Month=timevals_in_eu_cons)
lines(timevals_in_eu_cons, global_pred_eu_cons, col='red', lwd=2)

#Now, after predicting the global part, let's look at the locally predictable series
#We will model it as an ARMA series

#Removing the global pred from the EU Consumer Sales training data
local_pred_eu_cons <- tseries_eu_cons_sales_train-global_pred_eu_cons

#Plotting the local timeseries
plot(local_pred_eu_cons,col="yellow",lwd=2)

#Running ACF on the local timeseries to check whether it is a stationary time series or not
acf(local_pred_eu_cons)
acf(local_pred_eu_cons, type="partial")
#From ACF plot, it looks like a white noise, since only at lag 0, the boundary of confidence interval is crossed

#Running auto.arima again to check whether the series has any auto regressive properties in it
armafit_eu_cons <- auto.arima(local_pred_eu_cons)
par(mar = rep(2, 4)) # using this to avoid error message "Error in plot.new() : figure margins too large"
tsdiag(armafit_eu_cons)
armafit_eu_cons #ARIMA(0,0,0) with zero mean 
#AIC=893.96   AICc=894.06   BIC=895.7
#auto.arima also suggests it is pure white noise

#Now We'll check if the residual series is white noise
#From the local series removing the auto.ariam fitted series

resi_eu_cons <- local_pred_eu_cons - fitted(armafit_eu_cons)

#Running Dicky-Fuller Test and KPSS test on residual time series to check whether it is pure white noise or not
adf.test(resi_eu_cons,alternative = "stationary")
#p-value = 0.01
#Null hypothesis of Dicky Fuller test is Time Series is not stationary
#Since p-value which we got is less than threshold, hence we can reject null hypothesis and can say series is stationary

kpss.test(resi_eu_cons)
#p-value = 0.1
#Null hypothesis of KPSS test is Time Series is stationary
#Since p-value is more than threshold, we fail to reject null hypothesis and can say series is stationary

####Model Evaluation####

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 8 months
outdata_eu_cons <- EU_Consumer_agg[43:48,]

#Extracting the months from the test data
timevals_out_eu_cons <- outdata_eu_cons$Month_Seq

#Predicting the value corresponding to the months of the test data using the global prediction model built earlier
global_pred_out_eu_cons <- predict(lmfit_1,data.frame(Month =timevals_out_eu_cons))
fcast_eu_cons <- global_pred_out_eu_cons

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_eu_cons_sales<- accuracy(fcast_eu_cons,outdata_eu_cons[,4])[5]
MAPE_class_dec_eu_cons_sales #28.27462

#Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred_eu_cons <- c(ts(global_pred_eu_cons),ts(global_pred_out_eu_cons))
plot(tseries_eu_cons_sales, col = "black")
lines(class_dec_pred_eu_cons, col = "blue", lwd=2)

#So, that was classical decomposition, now let's do an ARIMA fit

####################Model building on EU Consumer Sales using ARIMA##############################
###################################################################################################

autoarima_eu_cons <- auto.arima(tseries_eu_cons_sales_train)
autoarima_eu_cons
#ARIMA(2,1,0)  is fitted on the data
#AIC=897.67   AICc=898.32   BIC=902.81 log likelihood=-445.84

par(mar = rep(2, 4)) # using this to avoid error message "Error in plot.new() : figure margins too large"

#Plotting the auto.arima timeseries with the fitted line
tsdiag(autoarima_eu_cons)
plot(autoarima_eu_cons$x, col="black")
lines(fitted(autoarima_eu_cons), col="red", lwd=2)

#Again, let's check if the residual series is white noise. 
#Removing fitted autoarima model from the training timeseries data

resi_auto_arima_eu_cons <- tseries_eu_cons_sales_train - fitted(autoarima_eu_cons)

adf.test(resi_auto_arima_eu_cons,alternative = "stationary")
#p-value = 0.01
#Null hypothesis of Dicky Fuller test is Time Series is not stationary
#Since p-value which we got is less than threshold, hence we can reject null hypothesis and can say series is stationary

kpss.test(resi_auto_arima_eu_cons)
#p-value = 0.1
#Null hypothesis of KPSS test is Time Series is stationary
#Since p-value is more than threshold, we fail to reject null hypothesis and can say series is stationary

####Model Evaluation####

#let's evaluate the model using MAPE
fcast_auto_arima_eu_cons <- predict(autoarima_eu_cons, n.ahead = 6)

MAPE_auto_arima_eu_cons_sales <- accuracy(fcast_auto_arima_eu_cons$pred,outdata_eu_cons[,4])[5]
MAPE_auto_arima_eu_cons_sales
#28.9226

#let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred_eu_cons <- c(fitted(autoarima_eu_cons),ts(fcast_auto_arima_eu_cons$pred))
plot(tseries_eu_cons_sales, col = "black")
lines(auto_arima_pred_eu_cons, col = "red", lwd=2)

##########################################################################################################

###########Model building on APAC Consumer Quantity using Classical Decomposition#####################
###################################################################################################

plot(tseries_apac_cons_quantity_train)
# After getting the above Trendy time seies , applying smoothing (Moving Average Smoothing)
# Checking for different width values to get the appropriate smoothened series

w<-2 #For w=2, the smoothened series is coming correctly compared to other values
smoothedseries_apac_cons_quan <- filter(tseries_apac_cons_quantity_train, 
                                   filter=rep(1/(2*w+1),(2*w+1)), 
                                   method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries_apac_cons_quan[w+2] - smoothedseries_apac_cons_quan[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_apac_cons_quan[i] <- smoothedseries_apac_cons_quan[i+1] - diff
}

#Smoothing right end of the time series
n <- length(tseries_apac_cons_quantity_train)
diff <- smoothedseries_apac_cons_quan[n-w] - smoothedseries_apac_cons_quan[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_apac_cons_quan[i] <- smoothedseries_apac_cons_quan[i-1] + diff
}

#Plotting the smoothed time series
lines(smoothedseries_apac_cons_quan, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#Changing the timeseries to a dataframe and adding month to it
timevals_in_apac_cons_quantity <- APAC_Consumer_agg_quantity_train$Month
smootheddf_apac_cons_quantity <- as.data.frame(cbind(timevals_in_apac_cons_quantity, as.vector(smoothedseries_apac_cons_quan)))
colnames(smootheddf_apac_cons_quantity ) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_3 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf_apac_cons_quantity)
global_pred_apac_cons_quantity <- predict(lmfit_3, Month=timevals_in_apac_cons_quantity)
lines(timevals_in_apac_cons_quantity, global_pred_apac_cons_quantity, col='red', lwd=2)

#Now, after predicting the global part, let's look at the locally predictable series
#We will model it as an ARMA series

#Removing the global pred from the APAC Consumer Sales training data
local_pred_apac_cons_quantity <- tseries_apac_cons_quantity_train - global_pred_apac_cons_quantity

#Plotting the local timeseries
plot(local_pred_apac_cons_quantity,col="yellow",lwd=2)

#Running ACF on the local timeseries to check whether it is a stationary time series or not
acf(local_pred_apac_cons_quantity)
acf(local_pred_apac_cons_quantity, type="partial")
#From ACF plot, it looks like a white noise, since only at lag 0, the boundary of confidence interval is crossed

#Running auto.arima again to check whether the series has any auto regressive properties in it
armafit_apac_cons_quantity <- auto.arima(local_pred_apac_cons_quantity)
par(mar = rep(2, 4)) # using this to avoid error message "Error in plot.new() : figure margins too large"
tsdiag(armafit_apac_cons_quantity)
armafit_apac_cons_quantity #ARIMA(0,0,0) with zero mean 
#AIC=512.44   AICc=512.54   BIC=514.18
#auto.arima also suggests it is pure white noise

#Now We'll check if the residual series is white noise
#From the local series removing the auto.ariam fitted series

resi_apac_cons_quantity <- local_pred_apac_cons_quantity - fitted(armafit_apac_cons_quantity)

#Running Dicky-Fuller Test and KPSS test on residual time series to check whether it is pure white noise or not
adf.test(resi_apac_cons_quantity ,alternative = "stationary")
#p-value = 0.01
#Null hypothesis of Dicky Fuller test is Time Series is not stationary
#Since p-value which we got is less than threshold, hence we can reject null hypothesis and can say series is stationary

kpss.test(resi_apac_cons_quantity)
#p-value = 0.1
#Null hypothesis of KPSS test is Time Series is stationary
#Since p-value is more than threshold, we fail to reject null hypothesis and can say series is stationary

####Model Evaluation####

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 8 months
outdata_apac_cons_quantity <- APAC_Consumer_agg[43:48,]

#Extracting the months from the test data
timevals_out_apac_cons_quantity <- outdata_apac_cons_quantity$Month_Seq

#Predicting the value corresponding to the months of the test data using the global prediction model built earlier
global_pred_out_apac_cons_quantity <- predict(lmfit_3,data.frame(Month =timevals_out_apac_cons_quantity))
fcast_apac_cons_quantity <- global_pred_out_apac_cons_quantity

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_apac_cons_quantity <- accuracy(fcast_apac_cons_quantity,outdata_apac_cons_quantity[,5])[5]
MAPE_class_dec_apac_cons_quantity #32.38628

#Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred_apac_cons_quantity <- c(ts(global_pred_apac_cons_quantity),ts(global_pred_out_apac_cons_quantity))
plot(tseries_apac_cons_quantity, col = "black")
lines(class_dec_pred_apac_cons_quantity, col = "blue", lwd=2)

#So, that was classical decomposition, now let's do an ARIMA fit

####################Model building on APAC Consumer Quantity using ARIMA###########################
###################################################################################################

autoarima_apac_cons_quantity <- auto.arima(tseries_apac_cons_quantity_train)
autoarima_apac_cons_quantity
#ARIMA(0,1,0) is fitted on the data
#AIC=534.14   AICc=534.24   BIC=535.85 log likelihood=-266.07

par(mar = rep(2, 4)) # using this to avoid error message "Error in plot.new() : figure margins too large"

#Plotting the auto.arima timeseries with the fitted line
tsdiag(autoarima_apac_cons_quantity)
plot(autoarima_apac_cons_quantity$x, col="black")
lines(fitted(autoarima_apac_cons_quantity), col="red", lwd=2)

#Again, let's check if the residual series is white noise. 
#Removing fitted autoarima model from the training timeseries data

resi_auto_arima_apac_cons_quantity <- tseries_apac_cons_quantity_train - fitted(autoarima_apac_cons_quantity)

adf.test(resi_auto_arima_apac_cons_quantity,alternative = "stationary")
#p-value = 0.01
#Null hypothesis of Dicky Fuller test is Time Series is not stationary
#Since p-value which we got is less than threshold, hence we can reject null hypothesis and can say series is stationary

kpss.test(resi_auto_arima_apac_cons_quantity)
#p-value = 0.1
#Null hypothesis of KPSS test is Time Series is stationary
#Since p-value is more than threshold, we fail to reject null hypothesis and can say series is stationary

####Model Evaluation####

#let's evaluate the model using MAPE
fcast_auto_arima_apac_cons_quantity <- predict(autoarima_apac_cons_quantity, n.ahead = 6)

MAPE_auto_arima_apac_cons_quantity <- accuracy(fcast_auto_arima_apac_cons_quantity$pred,outdata_apac_cons_quantity[,5])[5]
MAPE_auto_arima_apac_cons_quantity
#26.24458

#let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred_apac_cons_quantity <- c(fitted(autoarima_apac_cons_quantity),ts(fcast_auto_arima_apac_cons_quantity$pred))
plot(tseries_apac_cons_quantity, col = "black")
lines(auto_arima_pred_apac_cons_quantity, col = "red", lwd=2)

##########################################################################################################

###########Model building on EU Consumer Quantity using Classical Decomposition#####################
###################################################################################################

plot(tseries_eu_cons_quantity_train)
# After getting the above Trendy time seies , applying smoothing (Moving Average Smoothing)
# Checking for different width values to get the appropriate smoothened series

w<-2 #For w=2, the smoothened series is coming correctly compared to other values
smoothedseries_eu_cons_quan <- filter(tseries_eu_cons_quantity_train, 
                                        filter=rep(1/(2*w+1),(2*w+1)), 
                                        method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries_eu_cons_quan[w+2] - smoothedseries_eu_cons_quan[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_eu_cons_quan[i] <- smoothedseries_eu_cons_quan[i+1] - diff
}

#Smoothing right end of the time series
n <- length(tseries_eu_cons_quantity_train)
diff <- smoothedseries_eu_cons_quan[n-w] - smoothedseries_eu_cons_quan[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_eu_cons_quan[i] <- smoothedseries_eu_cons_quan[i-1] + diff
}

#Plotting the smoothed time series
lines(smoothedseries_eu_cons_quan, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#Changing the timeseries to a dataframe and adding month to it
timevals_in_eu_cons_quantity <- eu_Consumer_agg_quantity_train$Month
smootheddf_eu_cons_quantity <- as.data.frame(cbind(timevals_in_eu_cons_quantity, as.vector(smoothedseries_eu_cons_quan)))
colnames(smootheddf_eu_cons_quantity ) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_4 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
              + Month, data=smootheddf_eu_cons_quantity)
global_pred_eu_cons_quantity <- predict(lmfit_4, Month=timevals_in_eu_cons_quantity)
lines(timevals_in_eu_cons_quantity, global_pred_eu_cons_quantity, col='red', lwd=2)

#Now, after predicting the global part, let's look at the locally predictable series
#We will model it as an ARMA series

#Removing the global pred from the APAC Consumer Sales training data
local_pred_eu_cons_quantity <- tseries_eu_cons_quantity_train - global_pred_eu_cons_quantity

#Plotting the local timeseries
plot(local_pred_eu_cons_quantity,col="yellow",lwd=2)

#Running ACF on the local timeseries to check whether it is a stationary time series or not
acf(local_pred_eu_cons_quantity)
acf(local_pred_eu_cons_quantity, type="partial")
#From ACF plot, it looks there might be some autoregressive properties in it

#Running auto.arima again to check whether the series has any auto regressive properties in it
armafit_eu_cons_quantity <- auto.arima(local_pred_eu_cons_quantity)
par(mar = rep(2, 4)) # using this to avoid error message "Error in plot.new() : figure margins too large"
tsdiag(armafit_eu_cons_quantity)
armafit_eu_cons_quantity #ARIMA(2,0,0) with zero mean 
#AIC=504.12   AICc=504.75   BIC=509.33
#auto.arima also suggests it has auto regressive properties.

#Now We'll check if the residual series is white noise
#From the local series removing the auto.ariam fitted series

resi_eu_cons_quantity <- local_pred_eu_cons_quantity - fitted(armafit_eu_cons_quantity)

#Running Dicky-Fuller Test and KPSS test on residual time series to check whether it is pure white noise or not
adf.test(resi_eu_cons_quantity ,alternative = "stationary")
#p-value = 0.01
#Null hypothesis of Dicky Fuller test is Time Series is not stationary
#Since p-value which we got is less than threshold, hence we can reject null hypothesis and can say series is stationary

kpss.test(resi_eu_cons_quantity)
#p-value = 0.1
#Null hypothesis of KPSS test is Time Series is stationary
#Since p-value is more than threshold, we fail to reject null hypothesis and can say series is stationary

####Model Evaluation####

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 8 months
outdata_eu_cons_quantity <- EU_Consumer_agg[43:48,]

#Extracting the months from the test data
timevals_out_eu_cons_quantity <- outdata_eu_cons_quantity$Month_Seq

#Predicting the value corresponding to the months of the test data using the global prediction model built earlier
global_pred_out_eu_cons_quantity <- predict(lmfit_4,data.frame(Month =timevals_out_eu_cons_quantity))
fcast_eu_cons_quantity <- global_pred_out_eu_cons_quantity

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_eu_cons_quantity <- accuracy(fcast_eu_cons_quantity,outdata_eu_cons_quantity[,5])[5]
MAPE_class_dec_eu_cons_quantity #31.39889

#Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred_eu_cons_quantity <- c(ts(global_pred_eu_cons_quantity),ts(global_pred_out_eu_cons_quantity))
plot(tseries_eu_cons_quantity, col = "black")
lines(class_dec_pred_eu_cons_quantity, col = "blue", lwd=2)

####################Model building on EU Consumer Quantity using ARIMA###########################
###################################################################################################

autoarima_eu_cons_quantity <- auto.arima(tseries_eu_cons_quantity_train)
autoarima_eu_cons_quantity
#ARIMA(2,1,0)  is fitted on the data
#AIC=529.8   AICc=530.44   BIC=534.94 log likelihood=-261.9

par(mar = rep(2, 4)) # using this to avoid error message "Error in plot.new() : figure margins too large"

#Plotting the auto.arima timeseries with the fitted line
tsdiag(autoarima_eu_cons_quantity)
plot(autoarima_eu_cons_quantity$x, col="black")
lines(fitted(autoarima_eu_cons_quantity), col="red", lwd=2)

#Again, let's check if the residual series is white noise. 
#Removing fitted autoarima model from the training timeseries data

resi_auto_arima_eu_cons_quantity <- tseries_eu_cons_quantity_train - fitted(autoarima_eu_cons_quantity)

adf.test(resi_auto_arima_eu_cons_quantity,alternative = "stationary")
#p-value = 0.04521
#Null hypothesis of Dicky Fuller test is Time Series is not stationary
#Since p-value which we got is less than threshold, hence we can reject null hypothesis and can say series is stationary

kpss.test(resi_auto_arima_eu_cons_quantity)
#p-value = 0.1
#Null hypothesis of KPSS test is Time Series is stationary
#Since p-value is more than threshold, we fail to reject null hypothesis and can say series is stationary

####Model Evaluation####

#let's evaluate the model using MAPE
fcast_auto_arima_eu_cons_quantity <- predict(autoarima_eu_cons_quantity, n.ahead = 6)

MAPE_auto_arima_eu_cons_quantity <- accuracy(fcast_auto_arima_eu_cons_quantity$pred,outdata_eu_cons_quantity[,5])[5]
MAPE_auto_arima_eu_cons_quantity
#30.13319

#let's plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred_eu_cons_quantity <- c(fitted(autoarima_eu_cons_quantity),ts(fcast_auto_arima_eu_cons_quantity$pred))
plot(tseries_eu_cons_quantity, col = "black")
lines(auto_arima_pred_eu_cons_quantity, col = "red", lwd=2)

#################################################################################################################
## Plotting the MAPE values to decide which model to use for all the four Market/Segment combination predictions
#################################################################################################################

Market_Segment <- c("APAC Sales", "APAC Quantity","EU Sales", "EU Quantity")
MAPE <- c(MAPE_auto_arima_apac_cons_sales, MAPE_auto_arima_apac_cons_quantity, MAPE_auto_arima_eu_cons_sales, MAPE_auto_arima_eu_cons_quantity,
          MAPE_class_dec_apac_cons_sales, MAPE_class_dec_apac_cons_quantity, MAPE_class_dec_eu_cons_sales, MAPE_class_dec_eu_cons_quantity)
Mape_type <- c( rep("AA: ",4), rep("CD: ", 4))
MAPE_DF <- data.frame(Market_Segment, MAPE, Mape_type) 
MAPE_DF$Mape_type <- as.factor(MAPE_DF$Mape_type)

MAPE_Plot <- ggplot(MAPE_DF, aes(x = Market_Segment, y = MAPE, group = Mape_type)) + geom_col(position = "dodge", fill = "darkgrey", colour = "black") +
  geom_text(aes(label=paste(Mape_type, round(MAPE,2))),position=position_dodge(width=0.91), vjust=-0.25)+
  xlab("Market Segment")+ylab("MAPE")+ggtitle("Comparision of MAPE values for each Market Segment")

#To view the MAPE plot
MAPE_Plot

#From the above MAPE_Plot we can say that we have to use Classical Decomposition model for APAC-Sales
#and EU-Sales prediction, whereas Auto ARIMA model for APAC-Quantity and EU-Quantity prediction

###################################################################################
#Forecasting the next 6 months Sales and Quantity for APAC and EU consumer segments
###################################################################################

#Forecasting the APAC-Sales for the next 6 months.
forecast_apac_sales_Next_six_months <- forecast(class_dec_pred_apac_cons,h=6)
#Plotting the forecasted APAC-Sales
plot(forecast_apac_sales_Next_six_months, main = "Forecasted APAC-Sales")

#Forecasting the EU-Sales for the next 6 months.
forecast_eu_sales_Next_six_months <- forecast(class_dec_pred_eu_cons,h=6)
#Plotting the forecasted EU-Sales
plot(forecast_eu_sales_Next_six_months, main = "Forecasted EU-Sales")

#Forecasting the APAC-Quantity for the next 6 months.
forecast_apac_quantity_Next_six_months <- forecast(auto_arima_pred_apac_cons_quantity,h=6)
#Plotting the forecasted APAC-Quantity
plot(forecast_apac_quantity_Next_six_months, main = "Forecasted APAC-Quantity")

#Forecasting the EU-Quantity for the next 6 months.
forecast_eu_quantity_Next_six_months <- forecast(auto_arima_pred_eu_cons_quantity,h=6)
#Plotting the forecasted EU-Quantity
plot(forecast_eu_quantity_Next_six_months, main = "Forecasted EU-Quantity")

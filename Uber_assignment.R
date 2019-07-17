# Set the working directory 

#install ggplot2 package using below commends
 #install.packages("ggplot2")
#library(ggplot2)

#Load the csv file 
uber_data<- read.csv("Uber Request Data.csv")
# View the data
uber_data

# convert "Request timestamp" and "Drop timestamp" to date  and time field

uber_data$Request.timestamp<- as.POSIXlt(uber_data$Request.timestamp, 
                                         format = "%d-%m-%Y %H:%M")

uber_data$Drop.timestamp<- as.POSIXlt(uber_data$Drop.timestamp, 
                                              format = "%d-%m-%Y %H:%M")

uber_data$RequestDate <- as.Date(uber_data$Request.timestamp)

uber_data$RequestTime <- format(uber_data$Request.timestamp,"%H:%M:%S")

uber_data$ReqMin<- format(uber_data$Request.timestamp, "%M")
uber_data$ReqHour<- format(uber_data$Request.timestamp, "%H")

uber_data$DropDate <- as.Date(uber_data$Drop.timestamp)

uber_data$DropTime <- format(uber_data$Drop.timestamp,"%H:%M:%S")
uber_data$DropMin<- format(uber_data$Drop.timestamp, "%M")
uber_data$DropHour<- format(uber_data$Drop.timestamp, "%H")

# 1. Visually identify the most pressing problems for Uber. 
# Create Time Slot based on Request time .

uber_data$RequestTimeSlot<-ifelse(uber_data$RequestTime >= "05:00:00" & uber_data$RequestTime< "08:00:00",'01 Early Morning',
                                  ifelse(uber_data$RequestTime>="08:00:00"& uber_data$RequestTime< "11:00:00",'02 Morning',
                                         ifelse(uber_data$RequestTime>= "11:00:00" & uber_data$RequestTime<"12:00:00",'03 Late Morning',
                                                ifelse(uber_data$RequestTime>= "12:00:00" & uber_data$RequestTime<"15:00:00",'04 Early Afternoon',
                                                       ifelse(uber_data$RequestTime>= "15:00:00" & uber_data$RequestTime<"16:00:00",'05 Afternoon',
                                                              ifelse(uber_data$RequestTime>= "16:00:00" & uber_data$RequestTime<"17:00:00",'06 Late Afternoon',
                                                                     ifelse(uber_data$RequestTime>= "17:00:00" & uber_data$RequestTime<"19:00:00",'07 Early Evening',
                                                                            ifelse(uber_data$RequestTime>= "19:00:00" & uber_data$RequestTime<"20:00:00",'08 Evening',
                                                                                   ifelse(uber_data$RequestTime>= "20:00:00" & uber_data$RequestTime<"21:00:00",'09 Late Evening',
                                                                                          ifelse(uber_data$RequestTime>= "21:00:00" & uber_data$RequestTime<"24:00:00",'10 Night',
                                                                                                 ifelse(uber_data$RequestTime>= "00:00:00" & uber_data$RequestTime<"05:00:00",'10 Night','11 None'
                                                                                                 )))))))))))
# Create aggregated table based on RequestTimeSlot , Pickup.point, Status and Reqquest.id.

uber_data$count<- ifelse(uber_data$Request.id==uber_data$Request.id,1,0) # count column will help to create aggregate table

uber_aggdata<- aggregate(uber_data$count,list(RequestTimeSlot=uber_data$RequestTimeSlot,Pickup.point=uber_data$Pickup.point,Status=uber_data$Status),sum)
# Created above aggregated table where Dimension columns are RequestTimeSlot , Pickup.point and Status. Measure is "x".
# Plot graph based on Pickup.point , status (for "Cancelled", "No Cars Available") , RequestTimeslot and Requestcount
ggplot(data = uber_aggdata[uber_aggdata$Status != "Trip Completed",],aes(x=RequestTimeSlot,y=x,fill=Pickup.point, label = x))+geom_bar(stat="identity",position="stack", colour="black") +geom_text(size = 3, position = position_stack(vjust = 0.5))+labs(y = "RequestCount", x = "RequestTimeSlot",fill="Pickup.point") +ggtitle("Request Count Analysis")
ggplot(data = uber_aggdata[uber_aggdata$Status != "Trip Completed",],aes(x=RequestTimeSlot,y=x,fill=Status, label = x))+geom_bar(stat="identity",position="stack", colour="black") +geom_text(size = 3, position = position_stack(vjust = 0.5))+labs(y = "RequestCount", x = "RequestTimeSlot",fill="Status") +ggtitle("Request Count Analysis")

# Plotting only for Airport pickup point
ggplot(data = uber_aggdata[uber_aggdata$Pickup.point == "Airport",],aes(x=RequestTimeSlot,y=x,fill=Status, label = x))+geom_bar(stat="identity",position="stack", colour="black") +geom_text(size = 3, position = position_stack(vjust = 0.5))+labs(y = "RequestCount", x = "RequestTimeSlot",fill="Status") +ggtitle("Request Count Analysis for Airport pickup point")

# Plotting only for City pickup point
ggplot(data = uber_aggdata[uber_aggdata$Pickup.point == "City",],aes(x=RequestTimeSlot,y=x,fill=Status, label = x))+geom_bar(stat="identity",position="stack", colour="black") +geom_text(size = 3, position = position_stack(vjust = 0.5))+labs(y = "RequestCount", x = "RequestTimeSlot",fill="Status") +ggtitle("Request Count Analysis for City pickup point")

# Create percentage value for Request Count 

uber_aggdata$percent<- round((prop.table(uber_aggdata$x)*100),digits = 2)

# Plotting Request Percentage for Airport Pickup point and all 3 status
ggplot(data = uber_aggdata[uber_aggdata$Pickup.point == "Airport",],aes(x=RequestTimeSlot,y=percent,fill=Status, label = percent))+geom_bar(stat="identity",position="stack") +geom_text(size = 3, position = position_stack(vjust = 0.5))+labs(y = "Percentage", x = "RequestTimeSlot",fill="Status") +ggtitle("Request Percentage by Status for Airport Pickup point")

# Plotting Request Percentage for City Pickup point and all 3 status
ggplot(data = uber_aggdata[uber_aggdata$Pickup.point == "City",],aes(x=RequestTimeSlot,y=percent,fill=Status, label = percent))+geom_bar(stat="identity",position="stack") +geom_text(size = 3, position = position_stack(vjust = 0.5))+labs(y = "Percentage", x = "RequestTimeSlot",fill="Status") +ggtitle("Request Percentage by Status for City Pickup point")

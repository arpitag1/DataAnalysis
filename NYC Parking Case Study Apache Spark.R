#** All the analysis steps mentioned below should be done for 3 different years. Each metric you derive should be compared across the 3 years.
#A> Uploading data into S3

# Create a file in unix box and paste the contents of the cookies from the cookies.txt extension and save it
# sudo vi cookies.txt

###################First File###################
# Using the cookies.txt file run the below commands to import the kaggle file to unix box
# sudo wget -x --load-cookies cookies.txt "https://www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2015.csv/2"

# Rename the file name from 2 to proper name
# sudo mv www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2015.csv/2 www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2015.csv/Parking_Violations_Issued_2015.csv

# Command to copy the data from unix box to S3 bucket in AWS
# aws s3 cp www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2015.csv/Parking_Violations_Issued_2015.csv s3://data-science-101/Data/Group_Assignment/

#################Second File##################
# Using the cookies.txt file run the below commands to import the kaggle file to unix box
# sudo wget -x --load-cookies cookies.txt "https://www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2016.csv/2"

# Rename the file name from 2 to proper name
# sudo mv www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2016.csv/2 www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2016.csv/Parking_Violations_Issued_2016.csv

# Command to copy the data from unix box to S3 bucket in AWS
# aws s3 cp www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2016.csv/Parking_Violations_Issued_2016.csv s3://data-science-101/Data/Group_Assignment/

###################Third File################
# Using the cookies.txt file run the below commands to import the kaggle file to unix box
# sudo wget -x --load-cookies cookies.txt "https://www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2017.csv/2"

# Rename the file name from 2 to proper name
# sudo mv www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2017.csv/2 www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2017.csv/Parking_Violations_Issued_2017.csv

# Command to copy the data from unix box to S3 bucket in AWS
# aws s3 cp www.kaggle.com/new-york-city/nyc-parking-tickets/downloads/Parking_Violations_Issued_-_Fiscal_Year_2017.csv/Parking_Violations_Issued_2017.csv s3://data-science-101/Data/Group_Assignment/

####################################################################

#File names are:  Parking_Violations_Issued_2015.csv
#				 Parking_Violations_Issued_2016.csv
#				 Parking_Violations_Issued_2017.csv

#B> Questions to be answered in the analysis

#Examine the data.

library(cowplot)
#load dplyr
library(dplyr)
library(ggplot2)
# load SparkR
library(SparkR)

# initialise the spark session
sparkR.session(master='local')

# 2. Create a Spark DataFrame and examine structure
# reading CSV file from S3 bucket
parking_violations_2015 <- SparkR::read.df("s3://data-science-101/Data/Group_Assignment/Parking_Violations_Issued_2015.csv", header=T, "CSV")
parking_violations_2016 <- SparkR::read.df("s3://data-science-101/Data/Group_Assignment/Parking_Violations_Issued_2016.csv", header=T, "CSV")
parking_violations_2017 <- SparkR::read.df("s3://data-science-101/Data/Group_Assignment/Parking_Violations_Issued_2017.csv", header=T, "CSV")


# examine the size
nrow(parking_violations_2015) #11809233
ncol(parking_violations_2015) #51

nrow(parking_violations_2016) #10626899
ncol(parking_violations_2016) #51

nrow(parking_violations_2017) #10803028
ncol(parking_violations_2017) #43

# look at the first few rows
head(parking_violations_2015)
head(parking_violations_2016)
head(parking_violations_2017)

##################################################################################
# Checking for any duplicate rows and removing it based on Summons Number, as this is unique for all the files/ dataframe
#...........................................................................................................#

nrow(SparkR::distinct(SparkR::select(parking_violations_2015
                                     ,parking_violations_2015$`Summons Number`))) #10951256

# In the file of 2015 there are many dupicates which are removed in this section.
parking_violations_2015 <- dropDuplicates(parking_violations_2015, "Summons Number")


nrow(SparkR::distinct(SparkR::select(parking_violations_2016
                                     ,parking_violations_2016$`Summons Number`))) #10626899

#No duplicates found for parking_violations_2016 file


nrow(SparkR::distinct(SparkR::select(parking_violations_2017
                                     ,parking_violations_2017$`Summons Number`))) #10803028

#No duplicates found for parking_violations_2017 file

#Creating views to run any sql on top of them for analysis -

createOrReplaceTempView(parking_violations_2015, "parking_violations_2015_v")

createOrReplaceTempView(parking_violations_2016, "parking_violations_2016_v")

createOrReplaceTempView(parking_violations_2017, "parking_violations_2017_v")


#1. Find total number of tickets for each year.
# Ans: Number of tickets issued in the year 2015 => 10951256
#                tickets issued in the year 2016 => 10626899
#                tickets issued in the year 2017 => 10803028


#2. Find out how many unique states the cars which got parking tickets came from.

# Ans: 
nrow(SparkR::distinct(SparkR::select(parking_violations_2015
                                     ,parking_violations_2015$'Registration State'))) #69

nrow(SparkR::distinct(SparkR::select(parking_violations_2016
                                     ,parking_violations_2016$'Registration State')))  #68

nrow(SparkR::distinct(SparkR::select(parking_violations_2017
                                     ,parking_violations_2017$'Registration State')))  #67

# consolidating and finding the unique states for all the three years

nrow(SparkR::distinct(rbind(SparkR::select(parking_violations_2015
                                           ,parking_violations_2015$'Registration State')
                            , SparkR::select(parking_violations_2016
                                             ,parking_violations_2016$'Registration State')
                            , SparkR::select(parking_violations_2017
                                             ,parking_violations_2017$'Registration State')
)

)
)

# 69

#3. Some parking tickets don't have addresses on them, which is cause for concern. Find out how many such tickets there are.

# Ans: 

# Calculating the number of parking tickets which do not have Address for parking_violations_2015
nrow(SparkR::filter(parking_violations_2015, isNull(parking_violations_2015$'House Number') 
                    | isNull(parking_violations_2015$'Street Name')))  #1807864

# Calculating the number of parking tickets which do not have Address for parking_violations_2016
nrow(SparkR::filter(parking_violations_2016, isNull(parking_violations_2016$'House Number') 
                    | isNull(parking_violations_2016$'Street Name')))  #2035232

# Calculating the number of parking tickets which do not have Address for parking_violations_2017
nrow(SparkR::filter(parking_violations_2017, isNull(parking_violations_2017$'House Number') 
                    | isNull(parking_violations_2017$'Street Name')))  #2289944



##################################################################################
#Aggregation tasks

##################################################################################

#1. How often does each violation code occur? (frequency of violation codes - find the top 5)

freq_of_violation_2015 <- SparkR::summarize(SparkR::groupBy(parking_violations_2015, parking_violations_2015$'Violation Code'),
                                            count = SparkR::count(parking_violations_2015$'Summons Number'))



df_freq_of_violation_2015 <- head(SparkR::arrange(freq_of_violation_2015, SparkR::desc(freq_of_violation_2015$count)),5)

#creating a barplot to visulize the data -
plot_freq_of_violation_2015 <- ggplot(df_freq_of_violation_2015)

barplot_freq_of_violation_2015 <- plot_freq_of_violation_2015 + geom_bar(aes(x = factor(df_freq_of_violation_2015$"Violation Code")
                                                                             , y= df_freq_of_violation_2015$count
                                                                             ,fill=factor(df_freq_of_violation_2015$"Violation Code"))
                                                                         ,stat = "identity" ) 

final_barplot_freq_of_violation_2015 <- barplot_freq_of_violation_2015 +  ggtitle("Plot of freq of violation 2015") + xlab('Violation Code') + ylab('Count') +  scale_fill_discrete(name="Violation Code")

# Top 5 violation code - for year 2015 

#    Violation Code   count
#1             21 1501614
#2             38 1324586
#3             14  924627
#4             36  761571
#5             37  746278

freq_of_violation_2016 <- SparkR::summarize(SparkR::groupBy(parking_violations_2016, parking_violations_2016$'Violation Code'),
                                            count = SparkR::count(parking_violations_2016$'Summons Number'))

df_freq_of_violation_2016 <- head(SparkR::arrange(freq_of_violation_2016, SparkR::desc(freq_of_violation_2016$count)),5)

#creating a barplot to visulize the data -

plot_freq_of_violation_2016 <- ggplot(df_freq_of_violation_2016)

barplot_freq_of_violation_2016 <- plot_freq_of_violation_2016 + geom_bar(aes(x = factor(df_freq_of_violation_2016$"Violation Code")
                                                                             , y= df_freq_of_violation_2016$count
                                                                             ,fill=factor(df_freq_of_violation_2016$"Violation Code"))
                                                                         ,stat = "identity" ) 

final_barplot_freq_of_violation_2016 <- barplot_freq_of_violation_2016 +  ggtitle("Plot of freq of violation 2016") + xlab('Violation Code') + ylab('Count') +  scale_fill_discrete(name="Violation Code")

# Top 5 violation code - for year 2016 
#      Violation Code   count
#1             21 1531587
#2             36 1253512
#3             38 1143696
#4             14  875614
#5             37  686610

freq_of_violation_2017 <- SparkR::summarize(SparkR::groupBy(parking_violations_2017, parking_violations_2017$'Violation Code'),
                                            count = SparkR::count(parking_violations_2017$'Summons Number'))

df_freq_of_violation_2017 <- head(SparkR::arrange(freq_of_violation_2017, SparkR::desc(freq_of_violation_2017$count)),5)

#creating a barplot to visulize the data -

plot_freq_of_violation_2017 <- ggplot(df_freq_of_violation_2017)

barplot_freq_of_violation_2017 <- plot_freq_of_violation_2017 + geom_bar(aes(x = factor(df_freq_of_violation_2017$"Violation Code")
                                                                             , y= df_freq_of_violation_2017$count
                                                                             ,fill=factor(df_freq_of_violation_2017$"Violation Code"))
                                                                         ,stat = "identity" ) 

final_barplot_freq_of_violation_2017 <- barplot_freq_of_violation_2017 +  ggtitle("Plot of freq of violation 2017") + xlab('Violation Code') + ylab('Count') +  scale_fill_discrete(name="Violation Code")

# Top 5 violation code - for year 2017 

#Violation Code   count
#1             21 1528588
#2             36 1400614
#3             38 1062304
#4             14  893498
#5             20  618593

# Comparing all based on year 2015,2016 and 2017

SparkR::merge( 
  SparkR::merge(df_freq_of_violation_2015, df_freq_of_violation_2016,
                by="Violation Code")
  ,df_freq_of_violation_2017
  ,by="Violation Code", all=T)

#Violation Code count.2015 count.2016   count.2017
#1             14  924627  875614  893498
#2             20      NA      NA  618593
#3             21 1501614 1531587 1528588
#4             36  761571 1253512 1400614
#5             37  746278  686610      NA
#6             38 1324586 1143696 1062304

### Creating a grid plot for all three years .....

plot_grid(final_barplot_freq_of_violation_2015
          ,final_barplot_freq_of_violation_2016
          ,final_barplot_freq_of_violation_2017, nrow=3)


#2. How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)

######Analysis for year 2015#######
freq_of_body_type_2015 <- SparkR::summarize(SparkR::groupBy(parking_violations_2015
                                                            , parking_violations_2015$`Vehicle Body Type`),
                                            count = SparkR::count(parking_violations_2015$'Summons Number'))


#head(parking_violations_2015,2)
df_freq_of_body_type_2015 <- head(SparkR::arrange(freq_of_body_type_2015, SparkR::desc(freq_of_body_type_2015$count)),5)

View(df_freq_of_body_type_2015)
#Vehicle Body Type   count
#1              SUBN 3451963
#2              4DSD 3102510
#3               VAN 1605228
#4              DELV  840441
#5               SDN  453992

freq_of_vehicle_make_2015 <- SparkR::summarize(SparkR::groupBy(parking_violations_2015
                                                               , parking_violations_2015$`Vehicle Make`),
                                               count = SparkR::count(parking_violations_2015$'Summons Number'))


df_freq_of_vehicle_make_2015 <- head(SparkR::arrange(freq_of_vehicle_make_2015, SparkR::desc(freq_of_vehicle_make_2015$count)),5)

View(df_freq_of_vehicle_make_2015)
#Vehicle Make   count
#1         FORD 1417303
#2        TOYOT 1123523
#3        HONDA 1018049
#4        NISSA  837569
#5        CHEVR  836389

######Analysis for year 2016#######
freq_of_body_type_2016 <- SparkR::summarize(SparkR::groupBy(parking_violations_2016
                                                            , parking_violations_2016$`Vehicle Body Type`),
                                            count = SparkR::count(parking_violations_2016$'Summons Number'))


df_freq_of_body_type_2016 <- head(SparkR::arrange(freq_of_body_type_2016, SparkR::desc(freq_of_body_type_2016$count)),5)

View(df_freq_of_body_type_2016)
#Vehicle Body Type   count
#1              SUBN 3466037
#2              4DSD 2992107
#3               VAN 1518303
#4              DELV  755282
#5               SDN  424043


freq_of_vehicle_make_2016 <- SparkR::summarize(SparkR::groupBy(parking_violations_2016
                                                               , parking_violations_2016$`Vehicle Make`),
                                               count = SparkR::count(parking_violations_2016$'Summons Number'))


df_freq_of_vehicle_make_2016 <- head(SparkR::arrange(freq_of_vehicle_make_2016, SparkR::desc(freq_of_vehicle_make_2016$count)),5)

View(df_freq_of_vehicle_make_2016)
#Vehicle Make   count
#1         FORD 1324774
#2        TOYOT 1154790
#3        HONDA 1014074
#4        NISSA  834833
#5        CHEVR  759663

######Analysis for year 2017#######
freq_of_body_type_2017 <- SparkR::summarize(SparkR::groupBy(parking_violations_2017
                                                            , parking_violations_2017$`Vehicle Body Type`),
                                            count = SparkR::count(parking_violations_2017$'Summons Number'))


df_freq_of_body_type_2017  <- head(SparkR::arrange(freq_of_body_type_2017, SparkR::desc(freq_of_body_type_2017$count)),5)

View(df_freq_of_body_type_2017)
#Vehicle Body Type   count
#1              SUBN 3719802
#2              4DSD 3082020
#3               VAN 1411970
#4              DELV  687330
#5               SDN  438191


freq_of_vehicle_make_2017 <- SparkR::summarize(SparkR::groupBy(parking_violations_2017
                                                               , parking_violations_2017$`Vehicle Make`),
                                               count = SparkR::count(parking_violations_2017$'Summons Number'))


df_freq_of_vehicle_make_2017 <- head(SparkR::arrange(freq_of_vehicle_make_2017, SparkR::desc(freq_of_vehicle_make_2017$count)),5)

View(df_freq_of_vehicle_make_2017)
#Vehicle Make   count
#1         FORD 1280958
#2        TOYOT 1211451
#3        HONDA 1079238
#4        NISSA  918590
#5        CHEVR  714655

# Comparing frequency of body type accross year 2015,2016 and 2017

SparkR::merge( 
  SparkR::merge(df_freq_of_body_type_2015, df_freq_of_body_type_2016,
                by="Vehicle Body Type")
  ,df_freq_of_body_type_2017
  ,by="Vehicle Body Type", all=T)

#Vehicle Body Type count.2015 count.2016   count.2017
#1              4DSD 3102510 2992107 3082020
#2              DELV  840441  755282  687330
#3               SDN  453992  424043  438191
#4              SUBN 3451963 3466037 3719802
#5               VAN 1605228 1518303 1411970

# Comparing frequency of vehicle make accross year 2015,2016 and 2017

SparkR::merge( 
  SparkR::merge(df_freq_of_vehicle_make_2015, df_freq_of_vehicle_make_2016,
                by="Vehicle Make")
  ,df_freq_of_vehicle_make_2017
  ,by="Vehicle Make", all=T)

#  Vehicle Make count.x count.y   count
#1        CHEVR  836389  759663  714655
#2         FORD 1417303 1324774 1280958
#3        HONDA 1018049 1014074 1079238
#4        NISSA  837569  834833  918590
#5        TOYOT 1123523 1154790 1211451

#######################################################################################################

#3. A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:
#    1. Violating Precincts (this is the precinct of the zone where the violation occurred)

## Analysis for year 2015
freq_of_violating_precinct_2015 <- SparkR::summarize(SparkR::groupBy(parking_violations_2015
                                                                     , parking_violations_2015$`Violation Precinct`),
                                                     count = SparkR::count(parking_violations_2015$'Summons Number'))


df_freq_of_violating_precinct_2015 <- head(SparkR::arrange(freq_of_violating_precinct_2015, SparkR::desc(freq_of_violating_precinct_2015$count)),5)

View(df_freq_of_violating_precinct_2015)
#Violation Precinct   count
#1                  0 1633006
#2                 19  559716
#3                 18  400887
#4                 14  384596
#5                  1  307808

## Analysis for year 2016
freq_of_violating_precinct_2016 <- SparkR::summarize(SparkR::groupBy(parking_violations_2016
                                                                     , parking_violations_2016$`Violation Precinct`),
                                                     count = SparkR::count(parking_violations_2016$'Summons Number'))


df_freq_of_violating_precinct_2016 <- head(SparkR::arrange(freq_of_violating_precinct_2016, SparkR::desc(freq_of_violating_precinct_2016$count)),5)

View(df_freq_of_violating_precinct_2016)
#Violation Precinct   count
#1                  0 1868655
#2                 19  554465
#3                 18  331704
#4                 14  324467
#5                  1  303850

## Analysis for year 2017
freq_of_violating_precinct_2017 <- SparkR::summarize(SparkR::groupBy(parking_violations_2017
                                                                     , parking_violations_2017$`Violation Precinct`),
                                                     count = SparkR::count(parking_violations_2017$'Summons Number'))

df_freq_of_violating_precinct_2017 <- head(SparkR::arrange(freq_of_violating_precinct_2017, SparkR::desc(freq_of_violating_precinct_2017$count)),5)

View(df_freq_of_violating_precinct_2017)
#Violation Precinct   count
#1                  0 2072400
#2                 19  535671
#3                 14  352450
#4                  1  331810
#5                 18  306920

# Comparing Violating Precincts accross all years -

SparkR::merge( 
  SparkR::merge(df_freq_of_violating_precinct_2015, df_freq_of_violating_precinct_2016,
                by="Violation Precinct")
  ,df_freq_of_violating_precinct_2017
  ,by="Violation Precinct", all=T)

##  Violation Precinct count.2015 count.2016   count.2017
#1                  0 1633006 1868655 2072400
#2                  1  307808  303850  331810
#3                 14  384596  324467  352450
#4                 18  400887  331704  306920
#5                 19  559716  554465  535671

#    2. Issuing Precincts (this is the precinct that issued the ticket)

## Analysis for year 2015
freq_of_issuer_precinct_2015 <- SparkR::summarize(SparkR::groupBy(parking_violations_2015
                                                                  , parking_violations_2015$`Issuer Precinct`),
                                                  count = SparkR::count(parking_violations_2015$'Summons Number'))


df_freq_of_issuer_precinct_2015 <- head(SparkR::arrange(freq_of_issuer_precinct_2015, SparkR::desc(freq_of_issuer_precinct_2015$count)),5)

View(df_freq_of_issuer_precinct_2015)
#Issuer Precinct   count
#1               0 1834343
#2              19  544946
#3              18  391501
#4              14  369725
#5               1  298594

## Analysis for year 2016
freq_of_issuer_precinct_2016 <- SparkR::summarize(SparkR::groupBy(parking_violations_2016
                                                                  , parking_violations_2016$`Issuer Precinct`),
                                                  count = SparkR::count(parking_violations_2016$'Summons Number'))


df_freq_of_issuer_precinct_2016 <- head(SparkR::arrange(freq_of_issuer_precinct_2016, SparkR::desc(freq_of_issuer_precinct_2016$count)),5)

View(df_freq_of_issuer_precinct_2016)
#Issuer Precinct   count
#1               0 2140274
#2              19  540569
#3              18  323132
#4              14  315311
#5               1  295013

## Analysis for year 2017
freq_of_issuer_precinct_2017 <- SparkR::summarize(SparkR::groupBy(parking_violations_2017
                                                                  , parking_violations_2017$`Issuer Precinct`),
                                                  count = SparkR::count(parking_violations_2017$'Summons Number'))


df_freq_of_issuer_precinct_2017 <- head(SparkR::arrange(freq_of_issuer_precinct_2017, SparkR::desc(freq_of_issuer_precinct_2017$count)),5)

View(df_freq_of_issuer_precinct_2017)
#Issuer Precinct   count
#1               0 2388479
#2              19  521513
#3              14  344977
#4               1  321170
#5              18  296553

# Comparing Issuing Precincts accross all years -

SparkR::merge( 
  SparkR::merge(df_freq_of_issuer_precinct_2015, df_freq_of_issuer_precinct_2016,
                by="Issuer Precinct")
  ,df_freq_of_issuer_precinct_2017
  ,by="Issuer Precinct", all=T)

##  Issuer Precinct count.2015 count.2016   count.2017
#1               0 1834343 2140274 2388479
#2               1  298594  295013  321170
#3              14  369725  315311  344977
#4              18  391501  323132  296553
#5              19  544946  540569  521513

#4. Find the violation code frequency across 3 precincts which have issued the most number of tickets - 
#   do these precinct zones have an #exceptionally high frequency of certain violation codes? 
#   Are these codes common across precincts?

## Analysis for year 2015, 3 highest issuer precincts are 0, 19 and 18
high_issuer_presincts_2015 <- SparkR::filter(parking_violations_2015, parking_violations_2015$'Issuer Precinct' == 0
                                             | parking_violations_2015$'Issuer Precinct' == 19
                                             | parking_violations_2015$'Issuer Precinct' == 18)

freq_of_issuer_nd_code_2015 <- SparkR::summarize(SparkR::groupBy(high_issuer_presincts_2015
                                                                 , high_issuer_presincts_2015$`Issuer Precinct`
                                                                 , high_issuer_presincts_2015$'Violation Code'),
                                                 count = SparkR::count(high_issuer_presincts_2015$'Summons Number'))


show(collect(SparkR::arrange(freq_of_issuer_nd_code_2015, SparkR::desc(freq_of_issuer_nd_code_2015$count))))

#Sample output ----
##Issuer Precinct Violation Code  count
#1                 0             36 761571
#2                 0              7 662201
#3                 0              5 195352
#4                 0             21 180481
#5                18             14 121004
#6                19             38  90437
#7                19             37  79738
#8                19             14  60589
#9                18             69  57218
#10               19             21  56416

## Analysis for year 2016, 3 highest issuer precincts are 0, 19 and 18
high_issuer_presincts_2016 <- SparkR::filter(parking_violations_2016, parking_violations_2016$'Issuer Precinct' == 0
                                             | parking_violations_2016$'Issuer Precinct' == 19
                                             | parking_violations_2016$'Issuer Precinct' == 18)

freq_of_issuer_nd_code_2016 <- SparkR::summarize(SparkR::groupBy(high_issuer_presincts_2016
                                                                 , high_issuer_presincts_2016$`Issuer Precinct`
                                                                 , high_issuer_presincts_2016$'Violation Code'),
                                                 count = SparkR::count(high_issuer_presincts_2016$'Summons Number'))


show(collect(SparkR::arrange(freq_of_issuer_nd_code_2016, SparkR::desc(freq_of_issuer_nd_code_2016$count))))

#Sample output -
##Issuer Precinct Violation Code   count
#1                 0             36 1253511
#2                 0              7  492469
#3                 0             21  237174
#4                 0              5  112376
#5                18             14   99857
#6                19             38   77183
#7                19             37   75641
#8                19             46   73016
#9                19             14   61742
#10               19             21   58719

## Analysis for year 2017, 3 highest issuer precincts are 0, 19 and 14
high_issuer_presincts_2017 <- SparkR::filter(parking_violations_2017, parking_violations_2017$'Issuer Precinct' == 0
                                             | parking_violations_2017$'Issuer Precinct' == 19
                                             | parking_violations_2017$'Issuer Precinct' == 14)

freq_of_issuer_nd_code_2017 <- SparkR::summarize(SparkR::groupBy(high_issuer_presincts_2017
                                                                 , high_issuer_presincts_2017$`Issuer Precinct`
                                                                 , high_issuer_presincts_2017$'Violation Code'),
                                                 count = SparkR::count(high_issuer_presincts_2017$'Summons Number'))


show(collect(SparkR::arrange(freq_of_issuer_nd_code_2017, SparkR::desc(freq_of_issuer_nd_code_2017$count))))

#Sample output --

##Issuer Precinct Violation Code   count
#1                 0             36 1400614
#2                 0              7  516389
#3                 0             21  268591
#4                 0              5  145642
#5                19             46   86390
#6                14             14   73837
#7                19             37   72437
#8                19             38   72344
#9                14             69   58026
#10               19             14   57563

# Do these precinct zones have an exceptionally high frequency of certain violation codes? 

# Yes from the sample outputs we can clearly tell that the Violation codes (36,7,21 and 5) has an exceptionally
# high frequency of its occurance

# Are these codes common across precincts?

# Yes these violation codes (36,7,21 and 5) are common accross precincts

#5. You'd want to find out the properties of parking violations across different times of the day:
#       The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.

#####Need steps to modify time , there are many wrong format.........
head(SparkR::distinct(SparkR::select(
  SparkR::filter(parking_violations_2017, 
                 length(parking_violations_2017$'Violation Time') != 5),'Violation Time')), 50)  ## only length 4 1 exception 015A



nrow(SparkR::select(
  SparkR::filter(parking_violations_2016, 
                 rlike(parking_violations_2016$'Violation Time',"[0-9][0-9][0-9][0-9][AP]*")),'Violation Time'))

nrow(SparkR::select(
  SparkR::filter(parking_violations_2017, 
                 rlike(parking_violations_2017$'Violation Time',"[0-9][0-9][0-9][0-9][AP]*")),'Violation Time'))



###There are many wrong date format but numbers are really less so we will consider only the below patterns for analysis -
# HHMM or HHMM[AP] - Excluding others from all the three dataframes -

parking_violations_2015 <- SparkR::filter(parking_violations_2015, 
                                          rlike(parking_violations_2015$'Violation Time',"[0-9][0-9][0-9][0-9][AP]*"))

parking_violations_2016 <- SparkR::filter(parking_violations_2016, 
                                          rlike(parking_violations_2016$'Violation Time',"[0-9][0-9][0-9][0-9][AP]*"))

parking_violations_2017 <- SparkR::filter(parking_violations_2017, 
                                          rlike(parking_violations_2017$'Violation Time',"[0-9][0-9][0-9][0-9][AP]*"))


#       Find a way to deal with missing values, if any.

#Check for any null values in the Violation time column -

nrow(SparkR::filter(parking_violations_2015, isNull(parking_violations_2015$'Violation Time')))  #0
nrow(SparkR::filter(parking_violations_2015, isNull(parking_violations_2015$'Violation Time')))  #0
nrow(SparkR::filter(parking_violations_2015, isNull(parking_violations_2015$'Violation Time')))  #0


# There are no Nul values in any of the 3 data frame...
#       Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the 3 most commonly occurring violations

#       Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)

#6. Let's try and find some seasonality in this data

# First, divide the year into some number of seasons, and find frequencies of tickets for each season.

  ## Analysis for year 2015   
  Freq_of_tickets_each_season_2015 <- SparkR::sql("select 
                                                  (
                                                  case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                  else 'Winter'
                                                  end
                                                  ) as Seasons, 
                                                  count(`Summons Number`) as Frequency_of_tickets 
                                                  from parking_violations_2015_v
                                                  group by (
                                                  case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                  else 'Winter'
                                                  end
                                                  )")
  
  View(Freq_of_tickets_each_season_2015)
  # Seasons Frequency_of_tickets
  # 1  Spring              2951328
  # 2  Summer              3098729
  # 3    Fall              2718868
  # 4  Winter              2182331
  
  ## Analysis for year 2016
  Freq_of_tickets_each_season_2016 <- SparkR::sql("select 
                                                  (
                                                  case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                  else 'Winter'
                                                  end
                                                  ) as Seasons, 
                                                  count(`Summons Number`) as Frequency_of_tickets 
                                                  from parking_violations_2016_v
                                                  group by (
                                                  case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                  else 'Winter'
                                                  end
                                                  )")
  
  View(Freq_of_tickets_each_season_2016)
  # Seasons Frequency_of_tickets
  # 1  Spring              2790946
  # 2  Summer              2438069
  # 3    Fall              2973396
  # 4  Winter              2424488
  
  ## Analysis for year 2017
  Freq_of_tickets_each_season_2017 <- SparkR::sql("select 
                                                  (
                                                  case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                  else 'Winter'
                                                  end
                                                  ) as Seasons, 
                                                  count(`Summons Number`) as Frequency_of_tickets 
                                                  from parking_violations_2017_v
                                                  group by (
                                                  case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                  when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                  else 'Winter'
                                                  end
                                                  )")
  
  View(Freq_of_tickets_each_season_2017)
  # Seasons Frequency_of_tickets
  # 1  Spring              2880687
  # 2  Summer              2606208
  # 3    Fall              2830802
  # 4  Winter              2485331

# Then, find the 3 most common violations for each of these season

  # Analysis for 2015
  Violations_each_season_2015_Spring <- SparkR::sql("select 
                                                    (
                                                    case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                    else 'Winter'
                                                    end
                                                    ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                    from parking_violations_2015_v
                                                    where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 
                                                    group by (
                                                    case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                    else 'Winter'
                                                    end
                                                    ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2015_Spring,3)
  ## Result : 3 most common violation code 21, 38 , 14 	
  # Seasons	    Violation Code Frequency_of_tickets
  # Spring             21               425354
  # Spring             38               327057
  # Spring             14               243771
  
  
  Violations_each_season_2015_Summer<- SparkR::sql("select 
                                                   (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                   from parking_violations_2015_v
                                                   where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 
                                                   group by (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2015_Summer,3)
  
  # Result : 3 most common violations 21, 38, 14 
  #Seasons Violation Code Frequency_of_tickets
  # Summer             21               476855
  # Summer             38               364578
  # Summer             14               257327
  
  
  Violations_each_season_2015_Fall<- SparkR::sql("select 
                                                 (
                                                 case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                 else 'Winter'
                                                 end
                                                 ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                 from parking_violations_2015_v
                                                 where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11 
                                                 group by (
                                                 case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                 else 'Winter'
                                                 end
                                                 ),`Violation Code` order by count(`Summons Number`) desc")
  	
  
  head(Violations_each_season_2015_Fall,3)
  
  #Result: 3 most common violations 21, 38, 36
  #	Seasons Violation Code Frequency_of_tickets
  #   Fall             21               360548
  #   Fall             38               329955
  #   Fall             36               244788
  
  Violations_each_season_2015_Winter<- SparkR::sql("select 
                                                   (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                   from parking_violations_2015_v
                                                   where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) NOT IN (3,4,5,6,7,8,9,10,11) 
                                                   group by (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2015_Winter,3)
  
  #Result: 3 most common violatio 38, 21, 14
  #Seasons Violation Code Frequency_of_tickets
  # Winter             38               397037
  # Winter             21               368155
  # Winter             14               250831
  
  # Analysis for 2016
  Violations_each_season_2016_Spring <- SparkR::sql("select 
                                                    (
                                                    case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                    else 'Winter'
                                                    end
                                                    ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                    from parking_violations_2016_v
                                                    where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 
                                                    group by (
                                                    case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                    else 'Winter'
                                                    end
                                                    ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2016_Spring,3)
  ## Result : 3 most common violation code 21, 36, 38	
  # Seasons Violation Code Frequency_of_tickets
  #  Spring             21               383757
  #  Spring             36               374362
  #  Spring             38               299459
  
  Violations_each_season_2016_Summer<- SparkR::sql("select 
                                                   (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                   from parking_violations_2016_v
                                                   where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 
                                                   group by (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2016_Summer,3)
  
  # Result : 3 most common violations 21, 38, 14 
  #Seasons Violation Code Frequency_of_tickets
  # Summer             21               392205
  # Summer             38               272419
  # Summer             14               215683
  
  Violations_each_season_2016_Fall<- SparkR::sql("select 
                                                 (
                                                 case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                 else 'Winter'
                                                 end
                                                 ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                 from parking_violations_2016_v
                                                 where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11 
                                                 group by (
                                                 case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                 else 'Winter'
                                                 end
                                                 ),`Violation Code` order by count(`Summons Number`) desc")
  	
  
  head(Violations_each_season_2016_Fall,3)
  
  #Result: 3 most common violations 36, 21, 38
  #Seasons Violation Code Frequency_of_tickets
  #    Fall             36               438320
  #    Fall             21               395357
  #    Fall             38               303397
  
  
  Violations_each_season_2016_Winter<- SparkR::sql("select 
                                                   (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                   from parking_violations_2016_v
                                                   where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) NOT IN (3,4,5,6,7,8,9,10,11) 
                                                   group by (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2016_Winter,3)
  
  #Result: 3 most common violatio 38, 21, 14
  #Seasons Violation Code Frequency_of_tickets
  # Winter             21               360268
  # Winter             36               314765
  # Winter             38               268421
  
  #Analysis for 2017
  Violations_each_season_2017_Spring <- SparkR::sql("select 
                                                    (
                                                    case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                    else 'Winter'
                                                    end
                                                    ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                    from parking_violations_2017_v
                                                    where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 
                                                    group by (
                                                    case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                    when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                    else 'Winter'
                                                    end
                                                    ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2017_Spring,3)
  ## Result : 3 most common violation code 21, 36,38 	
  # Seasons Violation Code Frequency_of_tickets
  # Spring             21               402807
  # Spring             36               344834
  # Spring             38               271192
  
  Violations_each_season_2017_Summer<- SparkR::sql("select 
                                                   (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                   from parking_violations_2017_v
                                                   where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 
                                                   group by (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2017_Summer,3)
  
  # Result : 3 most common violations 21, 38, 36
  #Seasons Violation Code Frequency_of_tickets
  # Summer             21               405961
  # Summer             38               247561
  # Summer             36               240396
  
  
  Violations_each_season_2017_Fall<- SparkR::sql("select 
                                                 (
                                                 case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                 else 'Winter'
                                                 end
                                                 ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                 from parking_violations_2017_v
                                                 where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11 
                                                 group by (
                                                 case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                 when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                 else 'Winter'
                                                 end
                                                 ),`Violation Code` order by count(`Summons Number`) desc")
  	
  
  head(Violations_each_season_2017_Fall,3)
  
  #Result: 3 most common violations 36,21,38
  # Seasons Violation Code Frequency_of_tickets
  #   Fall             36               456046
  #   Fall             21               357479
  #   Fall             38               283828
  
  Violations_each_season_2017_Winter<- SparkR::sql("select 
                                                   (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ) as Seasons, `Violation Code` ,count(`Summons Number`) as Frequency_of_tickets 
                                                   from parking_violations_2017_v
                                                   where MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) NOT IN (3,4,5,6,7,8,9,10,11) 
                                                   group by (
                                                   case when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 3 and 5 THEN 'Spring'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 6 and 8 THEN 'Summer'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) BETWEEN 9 and 11  THEN 'Fall'
                                                   when MONTH(TO_DATE(CAST(UNIX_TIMESTAMP(`Issue Date`, 'MM/dd/yyyy') AS TIMESTAMP))) >= 12 THEN 'Winter'
                                                   else 'Winter'
                                                   end
                                                   ),`Violation Code` order by count(`Summons Number`) desc")
  	
  head(Violations_each_season_2017_Winter,3)
  
  #Result: 3 most common violatio 21,36,38
  #Seasons Violation Code Frequency_of_tickets
  # Winter             21               362341
  # Winter             36               359338
  # Winter             38               259723

#7. The fines collected from all the parking violation constitute a revenue source for the NYC police department. Let’s take an example of estimating that for the 3 most commonly occurring codes.
  #       Find total occurrences of the 3 most common violation codes
  count_violation_code_2015 <- SparkR::summarize(SparkR::groupBy(parking_violations_2015
                                                                 , parking_violations_2015$`Violation Code`),
                                                 count = SparkR::count(parking_violations_2015$'Summons Number'))
  
  
  most_common_violation_2015 <- head(SparkR::arrange(count_violation_code_2015, SparkR::desc(count_violation_code_2015$count)),3)
  View(most_common_violation_2015)
  
  #Most common violations of 2015
  #Violation Code  count
  #21              1630912
  #38              1418627
  #14              988469
  
  count_violation_code_2016 <- SparkR::summarize(SparkR::groupBy(parking_violations_2016
                                                                 , parking_violations_2016$`Violation Code`),
                                                 count = SparkR::count(parking_violations_2016$'Summons Number'))
  
  
  most_common_violation_2016 <- head(SparkR::arrange(count_violation_code_2016, SparkR::desc(count_violation_code_2016$count)),3)
  View(most_common_violation_2016)
  
  #Most common violations of 2016
  #Violation Code  count
  #21              1531587
  #36              1253512
  #38              1143696
  
  count_violation_code_2017 <- SparkR::summarize(SparkR::groupBy(parking_violations_2017
                                                                 , parking_violations_2017$`Violation Code`),
                                                 count = SparkR::count(parking_violations_2017$'Summons Number'))
  
  
  most_common_violation_2017 <- head(SparkR::arrange(count_violation_code_2017, SparkR::desc(count_violation_code_2017$count)),3)
  View(most_common_violation_2017)
  
  #Most common violations of 2017
  #Violation Code  count
  #21              1528588
  #36              1400614
  #38              1062304
  
  
  # Then, search the internet for NYC parking violation code fines. You will find a website (on the nyc.gov URL) that lists these fines. They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. For simplicity, take an average of the two.
  
  #Average fine of violation code 21 - 55$
  #Average fine of violation code 38 - 50$
  #Average fine of violation code 14 - 115$
  #Average fine of violation code 36 - 50$
  
  #Data fetched from https://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page
  
  # Using this information, find the total amount collected for all of the fines. State the code which has the highest total collection.
  
  fine_df <- data.frame(ViolationCode=c(21,38,14,36),AverageFine=c(55,50,115,50))
  fines_2015 <- merge(most_common_violation_2015,fine_df,by=1)
  fines_2016 <- merge(most_common_violation_2016,fine_df,by=1)
  fines_2017 <- merge(most_common_violation_2017,fine_df,by=1)
  
  fines_2015$Total_Fine <- fines_2015$count * fines_2015$AverageFine
  fines_2016$Total_Fine <- fines_2016$count * fines_2016$AverageFine  
  fines_2017$Total_Fine <- fines_2017$count * fines_2017$AverageFine  
  
  sum(fines_2015$Total_Fine) #274305445$
  sum(fines_2016$Total_Fine) #204097685$
  sum(fines_2017$Total_Fine) #207218240$
  
  #Total amount collected for 2015 is $274305445
  #Total amount collected for 2016 is $204097685
  #Total amount collected for 2017 is $207218240
  
  code_14_total_fine <- 
    sum((fines_2015[which(fines_2015$'Violation Code'=='14'),]$Total_Fine), (fines_2016[which(fines_2016$'Violation Code'=='14'),]$Total_Fine),(fines_2017[which(fines_2017$'Violation Code'=='14'),]$Total_Fine))
  #Total fine amount(3 years combined)collected for violation code 14 is 113673935
  
  code_21_total_fine <- 
    sum((fines_2015[which(fines_2015$'Violation Code'=='21'),]$Total_Fine), (fines_2016[which(fines_2016$'Violation Code'=='21'),]$Total_Fine),(fines_2017[which(fines_2017$'Violation Code'=='21'),]$Total_Fine))
  #Total fine amount collected(3 years combined) for violation code 21 is 258009785
  
  code_36_total_fine <- 
    sum((fines_2015[which(fines_2015$'Violation Code'=='36'),]$Total_Fine), (fines_2016[which(fines_2016$'Violation Code'=='36'),]$Total_Fine),(fines_2017[which(fines_2017$'Violation Code'=='36'),]$Total_Fine))
  #Total fine amount collected(3 years combined) for violation code 36 is 132706300
  
  code_38_total_fine <- 
    sum((fines_2015[which(fines_2015$'Violation Code'=='38'),]$Total_Fine), (fines_2016[which(fines_2016$'Violation Code'=='38'),]$Total_Fine),(fines_2017[which(fines_2017$'Violation Code'=='38'),]$Total_Fine))
  #Total fine amount collected(3 years combined) for violation code 38 is 181231350
  
  max(code_14_total_fine,code_21_total_fine,code_36_total_fine,code_38_total_fine) #258009785
  #Violation code 21 resulted in maxiumum fine collection
  
  # What can you intuitively infer from these findings?
  
  # We can infer the below things from these findings
  #1. Violation Code 21 is the most common form of violation as this resulted in highest number of violations for all 3 years
  #2. Due to the hefty fine on Violation Code 14, number of people violating this code has decreased from 2015.

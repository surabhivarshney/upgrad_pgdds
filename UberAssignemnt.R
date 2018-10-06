#*********************************************************************************************************
# ********************UBER SUPPLY DEMAND ASSIGNEMENT  - DATA ANALYSIS*************************************
#*********************************************************************************************************
# This is an indiviual assignment to be done by each student as a part of Exploratory Data analytics     *
#                                                                                                        *
# Brief about Assignment:                                                                                * 
# Uber is a leading global cab providing service.But recently they have identified that there is some    *
# issue with respect to availability of cars especially at the airport in one specific city where the    *
# airport is far away from the city.                                                                     *
# They want to use the historical data to identify the reasons for issues like cancellation of rides and *
# non availability of cars and suggest proper solutions as well. This task includes the identification   *
# the cause behind this demand supply gap issue and to provide suitable measures.                        *                                            
#                                                                                                        * 
# Objective:                                                                                    *
# The objective is to analyse the data given and identify the reasons behind common                      *
# issues being faced by customers like non availability of cars and requests getting cancelled especially*
# when the ride is between airport and a specific city. Also, we need to provide appropriate measures to *
# result in a better profitable customer service.                                                        *
#                                                                                                        *
# Input files used:                                                                                      *
#                                                                                                        *
# 1. Uber Request Data.csv  - This data set is a masked data set which is similar to what data analysts  *
# at Uber handle, which gives an idea about how problems are systematically solved using EDA and data    *
# visualisation.                                                                                         *
#--------------------------------------------------------------------------------------------------------*
#               Attribute     |     Description                                                          *
#--------------------------------------------------------------------------------------------------------*
#               Request id    |     A unique identifier of the request                                   *  
#               Pick point    |     Pick point (airport/city)                                            *
#                driver id    |     The unique identification number of the driver                       *
#                  status     |     final Status of the ride (cancelled/no cars available/trip completed)*
#            Request timestamp|     Date timestamp at which the customer made the request                * 
#              Drop timestamp |     Date timestamp of the drop incase the trip was completed             *
#--------------------------------------------------------------------------------------------------------*
#                                                                                                        *
# Discripencies observed in data of Uber Request Data.csv                                                *
# ===============================================                                                        *
# > the date timestamp columns were having different delimitters (/ and -)                               *
# > the format of the two date timestamp columns were neither uniform nor standard                       *
# > some records were missing the second compoment of date timestamp, which had to be then replaced by 00*
#*********************************************************************************************************
#--------------------------------------------------------------------------------------------------------*
#                                                                                                        *
# Libraries used:                                                                                        *
#--------------------------------------------------------------------------------------------------------*

library(tidyr)      # Data manipulation    
library(dplyr)      # Data manipulation 
library(ggplot2)    # for plotting graphs
library(lubridate)  # to parse the date

#--------------------------------------------------------------------------------------------------------*
# Importing input files to R                                                                             *
#--------------------------------------------------------------------------------------------------------*
# setting the working directory

setwd("D:/upgrad/uber")

# listing the files in the working dir
list.files()

# Reading the csv file to a dataframe
uber_df<-read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)

# viewing the structure of data frame to get an insight to the data provided
str(uber_df)

# viewing some sample rows in the data frame
uber_df[1:10,]

#########################################################################################################
# CLEANING/FORMATTING
#########################################################################################################
# creating a copy of the data frame to work on
# this will help in preserving one original data set provided so that in case roll back of activites 
# is required we can simply refer to the original copy of the data frame 

uber_df2<-uber_df

# From here on, we will perform operations on uber_df2 data frame and keep the original data frame intact

# Formatting of date timestamp columns
# here we are parsing the date timestamp columns (Drop tiestamp and Request timestamp) to convert them to 
# standard format

uber_df2$Drop.timestamp <- parse_date_time(uber_df2$Drop.timestamp , c('%d%m%y %H%M%S', '%d%m%y %H%M'))

uber_df2$Request.timestamp <- parse_date_time(uber_df2$Request.timestamp , c('%d%m%y %H%M%S', '%d%m%y %H%M'))

# splitting the timestamp columns to date and time separately based on space as the delimitter
# reading the documentation for separate function
??separate

uber_df2<-separate(data = uber_df2, col = Request.timestamp, into = c("Request.date", "Request.time"), sep = " ")
uber_df2<-separate(data = uber_df2, col = Drop.timestamp, into = c("Drop.date", "Drop.time"), sep = " ")

# verifying the splitting by viewing the first row
uber_df2[1,]


# splitting the time column into hour min and sec based on : colon as the delimitter

uber_df2<-separate(data = uber_df2, col = Request.time, into = c("Request.hour", "Request.min","Request.sec"), sep = ":")
uber_df2<-separate(data = uber_df2, col = Drop.time, into = c("Drop.hour", "Drop.min","Drop.sec"), sep = ":")

# verifying the splitting by viewing the first row
uber_df2[1,]

# finding the corresponding week day to the request dates
uber_df2$Request.Day<-weekdays(as.Date(uber_df2$Request.date))

# changing the class of Request Hour to numeric to help with further analysis
uber_df2$Request.hour<-as.numeric(uber_df2$Request.hour)

# verifying the class of the request  Hour
class(uber_df2$Request.hour)

########################GRAPH1#############################################################################
# Plotting the number of cabs requested wrt particular hours of day for all 05 days of the week
# Pickup points can either be airport or city therefore it will be displayed in two colors for easier analysis

hourly_request <- ggplot(uber_df2,aes(x=factor(Request.hour),fill=factor(Pickup.point)))

# displaying it in a bar chart format

graph1 <- hourly_request+geom_bar(stat='count',position = "dodge")+ 
  ggtitle("Hourly Demand of Uber Cabs")+ labs(x="Time in Hrs", y="Number of Cabs Requested")+
  labs(fill="Pickup Point")

#view the plot so generated : showing the hourly demand of the cabs wrt all 5 days of the week
graph1

###########################################################################################################
# CONCLUSION drawn from GRAPH1:
# no of cab requests are more from city during morning hours (5AM to 10AM)
# and more from airport during night hours (5PM to 10 PM)
# meaning demand of cabs is more in city during morning hours but demand of cars is more 
# in airport during evening hours, so the supply should be managed respectively

###########################################################################################################


# dividing the day (24hrs) into following buckets:
# pre morning - before 3AM
# morning rush - between 4 AM to 9 AM
# day_time - between 10 AM to 4 PM
# evening rush - between 5 PM to 9 PM
# late_night - between 10 PM to 12 AM
# and then mapping each hour to above timeslots

uber_df2$Time_Slot[uber_df2$Request.hour <= 3] <- c("Pre_Morning")

uber_df2$Time_Slot[((uber_df2$Request.hour >= 4) & (uber_df2$Request.hour <= 9))] <- c("Morning_Rush")

uber_df2$Time_Slot[((uber_df2$Request.hour >= 10) & (uber_df2$Request.hour <= 16))] <- c("Day_Time")

uber_df2$Time_Slot[((uber_df2$Request.hour >= 17) & (uber_df2$Request.hour <= 21))] <- c("Evening_Rush")

uber_df2$Time_Slot[((uber_df2$Request.hour >= 22) & (uber_df2$Request.hour <= 24))] <- c("Late_Night")

uber_df2

########################GRAPH2#############################################################################
# plotting the trips completed wrt part of day 
# creating a subset of completed trips
trips_completed <- subset(uber_df2,uber_df2$Status=="Trip Completed")
# Plot a bar chart with time-slots (part fo the day) on x axis and trips completed on Y-axis
Timeslot_bar <- ggplot(trips_completed,aes(x=Time_Slot))
graph2 <- Timeslot_bar+geom_bar(stat="count",col="black",fill="purple")+
  ggtitle("Trips completed during different Time Slots")+
  labs(x="Time Slots",y="Trips Completed")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  guides(fill=FALSE)+
  scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))

# viewing graph2
graph2

###########################################################################################################
# CONCLUSION2 drawn from GRAPH2:
# more no of trips get completed in morning rush  (from 4Am to 9AM)
###########################################################################################################


########################GRAPH3#############################################################################
# plot a bar chart with time slots on x-axis and cab request frequency on y-axis
# show the status of requests (completed/cancelled/not available) in different colors 
# save the plot as a object
timeslot_request_freq <- ggplot(uber_df2,aes(x=factor(Time_Slot),fill=factor(Status)))
graph3 <- timeslot_request_freq+geom_bar(stat="count",position = "stack",col="black")+
  ggtitle("Trips during Different Time Slots")+
  scale_x_discrete(limits=c("Evening_Rush","Morning_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))+
  labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
  scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))

# view the graph
graph3

##########################################################################################################
# CONCLUSION3 drawn from GRAPH3:
# more no of cars are unavailable during evening rush
##########################################################################################################


##################################ISSUES##################################################################
#Issue 1. 
# Large number of cab requests got cancelled during the Morning_Rush Time slot
# Subset the Morning Rush time slot data for further analysis

Problem_df <- subset(uber_df2,uber_df2$Time_Slot=="Morning_Rush")
# Plot the bargraph with status of request in x-axis and count in y-axis for Morning rush time slot
# Show the request from different pickup points in different colors for better visualization
Problem1_count <- ggplot(Problem_df,aes(x=factor(Status),fill=factor(Pickup.point)))
graph4 <- Problem1_count+geom_bar(stat="count",position = "stack")+  ggtitle("Morning Rush Cab Status")+labs(x="Status",y="Total count")+labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))

# view the graph
graph4

#########################################################################################################
# CONCLUSION4 drawn from GRAPH4:
# in the morning rush hour of the day more trips are getting cancelled as well as more 
# cars are unavailable if pick up point is city
# Over all during the morning rush hour : more demand of cabs in city but less supply
#########################################################################################################

# Issue2
# subset the data for Evening rush from dataframe for further analysis

Problem2_df <- subset(subset(uber_df2,uber_df2$Time_Slot=="Evening_Rush"))
# plot the bar graph with status of requests on x-axis and count in y-axis for evening rush time slot
# Show the request from different pickup points in different colors for better visualization
Problem2_count <- ggplot(Problem2_df,aes(x=factor(Status),fill=factor(Pickup.point)))
graph5 <- Problem2_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Evening Rush Cabs Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))
graph5


#########################################################################################################
# CONCLUSION5 drawn from GRAPH5: 
# in evening rush more no cars are unavailable from airport
# in other words in evening rush hours (from 5PM to 9PM) demand is more at airport 
# but supply is less
#########################################################################################################
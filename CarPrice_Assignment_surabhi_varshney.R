#*********************************************************************************************************
# **************************************Assignment- Linear Regression*************************************
#*********************************************************************************************************
# This is an indiviual assignment to be done by each student as a part of inear regression               *
#                                                                                                        *
# Submitted By:                                                                                          *
# SURABHI VARSHNEY                                                                                       *
#                                                                                                        *
# Brief about Assignment:                                                                                * 
# A Chinese automobile company Geely Auto aspires to enter the US market by setting up their             *
# manufacturing unit there and producing cars locally to give competition to their US and European       *
# counterparts.                                                                                          *
#                                                                                                        *
# They have contracted an automobile consulting company to understand the factors on which the pricing of* 
# a car depends. Specifically, they want to understand the factors affecting the pricing of cars in the  *
# American marketing, since those may be very different from the Chinese market.                         *
# Essentially, the company wants to know:                                                                *
# * Which variables are significant in predicting the price of a car                                     *
# * How well those variables describe the price of a car                                                 *
#                                                                                                        *
#                                                                                                        *
#                                                                                                        *
# Objective:                                                                                             *
#                                                                                                        *
# To model the price of cars with the available independent variables.                                   *
# It will be used by the management to understand how exactly the prices vary with the independent       *
# variables. They can accordingly manipulate the design of the cars, the business strategy etc. to meet  *
# certain price levels.                                                                                  *
# Further, the model will be a good way for the management to understand the pricing dynamics of a       *
# new market.                                                                                            *          
#                                                                                                        *
# Input files used:                                                                                      *
#                                                                                                        *
# 1. CarPrice_Assignment.csv  - Based on various market surveys, the consulting firm has gathered a      *
# large dataset of different types of cars across the Americal market.                                   *
##########################################################################################################

#################################################################################################################################################################
# ###################################DATA DICTONARY#############################################################################################################						                                                                                                                        #
# 						                                                                                                                                                 #
# 1	Car_ID			    Unique id of each observation (Interger)		                                                                                               #
# 2	Symboling 			Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) 	     #	
# 3	carCompany			Name of car company (Categorical)		                                                                                                       #
# 4	fueltype		   	Car fuel type i.e gas or diesel (Categorical)		                                                                                           #
# 5	aspiration			Aspiration used in a car (Categorical)		                                                                                                 #
# 6	doornumber			Number of doors in a car (Categorical)		                                                                                                 #
# 7	carbody			    body of car (Categorical)		                                                                                                               #
# 8	drivewheel			type of drive wheel (Categorical)		                                                                                                       #
# 9	enginelocation	Location of car engine (Categorical)		                                                                                                   #
# 10	wheelbase			Weelbase of car (Numeric)		                                                                                                               #
# 11	carlength			Length of car (Numeric)		                                                                                                                 #
# 12	carwidth			Width of car (Numeric)		                                                                                                                 #
# 13	carheight			height of car (Numeric)		                                                                                                                 #
# 14	curbweight		The weight of a car without occupants or baggage. (Numeric)		                                                                             #
# 15	enginetype		Type of engine. (Categorical)		                                                                                                           #
# 16	cylindernumbercylinder placed in the car (Categorical)		                                                                                               #
# 17	enginesize		Size of car (Numeric)		                                                                                                                   #
# 18	fuelsystem		Fuel system of car (Categorical)		                                                                                                       #
# 19	boreratio			Boreratio of car (Numeric)		                                                                                                             #
# 20	stroke			  Stroke or volume inside the engine (Numeric)		                                                                                           #
# 21	compressionratio			compression ratio of car (Numeric)		                                                                                             #
# 22	horsepower		Horsepower (Numeric)		                                                                                                                   #
# 23	peakrpm			  car peak rpm (Numeric)		                                                                                                                 #
# 24	citympg			  Mileage in city (Numeric)		                                                                                                               #
# 25	highwaympg		Mileage on highway (Numeric)		                                                                                                           #
# 26	price(Dependent variable)			Price of car (Numeric)		                                                                                                 #
#################################################################################################################################################################

list.files()

#reading the input file

dataset<- CarPrice_data <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)

# datadictonary TBD

#install the packaged required
library(stringr)
library(ggplot2)
library(MASS) #library in which stepAIC function exists
library(car)

library(tidyr)
#looking at he summary odf the data set
summary(CarPrice_data)

#structure of data
str(CarPrice_data)

#Check for duplicate values
sum(duplicated(CarPrice_data$car_id))

#Check for NA values for all variables

sapply(CarPrice_data, function(x) length(which(is.na(x))))
#All gave 0 NA values


#data peparation
# splitting carname into car company and car model as per requirement
dataset$CarName<-tolower(dataset$CarName)

dataset<- separate(data=dataset,col = CarName,into = c ("car_company","car_model") ,sep=" ")

#data correction
# replacing the incorrect names depending on other features of the car
dataset$car_company<-str_replace_all(dataset$car_company,"porcshce","porsche")
dataset$car_company<-str_replace_all(dataset$car_company,"toyouta","toyota")
dataset$car_company<-str_replace_all(dataset$car_company,"vw","volkswagen")
dataset$car_company<-str_replace_all(dataset$car_company,"vokswagen","volkswagen")
dataset$car_company<-str_replace_all(dataset$car_company,"maxda","mazda")

#checking NA values after separation
sum(is.na(dataset$model))

# ignoring the 2 NA values: as this column is not required for prediction

#derived metrics : risky (yes or no?) based on symbolling values
# if symbolling >0 risky=yes else risky=no (pretty safe)

myfunc<-function(x){
  
  ifelse (x > 0,     risky <- "yes",risky <- "no")
  
  return(risky)
}

dataset$IsRisky<-sapply(dataset$symboling,myfunc)

str(dataset)
View(dataset)

#replace the levels in case of categorical variables with 1 and 0

# categorical variables


IsRiskyYes <- data.frame(model.matrix( ~IsRisky, data = dataset))

#check the IsRisky data frame.
View(IsRiskyYes)

#This column should be removed from the newly created IsRiskyYes dataframe 
#containing the dummy values for the variable "IsRiskyYes". 
IsRiskyYes <- IsRiskyYes[,-1]

View(IsRiskyYes)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "IsRiskyYes" column
dataset_1 <- cbind(dataset[,-28], IsRiskyYes)
View(dataset_1)


#1 fuel type
#Converting "fueltype" into dummies . 
fueltypegas <- data.frame(model.matrix( ~fueltype, data = dataset_1))

#check the fueltypegas data frame.
View(fueltypegas)

#This column should be removed from the newly created fueltypegas dataframe 
#containing the dummy values for the variable "fueltype". 
fueltypegas <- fueltypegas[,-1]

View(fueltypegas)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "fueltype" column
dataset_1 <- cbind(dataset_1[,-5], fueltypegas)
View(dataset_1)

#############################################################################
# 2 aspiration
#Converting "aspiration" into dummies . 
aspiration_type_std <- data.frame(model.matrix( ~aspiration, data = dataset_1))

#check the aspiration_type_std data frame.
View(aspiration_type_std)

#This column should be removed from the newly created aspiration_type_std dataframe 
#containing the dummy values for the variable "fueltype". 
aspiration_type_std <- aspiration_type_std[,-1]

View(aspiration_type_std)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "aspiraton" column
dataset_1 <- cbind(dataset_1[,-5], aspiration_type_std)
View(dataset_1)

#################################################################################

# 3 doornumber
#Converting "doornumber" into dummies . 
doornumber_two <- data.frame(model.matrix( ~doornumber, data = dataset_1))

#check the doornumber_two data frame.

View(doornumber_two)

#This column should be removed from the newly created doornumber_two dataframe 
#containing the dummy values for the variable "doornumber". 
doornumber_two <- doornumber_two[,-1]

View(doornumber_two)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "doornumber" column
dataset_1 <- cbind(dataset_1[,-5], doornumber_two)
View(dataset_1)


#################################################################################

# 4 carbody
#Converting "carbody" into dummies . 
dummy1 <- data.frame(model.matrix( ~carbody, data = dataset_1))

#check the dummy_1 data frame.
View(dummy1)

#This column should be removed from the newly created dummy_1 dataframe 
#containing the dummy values for the variable "carbody". 
dummy1 <- dummy1[,-1]

View(dummy1)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "carbody" column
dataset_1 <- cbind(dataset_1[,-5], dummy1)
View(dataset_1)


#################################################################################

# 5 drivewheel
#Converting "drivewheel" into dummies . 
dummy1 <- data.frame(model.matrix( ~drivewheel, data = dataset_1))

#check the dummy_1 data frame.
View(dummy1)

#This column should be removed from the newly created dummy_1 dataframe 
#containing the dummy values for the variable "drivewheel". 
dummy1 <- dummy1[,-1]

View(dummy1)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "drivewheel" column
dataset_1 <- cbind(dataset_1[,-5], dummy1)
View(dataset_1)

#################################################################################

# 6 enginelocation
#Converting "enginelocation" into dummies . 
enginelocation_rear <- data.frame(model.matrix( ~enginelocation, data = dataset_1))

#check the dummy_1 data frame.
View(enginelocation_rear)
#This column should be removed from the newly created enginelocation_rear dataframe 
#containing the dummy values for the variable "enginelocation". 
enginelocation_rear <- enginelocation_rear[,-1]

View(enginelocation_rear)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "enginelocation" column
dataset_1 <- cbind(dataset_1[,-5], enginelocation_rear)
View(dataset_1)

###############################################################################3
# 7 enginetype
#Converting "enginetype" into dummies . 
dummy1 <- data.frame(model.matrix( ~enginetype, data = dataset_1))

#check the dummy_1 data frame.
View(dummy1)
#This column should be removed from the newly created dummy1 dataframe 
#containing the dummy values for the variable "enginetype". 
dummy1 <- dummy1[,-1]

View(dummy1)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "enginetype" column
dataset_1 <- cbind(dataset_1[,-10], dummy1)
View(dataset_1)


###############################################################################3
# 8 cylindernumber
#Converting "cylindernumber" into dummies . 
dummy1 <- data.frame(model.matrix( ~cylindernumber, data = dataset_1))

#check the dummy_1 data frame.
View(dummy1)
#This column should be removed from the newly created dummy1 dataframe 
#containing the dummy values for the variable "cylindernumber". 
dummy1 <- dummy1[,-1]

View(dummy1)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "cylindernumber" column
dataset_1 <- cbind(dataset_1[,-10], dummy1)
View(dataset_1)

################################################################################
# 9 fuelsystem
#Converting "fuelsystem" into dummies . 
dummy1 <- data.frame(model.matrix( ~fuelsystem, data = dataset_1))

#check the dummy_1 data frame.
View(dummy1)
#This column should be removed from the newly created dummy1 dataframe 
#containing the dummy values for the variable "fuelsystem". 
dummy1 <- dummy1[,-1]

View(dummy1)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "fuelsystem" column
dataset_1 <- cbind(dataset_1[,-11], dummy1)
View(dataset_1)


################################################################################
# 10 car_company
#Converting "fuelsystem" into dummies . 
dummy1 <- data.frame(model.matrix( ~car_company, data = dataset_1))

#check the dummy_1 data frame.
View(dummy1)
#This column should be removed from the newly created dummy1 dataframe 
#containing the dummy values for the variable "car_company". 
dummy1 <- dummy1[,-1]

View(dummy1)

# Combine the dummy variables to the main data set, 
# after removing the original categorical "car_company" column
dataset_1 <- cbind(dataset_1[,-3], dummy1)
View(dataset_1)

##############################################################################

# removing unwanted independent variables
# reson for removing:
# carID wont help in predicting the price
# car model name has to be dropped as per requirements
# symbolling has been taken care by IRiskyYes variable
col_removed<-c(1,2,3)
dataset_2<-dataset_1[-col_removed]
summary(dataset_2)
View(dataset_2)

# Divide into training and test data set
#set the seed to 100

set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(dataset_2), 0.7*nrow(dataset_2))
# generate the train data set
# 143 records
train = dataset_2[trainindices,]

#Similarly store the rest of the observations into an object "test".
# 62 records
test = dataset_2[-trainindices,]

############################################################################

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)



# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step

#removing variables coming with + sign
model_2 <- lm(formula = price ~ carlength + carwidth + curbweight + enginesize + 
                boreratio + stroke + peakrpm + citympg + IsRiskyYes + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model
summary(model_2)

vif(model_2)

#comapring VIF values, curbweight is having high VIF but it i ssignificant as p val<0.05, so look for other insignificant variables
#second highest vif is for enginesize but its p values <0.05 so it cant be removed as it is significant
#car bodysedan is also significant
#removing carlength

model_3 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                boreratio + stroke + peakrpm + citympg + IsRiskyYes + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model

summary(model_3)

vif(model_3)


# curbweight,enginesize,carbodysedan,carbodyhatchback,carwidth,carbodywagon ->signficant
# removing fuelsystemmpfi as it has next high VIF and p val>0.05

model_4 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                boreratio + stroke + peakrpm + citympg + IsRiskyYes + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model
summary(model_4)

vif(model_4)

# drivewheelrwd has high VIF but is significant as its p val <0.05
#stroke has high VIF but is significant as its p val <0.05
# boreratio has next high VIF and is insignificant as its p val >0.05

model_5 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                stroke + peakrpm + citympg + IsRiskyYes + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model
summary(model_5)

vif(model_5)

#curbweight,enginesize,carbodysedan,carbodyhatchback,carwidth,carbodywagon
# citympg high VIF and high P val so insignificant
# removing citympg

model_6 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                stroke + peakrpm  + IsRiskyYes + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model
summary(model_6)

vif(model_6)

#curbweight,enginesize,carbodyhatchback,carbodysedan,carwidth,carbodywagon,drivewheelrwd,stroke
#car_companyhonda,car_companytoyota
# removing fuelsystem2bbl as it is insignificant based on p value

model_7 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                stroke + peakrpm  + IsRiskyYes + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                 car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model
summary(model_7)

vif(model_7)

#curbweight,enginesize,carbodyhatchback,carbodysedan,carwidth,carbodywagon,drivewheelrwd,stroke
#car_companyhonda,car_companytoyota,enginetypeohcf,carbodyhardtop

#removing IsRiskyYes


model_8 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                stroke + peakrpm + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda + car_companymercury + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model
summary(model_8)

vif(model_8)

#removing car_companymercury

model_9 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                stroke + peakrpm + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda  + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model
summary(model_9)

vif(model_9)

#removing cylindernumberfive
model_10 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                stroke + peakrpm + aspiration_type_std + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor  + cylindernumberthree + 
                car_companybmw + car_companybuick + 
                car_companydodge + car_companyhonda + car_companyjaguar + 
                car_companymazda  + car_companymitsubishi + 
                car_companynissan + car_companyplymouth + car_companyrenault + 
                car_companysaab + car_companytoyota + car_companyvolkswagen, 
              data = train)

# Let us look at the summary of the model
summary(model_10)

vif(model_10)

#removing car_companysaab based on p value

model_11 <- lm(formula = price ~  carwidth + curbweight + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor  + cylindernumberthree + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                  car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_11)

vif(model_11)

#removing curbweight

model_12 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginelocation_rear + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor  + cylindernumberthree + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_12)

vif(model_12)

#removing enginetypel


model_13 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginelocation_rear + enginetypedohcv  + 
                 enginetypeohcf + enginetyperotor  + cylindernumberthree + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_13)

vif(model_13)

#remocing cylindernumberthree
model_14 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginelocation_rear + enginetypedohcv  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_14)

vif(model_14)

#removing carbodywagon

model_15 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + 
                 drivewheelrwd + enginelocation_rear + enginetypedohcv  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_15)

vif(model_15)

#removing carbodysedan

model_16 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 carbodyhardtop + carbodyhatchback  + 
                 drivewheelrwd + enginelocation_rear + enginetypedohcv  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_16)

vif(model_16)

#removing carbodyhardtop

model_17 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                  carbodyhatchback  + 
                 drivewheelrwd + enginelocation_rear + enginetypedohcv  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_17)

vif(model_17)

#removing carbodyhatchback

model_18 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear + enginetypedohcv  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_18)

vif(model_18)

#removing enginetypedohcv

model_19 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota + car_companyvolkswagen, 
               data = train)

# Let us look at the summary of the model
summary(model_19)

vif(model_19)

#removing car_companyvolkswagen

model_20 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge + car_companyhonda + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota , 
               data = train)

# Let us look at the summary of the model
summary(model_20)

vif(model_20)

#removing car_companyhonda

model_21 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge  + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth + car_companyrenault + 
                 car_companytoyota , 
               data = train)

# Let us look at the summary of the model
summary(model_21)

vif(model_21)

#removign car_companyrenault

model_22 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge  + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth  + 
                 car_companytoyota , 
               data = train)

# Let us look at the summary of the model
summary(model_22)

vif(model_22)

#removing car_companytoyota

model_23 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge  + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                 car_companynissan + car_companyplymouth , 
               data = train)

# Let us look at the summary of the model
summary(model_23)

vif(model_23)

#removing car_companynissan

model_24 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge  + car_companyjaguar + 
                 car_companymazda  + car_companymitsubishi + 
                  car_companyplymouth , 
               data = train)

# Let us look at the summary of the model

summary(model_24)

vif(model_24)
#removing car_companymazda

model_25 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge  + car_companyjaguar + 
                  car_companymitsubishi + 
                 car_companyplymouth , 
               data = train)

# Let us look at the summary of the model
summary(model_25)

vif(model_25)

#removing car_companyplymouth

model_26 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companydodge  + car_companyjaguar + 
                 car_companymitsubishi  , 
               data = train)

# Let us look at the summary of the model
summary(model_26)

vif(model_26)

#removing car_companydodge

model_27 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 drivewheelrwd + enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                  car_companyjaguar + 
                 car_companymitsubishi  , 
               data = train)

# Let us look at the summary of the model
summary(model_27)

vif(model_27)

#removing drivewheelrwd

model_28 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                  enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companyjaguar + 
                 car_companymitsubishi  , 
               data = train)

# Let us look at the summary of the model
summary(model_28)

vif(model_28)

#removign car_companymitsubishi

model_29 <- lm(formula = price ~  carwidth  + enginesize + 
                 stroke + peakrpm + aspiration_type_std + 
                 enginelocation_rear  + 
                 enginetypeohcf + enginetyperotor  + 
                 car_companybmw + car_companybuick + 
                 car_companyjaguar  , 
               data = train)

# Let us look at the summary of the model

summary(model_29)

vif(model_29)
#adjusted R square (train) approx 95%
#seeing the plot
plot(model_29)

################################################################
#finally left with variables having less p value and less VIF implying 
# they are significant and no collinearality 

###########################TESTING###############################
# Predict the car prices in the testing dataset using the final model so reached
View(test)

Predict_1 <- predict(model_29,test[,-14])
test$predicted_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$predicted_price)
# calculate R squared by squaring correlation
rsquared <- r^2

# check test R-square
# adj R square (test) approx 84%
rsquared

#error
test$error<-test$price-test$predicted_price
test$error<-test$error^2

#plotting error to check if they are uniform/random?
plot(test$error)
#errors are randomly distributed

#############################INTERPRETATION#############################################################

########################################################################################################
#From the model reached it can be concluded that in US market, prices of car largely depends on:       #
#1) the brand/company being jaguar/bmw/buick, which means US customers go by the company name i.e we   #
# can keep higher price if the brand is famous                                                         #
#2) secondly, engine type being ohcf or rotor out of which ohcf affects the car price negatively       #
#3) engine location being rear                                                                         #
#4) aspiration type used being STD                                                                     #
#5) peakrpm, engine size and car width have                                                            #
#6) also stroke has negative impact on car price meaning as its coefficient is coming as negative, it  #
# will have inverse impact on car price                                                                #
########################################################################################################

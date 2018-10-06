#**********************************************************************************************************
# EDA CASE STUDY  - DATA ANALYSIS                                                                         *
# This is a group case study                                                                              *
#                                                                                                         *
# Members:                                                                                                *
# 1.Anargha Biswas                                                                                        *     
# 2.Prasasthy .K.B                                                                                        *
# 3.Sekhar Sahu                                                                                           *
# 4.Surabhi Varshney                                                                                      *
#                                                                                                         *
# Brief on subject:                                                                                       * 
# The consumer finance company specializes in different types of loans to the urban customer.             *
# The major problem they face is when the customer fails to replay the loan .                             *
#                                                                                                         *
# Business objective:                                                                                     *
# The aim of this analysis is to find the trend of borrowers who are likely to default and non-default    *                                                                                   
#                                                                                                         *
# Input files used:                                                                                       *
#                                                                                                         *
# 1. Loan.csv  - Provides details of past loan applicants and whether are defaulters or not.              *
#                Id is unique LC assigned ID for the loan listing.                                        *
#                member_id	is a unique LC assigned Id for the borrower member.                           * 
#-------------------------------------------------------------------------------------------------------- *
#               Attribute     |     Description                                                           *
#-------------------------------------------------------------------------------------------------------- *
#           annual_inc	        The self-reported annual income provided by the borrower during           *
#							                	registration.                                                             *
#           dti                 A ratio calculated using the borrowers total monthly debt payments on     *
#							                	the total debt obligations, excluding mortgage and the requested LC loan, *
#							                 	divided by the borrowers self-reported monthly income.                    *
#           earliest_cr_line  	The month the borrower's earliest reported credit line was opened         *
#           emp_length	        Employment length in years. Possible values are between 0 and 10 where 0  *
#                    		      	means less than one year and 10 means ten or more years.                  *
#           grade	              LC assigned loan grade                                                    *
#           home_ownership	    The home ownership status provided by the borrower during registration.   *
#			                          Our values are: RENT, OWN, MORTGAGE, OTHER.                               *
#           int_rate	          Interest Rate on the loan                                                 *
#           issue_d	            The month which the loan was funded                                       *
#           loan_amnt	          The listed amount of the loan applied for by the borrower. If at some     *
#			                          point in time, the credit department reduces the loan amount, then it will*
#								                be reflected in this value.                                               *
#           loan_status	        Current status of the loan                                                *
#           purpose	            A category provided by the borrower for the loan request.                 *
#           sub_grade	          LC assigned loan subgrade                                                 *
#           term	              The number of payments on the loan. Values are in months and can be either*
#                    			      36 or 60.                                                                 *
#           verification_status	Indicates if income was verified by LC, not verified, or if the income    *
#			                          source was verified                                                       *
#           addr_state	        The state provided by the borrower in the loan application                *
#                                                                                                         *
#---------------------------------------------------------------------------------------------------------*

library(ggplot2)            #for plotting
library(dplyr)              #for data formatting
library(treemap)            #for plotting
library(plotly)             #for plotting
library(stringr)            #for string manipulation
library(corrplot)           #for corelation matrix 
library(choroplethr)        #for goegraphical plots
library(choroplethrMaps)    #for goegraphical plots
library(tidyr)              #for data formatting
library(lubridate)          # to parse the date

#readint the input loan data file
loan_data <- read.csv("loan.csv") # Data provided for years - 2007, 2008, 2009, 2010, 2011
#checking the structure of the data set

str(loan_data)

#subsetting only useful data as these column contain majority of NA values
loan_sample <- subset(loan_data, select = c(1:17,19:35, 36:50, 79, 106, 107))

# further removing columns

drop<-c(11,18,19,21,22,28,29,32,35:37,44:50,52)

loan_sample <- loan_sample[, -drop]

#Data cleaning for useful fields 

#Check for duplicate records
sum(duplicated(loan_sample$id)) #Return 0

#check for NA values 
sapply(loan_sample, function(x) length(which(is.na(x)))) #pub_rec_bankruptcies field is having 697 records with
                                                         #NA's which is ignored for analysis

#Employee experience field modifications. There 1075 records have NA's, those are replaced it as "unkown"
sum(str_detect( loan_sample$emp_length, "n/a")) #Returns 1075

#putting <1 experinece as 0 and 10+ experinec as 11 as suggested in data dictionary
loan_sample$emp_length <- str_replace_all(loan_sample$emp_length, "< 1 year", "0 years")
loan_sample$emp_length <- str_replace_all(loan_sample$emp_length, "1 year", "1 years")
loan_sample$emp_length <- str_replace_all(loan_sample$emp_length, "[+]", "")
loan_sample$emp_length <- str_replace_all(loan_sample$emp_length, "[ years]", "")
loan_sample$emp_length <- str_replace(loan_sample$emp_length, "n/", "unknown")
loan_sample$emp_length <- as.factor(loan_sample$emp_length)

#check for blank values "" in any of the fields of dataset
sapply(loan_sample, function(x) length(which(x == ""))) #No blanks in useful fields

#blanks in revol_util field is replaced with zero 
loan_sample$revol_util <- as.numeric(loan_sample$revol_util)
loan_sample$revol_util[loan_sample$revol_util == ""] <- 0

#date conversion for issue_d & earliest_cr_line fields
loan_sample$issue_d <- as.character(loan_sample$issue_d)
loan_sample$issue_date <- paste(loan_sample$issue_d, "-01", sep = "")
loan_sample$issue_date <- parse_date_time(loan_sample$issue_date, "myd")

loan_sample$earliest_cr_line <- as.character(loan_sample$earliest_cr_line)
loan_sample$earliest_cr_linedate <- paste(loan_sample$earliest_cr_line, "-01", sep = "")
loan_sample$earliest_cr_linedate <- parse_date_time(loan_sample$earliest_cr_linedate, "myd", tz= "")

#Removing "%" sign from int_rate field
loan_sample$int_rate <- as.numeric(gsub('%', '', loan_sample$int_rate))

#Changing into factor fields for analysis purpose
loan_sample$open_acc  <- as.factor(loan_sample$open_acc)
loan_sample$total_acc <- as.factor(loan_sample$total_acc)
loan_sample$pub_rec   <- as.factor(loan_sample$pub_rec)
loan_sample$pub_rec_bankruptcies <- as.factor(loan_sample$pub_rec_bankruptcies)

#check for categorical fields for upper case & lower case irregularities
summary(loan_sample$loan_status) 
summary(loan_sample$verification_status)
summary(loan_sample$purpose)
summary(loan_sample$grade)
summary(loan_sample$addr_state)
summary(loan_sample$emp_length)
summary(loan_sample$home_ownership) #None value removed as only 3 records
summary(loan_sample$pub_rec)
summary(loan_sample$pub_rec_bankruptcies) #NA's are ignored

#fetching year from issue_date
loan_sample$issue_yr  <- year(loan_sample$issue_date)

#Creating defaulters & non-defaulters dataset
loan_charged_off <- subset(loan_sample, loan_status == "Charged Off")
loan_non_defaulters <- subset(loan_sample, loan_status != "Charged Off")

#defaulters dataset
#checking count in each loan_status
table(loan_sample$loan_status)

#######################################################################################################################
#Overall plotting of data on the basis of status of the loan
#######################################################################################################################
loan_over_all <- loan_sample %>% group_by(loan_status) %>% summarise(count1 = n())
loan_over_all$count1 <- 100 * loan_over_all$count1/nrow(loan_sample)
loan_over_all$count2 <- str_c(round(loan_over_all$count1,2),"%")
Plot_over_all <- ggplot(loan_over_all,aes(x=loan_status,y=count1,fill=loan_status)) + geom_bar(stat="identity") +
                 geom_text(aes(label=count2),vjust = 2)
Plot_over_all
#Conclusion : 14.17% of loans are defualted (years - 2007, 2008, 2009, 2010, 2011)

######################################################################################################################
#Distribution of loan amounts by status
######################################################################################################################
plot_amnt_status <- ggplot(loan_sample, aes(loan_status, loan_amnt)) +geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +labs(list(title = "Loan amount by status",x = "Status",y = "Amount")) 
plot_amnt_status
#Conclusion : Loan amount sanctioned is slightly higher for defaulted or charged off loans compared to fully paid

######################################################################################################################
#Classifying into long term and short term
#####################################################################################################################
plot_term1 <- loan_sample %>% count(term, loan_status) %>% plot_ly(x = ~term, y = ~n, color = ~loan_status,
                                                                   colors = c("tomato", "deepskyblue2", "darkgoldenrod1"), type = "bar", text = ~n, textposition = 'auto',
                                                                   textfont = list(color = '#000000', size = 14))  %>% 
  layout(title = "Long & Short term for all",yaxis = list(title = 'Count'))
plot_term1

plot_term2 <- loan_charged_off %>% count(term) %>% plot_ly(x = ~term, y = ~n, color = ~term, colors = c("tomato"),
                                                           type = "bar", text = ~n,textposition = 'auto', textfont = list(color = '#000000', size = 12)) %>%
  layout(title = "Long & Short term for defaulted",yaxis = list(title = 'Count'),
         xaxis = list(title = "terms of charged off(defaulters)"))

plot_term2

#putting both plots in single frame
subplot(plot_term1, plot_term2) %>% layout(title = "Long & Short term") 

#Conclusion : short term loans are more defaulted and currently there are no short term loans

#Ratio of defaulters with respect to total loans sanctioned for different terms

loan_term_ratio <- loan_sample %>% group_by(term) %>% summarise(countk = n())
loan_term_charged_off <- loan_charged_off %>% group_by(term) %>% summarise(countl = n())
loan_term_ratio$countk <- 100 * loan_term_charged_off$countl/loan_term_ratio$countk
loan_term_ratio$counti <- str_c(round(loan_term_ratio$countk,2),"%")
Plot_term_ratio <- ggplot(loan_term_ratio,aes(x=term,y=countk, fill= term)) + geom_bar(stat="identity") +
  geom_text(aes(label=counti), vjust = 2, hjust = 1.3) + 
  labs(list(title = "Loan defulters ratio with respect to terms ",
            x = "Terms",y = "Percentage distribution"))
Plot_term_ratio
#Conclusion: Though the number of defaulters are high for 36 month term if we consider rate of defaulting then 60 month terms 
#are almost double. (36 month = 12% & 60 month = 23%)

#######################################################################################################################
#Classifying in terms of grades
#######################################################################################################################
plot_grade1 <- loan_sample %>% count(grade, loan_status) %>% plot_ly(x = ~grade, y = ~n, color = ~loan_status, type = "bar", text = ~n,
                                                      textposition = 'auto', textfont = list(color = '#000000', size = 12)) %>% 
  layout(title = "Classifying in terms of grades",yaxis = list(title = 'Count'))
plot_grade1

#Checking the grade of loan which is more defaulted
Plot_grade2 <- loan_charged_off %>% count(grade) %>% plot_ly(x = ~grade, y = ~n, color = ~grade, type = "bar", text = ~n,
                                              textposition = 'auto', textfont = list(color = '#000000', size = 12))%>% 
  layout(title = "Classifying in terms of grades for defaulted loans",yaxis = list(title = 'Count'))
Plot_grade2
#Checking the sub-grade of loan which is more defaulted
 plot_grade3 <- loan_charged_off %>% count(sub_grade) %>% plot_ly(x = ~sub_grade, y = ~n, color = ~sub_grade, type = "bar", text = ~n,
                                                  textposition = 'auto', textfont = list(color = '#000000', size = 12))%>% 
  layout(title = "Classifying in terms of sub-grades for defaulted loans",yaxis = list(title = 'Count'))
 plot_grade3
#Conclusion : MOre defaulters in lower grades B, C, & D
#Though the loan volume is more for low grade - short term loans, the defaulters rate is high for high grade -long term, which is an indication of risk for high grade -long #term loans. 
#####################################################################################################################
# Grade, term & volume of loan in years ( Year extracted from issue_d- issue date)
#####################################################################################################################
 
# Compare loans with different terms

loan_vol_year <- loan_sample %>% select(issue_yr, grade, term)
loan_vol_year <- loan_vol_year[complete.cases(loan_vol_year), ]

#Loan volume by year
plot_vol_year1 <- ggplot(loan_vol_year, aes(x = issue_yr)) + geom_bar(aes(fill = grade)) + 
  labs(title = "Loan Volume by Year", x = "Issued Year", y = "Volume") + theme_bw()
plot_vol_year1

#Conclusion :
# volume of loan increases with year and more loans are allotted for lower grades A, B, C.

# loan volume by year for short & long term (36 months & 60 months)
plot_vol_year2 <- ggplot(loan_vol_year, aes(x = issue_yr)) + geom_bar(aes(fill = grade)) + facet_grid(~term) +
                 labs(title = "Loan Volume by Year", x = "Issued Year", y = "Volume") + theme_bw()
plot_vol_year2

#Conclusion

# the number of 36-month loans is greater than 60-month loans. In addition,
# the proportion of loans with different grades varies with terms. As for 36-month loans, the majority are 
# in grade A, B, and C. As for 60-month loans, only a small percentage of loans are in grade A, and most of loans are in
# grade B, C, D, and E.

#########################################################################################################################
#Considering dafault rate in each grade in terms of year. Default rate is nothing but number of defaulted borrowers with 
#respected to sanctioned loans of each grade in terms of year.
#########################################################################################################################
calc_default_val_cnt <- function(samp1){
  samp_d1 <- samp1 %>% filter(loan_status == "Charged Off") %>% group_by(grade) %>% summarise(default_count = n())
  samp_d2 <- samp1 %>% group_by(grade) %>% summarise(count = n(),int_rate=mean(int_rate))
  samp_d2 %>% left_join(samp_d1) %>% mutate(default_rate = 100*default_count/count) %>% select(grade,count,default_count,int_rate,default_rate)
}

calc_default_rate1 <- calc_default_val_cnt(loan_sample %>% filter(issue_yr==2011))
calc_default_rate1$year <- 2011

calc_default_rate2 <- calc_default_val_cnt(loan_sample%>% filter(issue_yr==2010))
calc_default_rate2$year <- 2010

calc_default_rate3 <- calc_default_val_cnt(loan_sample %>% filter(issue_yr==2009))
calc_default_rate3$year <- 2009

calc_default_rate4 <- calc_default_val_cnt(loan_sample %>% filter(issue_yr==2008))
calc_default_rate4$year <- 2008

calc_default_rate5 <- calc_default_val_cnt(loan_sample %>% filter(issue_yr==2007))
calc_default_rate5$year <- 2007

calc_default_rate <- rbind(calc_default_rate1, calc_default_rate2, calc_default_rate3, calc_default_rate4, calc_default_rate5)

plot_default_rate <- ggplot(calc_default_rate, aes(x=as.factor(year), y=default_rate,fill= grade)) + geom_bar(stat="identity",position="dodge") +
  ggtitle("Default rate with respect to grade per year(%)")

plot_default_rate

#Conclusion:
#Though the volume of loan sanctioned for lower grade is high, the default rate of higher grade loans are 
#much higher than
#lower grade which can be a major factor for loss.

###########################################################################################################################
#Classifying in terms of employees experience level
##########################################################################################################################
plot_exp1 <- loan_sample %>% count(emp_length) %>% plot_ly(x = ~emp_length, y = ~n, color = ~emp_length, type = "bar", 
 text = ~n, textposition = 'auto', textfont = list(color = '#000000', size = 12)) %>% 
  layout(
    title = "Classifying in terms of employees experience level",
      xaxis = list(title = "Employees experience level"),
      yaxis = list(title = "Count of loan applicants")
    )
  
plot_exp1
#Conclusion: Employee of lower experience level and employees having 10+ experience are more tend to take up loans.

#Checking for defaulted
plot_exp2 <- loan_charged_off %>% count(emp_length) %>% plot_ly(x = ~emp_length, y = ~n, color = ~emp_length, 
             type = "bar", text = ~n, textposition = 'auto', textfont = list(color = '#000000', size = 12)) %>%
  layout(
    title = "Employees experience level for defaulters",
    xaxis = list(title = "Employees experience level"),
    yaxis = list(title = "Count of defaulter loan applicants"))

plot_exp2
#Conclusion: Employees having 10+ years of experience are much more compared to others
######################################################################################################################
#How Home_ownership effects loan volume & defaulters- 
# NOTE: Ownership of "NONE" "OTHER" are excluded as data is comparitively less
######################################################################################################################
loan_samp_ownership <- loan_sample[loan_sample$home_ownership %in% c("MORTGAGE", "OWN", "RENT"), ] 

plot_own1 <-  ggplot(loan_samp_ownership, aes(grade)) + geom_bar(aes(fill = grade)) + facet_wrap(~home_ownership) + labs(x = "Grade", 
                y = "Number of Loans", title = "Issued Loans of Different Home Ownership") + theme_bw()
plot_own1
#Conclusion: 
#Loan volume is high for "MORTGAGE" &  "RENT categories, as the financial stability is less compared to those "OWN" 
 
#Checking for defualters or charged off
loan_charged_off_ownership <- loan_charged_off[loan_charged_off$home_ownership %in% c("MORTGAGE", "OWN", "RENT"), ]

plot_own2<-ggplot(loan_charged_off_ownership, aes(home_ownership)) + geom_bar(aes(fill = home_ownership)) + labs(x = "home ownership", 
                                                                                                           y = "Number of defaulter Loans", title = "Employees home ownership for defaulters") + theme_bw()
plot_own2
#Conclusion: 
#Defaulters are more for "MORTGAGE" &  "RENT" categories compared to those "OWN" 
 
########################################################################################################################
#Annual income Vs loan status
########################################################################################################################
loan_sample %>% count(annual_inc, loan_status) %>% 
  plot_ly( x = ~loan_status, y = ~annual_inc,  color = ~loan_status, type = "bar")%>%
            layout(
              title = "Annual income Vs loan status",
              xaxis = list(title = "Loan status"),
              yaxis = list(title = "Employees annual income"))

#Conclusion: defaulters have much less annual income compared to fully
#paid borrowers.

########################################################################################################################
# Percentage of verified loans Vs non verfied loans (verification of annual income of applicant)
#######################################################################################################################
loan_verified        <- loan_sample %>% group_by(verification_status) %>% summarise(count3 = n())
loan_verified$count3 <- 100 * loan_verified$count3/nrow(loan_sample)
loan_verified$count4 <- str_c(round(loan_verified$count3,2),"%")
Plot_verified1       <- ggplot(loan_verified,aes(x=verification_status,y=count3,fill=verification_status)) +
                        geom_bar(stat="identity") + geom_text(aes(label=count4),vjust = 2)
Plot_verified1

#Conclusion: Almost 43% of loan are sanctioned without verifying the annual income, which is a considerable amount of risk

#Checking percentage in defaulters
loan_verified_default        <- loan_charged_off %>% group_by(verification_status) %>% summarise(count5 = n())
loan_verified_default$count5 <- 100 * loan_verified_default$count5/nrow(loan_charged_off)
loan_verified_default$count6 <- str_c(round(loan_verified_default$count5,2),"%")
Plot_verified2               <- ggplot(loan_verified_default,aes(x=verification_status,y=count5,fill=verification_status)) +
                                geom_bar(stat="identity") + geom_text(aes(label=count6),vjust = 2)
Plot_verified2
#Conclusion: Out of defualters 38% were sanctioned without of verification of annual income of applicant

##########################################################################################################################
# Influence of purpose of loan on defaulters volume: WHy PEOPLE BORROW?
##########################################################################################################################
loan_purpose <- loan_sample %>% select(purpose, loan_amnt) %>% na.omit() %>% group_by(purpose) %>% 
  dplyr::summarise(volume = n(), average_amnt = sum(as.numeric(loan_amnt), 
                                                    rm.na = TRUE)/n())

loan_purpose <- loan_purpose[!loan_purpose$purpose == "", ]

treemap(loan_purpose, index = "purpose", vSize = "volume", vColor = "average_amnt", 
        range = c(6000, 16000), type = "manual", palette = c("yellow", "green", 
        "orange", "orange2", "firebrick"), algorithm = "pivotSize", sortID = "-size", 
        title = "Purposes of Loans", title.legend = "Avg_Amnt", fontfamily.labels = "comic sans", 
        fontsize.labels = 16, fontsize.legend = 10, fontface.labels = 1, position.legend = "bottom", 
        force.print.labels = T, border.col = "white")

#conclusion: Debt consolidation and credit card are the most popular reasons for borrowing.The different color is 
#related to the average amount of a loan. Loans for debt consolidation, credit card, house, and small business usually
#have higher average amount than other purposes.

loan_purpose_default <- loan_charged_off %>% select(purpose, loan_amnt) %>% na.omit() %>% group_by(purpose) %>% 
  dplyr::summarise(volume = n(), average_amnt = sum(as.numeric(loan_amnt), 
                                                    rm.na = TRUE)/n())

loan_purpose_default <- loan_purpose_default[!loan_purpose_default$purpose == "", ]

treemap(loan_purpose_default, index = "purpose", vSize = "volume", vColor = "average_amnt", 
        range = c(6000, 16000), type = "manual", palette = c("yellow", "green", 
         "orange", "orange2", "firebrick"), algorithm = "pivotSize", sortID = "-size", 
        title = "Purposes of Loans- Defaulted cases", title.legend = "Avg_Amnt", fontfamily.labels = "comic sans", 
        fontsize.labels = 16, fontsize.legend = 10, fontface.labels = 1, position.legend = "bottom", 
        force.print.labels = T, border.col = "white")

#conclusion: Higher volume of loans with purpose is in debt_consolidation whereas higher amount of loan is in small_business
#Debt consolidation is the most common reason for borrowing among defaulters also. As the interest rate is less consumer
#tend to borrow more to pay off their debts.
#There are three variables in the above tree map: purposes, average amount of a loan, and the total 
#volume of loans. It gives an overall view of the relationship between purposes of loans and the volume and amount 
#of loans.The various sizes in the tree map are directly proportional to the volume of loans with different purposes.

##########################################################################################################################
# Analysis on open credit lines open_acc & total credit total_acc lines for the total sample & 
# defaulted borrowers
##########################################################################################################################
loan_charged_off %>% count(open_acc) %>% plot_ly(x = ~open_acc, y = ~n, color = ~open_acc, type = "bar", mode = 'lines', 
                                                 text = ~n, textposition = 'auto', textfont = list(color = '#000000', size = 14)) %>% 
  layout(title = "Loan volume Vs open credit lines available for borrower",
         xaxis = list(title = 'Number of open credit lines available for borrower'),
         yaixs= list(title='volume'))

#Conclusion: As we could see, the loan volume decreases as the number of open credit line increases after 
#open credit line count of 10 which is as expected. The presence of more open creditlines is correlated to higher stability
#in financial status. Over all credit lines have some impact on loan volume and defualters

loan_sample %>% count(total_acc, loan_status) %>% plot_ly(x = ~total_acc, y = ~n, color = ~loan_status, type = "bar",
                                                          text = ~n, textposition = 'auto', textfont = list(color = '#000000', size = 14)) %>% 
  layout(title = "loan volume & total credit lines in terms of loan status",
         xaxis = list(title = 'Number of credit lines available for borrower'))

#Conclusion: As we could see there are more loans taken and defaulted by borrowers with lower credit lines. Though the
#volume is much high for fully paid borrowers, in case of defualted borrowers the proportionality of volume is high in
#lower credit lines. 

##########################################################################################################################
#DTI (Debt To Income ratio):
#The lower DTI is considered better.A DTI of 10 means, the debt payment excluding mortgage, are only 10% of the
#gross income
#########################################################################################################################
plot_DTI <- plot_ly(loan_sample, x = ~grade, y = ~dti, color = ~ grade, type = "box") %>% 
            layout(title = "DTI Vs. Grades",yaxis = list(title = 'DTI(Debit to income ratio)'))

plot_DTI
ggplot(loan_sample, aes(dti, fill = loan_status, color = loan_status)) + geom_histogram(position = "dodge", binwidth = 5)

#Conclusion/Observation:

# The plot shows an expected trend, does not seem to show an strong dependency. Overall DTI has some impact on 
# charge off probabilities
#############################################################################################################################
#Analyzing the effect of revolving line utilization rate on loan volume with respect to purpose of loan
#revol_util :Revolving line utilization rate, or the amount of credit the borrower is using relative to all available
#revolving credit.
#############################################################################################################################
loan_sample$revol_util <- as.numeric(gsub('%', '', loan_sample$revol_util))

plot_ly(loan_sample, x = ~revol_util, color = ~purpose,type = "histogram")

#Conclusion: We could see at 0 and 5 of revolving utilization rate, loan volume is high for home improvement. Further
#as the revolving utilization rate increases,the loan volume is high for debt consolidation. 

plot_ly(loan_charged_off, x = ~revol_util, color = ~purpose,type = "histogram")

#Conclusion: In the case of defaulters, loan volume is high for debit consolidation. This shows if revolving utilization
#rate is high and borrower is lending money for debit consolidation there is high chance of defaulting
##########################################################################################
# geographical distribution of loans
###########Created function to convert state code to state name###
#'x' is the column of a data.frame that holds 2 digit state codes

stateLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

#Created a new column region 
loan_sample$region<-stateLower(loan_sample$addr_state)
locVol_df <- select(loan_sample, region)
locVol_df[2,]

locVol_df <- locVol_df %>% na.omit() %>% group_by(region) %>% dplyr::summarise(value = n())


locAmt_df <- select(loan_sample, region, loan_amnt)
locAmt_df$loan_amnt <- as.numeric(locAmt_df$loan_amnt)  # Integer overflow: +/-2*10^9
locAmt_df <- locAmt_df %>% na.omit() %>% group_by(region) %>% dplyr::summarise(value = sum(loan_amnt, 
                                                                                           na.rm = TRUE))
# Detailed parameter preparation of functions are hidden due to limited
# space Draw two maps
# NOTE:one of the states North Dakota is not present in the data file given so it is fetched as NA
# volume of loans
state_choropleth(locVol_df, title = "Loan Volume by State", num_colors = 9)

# amount of loans
state_choropleth(locAmt_df, title = "Loan Amount by State", num_colors = 9)
#conclusion:
#From a geographical perspective, California, Texas, New York, Florida, 
#and Illinois have the largest dollar amounts and volumes of loans. 

###########################################################################################################################
#Number of derogatory public records of borrower and number of public record bankruptcies (pub_rec & pub_rec_bankruptcies) :
#A derogatory item is an entry that may be considered negative by lenders because it indicates risk and hurts the 
#ability to qualify for credit or other services. Public records and collections are derogatory items because they all 
#reflect financial obligations that were not paid as agreed.There are three kinds of public records that appear on 
#a credit report: bankruptcy filings, tax liens, and judgments
#
# In our dataset tax_liens record of borrower are zero and there are no judgement records so we have to consider only 
# bankruptcies.
##########################################################################################################################
loan_pub_rec_non_defaulted <- loan_non_defaulters %>% group_by(pub_rec) %>% summarise(countc = n())
loan_pub_rec_non_defaulted$countc <- 100 * loan_pub_rec_non_defaulted$countc/nrow(loan_non_defaulters)
loan_pub_rec_non_defaulted$countd <- str_c(round(loan_pub_rec_non_defaulted$countc,2),"%")
Plot_pub_rec_non_defaulted <- ggplot(loan_pub_rec_non_defaulted,aes(x=pub_rec,y=countc,fill=pub_rec)) + geom_bar(stat="identity") +
  geom_text(aes(label=countd),vjust = 2) + 
  labs(list(title = "Percentage of detrogatory public records of non-defualters ",
            x = "Count of records",y = "Percentage distribution"))

Plot_pub_rec_non_defaulted

loan_pub_rec_defaulted <- loan_charged_off %>% group_by(pub_rec) %>% summarise(counta = n())
loan_pub_rec_defaulted$counta <- 100 * loan_pub_rec_defaulted$counta/nrow(loan_charged_off)
loan_pub_rec_defaulted$countb <- str_c(round(loan_pub_rec_defaulted$counta,2),"%")
Plot_pub_rec_defaulted <- ggplot(loan_pub_rec_defaulted,aes(x=pub_rec,y=counta,fill=pub_rec)) + geom_bar(stat="identity") +
                          geom_text(aes(label=countb),vjust = 2) + 
                          labs(list(title = "Percentage of detrogatory public records of defualters ",
                                        x = "Count of records",y = "Percentage distribution"))
Plot_pub_rec_defaulted

#ignoring NA values in pub_rec_bankruptcies asassumed while data cleaning in the begining
loan_non_defaulters1<-na.omit(loan_non_defaulters)


loan_pub_bank_rec_non_defaulted <- loan_non_defaulters1 %>% group_by(pub_rec_bankruptcies) %>% summarise(counte = n())

loan_pub_bank_rec_non_defaulted$counte <- 100 * loan_pub_bank_rec_non_defaulted$counte/nrow(loan_non_defaulters1)

loan_pub_bank_rec_non_defaulted$countf <- str_c(round(loan_pub_bank_rec_non_defaulted$counte,2),"%")
Plot_pub_bank_rec_non_defaulted <- ggplot(loan_pub_bank_rec_non_defaulted,aes(x=pub_rec_bankruptcies,y=counte,fill=pub_rec_bankruptcies)) + geom_bar(stat="identity") +
  geom_text(aes(label=countf),vjust = 2) + 
  labs(list(title = "Percentage of bankruptcy public records of non-defaulters ",
            x = "Count of records",y = "Percentage distribution"))

Plot_pub_bank_rec_non_defaulted


#ignoring NA values in pub_rec_bankruptcies asassumed while data cleaning in the begining
loan_charged_off1<-na.omit(loan_charged_off)

loan_pub_bank_rec_defaulted <- loan_charged_off1 %>% group_by(pub_rec_bankruptcies) %>% summarise(countg = n())
loan_pub_bank_rec_defaulted$countg <- 100 * loan_pub_bank_rec_defaulted$countg/nrow(loan_charged_off1)
loan_pub_bank_rec_defaulted$counth <- str_c(round(loan_pub_bank_rec_defaulted$countg,2),"%")
Plot_pub_bank_rec_defaulted <- ggplot(loan_pub_bank_rec_defaulted,aes(x= pub_rec_bankruptcies,y = countg,fill=pub_rec_bankruptcies)) + geom_bar(stat="identity") +
  geom_text(aes(label=counth),vjust = 2) + 
  labs(list(title = "Percentage of bankruptcy public records of defaulters ",
            x = "Count of records",y = "Percentage distribution"))
Plot_pub_bank_rec_defaulted

#Conclusion/Observation:
#Both Detrogatory reccords and bankruptcy records are showing similar trends. Around 4% of non-defaulted borrowers have public records
#of atleast one and defaulter have around 8% which is kind of double. Similarly for bankruptcy records non-defaulters have around
#3.8% and defaulters have 6.5%. Since public record is an important factor of risk, it need to considered while doing 
#predictive risk analysis
##########################################################################################################################
#Correlation matrix between numeric fields of loan data
##########################################################################################################################
cor_var_name = c("loan_amnt", "int_rate", "installment", "sub_grade", "annual_inc", 
                 "issue_date", "dti","earliest_cr_linedate", "open_acc", "total_acc", "total_pymnt", 
                 "total_rec_prncp", "total_rec_int")

cor_var <- select(loan_sample, one_of(cor_var_name))
cor_var <- cor_var[complete.cases(cor_var), ]

cor_var$credit_tm <- as.numeric(cor_var$issue_date - cor_var$earliest_cr_linedate)
cor_var$open_acc <- as.numeric(cor_var$open_acc)
cor_var$total_acc <- as.numeric(cor_var$total_acc)
cor_var$num_subgrade <- as.numeric(as.factor(cor_var$sub_grade))
cor_var <- select(cor_var, -sub_grade, -issue_date, -earliest_cr_linedate)  # remove old variables
summary(cor_var)

cor_map <- cor(cor_var)  # transfer to matrix 
corrplot(cor_map, method = "number", title = "Correlation Map", mar=c(0,0,1,0),
         type = "lower", order = "FPC",col = c("red", "orange", "blue", "green"), number.cex = 1.0, tl.cex = 0.8)

#Conclusion:
# high corelation between total principal received till date and Payments received to date and
# high dependency can be seen between interest rate and sub grade of the applicant
# Also, there is a negative dependency between dti and annual income of the loan applicant
##########################################################################################################################
# dti, open_acc, total_acc, pub_rec, pub_rec_bankruptcies : Analysis of these fields are not showing much  
# effect on current dataset. Since these fields are important and contributing towards the credit score of 
# a borrower, the analysis is included. 
##########################################################################################################################
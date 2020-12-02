############*************** Step 1 : IMPORTING DATA

data <- read.csv(file.choose())

############*************** Step 2 : DATA EXPLORATION
dim(data)
# Insight: There are 1999 rows and 18 columns in the dataset.

head(data,10) # Displaying the top 10 rows ?n the dataset.
tail(data,10) # Displaying bottom 10 rows in the dataset.
str(data) # Displays the structure of the data.

table(data$offer_dropped)

# Insight: We can see that the number of people who have dropped the offer are more i.e. 1024 whereas the c?unt for people who have not dropped the offer are 975.

# Percentage of people who has dropped the offer
temp1 <- table(data$offer_dropped)[2]/sum(table(data$offer_dropped))
temp1 <- temp1*100
temp1
# Insight: As per the data, we have 51.22561% of people w?o have dropped the offer.


############*************** Step 3 : UNIVARIATE ANALYSIS

# We will start with categorical variables
# Identify first which all variables are categorical and perform univariate analysis

# Starting with the variable: marital_sta?us
#Visual approach for marital_status

plot(data$marital_status,xlab="marital_status")
#Insight: The bar chart shows married candidates have the highest frequency.

#Metric approach for marital_status
tab_marital <- table(data$marital_status)
tab_marital
?Insight: The count for married candidate is highest i.e. 1389, followed by single candidates i.e 400.

# Variable: educational_level
#Visual approach for educational_level
plot(data$education_level,xlab="educational_level")
#Insight: The bar chart shows As?ociate Degreee candidates have the highest frequency.

#Metric approach for educational_level
tab_education <- table(data$education_level)
tab_education
#Insight: The count for Associate Degreee candidates is highest i.e. 624, followed by candidates with A?sociate Certification i.e. 590.

# Variable: gender
#Visual approach for gender
plot(data$gender,xlab="gender")
#Insight: The bar chart shows "Male" candidates have the highest frequency.

#Metric approach for gender
tab_gender <- table(data$gender)
tab_ge?der 
#Insight: The count of male candidates is highest i.e. 1517 whereas the count for females is 482. 


# Variable: distance_from_home
#Visual approach for distance_from_home
plot(data$distance_from_home,xlab="distance_from_home")
#Insight: The bar chart?shows ">20kms" distance have the highest frequency.

#Metric approach for distance_from_home
tab_distanceHome <- table(data$distance_from_home)
tab_distanceHome
#Insight: The count for ">20kms" distance having the highest frequency is 781 followed by "<15k?s" i.e. 694.

# Variable: sourcing_channel
#Visual approach for sourcing_channel
plot(data$sourcing_channel,xlab="sourcing_channel")
#Insight: The bar chart shows "Job Portals" is the source having the highest frequency.

#Metric approach for sourcing_chan?el
tab_sourcing <- table(data$sourcing_channel)
tab_sourcing
# Insights: the count for "Job Portals" is 486 which is highest followed by the count of "Consultants" i.e. 459.

# Variable: career_growth
#Visual approach for career_growth
plot(data$career_gro?th,xlab="career_growth")
#Insight: The bar chart shows "Lateral" is the career growth having the highest frequency.

#Metric approach for career_growth
tab_careerGrowth <- table(data$career_growth)
tab_careerGrowth
# Insights: The count for "Laterals" bein? the highest i.e. 1286 followed by the count for "Vertical" i.e. 713.

# Variable: flexi_work
#Visual approach for flexi_work
plot(data$flexi_work,xlab="flexi_work")
#Insight: The bar chart shows "Yes" as the option for flexi work having the highest freque?cy.

#Metric approach for flexi_work
tab_flexi <- table(data$flexi_work)
tab_flexi
# The count for "Yes" being the highest i.e. 1177 and the count for "No" is 822.

# Variable: timely_communication
#Visual approach for timely_communication
plot(data$timely?communication,xlab="timely_communication")
#Insight: The bar chart shows "NO" as the option for timely communication having the highest frequency.

#Metric approach for timely_communication
tab_communication <- table(data$timely_communication)
tab_communic?tion
# Insights: The count for "No" is more i.e. 1188 and the count for "Yes" 811.

# Then follow with the numerical variables

# Variable: Age
#Visual approach for Age
hist(data$age)
# Insight: Age of candidates ranges from 25 to 46 years and seems to be ?ormally distributed but is uniform at the moment.

#Metric approach for age
tab_age <- table(data$age)
tab_age
prop.table(tab_age)*100
# Insight: Almost same number of candidates lie in the age group of 25 to 45. Although least number of candidates are of ?ge 46.

# Variable: percent_hike
#Visual approach for percent_hike
hist(data$percent_hike)
# Insight: Hike of candidates ranges from 10% to 40%  and distribution is rightly skewed. So, we can say that most of the candidates are getting percentage hikes bet?een 10-15 percent.

#Metric approach for percent_hike
tab_hike <- table(data$percent_hike)
tab_hike
prop.table(tab_hike)*100
# Insight: Most of the people are getting 10-14% hike and least number of candidates are getting hike between 36-40% which is aroun? 8%.

# Used to get detailed analysis or descriptive statistics for the data but fails for kurtosis and skewness values as it shows it for categorical variable also.
install.packages("psych",dependencies = TRUE)
library(psych)
describe(data) # Gives us the?detailed description of the data including mean, standard deviation, median, min, max, range, skewness and kurtosis for the all the variables.

# Used to get detailed analysis or descriptive statistics for the data but fails for kurtosis and skewness value? as it shows it for categorical variable also.
install.packages("hmisc",dependencies = TRUE)
library(hmisc)
decribe(data)

# Best tool to access decriptive analysis of data
install.packages("summarytools", dependencies = TRUE)
library(summarytools)
dfSumma?y(data) # Summarytools gives us complete description about each variables i.e. Different type of values the variables hold, their frequencies. Also, tells us whether there are any Null values in our data.
# As per the results, none of the variables have an? NULL values. Therefore, there is no need to perform NULL value treatment.

#Check null values
colSums(is.na(data)) # There are no Null values for any of the variables.

#Checking outliers
plot(data$age)
plot(data$marital_status)
plot(data$education_level)?plot(data$gender)
plot(data$percent_hike)
plot(data$distance_from_home)
plot(data$sourcing_channel)
plot(data$total_rounds)
plot(data$date_1st_contact)
plot(data$date_offered)
plot(data$satisfaction_index)
plot(data$no_companies_worked)
plot(data$career_gr?wth)
plot(data$flexi_work)
plot(data$total_experience)
plot(data$timely_communication)
plot(data$offer_dropped)

############*************** CREATE NEW VARIABLE (FEATURE ENGINEERING)

# 1. TIME_TO_OFFER

#data$date_offered <- as.Date(data$date_offered,"%m%?%Y") - as.Date(data$date_1st_contact,"%m%d%Y")
#data$time_to_offer = (data$date_offered) - (data$date_1st_contact)

# JOB_HOPPING_INDEX

data$job_hopping_index = data$total_experience/data$no_companies_worked

# Insights: Removing the old variables from wh?ch the new variables were created.
data$date_1st_contact = NULL 
data$date_offered = NULL
# Insights: Successfully, removed the variables from which new variables were created.

############*************** Step 4 : Bivariate Analysis


# Creating intervals?for few variables:


data_grpd <- data
# Creating percent_hike_grouped to plot stacked column chart of percent_hike_grouped with other variables
data_grpd$percent_hike_grouped <-ifelse(data_grpd$percent_hike<=20,data_grpd$percent_hike_grouped <- "Hike: 11-?0",
                                        ifelse(data_grpd$percent_hike>20 & data_grpd$percent_hike<=30,data_grpd$percent_hike_grouped <- "Hike: 21-30",
                                               data_grpd$percent_hike_grouped <- "Hike: 31-40"))
head?data_grpd$percent_hike_grouped,10)
tail(data_grpd$percent_hike_grouped,10)
data_grpd$percent_hike <- NULL

# Creating satisfaction_index_grouped to plot stacked column chart of satisfaction_index_grouped with other variables
data_grpd$satisfaction_index_gr?uped <-ifelse(data_grpd$satisfaction_index<=30,data_grpd$satisfaction_index_grouped <- "Satisfaction: 15-30",
                                              ifelse(data_grpd$satisfaction_index>30 & data_grpd$satisfaction_index<=45,data_grpd$satisfaction_ind?x_grouped <- "Satisfaction: 30-45",
                                                     ifelse(data_grpd$satisfaction_index>45 & data_grpd$satisfaction_index<=60,data_grpd$satisfaction_index_grouped <- "Satisfaction: 45-60",
                              ?                             ifelse(data_grpd$satisfaction_index>60 & data_grpd$satisfaction_index<=75,data_grpd$satisfaction_index_grouped <- "Satisfaction: 60-75",
                                                                   data_grpd$satisfaction_?ndex_grouped <- "Satisfaction: 75 and Above"))))

head(data_grpd$satisfaction_index_grouped,10)
tail(data_grpd$satisfaction_index_grouped,10)
data_grpd$satisfaction_index <- NULL

# Creating age_grouped to plot stacked column chart of age_grouped with othe? variables
data_grpd$age_grouped <-ifelse(data_grpd$age<=30,data_grpd$age_grouped <- "Age: 25-30",
                               ifelse(data_grpd$age>30 & data_grpd$age<=35,data_grpd$age_grouped <- "Age: 30-35",
                                      ifels?(data_grpd$age>35 & data_grpd$age<=40,data_grpd$age_grouped <- "Age: 35-40",
                                             data_grpd$age_grouped <- "Age: 40 and Above")))

head(data_grpd$age_grouped,10)
tail(data_grpd$age_grouped,10)
data_grpd$age <- NULL

? Creating total_experience_grouped to plot stacked column chart of total_experience_grouped with other variables
data_grpd$total_experience_grouped <-ifelse(data_grpd$total_experience<=3,data_grpd$total_experience_grouped <- "Job Experience: Upto 3 yrs",
 ?                                          ifelse(data_grpd$total_experience>3 & data_grpd$total_experience<=6,data_grpd$total_experience_grouped <- "Job Experience: 3-6 yrs",
                                                   ifelse(data_grpd$total_experie?ce>6 & data_grpd$total_experience<=9,data_grpd$total_experience_grouped <- "Job Experience: 6-9 yrs",
                                                          ifelse(data_grpd$total_experience>9 & data_grpd$total_experience<=12,data_grpd$total_experience_?rouped <- "Job Experience: 9-12 yrs",
                                                                 ifelse(data_grpd$total_experience>12 & data_grpd$total_experience<=15,data_grpd$total_experience_grouped <- "Job Experience: 12-15 yrs",
                ?                                                       ifelse(data_grpd$total_experience>15 & data_grpd$total_experience<=18,data_grpd$total_experience_grouped <- "Job Experience: 15-18 yrs",
                                                                ?              data_grpd$total_experience_grouped <- "Job Experience: Aboe 18 years"))))))

head(data_grpd$total_experience_grouped,10)
tail(data_grpd$total_experience_grouped,10)
data_grpd$total_experience <- NULL

# Creating time_to_offer_grouped to plot ?tacked column chart of time_to_offer_grouped with other variables
data_grpd$time_to_offer_grouped <-ifelse(data_grpd$Time_to_offer<=30,data_grpd$time_to_offer_grouped <- "Time to Offer: Upto 30 days",
                                         ifelse(data_gr?d$Time_to_offer>30 & data_grpd$Time_to_offer<=60,data_grpd$time_to_offer_grouped <- "Time to Offer: 30-60 days",
                                                ifelse(data_grpd$Time_to_offer>60 & data_grpd$Time_to_offer<=90,data_grpd$time_to_offer_grouped?<- "Time to Offer: 60-90 days",
                                                       ifelse(data_grpd$Time_to_offer>90 & data_grpd$Time_to_offer<=120,data_grpd$time_to_offer_grouped <- "Time to Offer: 90-120 days",
                                       ?                      ifelse(data_grpd$Time_to_offer>120 & data_grpd$Time_to_offer<=150,data_grpd$time_to_offer_grouped <- "Time to Offer: 120-150 days",
                                                                     data_grpd$time_to_offer_grouped <? "Time to Offer: 150-180 days")))))

head(data_grpd$time_to_offer_grouped,10)
tail(data_grpd$time_to_offer_grouped,10)
data_grpd$Time_to_offer <- NULL

# Creating stacked column chart for education_level and percent_hike_grouped
count <- table(data_grpd$ed?cation_level,data_grpd$percent_hike_grouped)
count
barplot(count,legend = rownames(count))
# Insights: Candidates with Associate Certification are getting good hikes as compared to people with other degrees.

# Creating stacked column chart for no_companie?_worked and percent_hike_grouped
count <- table(data_grpd$no_companies_worked,data_grpd$percent_hike_grouped)
count
barplot(count,legend = rownames(count))

# Creating stacked column chart for satisfaction_index_grouped and percent_hike_grouped
count <- ta?le(data_grpd$satisfaction_index_grouped,data_grpd$percent_hike_grouped)
count
barplot(count,legend = rownames(count))
prop.table(count[1,])*100
prop.table(count[2,])*100
prop.table(count[3,])*100
prop.table(count[4,])*100
prop.table(count[5,])*100
# Insigh?s: We see that for candidates with satisfaction level 15-30, maximum number of candidates are having a hike percent i.e. 11-20% whereas for candidates with satisfaction level 60-75,
# the satisfaction level is more as maximum number of people in that categ?ry of satisfaction level are getting maximum hikes i.e. 31-40%.

# Creating stacked column chart for age_grouped and satisfaction_index_grouped
count <- table(data_grpd$satisfaction_index_grouped,data_grpd$age_grouped)
count
barplot(count,legend = rownames?count))
# Insights: Across all the Age groups, most number of candidates have a satisfaction score of 15-30. (Basis the current data)

# Creating stacked column chart for age_grouped and offer_dropped
count <- table(data_grpd$age_grouped,data_grpd$offer_dr?pped)
count
barplot(count,legend = rownames(count))
# Insights: With increase in Age, the candidates drop the offer less as compared to the candidates belonging to 25-30 age group where the offer dropped are maximum.

# Creating stacked column chart for sa?isfaction_index_grouped and offer_dropped
count <- table(data_grpd$offer_dropped,data_grpd$satisfaction_index_grouped)
count
barplot(count,legend = rownames(count))
# Insights: Those who have dropped the offer are the ones with the least satisfaction level?

# Creating stacked column chart for percent_hike_grouped and offer_dropped
count <- table(data_grpd$percent_hike_grouped,data_grpd$offer_dropped)
count
barplot(count,legend = rownames(count))
# Insights: Majority of candidates with percent hike greater t?an 30 are the ones who did not drop their offers. 

# Creating stacked column chart for total_experience_grouped and offer_dropped
count <- table(data_grpd$total_experience_grouped,data_grpd$offer_dropped)
count
barplot(count,legend = rownames(count))
# In?ights: Majority of Candidates with total experience greater than 15 years are the ones who do not drop the offer.

# Creating stacked column chart for time_to_offer_grouped and offer_dropped
count <- table(data_grpd$time_to_offer_grouped,data_grpd$offer_dr?pped)
count
barplot(count,legend = rownames(count))
# Insights: All the candidates who got the offer after 150 days, have rejected the offer.

# Performed factor analysis using SPSS.

############*************** Step 5 : MERGING DATA POST FACTOR ANALYSIS

? Categorical variable set
data_categorical = subset(data, select = -c(age,percent_hike,total_rounds,satisfaction_index,no_companies_worked,total_experience,Time_to_offer,job_hopping_index) )
str(data_categorical)
data_cont = subset(data, select = c(age,per?ent_hike,total_rounds,satisfaction_index,no_companies_worked,total_experience,Time_to_offer,job_hopping_index) )
str(data_cont)
data_scaled <- scale(data_cont)

fact_data <- read.csv(file.choose())

data_merged <- cbind(data_categorical , fact_data)
data_m?rged_scaled <- cbind(data_categorical , data_scaled)
str(data_merged)
str(data_merged_scaled)
str(data_grpd)

############*************** Step 6 : CREATING DUMMY VARIABLES

install.packages("fastDummies",dependencies = TRUE)
library(fastDummies)
#creating ?ummy variables
dummy_data <- fastDummies::dummy_cols(data, remove_most_frequent_dummy = 1)
names(dummy_data)
dummy_data[,c('marital_status','education_level','gender','distance_from_home','sourcing_channel','career_growth','flexi_work','timely_communicatio?','offer_dropped')] <-list(NULL)
# Insights: We are getting 25 variables in total.

dummy_data_scaled <- fastDummies::dummy_cols(data_merged_scaled, remove_most_frequent_dummy = 1)
names(dummy_data_scaled)
dummy_data_scaled[,c('marital_status','education_l?vel','gender','distance_from_home','sourcing_channel','career_growth','flexi_work','timely_communication','offer_dropped')] <-list(NULL)
# Insights: We are getting 25 variables.

dummy_data_grpd <- fastDummies::dummy_cols(data_grpd, remove_most_frequent_du?my = 1)
names(dummy_data_grpd)
dummy_data_grpd[,c('marital_status','education_level','gender','distance_from_home','sourcing_channel','career_growth','flexi_work','timely_communication','offer_dropped','percent_hike_grouped','satisfaction_index_grouped','a?e_grouped','time_to_offer_grouped','total_experience_grouped')] <-list(NULL)
# Insights: We are getting 40 variables.

dummy_data_merged <- fastDummies::dummy_cols(data_merged, remove_most_frequent_dummy = 1)
names(dummy_data_merged)
dummy_data_merged[,c('?arital_status','education_level','gender','distance_from_home','sourcing_channel','career_growth','flexi_work','timely_communication','offer_dropped')] <-list(NULL)
# Insights: We are getting 21 variables.



############*************** Step 7 : Test/Train?split and applying logistic regression

# Correlation test (Not necessary as factor analysis already done)
install.packages("corrplot",dependencies = TRUE)
library('corrplot')
correlationMatrix <- cor(dummy_data) #computes the correlation coefficient
corrp?ot(correlationMatrix, method="circle")

install.packages("caret",dependencies = TRUE)
library(caret)
install.packages("dplyr",dependencies = TRUE)
library(dplyr)

# We have made train-test split as 80-20 respectively.

set.seed("2341")
trainIndex <- create?ataPartition(dummy_data$offer_dropped_No,p=0.8,list = FALSE)
train_data <- dummy_data[trainIndex,]
test_data <- dummy_data[-trainIndex,]
trainIndex <- createDataPartition(dummy_data_scaled$offer_dropped_No,p=0.8,list = FALSE)
train_data_scaled <- dummy_dat?_scaled[trainIndex,]
test_data_scaled <- dummy_data_scaled[-trainIndex,]
trainIndex <- createDataPartition(dummy_data_grpd$offer_dropped_No,p=0.8,list = FALSE)
train_data_grpd <- dummy_data_grpd[trainIndex,]
test_data_grpd <- dummy_data_grpd[-trainIndex,]
?rainIndex <- createDataPartition(dummy_data_merged$offer_dropped_No,p=0.8,list = FALSE)
train_data_merged <- dummy_data_merged[trainIndex,]
test_data_merged <- dummy_data_merged[-trainIndex,]
test_data_grpd <- dummy_data_grpd[-trainIndex,]


##############?# logistic regression - with dummy variables on original data - 96.49
model <- glm(offer_dropped_No ~., data = train_data, family = binomial)
summary(model)

pred <- predict(model,newdata = test_data,type='response')
summary(pred)

#performance(pred, measu?e = "auc")

matrix=table(test_data$offer_dropped_No, pred > 0.5)
n <- sum(matrix)
dn <- diag(matrix)
accuracy <- sum(dn) / n
accuracy

install.packages("ROCR",dependencies = TRUE)
library(ROCR)
ROCRpred <- prediction(pred, test_data$offer_dropped_No)
ROCRp?rf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# Insights: Large area under the curve shows that we are getting a model with fairly good accuracy.
# Insights: Accuracy of the model is 95.49.

##############?# logistic regression - variable-scaled data 
model <- glm(offer_dropped_No ~., data = train_data_scaled, family = binomial)
summary(model)

pred <- predict(model,newdata = test_data_scaled,type='response')
summary(pred)

#performance(pred, measure = "auc"?

matrix=table(test_data_scaled$offer_dropped_No, pred > 0.5)
n <- sum(matrix)
dn <- diag(matrix)
accuracy <- sum(dn) / n
accuracy

library(ROCR)
ROCRpred <- prediction(pred, test_data_scaled$offer_dropped_No)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')?plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# Insights: Large area under the curve shows that we are getting a model with fairly good accuracy.
# Insights: Accuracy of the model is 93.98.

################ logistic regression - variable-grpd da?a
model <- glm(offer_dropped_No ~., data = train_data_grpd, family = binomial)
summary(model)

pred <- predict(model,newdata = test_data_grpd,type='response')
summary(pred)

#performance(pred, measure = "auc")

matrix=table(test_data_grpd$offer_dropped_No,?pred > 0.5)
n <- sum(matrix)
dn <- diag(matrix)
accuracy <- sum(dn) / n
accuracy

library(ROCR)
ROCRpred <- prediction(pred, test_data_grpd$offer_dropped_No)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1?7))
# Insights: Large area under the curve shows that we are getting a model with fairly good accuracy.
# Insights: Accuracy of the model is 96.49.

############### logistic regression - factor cont. data
model <- glm(offer_dropped_No ~., data = train_data?merged, family = binomial)
summary(model)

pred <- predict(model,newdata = test_data_merged,type='response')
summary(pred)

#performance(pred, measure = "auc")

matrix=table(test_data_merged$offer_dropped_No, pred > 0.5)
n <- sum(matrix)
dn <- diag(matrix)?accuracy <- sum(dn) / n
accuracy

library(ROCR)
ROCRpred <- prediction(pred, test_data_merged$offer_dropped_No)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
# Insights: Accuracy of the model is 88.7?.

############### END OF CODE
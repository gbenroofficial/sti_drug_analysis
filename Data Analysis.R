#Step 1: install packages and load libraries:


#install.packages('janitor')
#install.packages('caTools')
#install.packages('car')

library(janitor)
library(dplyr)
library(caTools)
library(stringr)
library(car)


#Read dataset into object
dataset = read.csv('C:/Users/smesh/Desktop/dataset1.csv')






#Remove the values with 0:
dataset = dataset[dataset$Age != 0, ]
dataset = dataset[dataset$BP != 0, ]


#Separate the blood pressure into 2 columns:
dataset = dataset %>%  
  mutate(s0 = str_split(BP,"/")) %>%
  rowwise() %>%
  mutate(systole = as.numeric(s0[1]), 
         diastole = as.numeric(s0[2])) 
dataset = select(dataset, -s0)  #remove unwanted column


#Create column as sum of refill columns to represent rate of refill
dataset$sumOfRefill = as.numeric(dataset$Refill1month) + as.numeric(dataset$Refill2months) + as.numeric(dataset$Refill3months) + as.numeric(dataset$Refill6months)


#remove values that are abnormal from columns
dataset = dataset %>% filter((systole<=200 && systole >= 90))
dataset = dataset %>% filter((diastole<=90 && diastole >= 60))
dataset = dataset %>% filter((Height<=200 && Height >= 50))
dataset = dataset %>% filter((Weight_<=200 && Weight_ >= 40))
dataset = dataset[dataset$sumOfRefill != 0, ]
dataset = dataset %>% filter((Age>=1 && Age <= 100))
dataset$Weight = dataset[dataset$Weight_] #changing the name of weight column

#remove the weight column with unwanted name
dataset = select(dataset, -Weight_)

#remove na values from dataset
dataset = na.omit(dataset)

#remove 0 values in received_dp column
dataset = dataset[dataset$Received_DP != 0,]


#convert categorical data from string to factor level data for machine to understand
dataset$Gender = factor(dataset$Gender)
dataset$CountyOfBirth = factor(dataset$CountyOfBirth)
dataset$EntryPoint = factor(dataset$EntryPoint)
dataset$ReferredOrTransferredFrom = factor(dataset$ReferredOrTransferredFrom)
dataset$FacilityType = factor(dataset$FacilityType)
dataset$SignOfSti = factor(dataset$SignOfSti)
dataset$Cluster = factor(dataset$Cluster)
dataset$received_counseling = factor(dataset$received_counseling)


#remove unwanted columns
dataset = select(dataset, -Refill1month)
dataset = select(dataset, -Refill2months)
dataset = select(dataset, -Refill3months)
dataset = select(dataset, -Refill6months)
dataset = select(dataset, -Received_DP)
dataset = select(dataset, -BP)
dataset = select(dataset, -InitialVisitDate)
dataset = select(dataset, -ClientId)
dataset = select(dataset, -Lmp)




#create train data:
set.seed(123)
split = sample.split(dataset$sumOfRefill, SplitRatio = 0.8)
training_set = subset(dataset, split = TRUE)
test_set = subset(dataset, split = TRUE)

#create model for relationship between dependent (sumofrefill) and other independent variable
regressor <- lm(formula = sumOfRefill ~ Gender + CountyOfBirth + Age + EntryPoint + ReferredOrTransferredFrom + Height + SignOfSti + Cluster + FacilityType + received_counseling + systole + diastole,
               data = training_set)

summary(regressor)

#from p value shown in summary begin backward elimination using p-value of 0.05 i.e. if higher than chosen p-value remove from highest to lowest:

 #remove county of birth
regressor <- lm(formula = sumOfRefill ~ Gender + Age + EntryPoint + ReferredOrTransferredFrom + Height + SignOfSti + Cluster + FacilityType + received_counseling + systole + diastole,
                data = training_set)

summary(regressor)

#remove systole
regressor <- lm(formula = sumOfRefill ~ Gender + Age + EntryPoint + ReferredOrTransferredFrom + Height + SignOfSti + Cluster + FacilityType + received_counseling + diastole,
                data = training_set)

summary(regressor)

#remove gender
regressor <- lm(formula = sumOfRefill ~ Age + EntryPoint + ReferredOrTransferredFrom + Height + SignOfSti + Cluster + FacilityType + received_counseling + diastole,
                data = training_set)

summary(regressor)

#remove entry point
regressor <- lm(formula = sumOfRefill ~ Age + ReferredOrTransferredFrom + Height + SignOfSti + Cluster + FacilityType + received_counseling + diastole,
                data = training_set)

summary(regressor)

#remove referredortransferred
regressor <- lm(formula = sumOfRefill ~ Age + Height + SignOfSti + Cluster + FacilityType + received_counseling + diastole,
                data = training_set)

summary(regressor)

#remove cluster
regressor <- lm(formula = sumOfRefill ~ Age + Height + SignOfSti + FacilityType + received_counseling + diastole,
                data = training_set)

summary(regressor)

#remove facility
regressor <- lm(formula = sumOfRefill ~ Age + Height + SignOfSti + received_counseling + diastole,
                data = training_set)

summary(regressor)

#Chosen predictors are Age, Height, Sign of Sti, Received Counseling, Diastole based on 5% P-value

#produce added variable plots
avPlots(regressor)

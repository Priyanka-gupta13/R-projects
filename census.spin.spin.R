census<-read.csv("C:/Users/priyanka gupta/Desktop/R projects/cencus/train.csv",na.strings = c(" ","NA"))

str(census)
#change the income level as 0 and 1 (0 means <50000,1 means >50000)
census$income_level<-ifelse(census$income_level==-50000,0,1)
census$income_level<-as.factor(census$income_level)

colSums(is.na(census))

#removing columns with more than equal to 50 % values as NAs
ff<-function(x){sum(is.na(x))/(length(x))}*100
c<-sapply(census,ff)
round(c)
which(round(c)>=50)
census<-census[,-c(25,26,27,29)]


str(census)

census$industry_code<-as.factor(census$industry_code)
levels(census$industry_code)

census$occupation_code<-sapply(census$occupation_code,as.factor)
levels(census$occupation_code)

levels(census$hispanic_origin)
sum(is.na(census$hispanic_origin))
census$hispanic_origin[which(is.na(census$hispanic_origin))]<-"Do not know"

str(census[,1:15]) #done cleaning for first 15 columns 

str(census[,15:30])
colSums(is.na(census[,15:30]))

levels(census$state_of_previous_residence)
census$state_of_previous_residence[which(is.na(census$state_of_previous_residence))]<-"Not in universe"
sum(is.na(census$state_of_previous_residence))

library("dplyr")

levels(census$country_mother)
sum(is.na(census$country_mother))
census$country_mother<-as.character(census$country_mother)
census$country_mother[which(is.na(census$country_mother))]<-"Do not know"
census$country_mother<-as.factor(census$country_mother)

levels(census$country_father)
sum(is.na(census$country_father))
census$country_father<-as.character(census$country_father)
census$country_father[which(is.na(census$country_father))]<-"Do not know"
census$country_father<-as.factor(census$country_father)

levels(census$country_self)
census$country_self<-as.character(census$country_self)
census$country_self[which(is.na(census$country_self))]<-"Do not know"
census$country_self<-as.factor(census$country_self)

sum(is.na(census))
#Na taken care of 

library("dplyr")
library("caret")
census1<-downSample(census[,1:36],census$income_level,list = FALSE,yname = "income_level")
table(census1$income_level)



#finding important variables using step regression 

ind<-createDataPartition(census1$income_level,p=0.6,list=FALSE)
data1=census1[ind,]
data2=census1[-ind,]
table(data1$income_level)



Intercept<-glm(income_level~1,data = data1,family = "binomial")
all.data<-glm(income_level~.,data = data1 ,family = "binomial")
summary(all.data)
stepmod<-step(Intercept,scope = list(lower=Intercept,upper=all.data),direction = "both",trace = 1,steps = 1000)
shortlisted_elements<-names(unlist(stepmod[[1]]))
class(shortlisted_elements)

#shortlisted elements :
#industry_code
#occupational_code
#education
#d_household_family_statChild 
#capital_gains   
#sexMale 
#weeks_worked_in_year 
#age 
#capital_losses   
#num_person_Worked_employer  
#tax_filer_statusJoint
#industry_code
#class_of_worker
#region_of_previous_residence
#marital_status
#veterans_benefits 
#live_1_year_ago
#race
#member_of_labor_union
#dividend_from_Stocks

library("e1071")
model1<-naiveBayes(income_level~industry_code+
                  occupation_code+
                  education+
                  d_household_family_stat+ 
                  capital_gains+  
                  sex+
                  weeks_worked_in_year+
                  age+
                  capital_losses+   
                  num_person_Worked_employer+
                  tax_filer_status+
                  industry_code+
                  class_of_worker+
                  region_of_previous_residence+
                  marital_status+
                  veterans_benefits+
                  live_1_year_ago+
                  race+
                  member_of_labor_union+
                  dividend_from_Stocks
                    ,data = data1)
summary(model1)
model1
census.pred<-predict(model1,data2)
head(census.pred)
confusionMatrix(census.pred,data2$income_level)

#Accuracy : 83.45%

census.test<-read.csv("C:/Users/priyanka gupta/Desktop/R projects/cencus/test.csv",na.strings = c("NA"," ?"))
colSums(is.na(census.test))
sum(is.na(census.test))
str(census.test2)
which(colnames(census.test)%in%colnames(census))
census.test<-census.test[,-c(25,26,27,29)]

#we will remove the income column because we have to use it further for testing the accuracy of out nb model
census.test$income_level<-ifelse(census.test$income_level=="-50000",0,1)
census.test$income_level<-as.factor(census.test$income_level)
census.test1<-census.test[,-37]

colSums(is.na(census.test1))

0.5*99762

levels(census.test1$state_of_previous_residence)
census.test1$state_of_previous_residence[which(is.na(census.test1$state_of_previous_residence))]<-"Not in universe"
census.test1$state_of_previous_residence<-as.character(census.test1$state_of_previous_residence)
census.test1$state_of_previous_residence<-as.factor(census.test1$state_of_previous_residence)
sum(is.na(census.test1$state_of_previous_residence))

sum(is.na(census.test1$country_mother))
census.test1$country_mother<-as.character(census.test1$country_mother)
census.test1$country_mother[which(is.na(census.test1$country_mother))]<-"Do not know"
census.test1$country_mother<-as.factor(census.test1$country_mother)


sum(is.na(census.test1$country_father))
census.test1$country_father<-as.character(census.test1$country_father)
census.test1$country_father[which(is.na(census.test1$country_father))]<-"Do not know"
census.test1$country_father<-as.factor(census.test1$country_father)

census.test1$country_self<-as.character(census.test1$country_self)
census.test1$country_self[which(is.na(census.test1$country_self))]<-"Do not know"
census.test1$country_self<-as.factor(census.test1$country_self)


colSums(is.na(census.test1))
sum(is.na(census.test1))

Income_pred<-predict(model1,census.test1)
head(Income_pred)
confusionMatrix(Income_pred,census.test$income_level)

#accuracy: 64.39%

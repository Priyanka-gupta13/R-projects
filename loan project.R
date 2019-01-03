Loan<-read.csv("C:/Users/priyanka gupta/Desktop/train_u6lujuX_CVtuZ9i.csv",stringsAsFactors = FALSE,na.strings = c(""))
library("caret")
library("dplyr")
library("e1071")
library("stringi")
library("stringr")
str(Loan)
#remove loan id column
Loan=Loan[,-1]
#check for blank values and na
colSums(is.na(Loan))
#Gender,Married,Dependents,Self employed:

Loan$Gender<-ifelse(Loan$Gender=="Female",1,0)
Loan$Married<-ifelse(Loan$Married=="Yes",0,1)

Loan$Gender[which(is.na(Loan$Gender))]=median(Loan$Gender,na.rm=T)
Loan$Married[which(is.na(Loan$Married))]=median(Loan$Married,na.rm=T)


Loan$Self_Employed<-ifelse(Loan$Self_Employed=="Yes",1,0)
Loan$Self_Employed[which(is.na(Loan$Self_Employed))]=median(Loan$Self_Employed,na.rm=T)


Loan$Education<-ifelse(Loan$Education=="Graduate",1,0)
Loan$Education[which(is.na(Loan$Education))]=median(Loan$Education,na.rm=T)


Loan$Dependents[which(Loan$Dependents=="3+")]="3"
Loan$Dependents<-as.integer(Loan$Dependents)
Loan$Dependents[which(is.na(Loan$Dependents))]=mean(Loan$Dependents,na.rm=TRUE)


#Loan_amount
Loan$LoanAmount[which(is.na(Loan$LoanAmount))]=mean(Loan$LoanAmount,na.rm=T)
#Loan_term
Loan$Loan_Amount_Term[which(is.na(Loan$Loan_Amount_Term))]=mean(Loan$Loan_Amount_Term,na.rm=T)
#Credit_history 
Loan$Credit_History[which(is.na(Loan$Credit_History))]=median(Loan$Credit_History,na.rm=T)
#Loan_Status
Loan$Loan_Status<-ifelse(Loan$Loan_Status=="Y",1,0)
Loan$Loan_Status<-as.factor(Loan$Loan_Status)

is.unsorted(Loan$S.) #means data is now sorted
#if the data is biased
table(Loan$Loan_Status)
#less number of 0 so

#upsample the data
Loan2<-upSample(x=Loan[,c(1:11)],y=Loan$Loan_Status)
names(Loan2)[12]=paste("Loan_Status")
table(Loan2$Loan_Status)
str(Loan2)


#split into test and train to check accuracy of the model
ind<-createDataPartition(Loan2$Loan_Status,p=0.75,list = FALSE)
training<-Loan2[ind,]
testing<-Loan2[-ind,]
str(testing)
str(training)

#logistic model
logistic<-glm(Loan_Status~Credit_History+Property_Area,data = training,family = "binomial")
summary(logistic)
logistic
plot(logistic)
scatter.smooth(Loan2$LoanAmount,Loan2$Loan_Status)
scatter.smooth(Loan2$ApplicantIncome,Loan2$Loan_Status)
which(Loan2$LoanAmount>500)
t<-filter(Loan2,Loan2$LoanAmount>500) #when loan amount is greater than 570,loan status is positive but that isbecause of outliars
a<-filter(Loan2,Loan2$ApplicantIncome>60000) #applicant income has no effect on loan status


#testing the model on testdata
Predictions<-predict(logistic,newdata = testing,type = "response")
Predictions<-round(Predictions)
Predictions<-as.factor(Predictions)
confusionMatrix(Predictions,testing$Loan_Status)
#Accuracy : 0.7048   



Loantest<-read.csv("C:/Users/priyanka gupta/Desktop/test_Y3wMUE5_7gLdaTN.csv",stringsAsFactors = FALSE,na.strings = c(""))
Loantest<-Loantest[,-1]
Loantest<-Loantest[,c(10,11)]

Loantest$Credit_History[which(is.na(Loantest$Credit_History))]<-median(Loantest$Credit_History,na.rm=TRUE)
Loantest$Credit_History<-as.numeric(Loantest$Credit_History)
str(Loan2)
str(Loantest)


Predicted_data<-predict(logistic,newdata = Loantest,type = "response")
Predicted_data<-round(Predicted_data,digits = 0)
table(Predicted_data)


write.csv(Predicted_data,file = "C:/Users/priyanka gupta/Desktop/R projects/loan_pred.csv")


library("class")


#Loantest

#check for blank values and na
colSums(is.na(Loan))
#Gender,Married,Dependents,Self employed:


Loantest<-Loantest[,-1]
Loantest$Gender[which(is.na(Loantest$Gender))]=median(Loantest$Gender,na.rm=T)
Loantest$Gender<-ifelse(Loantest$Gender=="Female",1,0)


Loantest$Married[which(is.na(Loantest$Married))]=median(Loantest$Married,na.rm=T)
Loantest$Married<-ifelse(Loantest$Married=="Yes",0,1)



Loantest$Self_Employed[which(is.na(Loantest$Self_Employed))]=median(Loantest$Self_Employed,na.rm=T)
Loantest$Self_Employed<-ifelse(Loantest$Self_Employed=="Yes",1,0)



Loantest$Education[which(is.na(Loantest$Education))]=median(Loantest$Education,na.rm=T)
Loantest$Education<-ifelse(Loantest$Education=="Graduate",1,0)


Loantest$Dependents[which(Loantest$Dependents=="3+")]="3"
Loantest$Dependents<-as.integer(Loantest$Dependents)
Loantest$Dependents[which(is.na(Loantest$Dependents))]=mean(Loantest$Dependents,na.rm=TRUE)
Loantest$Dependents<-as.integer(Loantest$Dependents)

#Loan_amount
Loantest$LoanAmount[which(is.na(Loantest$LoanAmount))]=mean(Loantest$LoanAmount,na.rm=T)
#Loan_term
Loantest$Loan_Amount_Term[which(is.na(Loantest$Loan_Amount_Term))]=mean(Loantest$Loan_Amount_Term,na.rm=T)
#Credit_history 
Loantest$Credit_History[which(is.na(Loantest$Credit_History))]=median(Loantest$Credit_History,na.rm=T)
#Loan_Status
Loantest$Loan_Status<-ifelse(Loantest$Loan_Status=="Y",1,0)
Loantest$Loan_Status<-as.factor(Loantest$Loan_Status)

colSums(is.na(Loantest))

#svm
modelsvm<-svm(Loan_Status~.,data = Loan2)
summary(modelsvm)
Prediction2<-predict(modelsvm,Loantest)
Prediction2<-as.data.frame(Prediction2)
write.csv(Prediction2,file = "C:/Users/priyanka gupta/Desktop/R projects/loan_pred.csv")


#random forest

Loan$Property_Area<-as.factor(Loan$Property_Area)
Loantest$Property_Area<-as.factor(Loantest$Property_Area)


library(randomForest)
set.seed(123)
modelrf1<-randomForest(Loan_Status~.,data = Loan,ntree=300,mtry=2)


modelrf
modelrf$mtry
modelrf$ntree
plot(modelrf)
#tuning the model 
Rtune<-tuneRF(Loan[,-12],Loan[,12],ntreeTry = 300,stepFactor = 2,improve = 0.05,trace = TRUE,plot = TRUE)
Rtune

set.seed(12345)
model2<-randomForest(Loan_Status~.,data = Loan,ntree=300,mtry=2,importance=TRUE,proximity=TRUE)
model2
varImpPlot(model2)
varUsed(model2)
partialPlot(model2,Loan,Property_Area,"1")
partialPlot(model2,Loan,Credit_History,"1")
randomforest<-as.data.frame(predict(model2,Loantest))

write.csv(randomforest,file = "C:/Users/priyanka gupta/Desktop/R projects/loan_pred3.csv")

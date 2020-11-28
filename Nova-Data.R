#Reading from csv files
nova.train <- read.csv('Competitions/Novartis/Dataset/Train.csv')
nova.test <- read.csv('Competitions/Novartis/Dataset/Test.csv')

#Creating train and test data frame for further processing
train <- nova.train
test <- nova.test

library(dplyr)
#Adding YMD column as a Date formatted value of date
train$YMD <- as.Date(nova.train$DATE,'%d-%b-%y')
train$MALICIOUS_OFFENSE <- as.factor(nova.train$MULTIPLE_OFFENSE)

test$YMD <- as.Date(nova.test$DATE,'%d-%b-%y')

#Checking structure ,duplicate and summary of data as a part of EDA

attach(train)

str(train)
summary(train)
nrow(unique(train))

#Checking response variable distribution i.e. MALICIOUS_OFFENSE for balance of data
library(ggplot2)

ggplot(train,aes(MALICIOUS_OFFENSE))+geom_bar(aes(fill=MALICIOUS_OFFENSE))+ggtitle('Distribution of Offense')
table(MALICIOUS_OFFENSE)    # 0:1 - 1068:22788
prop.table(table(MALICIOUS_OFFENSE)) #0:1 -0.045:0.955

#Response variable is highly imbalanced (ie. <5% belongs to one class and remaining 95% belongs to another class)

#Look at individual summaries of X's

class(X_1)   #checking class of variable
summary(X_1) #Checking summary of variable
quantile(X_1,p=c(1:100)/100,na.rm=TRUE) #checking distribution of individual variable
hist(X_1) #Histogram of variable
nrow(filter(train,X_1 ==0)) #fetching no. of rows having 0 value, considerable number of 0's ,so leaving as it is.

class(X_2)
summary(X_2)
quantile(X_2,p=c(1:100)/100,na.rm=TRUE)
hist(X_2)
nrow(filter(train,X_2 ==0))
#Since only 22 rows are there with 0 value so capping at median value of 24
train$X_2 <- ifelse(train$X_2==0,24,train$X_2)


class(X_3)
summary(X_3)
quantile(X_3,p=c(1:100)/100,na.rm=TRUE)
hist(X_3)
nrow(filter(train,X_3 ==0))
#20 rows with 0, so replacing with median value of 24
train$X_3 <- ifelse(train$X_3==0,24,train$X_3)


#X_2 and X_3 looks quite similar, checking correlation
cor(X_2,X_3)  #0.9969851

class(X_4)
summary(X_4)
quantile(X_4,p=c(1:100)/100,na.rm=TRUE)
hist(X_4)
nrow(filter(train,X_4 ==0)) #3335 rows have 0 value, so leaving them as it is.


class(X_5)
summary(X_5)
quantile(X_5,p=c(1:100)/100,na.rm=TRUE)
hist(X_5)
nrow(filter(train,X_5 ==0)) #4695 rows have 0 value , leaving them as it it.


class(X_6)
summary(X_6)
quantile(X_6,p=c(1:100)/100,na.rm=TRUE)
hist(X_6)


class(X_7)
summary(X_7)
quantile(X_7,p=c(1:100)/100,na.rm=TRUE)
hist(X_7)
nrow(filter(train,X_7 ==0)) #3461 rows have value 0, so leaving them as it is.

#X_6 and X_7 looks quite similar, checking correlation
cor(X_6,X_7) # 0.6952431


class(X_8)
summary(X_8)
quantile(X_8,p=c(1:100)/100,na.rm=TRUE) #36% is having value 0, so leaving them as it is,checking for outliers.
hist(X_8)
nrow(filter(train,X_8 ==0))
#99% values are less than 5, checking about remaining values
quantile(X_8,p=c(990:1000)/1000,na.rm=TRUE)
nrow(filter(train,X_8==6))  #79 rows are b/w 5 & 6
nrow(filter(train,X_8==7))  #33 rows are b/w 6 & 7
nrow(filter(train,X_8==8))  #32 rows are b/w 7 & 8
nrow(filter(train,X_8==9))  #16 rows are b/w 8 & 9
nrow(filter(train,X_8==10)) #23 rows are b/w 9 & 10
nrow(filter(train,X_8==14)) #2  rows are b/w 10 & 14
nrow(filter(train,X_8==99)) #1  rows are b/w 14 & 99
i <- which(X_8==99) # checking row for 99 value - row no. 7055, 
#Deleting the row which contains outlier
train <- train[-i,]


class(X_9)
summary(X_9)
quantile(X_9,p=c(1:100)/100,na.rm=TRUE)
hist(X_9)
nrow(filter(train,X_9 ==0)) #118 rows, replacing with median value of 5.
train$X_9 <- ifelse(train$X_9==0,5,train$X_9)


class(X_10)
summary(X_10)
hist(X_10)
quantile(X_10,p=c(1:100)/100,na.rm=TRUE)
#99% values are less than 4, checking about remaining values
quantile(X_10,p=c(990:1000)/1000,na.rm=TRUE)
nrow(filter(train,X_10==5))  #71
nrow(filter(train,X_10==6))  #54
nrow(filter(train,X_10==8))  #15
nrow(filter(train,X_10==10)) #14
nrow(filter(train,X_10==90)) #1
i <- which(X_10==90) #row no. 2352 removing this row
train <- train[-i,]


class(X_11)
summary(X_11)
hist(X_11)
quantile(X_11,p=c(1:100)/100,na.rm=TRUE)
#Since 10% values contains 0 and  median is 249(76%), so replacing 10% values with median value
train$X_11 <- ifelse(train$X_11 ==0,249,train$X_11)


class(X_12)
summary(X_12) #182 rows have 'NA' values
hist(X_12)
quantile(X_12,p=c(1:100)/100,na.rm=TRUE)
nrow(filter(train,X_12 ==0))
#99% values are less than 4, checking about remaining values
quantile(X_12,p=c(990:1000)/1000,na.rm=TRUE)
nrow(filter(train,X_12==5))  #59
nrow(filter(train,X_12==6))  #36
nrow(filter(train,X_12==9))  #6
nrow(filter(train,X_12==90)) #1  row no. 2352

#X_10 & X_12 looks quite similar, so would do further analysis

class(X_13)
summary(X_13)
hist(X_13)
quantile(X_13,p=c(1:100)/100,na.rm=TRUE)
nrow(filter(train,X_13 ==0))
i <- which(X_13==0)   #row no.18566 deleting this row
train <- train[-i,]


class(X_14)
summary(X_14)
hist(X_14)
quantile(X_14,p=c(1:100)/100,na.rm=TRUE)
nrow(filter(train,X_14 ==0))
#Replacing with Median value of 62
train$X_14 <- ifelse(train$X_14==0,62,train$X_14)


class(X_15)
summary(X_15)
hist(X_15)
quantile(X_15,p=c(1:100)/100,na.rm=TRUE)
nrow(filter(train,X_15 ==0))
#4% values contains 0, so replacing with median value of 34
train$X_15 <- ifelse(train$X_15==0,34,train$X_15)


library(corrgram)
corrgram(train,order = TRUE)
#(X_2 & X_3)(X_10 & X_12) and cor(X_6,X_7) are highly correlated 

#X_10 and X_12 seems to be highly correlated
cor(X_10,X_12,use="complete",method="pearson") #0.8777426
#X_10 and X_12 seems to be highly correlated and X_12 contains many missing values so deleting X_12
train <- train[,-14]
#X_2 and X_3 are also highly correlated, so will take any of these X_2 or X_3 for further analysis.
#similarly X_6 and X_7 are also highly correlated, so take either of them.


#Building Logistic Reg Model and since data is highly imbalanced in nature, 
#we use ROSE library to get balanced sample and then applying logistic regression on it.

library(ROSE)
library(pROC)
library(rms)

df <- train[,-18]
df.ROSE <- ROSE(MALICIOUS_OFFENSE~.,data=df,seed=20)$data
table(df.ROSE$MALICIOUS_OFFENSE)

#Did logistic regression on individual variable and X_4,X_5,X_6,X_7,X_9,X_13 seems to be insignificant.
#X_2 & X_3 are highly correlated so considering only X_2 for analysis.

ROSE.logmod <- glm(MALICIOUS_OFFENSE~X_1+X_2+X_8+X_10+X_11+X_14+X_15,data=df.ROSE,family = "binomial")
summary(ROSE.logmod)
vif(ROSE.logmod) #Checking variance inflation factors to check for multicollinearity among the variables.
roc(df.ROSE$MALICIOUS_OFFENSE,ROSE.logmod$fitted.values)
accuracy.meas(df.ROSE$MALICIOUS_OFFENSE,ROSE.logmod$fitted.values,threshold = 0.5)

#Confusion Matrix
ROSE.logmod.response <- ifelse(ROSE.logmod$fitted.values>=0.5,1,0) 
table(df.ROSE$MALICIOUS_OFFENSE,ROSE.logmod.response)

#Logistic Regression is not performing well inspite of balancing the data,
#so we need to consider more advanced techniques like bagging Random forest.
#To apply and check the accuracy, splitting the train dataframe into 
#train.train and train.test (hold-out) sample in the ratio of 0.8:0.2.

library(caTools)

set.seed(20) #for reproducibility

index<-sample.split(train$MALICIOUS_OFFENSE,SplitRatio = 0.8) 
train.train <- subset(train,index==TRUE)
train.test <- subset(train,index==FALSE)


#Checking ratio of response variable in train-test set split

table(train.train$MALICIOUS_OFFENSE) #0:1 - 854:18228
table(train.test$MALICIOUS_OFFENSE)  #0:1 - 214:4557

#Random Forest model building using bagging(by supplying argument mtry=7)
library(randomForest)
names(train.train)
train.train <- train.train[,-c(2,17,18)]
train.train$INCIDENT_ID <- as.character(train.train$INCIDENT_ID)

rf.model <- randomForest(MALICIOUS_OFFENSE~X_1+X_2+X_8+X_10+X_11+X_14+X_15,
                         data=train.train,mtry=7,importance=TRUE)

#Checking misclassification using confusion matrix in train set
rf.model$confusion

#Model id doing reasonably good so checking it accuracy in holdout sample
#Predicting Response variable for hold-out test set from train set

p<- predict(rf.model,train.test)

#Checking misclassification using confusion matrix in hold-out test set of train set
table(p,train.test$MALICIOUS_OFFENSE)

#Checking accuracy measures
accuracy.meas(train.test$MALICIOUS_OFFENSE,p)

#Model has done reasonably prediction on hold-out set
#Appling Random Forest model on given test set to predict response

test.MALICIOUS_OFFENSE <- predict(rf.model,test)
table(test.MALICIOUS_OFFENSE)


outputDataSet <- data.frame("INCIDENT_ID" = test$INCIDENT_ID,
                            "MULTIPLE_OFFENSE" = test.MALICIOUS_OFFENSE)


write.csv(outputDataSet,"Competitions/Novartis/Dataset/Result.csv",row.names = FALSE)


###########################END--OF--FILE###################################################






















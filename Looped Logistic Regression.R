library(RODBC)
library(caret)
library(utils)
library(sqldf)
library(pROC)

options(scipen=999)

channel <- odbcConnect("DMP_WR3", uid="PAO", pwd="hamdi787",believeNRows=FALSE)

data <- sqlQuery(channel,
                 "SELECT
                 COL1,
                 COL2,
                 COL3,
                 COL4,
                 COL5
                 .
                 .
                 ...,
                 TARGET_FLAG
                 FROM 
                 DATA_TABLE
                 WHERE 
                 FILTER1
                 FILTER2
                 ")

model_data <- data



#Transform null categoric data into a new factor level
model_data$COL3 <- addNA(model_data$COL3)

#Give a name to NA level
levels(model_data$COL3)[] <- "Unknown Level"


#transform continuous column null's into zero
for (i in 1:(ncol(model_data)-1)) {
  if(!is.factor(model_data[,i])){
    model_data[is.na(model_data[,i]),i] <- 0
  }
}


#Train - Test Sets
inTrain <- createDataPartition(model_data$CC_TARGET_FLAG, p=0.8, list=FALSE)

train_set <- model_data[inTrain,]
test_set <- model_data[-inTrain,]

#Checked Train - Test Set Target Ratio
nrow(train_set[train_set$CC_TARGET_FLAG==1,])/nrow(train_set[train_set$CC_TARGET_FLAG==0,])
nrow(test_set[test_set$CC_TARGET_FLAG==1,])/nrow(test_set[test_set$CC_TARGET_FLAG==0,])



#Define necessary variables-dataframes for outputs

columns <- colnames(train_set)
GiniTrain <- data.frame(Doubles=double())
GiniTest <- data.frame(Doubles=double())
NofCols <- data.frame(Doubles=double())
SummaryTable <- data.frame(GiniTrain=double(),GiniTest=double(),NofCols=double())



#START LOOP & MODEL

#Note that columns are sorted by Information Values with decreasing order
#Start to build model with first variable, then add the other ones respectively and measure performance

j <- 1

for (j in 1:(length(train_set)-1)){
  
if(j==1) {
    string=paste(columns[j])
    } else {
    string=paste(string, "+" ,(columns)[j]) 
  }


model <- glm(as.formula(paste("TARGET_FLAG ~", (string))) ,family=binomial(link="logit"),data=train_set)
             

predict_train <- predict(model,train_set,type="response")
predict_test <- predict(model,test_set,type="response")

roccurve_train <- roc(train_set$CTARGET_FLAG ~ predict_train)
roccurve_test <- roc(test_set$TARGET_FLAG ~ predict_test)

gini_train <- 2*auc(roccurve_train) - 1
gini_test <- 2*auc(roccurve_test) - 1  
  

GiniTrain[j,1] <- gini_train
GiniTest[j,1] <- gini_test
NofCols[j,1] <- j


SummaryTable[j,] <- data.frame(GiniTrain[j,1], GiniTest[j,1],NofCols[j,1])


}

View(SummaryTable)
  

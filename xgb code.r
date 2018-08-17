library(RODBC)
library(caret)
library(utils)
library(sqldf)
library(xgboost)
library(dplyr)
library(pROC)

options(scipen=999)

channel <- odbcConnect("DMP_WR3", uid="schema_name", pwd="password",believeNRows=FALSE)

data <- sqlQuery(channel,
                 "SELECT
                 col1,
                 col2,
                 col3,
                 .
                 .
                 target_col
                 FROM 
                 table
                 ")

model_data <- data




# Transform numeric null data into 0

for (i in 1:(ncol(model_data)-1)) {
  if(!is.factor(model_data[,i])){
    model_data[is.na(model_data[,i]),i] <- 0
    }
}

# Create dummy variable as a necessity of XGBOOST

dummies <- dummyVars( ~ ., data = model_data)
model_data <- data.frame(predict(dummies, newdata = model_data))


# Split data as Train-Test

inTrain <- createDataPartition(model_data$OTOMATIK_DOVIZ_EKSTRE_TARGET_FLAG, p=0.8, list=FALSE)
train_set <- model_data[inTrain,]
test_set <- model_data[-inTrain,]


# Split Train-Test dataframes into data and label in order to create matrix

X_train = train_set %>% select(-target_col)
y_train = train_set$target_col

X_test = test_set %>% select(-target_col)
y_test = test_set$target_col


# Create matrix data

final_train_data <- xgb.DMatrix(data = as.matrix(X_train), label= y_train)
final_test_data <- xgb.DMatrix(data = as.matrix(X_test), label= y_test)

# Build model

model_xgb <- xgb.cv(data = final_train_data,
                    nrounds = 25,
                    nthread = 3,
                    nfold = 5,
                    metrics = "auc",
                    max_depth = 3,
                    eta = 2,
                    objective = "binary:logistic")



# Predict on test data

predict_test <- predict(model_xgb,final_test_data,type="prob")

# Calculate AUC

roccurve_test <- roc(y_test ~ predict_test)

plot(roccurve_test, legacy.axes = TRUE)

auc_test <- auc(roccurve_test)
gini_test <- 2*auc_test - 1

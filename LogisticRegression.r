# Importing libraries

library(RODBC)
library(caret)
library(utils)
library(sqldf)


# Define connection
options(scipen=999)
channel <- odbcConnect("schema_name", uid="user_id", pwd="password",believeNRows=FALSE)

# Get data
model_data <- sqlQuery(channel,
                       "SELECT
                       COL1,
                       COL2,
                       COL3,
                       COL4,
                       ...
                       TARGET_COLUMN
                       FROM
                       TABLE1
                       ")


# If needed: transform missing categoric data into "0"

model_data[is.na(model_data[,2]),2] <- 0
for (i in c()) {
  model_data[,i] <- as.factor(model_data[,i])
}

# If needed: transform missing numeric data into column mean

model_data$col1 = ifelse(is.na(model_data$col1),
                         ave(model_data$col1, FUN = function(x) mean(x, na.rm = TRUE)),
                         model_data$col1)


# If needed: transform missing numeric data into "0"
# and change column names into ISNULL expressions in order to use it in SQL
# I usually run this part when I consider my data and its business meaning
                         
for (i in 1:(ncol(model_data)-1)) {
  if(!is.factor(model_data[,i])){
    model_data[is.na(model_data[,i]),i] <- 0
    names(model_data)[i] <- paste0("ISNULL(",names(model_data)[i],",0)")
  }
}


# Split into train and test
library(caTools)

set.seed(123)
split = sample.split(model_data$target, SplitRatio = 0.8)

training_set = subset(model_data, split == TRUE)
test_set = subset(model_data, split == FALSE)



# Build model
model <- glm(TARGET_COLUMN_NAME ~.,family=binomial(link='logit'),data=training_set)

# Check the statistics 
summary(model)

# Plot the ROC curve for training set
library(pROC)
prob_training=predict.glm(model,training_set,type=c("response"))
roccurve_training <- roc(training_set$TARGET_COLUMN_NAME ~ prob_training)
plot(roccurve, legacy.axes = TRUE)


# Calculate AUC and gini coefficient for training set
auc_training = auc(roccurve_training)
gini_training = 2*auc - 1



# Plot the ROC curve for training set
library(pROC)
prob_test=predict.glm(model,test_set,type=c("response"))
roccurve_test <- roc(training_set$TARGET_COLUMN_NAME ~ prob_test)
plot(roccurve, legacy.axes = TRUE)


# Calculate AUC and ginicoefficient for training set
auc_test = auc(roccurve_test)
gini_test = 2*auc - 1

                         
                         
# If everything is fine
# Transform model coefficients into an SQL Code to use it in customer scoring
# glm_to_sql is a code that me and my colleagues created

sql_code = glm_to_sql(model)

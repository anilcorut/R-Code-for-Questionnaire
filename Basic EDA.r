
library(ggplot2)
library(dplyr)

#Telco Data Analysis

raw_data <- read.csv(file = "WA_Fn-UseC_-Telco-Customer-Churn.csv")



str(raw_data)

raw_data$SeniorCitizen <- as.factor(raw_data$SeniorCitizen)
levels(raw_data$SeniorCitizen) <- c("No","Yes")


ggplot(raw_data, aes(Partner, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Partner Status", 
       x = "Does the Customer have a Partner?", 
       y = "Count")


ggplot(raw_data, aes(Dependents, fill = Churn)) + 
  geom_bar() +
  labs(title = "Customer Dependents Status", 
       x = "Does the Customer have Dependents?", 
       y = "Count")



ggplot(raw_data, aes(tenure, fill = Churn)) + 
  geom_histogram() +
  labs(title = "Customer Tenure Histogram",
       x = "Length of Customer Tenure", 
       y = "Count")



ggplot(raw_data, aes(MonthlyCharges, fill = InternetService)) + 
  geom_histogram() +
  labs(title = "Monthly Charges Histogram By Internet Service Option",
       x = "Monthly Charge to Customer", 
       y = "Count")



Churn_Bill = raw_data %>%
  filter(tenure > 1) %>%
  mutate(AverageBill = (TotalCharges-MonthlyCharges)/tenure,
         BillChange = MonthlyCharges - AverageBill)


ggplot(Churn_Bill, aes(BillChange)) + 
  geom_histogram(bins = 20) +
  labs(title = "Histogram of Bill Changes", 
       x = "Change in Bill", 
       y = "Count")


ggplot(Churn_Bill, aes(y = BillChange, x = tenure, color = Churn)) + 
  geom_point() +
  labs(title = "Change in Current Bill From Past Averge BIll", 
       y = "Change in Bill", 
       x = "tenure")



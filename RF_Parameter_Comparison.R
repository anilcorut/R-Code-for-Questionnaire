
#RANDOM FOREST PARAMETER COMPARISON

n_tree <- c(50,100,250,500,1000)
m_try <- c(3,5,7,9)
node_size <- c(250,500,750,1000)

func <- function(n_tree = )


SummaryTable <- data.frame(ntree=integer(),
                           mtry=integer(),
                           nodesize=integer(),
                           duration=double(),
                           gini_train=double(),
                           gini_test=double())

x <- 1
y <- 1
z <- 1
count <- 1

for (x in 1:length(n_tree))
{
  for (y in 1:length(m_try))
    
  {
    
    for (z in 1:length(node_size))
    {
      
      tic <- Sys.time()
      
      model <- randomForest(CC_TARGET_FLAG~.,
                            data= train_set,
                            ntree = n_tree[x],
                            mtry = m_try[y],
                            nodesize = node_size[z])
      toc <- Sys.time()
      
      code_duration <- as.double(toc-tic)
      
      
      predict_train <- predict(model,train_set,type="response")
      predict_test <- predict(model,test_set,type="response")
      
      
      roccurve_train <- roc(train_set$TARGET_FLAG ~ predict_train)
      roccurve_test <- roc(test_set$TARGET_FLAG ~ predict_test)
      
      gini_train <- 2*auc(roccurve_train) - 1
      gini_test <- 2*auc(roccurve_test) - 1
      
      SummaryTable[count,] <- data.frame(n_tree[x], m_try[y], node_size[z],
                                     code_duration, gini_train, gini_test)
      count <- count + 1

      
    }  
  }
}


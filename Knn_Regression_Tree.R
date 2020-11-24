#Required packages list:
library(caret)
library(adabag)
library(rpart)
library(randomForest)
library(rpart.plot)

#function to create partitions for any dataset 

create.partition <- function(dataset,no_of_partition){
  set.seed(1) 
  partition.list<-list()
  test.rows<-c();
  if(no_of_partition == 2){
    #partitioning into training (60%) and validation (40%)
    train.rows <- sample(rownames(dataset), dim(dataset)[1]*0.6)
    # assign row IDs that are not already in the training set, into validation 
    valid.rows <- setdiff(rownames(dataset), train.rows) 
  }
  #else divide into three partitions:#training  Data-  (50%), #validation Data- (30%), #Test Data -(20%)
  else
  {
    train.rows <- sample(rownames(dataset), dim(dataset)[1]*0.5)
    valid.rows <- sample(setdiff(rownames(dataset), train.rows), dim(dataset)[1]*0.3)
    test.rows <- setdiff(rownames(dataset), union(train.rows, valid.rows))
  }
  #Assigning the calculated rows above to the respective variables
  train.data <- dataset[train.rows, ]
  valid.data <- dataset[valid.rows, ]
  test.data<-dataset[test.rows, ]
  
  #Adding the above partitions into a list and returning to the function call
  partition.list$valid.data<-valid.data
  partition.list$train.data<-train.data
  partition.list$test.data<-test.data
  return (partition.list)
}


#function to convert categorical variables to factors
create.factors<-function(colnames,dataset){
  dataset[colnames] <- lapply(dataset[colnames], factor) 
  return(dataset)
  
}

#function to create knn model
create.knnmodel<-function(train.data,test.data,response.variable,k.value){
  
  #get column index of the response variable
   response.variable=as.name(response.variable)
   index=which(colnames(train.data) == response.variable) 
  
  #get the response variable values
  train.response<-train.data[,index]
  
  #get all the predictors
  train.predictor<-train.data[,-index]
  
  #Remove the response variable column in the valid data if the column is present
  if(any(names(test.data) == response.variable)){
    test.predictor<-test.data[,-index]
  }
  #No need to remove response variable column if its not present in the dataset
  else{
    test.predictor<-test.data
  }
  
  #performing KNN using class package which handles all the categorical variables
  knn.fit <- class::knn(train = train.predictor,test=test.predictor,
                        cl=train.response,k=k.value)
  #predicted class of the new data
  knn.fit
  summary(knn.fit) 
  return(knn.fit)
}

#function to check performance of  classification tress

check.ct.performance<-function(train.data,valid.data,response.variable,cmodel){
  response.variable=as.name(response.variable)
  index=which(colnames(train.data) == response.variable) 
  
  #checking models performance on training  data
  print(".....................................Performance on the training data...................................")
  pred.train <- predict(cmodel,train.data,type = "class")
  print(confusionMatrix(pred.train,train.data[,index]))
 
  #checking models performance on validation  data
  
  print(".....................................Performance on the validation data...................................")
  pred.valid <- predict(cmodel,valid.data,type = "class")
  print(confusionMatrix(pred.valid,valid.data[,index]))
}

#function to preprocess data for creating KNN models.
preparedata.knn<-function(dataset,no.partition){
  dataset.list=list()
  
  #Pre-processing of the data before training the model
  #Categorical columns-"Family","Education","Personal Loan","Securities Account","CD Account","Online","CreditCard"
  factor.columns<-c(4,6,seq(8,12))
  dataset <-create.factors(factor.columns,dataset)

  #partitioning the dataset
  partitioning.list<-create.partition(dataset,no.partition)
  bank.training.data<-partitioning.list$train.data
  bank.valid.data<-partitioning.list$valid.data
  bank.test.data<-partitioning.list$test.data
  
  print(dim(bank.training.data))
  print(dim(bank.valid.data))
  print(dim(bank.test.data))
  
  #Standardizing numerical columns of the dataset
  bank.training.norm.data <- bank.training.data
  bank.valid.norm.data <- bank.valid.data
  bank.test.norm.data <- bank.test.data
  bank.copy.norm.dataset <- dataset
  
  #1-5:Age,exp,Income,Family,CCAvg,Mortgage
  scale.columns<-c("Age","Experience","Income","CCAvg","Mortgage")
  norm.values <- preProcess(bank.training.data[, scale.columns], method=c("center", "scale"))
  bank.training.norm.data[, scale.columns] <- predict(norm.values, bank.training.data[, scale.columns])
  head(bank.training.norm.data)
  
  ##Similarly standardize valid data, the complete dataset and the new data or the test data
  bank.valid.norm.data[, scale.columns] <- predict(norm.values, bank.valid.data[,scale.columns])

  #we will have the test data from above partition call only when we are dividing into 3 partitions.
  #Therefore before standardizing test data;we are checking if it has any rows
  if(nrow(bank.test.norm.data!=0)){
    bank.test.norm.data[, scale.columns] <- predict(norm.values, bank.test.data[,scale.columns])
    }
  bank.copy.norm.dataset[, scale.columns] <- predict(norm.values, dataset[, scale.columns])
  
  #Assigning above values to specific variables and returning to the function call
  
  dataset.list$train.data<-bank.training.norm.data
  dataset.list$valid.data<-bank.valid.norm.data
  dataset.list$test.data<-bank.test.norm.data
  dataset.list$norm.dataset<-bank.copy.norm.dataset
  dataset.list$norm.values<-norm.values
  return(dataset.list)
}

#..................................................Problem 1 start....................................................................

#1. Use Universal Bank dataset. Note that Personal.Loan is the outcome variable of interest.

#a. Perform a k-NN classification with k=3 for a new data (You are welcome to choose your
    #own values for the new data. Please clearly state it in your report).

#Reading the csv file
bank.dataset <- read.csv("UniversalBank.csv", header = TRUE)

summary(bank.dataset)
#Removing ID and ZIP CODE values as those won't be helpful in classifying a new record
bank.copy.dataset <- bank.dataset[,-c(1,5)]

#preprocessing the data

knn.dataset<-preparedata.knn(bank.copy.dataset,2)

knn.train.data<-knn.dataset$train.data
knn.valid.data<-knn.dataset$valid.data
knn.test.data<-knn.dataset$test.data
knn.norm.values<-knn.dataset$norm.values
knn.norm.dataset<-knn.dataset$norm.dataset

head(knn.train.data)
head(knn.valid.data)
head(knn.test.data)
head(knn.norm.dataset)

#preparing the test  data 

#Categorical columns-"Family","Education","Personal Loan","Securities Account","CD Account","Online","CreditCard"
new.data <- data.frame(Age  = 40,Experience= 10,Income =84, Family=2, CCAvg=2, Education=2, Mortgage=0, 
                       Securities.Account=0, CD.Account=0, Online= 0, CreditCard= 1)
new.norm.df<-new.data
#converting categorical variables of test data to factors
factor.columns<-c(4,6,seq(8,11))
new.norm.df<-create.factors(factor.columns,new.norm.df)

scale.columns<-c("Age","Experience","Income","CCAvg","Mortgage")
#scaling continuous variables.
new.norm.df[,scale.columns] <- predict(knn.norm.values, new.norm.df[, scale.columns])
head(new.norm.df)

#performing KNN using class package which handles all the categorical variables
knn.fit<-create.knnmodel(knn.train.data,new.norm.df,"Personal.Loan",3)
#predicted class of the new data
knn.fit



#b. Identify the best k. Why do you think this is the best? (Hint: Explain what happens if k )

banking.accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
banking.accuracy.df

# compute knn for different k on validation.
for(i in 1:14) {
  #Use knn function with k=i and predict for valid dataset
         banking.knn.pred<-create.knnmodel(knn.train.data,knn.valid.data,"Personal.Loan",i)
         banking.accuracy.df[i, 2] <- confusionMatrix(banking.knn.pred,knn.valid.data$Personal.Loan)$overall[1] 
}
banking.accuracy.df

maxk.value=banking.accuracy.df[which.max(banking.accuracy.df[,"accuracy"]),"k"]

#k=3 shows maximum accuracy-0.9625

#c. Calculate accuracy, sensitivity, and specificity for your validation data using the best k
#(from part b) without calling or using the confusion matrix (that is, compute them directly from the validation data) and verify your computation by directly calling the
#confusion matrix using R (This is to just make you better in using R).


knn.fit2 <- create.knnmodel(knn.train.data,knn.valid.data,"Personal.Loan",maxk.value)
confusionMatrix(knn.fit2,knn.valid.data$Personal.Loan)

#Calculating specificity and sensitivity
temp.table<-table(knn.valid.data$Personal.Loan,knn.fit2)
print(temp.table)
#temp.table[1, "0"]-->1st row having column-name as "0"
sensitivity.value<-temp.table[1, "0"]/(temp.table[1, "0"]+temp.table[1, "1"])
specificity.value<-temp.table[2, "1"]/(temp.table[2, "1"]+temp.table[2, "0"])
cat("specificity is :",specificity.value)
cat("sensitivity  is :",sensitivity.value)



#d Partition your dataset into 3. Compare the accuracy metrics from part c for both validation data and test data. Summarize your results.

#Here we want 3 partitions,so we pass 3 in the function argument
knn.dataset<-preparedata.knn(bank.copy.dataset,3)

knn.train.data<-knn.dataset$train.data
knn.valid.data<-knn.dataset$valid.data
knn.test.data<-knn.dataset$test.data
knn.norm.values<-knn.dataset$norm.values
knn.norm.dataset<-knn.dataset$norm.dataset

head(knn.train.data)
head(knn.valid.data)
head(knn.test.data)
head(knn.norm.dataset)

#Finding best k value for this dataset

banking.accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
banking.accuracy.df

# compute knn for different k on validation.
for(i in 1:14) {
  #Use knn function with k=i and predict for valid dataset
  banking.knn.pred<-create.knnmodel(knn.train.data,knn.valid.data,"Personal.Loan",i)
  banking.accuracy.df[i, 2] <- confusionMatrix(banking.knn.pred,knn.valid.data$Personal.Loan)$overall[1] 
}
banking.accuracy.df

#finding the optimal k value
maxk.value=banking.accuracy.df[which.max(banking.accuracy.df[,"accuracy"]),"k"]

#giving accuracy of 96.85% for k=3

#predicting for valid data
knn.fit3<- create.knnmodel(knn.train.data,knn.valid.data,"Personal.Loan",maxk.value)
confusionMatrix(knn.fit3,knn.valid.data$Personal.Loan)


#predicting for test data
knn.fit4 <- create.knnmodel(knn.train.data,knn.test.data,"Personal.Loan",maxk.value)
confusionMatrix(knn.fit4,knn.test.data$Personal.Loan)

#On test data accuracy and specificity is the lowest


#..................................................Problem 1 end....................................................................



#..................................................Problem 2 start....................................................................

# a.Create a classification tree (you may use the default one that we got in class).

#Pre-processing the data-Factor conversion is required for categorical variables

factor.columns<-c(4,6,seq(8,12))
bank.copy.dataset <-create.factors(factor.columns,bank.copy.dataset)

#Divide the dataset into partition
partition.list<-create.partition(bank.copy.dataset,2)
ct.train.df<-partition.list$train.data
ct.valid.df<-partition.list$valid.data

#creating ct model
bank.ctmodel <- rpart(Personal.Loan ~ ., data = ct.train.df, method = "class",
                      control = rpart.control(minsplit=2, maxdepth = 6))

#plotting the above tree
prp(bank.ctmodel, type = 1, extra = 1, split.font = 1, varlen = -10) 

check.ct.performance(ct.train.df,ct.valid.df,"Personal.Loan",bank.ctmodel)




#b.

#Go with Trees as accuracy is more.Trees also let us know the variable importance 
#and also works better with categorical variables as compared to KNN.
bank.ctmodel$variable.importance


#..................................................Problem 2 end....................................................................




#..................................................Problem 3 start....................................................................

#a. Use all the predictors to create a classification tree. Set minbucket=50 (ensures the
#terminal nodes have at least 50 items) and maxdepth=7 (ensures the tree is at most 7
                                                       #nodes deep). Describe any one of the rules based on the tree

#Reading the csv file
ebay.dataset <- read.csv("eBayAuctions.csv", header = TRUE)

#preprocess the data:

names(ebay.dataset) <- gsub("\\.","",names(ebay.dataset))
factor.columns=c("Competitive","Category","currency","endDay","Duration")
ebay.dataset<-create.factors(factor.columns,ebay.dataset)

#partitioning the dataset
partition.list<-create.partition(ebay.dataset,2)
ebay.ct.train.df<-partition.list$train.data
ebay.ct.valid.df<-partition.list$valid.data


#Creating a classification tree model using all the predictors
ebay.ctmodel <- rpart(Competitive  ~ ., data = ebay.ct.train.df, method = "class",
                      control = rpart.control(minbucket = 50, maxdepth = 7),)
prp(ebay.ctmodel, type = 1, extra = 1, split.font = 1, varlen = -10) 



#b.Is your tree above helpful for prediction of a new auction? If not, how would you change
#your approach? Develop a classification tree that will be helpful for predicting a new


#checking performance of the above model
check.ct.performance(ebay.ct.train.df,ebay.ct.valid.df,"Competitive",ebay.ctmodel)

print(ebay.ctmodel$variable.importance)
#Above execution shows that ClosePrice is the most important feature which most probably won't be available to seller before the auction.
#ALso ;Accuracy is very low:WE have to prune the tree.
#So Dropping the ClosePrice predictor while creating the trees

#To increase the accuracy  of above tree ;we will do cross validation and choose the  cp value  corresponding to the lowest
#xerror on validation data

#pruning after considering the lowest xerror value
ebay.ct <- rpart(Competitive ~ .-ClosePrice, data = ebay.ct.train.df, method = "class", 
                 cp = 0.00001, minsplit = 10,xval=5,control = rpart.control(minbucket = 50, maxdepth = 7))  

printcp(ebay.ct)
plotcp(ebay.ct)

ebaypruned.ct <- prune(ebay.ct, 
                       cp = ebay.ct$cptable[which.min(ebay.ct$cptable[,"xerror"]),"CP"])


#checking performance of tree after pruning
check.ct.performance(ebay.ct.train.df,ebay.ct.valid.df,"Competitive",ebaypruned.ct)
#Accuracy decreased to 69% after excluding the ClosePrice.

#Find the length of the pruned tree
length(ebaypruned.ct$frame$var[ebaypruned.ct$frame$var == "<leaf>"])

#Plot the pruned tree
prp(ebaypruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(ebaypruned.ct$frame$var == "<leaf>", 'gray', 'white'))

#printing predictor Variable importance 

print(ebaypruned.ct$variable.importance)


#Checking randomForest performance

ebay.rf <- randomForest(Competitive~ .-ClosePrice, data = ebay.ct.train.df, ntree = 500, 
                       mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(ebay.rf, type = 1)

#checking performance of random forest model:
check.ct.performance(ebay.ct.train.df,ebay.ct.valid.df,"Competitive",ebay.rf)

#Accuracy increased to 70% from 69%(tree after pruning)


#Lets evaluate the variable importance
ebay.defaultrf<-randomForest(Competitive~ .-ClosePrice, data = ebay.dataset, ntree = 500, 
                             mtry = 4, nodesize = 5, importance = TRUE) 
varImpPlot(ebay.defaultrf,type=1)

#Relation between outcome and predictors
partialPlot(ebay.defaultrf,ebay.dataset,OpenPrice,"1")
partialPlot(ebay.defaultrf,ebay.dataset,Category,"1")
partialPlot(ebay.defaultrf,ebay.dataset,currency,"1")


# Implementing boosted trees

ebay.boost <- boosting(Competitive~ .-ClosePrice, data = ebay.ct.train.df)
ebay.boost.pred.valid <- predict(ebay.boost,ebay.ct.valid.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(as.factor(ebay.boost.pred.valid$class), as.factor(ebay.ct.valid.df$Competitive))

#Accuracy increased to 70.8% from 70%(in random forest)

#Boosting algorithm can predict more accurately as compared to other options as discussed above


#c Based on the trees you have developed summarize your recommendations for your seller-friend.


#Lets check with variables chosen by the seller

# Seller chose variables-duration, opening price, currency and endDay are


ebay.selected.boost <- boosting(Competitive ~ Duration+OpenPrice+endDay+currency, data = ebay.ct.train.df)
ebay.selected.boost.pred.valid <- predict(ebay.selected.boost,ebay.ct.valid.df,type = "class")

# generate confusion matrix for validation data
confusionMatrix(as.factor(ebay.selected.boost.pred.valid$class), as.factor(ebay.ct.valid.df$Competitive))

#Accuracy dropped by 3% if we select the predictors chosen by the seller.

#WE can select different set of predictors and see if there is improvement in accuracy
#Replacing Currency with Category-
#Adding sellerRating
ebay.selected.boost2 <- boosting(Competitive ~ Duration+OpenPrice+endDay+Category+sellerRating, data = ebay.ct.train.df)
ebay.selected.boost.pred.valid2 <- predict(ebay.selected.boost2,ebay.ct.valid.df,type = "class")

# generate confusion matrix for validation data
confusionMatrix(as.factor(ebay.selected.boost.pred.valid2$class), as.factor(ebay.ct.valid.df$Competitive))

#Accuracy increased to 71% after including the above predictors
#..................................................Problem 3 end....................................................................

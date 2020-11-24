#Required packages list:

#install.packages("scales")#required for scale() function
library(scales)
#install.packages("dummies") #required to create dummy variables for categorical variable
library(dummies) 
#required to create plots
library(ggplot2)
#for predicting y values for the validation data
library(forecast)
# for Creating Pivot Tables 
library(reshape) 
#for exhaustive search functionality
library(leaps)

#function to create partitions for any dataset 
create.partition <- function(dataset){
  set.seed(1) 
  partition.list<-list()
  
  #partitioning into training (60%) and validation (40%)
  train.rows <- sample(rownames(dataset), dim(dataset)[1]*0.6)
  valid.rows <- setdiff(rownames(dataset), train.rows) 
  
  #Assigning the  above calculated rows to the respective variables
  train.data <- dataset[train.rows, ]
  valid.data <- dataset[valid.rows, ]
  
  
  #Adding the above partitions into a list and returning to the function call
  partition.list$valid.data<-valid.data
  partition.list$train.data<-train.data
  
  return (partition.list)
}



#function to create factors for the categorical variables

create.factors<-function(colnames,dataset){
  dataset[colnames] <- lapply(dataset[colnames], factor) 
  return(dataset)
  
}


#function to perform regression and calculate accuracy measurements
perform.regression<-function(dataset){
  
  print("...................Model for following  variables  start:.......................")
  
  #Printing the predictors of the Models
  for( column in colnames(dataset)){
    cat(paste(column," + "))
  }
  
  #Calling create.partition() to divide the dataset into 2 parts-training set and validation set
  airfaremodel.partition<-create.partition(dataset)
  airfaremodel.valid.data<-airfaremodel.partition$valid.data
  airfaremodel.train.data<-airfaremodel.partition$train.data
  
  #performing regression
  airfaremodel.regressor <- lm(FARE ~ .,data=airfaremodel.train.data)
  print(summary(airfaremodel.regressor))
  
  #performing prediction on validation data
  airfaremodel.pred <- predict(airfaremodel.regressor, newdata = airfaremodel.valid.data)
  airfaremodel.valid.res <- data.frame(airfaremodel.valid.data$FARE, airfaremodel.pred , residuals =airfaremodel.valid.data$FARE - airfaremodel.pred) 
  print(head(airfaremodel.valid.res))
  
  #calculating accuracy measures on the validation data
  airfaremodel.accuracy=accuracy(airfaremodel.pred,airfaremodel.valid.data$FARE)
  print(airfaremodel.accuracy)
  
  cat("..............................Model ends.............................................")
  
}



#..................................................Problem 1 start....................................................................

# Importing the dataset
toyota.dataset = read.csv('ToyotaCorolla.csv')

#Remove Id ,Model Type,and Quarterly tax columns in the dataset as we are sure these variables wont effect the price
toyota.copy.dataset <- toyota.dataset[-c(1,2,17)]

#Create dummies for the variable Fuel Type. Use the dummy variables petrol and diesel and one other
#variable of your choice to explain price. Call this Model 1

toyota.dummy.dataset <- dummy.data.frame(toyota.copy.dataset,names = c("Fuel_Type"), sep = ".")

selected.var1 <- c("Price","Fuel_Type.Petrol", "Fuel_Type.Diesel","HP")

#Partitioning the data
model1.dataset<-toyota.dummy.dataset[, selected.var1]
model1.partition<-create.partition(model1.dataset)
model1.valid.data<-model1.partition$valid.data
model1.train.data<-model1.partition$train.data

#Performing regression by creating regression model with the training data
model1.regressor <- lm(Price ~ .,data=model1.train.data)
summary(model1.regressor)

#Predicting price values over the validation data
model1.pred <- predict(model1.regressor, newdata = model1.valid.data)

#Comparing the predicted and the original Price values and displaying the corresponding residual values
model1.valid.res <- data.frame(model1.valid.data$Price, model1.pred, residuals = 
                                 model1.valid.data$Price - model1.pred)
head(model1.valid.res)

#Calculating accuracy over the validation data
accuracy(model1.pred,model1.valid.data$Price)


#Use the dummy variables petrol and CNG and the variable you chose before to explain price-Model2

selected.var2 <- c("Price","Fuel_Type.Petrol", "Fuel_Type.CNG","HP")
model2.dataset<-toyota.dummy.dataset[, selected.var2]

#Partitioning the data
model2.partition<-create.partition(model2.dataset)
model2.valid.data<-model2.partition$valid.data
model2.train.data<-model2.partition$train.data

#Performing regression
model2.regressor <- lm(Price ~ .,data=model2.train.data)
summary(model2.regressor)

#Predicting price values over the validation data
model2.pred <- predict(model2.regressor, newdata = model2.valid.data)
model2.valid.res <- data.frame(model2.valid.data$Price, model2.pred, residuals = 
                             model2.valid.data$Price - model2.pred)
head(model2.valid.res)

#Calculating accuracy over the validation data set
accuracy(model2.pred,model2.valid.data$Price)

#Both the above models will have the same adjusted R square values and same RMSE.


#b. In class we treated some categorical variables (like automatic) as continuous variables.
#Try treating the variables (not just automatic, but all the variables) appropriately (based
#on the variable definition), and perform the analysis (that is, develop a model to predict

selected.var3 <-c("Price","Age_08_04","KM","HP","Met_Color","Automatic","CC","Doors","Weight") 

#combining all the variables which needs to be converted to factors into a vector
factor.columns<-c("Met_Color","Automatic","Doors")

#Calling create.factor() function to convert categorical variables to factor variables
toyota.copy.dataset<-create.factors(factor.columns,toyota.copy.dataset)
head(toyota.copy.dataset)

model3.factor.dataset<-toyota.copy.dataset[,selected.var3]

#partitioning data before calling lm()
model3.partition<-create.partition(model3.factor.dataset)
model3.valid.data<-model3.partition$valid.data
model3.train.data<-model3.partition$train.data

#passing the training data to lm()
options(scipen = 999)
model3.regressor <- lm(Price ~ .,data=model3.train.data)
summary(model3.regressor)

#checking the performance of the model  on the validation data
model3.pred <- predict(model3.regressor, newdata = model3.valid.data)
model3.valid.res <- data.frame(model3.valid.data$Price, model3.pred, residuals = 
                                 model3.valid.data$Price - model3.pred)

head(model3.valid.res)
#calculating accuracy measures based on above performance
accuracy(model3.pred,model3.valid.data$Price)


#As compared to model created in Class(without converting categorical variables to factor),the model created
#(after converting the categorical variables into factors) has lesser RMSE which conveys that it is more accurate in predicting the values

#.....................................................................................................

#Taking all categorical variables and converting it to factor and passing in lm function
#Not converting cylinder  variable to factor as it has only 1 distinct value
#Not converting  Guarantee_Period column to factor as its values are very much skewed

#WE are performing this exercise just to observe how the model behaves if we include all the variable in the model
factor.columns<-c(6,14,16,17,19:ncol(toyota.copy.dataset))
toyota.allfactors.dataset<-create.factors(factor.columns,toyota.copy.dataset)


#partitioning data before calling lm()
model4.partition<-create.partition(toyota.allfactors.dataset)
model4.valid.data<-model4.partition$valid.data
model4.train.data<-model4.partition$train.data

#passing the training data to lm()
model4.regressor <- lm(Price ~ .,data=model4.train.data)
summary(model4.regressor)

#predicting above model on the validation data
model4.pred <- predict(model4.regressor, newdata = model4.valid.data)
model4.valid.res <- data.frame(model4.valid.data$Price, model4.pred, residuals = 
                                 model4.valid.data$Price - model4.pred)

head(model4.valid.res)
#calculating accuracy measures on the validation data
accuracy(model4.pred,model4.valid.data$Price)



#..................................................Problem 1 end.......................................................




#..................................................Problem 2 start.......................................................


#2. Use Airfares dataset. You may ignore the first 4 variables.

#a. Explore the relationship between FARE and other numerical predictors. Summarize your observations

#Loading the Airfares.csv file 
airfares.dataset = read.csv('Airfares.csv')

#Removing first 4 columns of the dataset as per the question.
airfares.dataset=airfares.dataset[,5:ncol(airfares.dataset)]
airfares.copy.dataset=airfares.dataset

#Combining the columns containing numerical values into a single vector
numerical.cols<- c("FARE","HI", "S_INCOME","E_INCOME","S_POP","E_POP","COUPON","DISTANCE","PAX")

#Selecting rows only for above columns
airfares.numeric.dataset <- airfares.dataset[, numerical.cols]

# computing  mean, standard dev., min, max, median, length, and missing values for all the above variables
data.frame(mean=sapply(airfares.numeric.dataset, mean), 
           sd=sapply(airfares.numeric.dataset, sd), 
           min=sapply(airfares.numeric.dataset, min), 
           max=sapply(airfares.numeric.dataset, max), 
           median=sapply(airfares.numeric.dataset, median), 
           length=sapply(airfares.numeric.dataset, length),
           miss.val=sapply(airfares.numeric.dataset, function(x) 
             sum(length(which(is.na(x))))))

#Using cor() function to interpret the relationship between FARE and other numerical predictor variables
round(cor(airfares.numeric.dataset),2)

#Creating heatmap between FARE and other numerical predictor variables
cor.mat <- round(cor(airfares.numeric.dataset),2)
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x=X1, y=X2, fill =value))+geom_tile()+geom_text(aes(x=X1, y=X2, label = value))

#Creating scatterplot for Fare vs Distance--We chose distance as its correlation coefficient with Fare is the largest

vacation.status=airfares.dataset$VACATION
qplot(DISTANCE,FARE, data = airfares.copy.dataset,
      xlab = "Distance", ylab = "Fare",color=vacation.status,
      main = "Fare vs Distance")


#Creating scatterplot for FARE vs E_Income

vacation.status=airfares.dataset$VACATION
qplot(E_INCOME ,FARE, data = airfares.copy.dataset,
      xlab = "Ending city Income", ylab = "Fare",color=vacation.status,
      main = "Fare vs Ending city Income")


# b.Using pivot tables analyze the effect of categorical predictors on FARE.


#Analyzing the interaction between FARE and VACATION status 
aggregate(airfares.dataset$FARE, by=list(VACATION=airfares.dataset$VACATION),
                                        FUN=mean) 

#Analyzing the interaction between FARE and SW status(if SW serves the route)
aggregate(airfares.dataset$FARE, by=list(SW=airfares.dataset$SW),
          FUN=mean) 

#Analyzing the combined effect of VACATION and SW status on FARE
aggregate(airfares.dataset$FARE, by=list(VACATION=airfares.dataset$VACATION, 
                                 SW=airfares.dataset$SW), FUN=mean) 
vac.sw.mlt <- melt(airfares.dataset, id=c("VACATION", "SW"), measure=c("FARE"))
head(vac.sw.mlt, 5)
# use cast() to reshape data and generate pivot table
cast(vac.sw.mlt, VACATION ~ SW, subset=variable=="FARE", margins=c("grand_row", "grand_col"), mean)


#Similarly we can find the behavior of  other categorical variables as well.

#Analyzing the interaction between FARE and SLOT status
aggregate(airfares.dataset$FARE, by=list(SLOT=airfares.dataset$SLOT),
          FUN=mean) 

#Analyzing the interaction between FARE and GATE status
aggregate(airfares.dataset$FARE, by=list(GATE=airfares.dataset$GATE),
          FUN=mean) 

#Analyzing the combined effect of SLOT and GATE status on FARE
aggregate(airfares.dataset$FARE, by=list(SLOT=airfares.dataset$SLOT, 
                                         GATE=airfares.dataset$GATE), FUN=mean) 

slot.gate.mlt <- melt(airfares.dataset, id=c("SLOT", "GATE"), measure=c("FARE"))
head(slot.gate.mlt, 5)
cast(slot.gate.mlt, SLOT ~ GATE, subset=variable=="FARE", 
     margins=c("grand_row", "grand_col"), mean)


#c. Develop a model to predict FARE. Summarize the accuracy measures based on the
#validation data. What is the final model you would recommend to predict FARE?

#Converting  NEW (categorical variable) to factor variables

airfares.copy.dataset<-create.factors(c("NEW"),airfares.copy.dataset)

#No need to explicitly create dummies;lm function handles it 

#Partitioning the airfare dataset into training and validation data
airfaremodel.partition<-create.partition(airfares.copy.dataset)
airfaremodel.valid.data<-airfaremodel.partition$valid.data
airfaremodel.train.data<-airfaremodel.partition$train.data

dim(airfaremodel.train.data)
dim(airfaremodel.valid.data)


#Passing training data into the regsubsets function to get the possible models for this dataset
models.list <- regsubsets(FARE ~ ., data = airfaremodel.train.data, nbest = 1, nvmax = dim(airfaremodel.train.data)[2],
                     method = "exhaustive")
models.list.summary<- summary(models.list)
print(models.list.summary$which)
# showing Ad.Rsquare values for each model
print(models.list.summary$adjr2)
print(models.list.summary$cp)


#we can select upto 3-4 models from above based on adjusted R square and then check its performance over validation data
#row 9 t0 11 shows the best possible model in the  output of above command-(print(models.list.summary$which)).


#Model corresponding to row 9:-(Model with 9 variables)

selected.column1<-c("FARE","VACATION","SW","HI","E_INCOME","S_POP","E_POP","DISTANCE","PAX","GATE")

#Calling perform.regression() to perform regression and check accuracy parameters on the validation data
perform.regression(airfares.copy.dataset[,selected.column1])


#For Model 2(Model with 10 variables)

#Adding "S_INCOME" to the above model
selected.column2<-append(selected.column1,"S_INCOME")
perform.regression(airfares.copy.dataset[,selected.column2])



#For Model 3(Model with 11 variables)

#Adding "SLOT" to the above model
selected.column3<-append(selected.column2,"SLOT")
perform.regression(airfares.copy.dataset[,selected.column3])

#Model 3(with 11 Variables) RMSE is the lowest

#Finding models using other algorithms-backward

airfaremodel.regressor <- lm(FARE ~ .,data=airfaremodel.train.data)
airfaremodel.lm.bkwd <- step(airfaremodel.regressor , direction = "backward")
summary(airfaremodel.lm.bkwd) 

#checking the performnace of the model over the validation data
airfaremodel.lm.predbkwd <- predict(airfaremodel.lm.bkwd, airfaremodel.valid.data)
airfaremodel.res.bkwd <- data.frame(airfaremodel.valid.data$FARE, airfaremodel.lm.predbkwd , residuals =airfaremodel.valid.data$FARE - airfaremodel.lm.predbkwd) 
print(head(airfaremodel.res.bkwd))

#Calculating accuracy parameters based on above prediction
accuracy(airfaremodel.lm.predbkwd, airfaremodel.valid.data$FARE)

#This algorithm also gives a model with 11 predictors and same RMSE as that of Exhaustive search.


#Finding models using other algorithms-forward

# create model with no predictors
airfaremodel.lm.null <- lm(FARE~1, data = airfaremodel.train.data)
# use step() to run forward regression.
airfaremodel.lm.fwd <- step(airfaremodel.lm.null, scope=list(lower=airfaremodel.lm.null, upper=airfaremodel.regressor), direction = "forward")
summary(airfaremodel.lm.fwd) 

#Predicting fare values on the validation data
airfaremodel.lm.predfwd <- predict(airfaremodel.lm.fwd, airfaremodel.valid.data)

#Finding the difference between the observed and predicted fare values of the validation data
airfaremodel.res.fwd <- data.frame(airfaremodel.valid.data$FARE, airfaremodel.lm.predfwd , residuals =airfaremodel.valid.data$FARE - airfaremodel.lm.predfwd) 
print(head(airfaremodel.res.fwd))

#Calculating accuracy parameters based on above prediction
accuracy(airfaremodel.lm.predfwd, airfaremodel.valid.data$FARE)

#THe FORWARD algorithm is also providing the same model with the same accuracy parameters as that of BACKWARD and EXHAUSTIVE.

#ALl the algorithms are  providing the same model(with 11 possible predictors) ---Coupons and NEW are excluded from this model.


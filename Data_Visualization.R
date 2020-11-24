
#Required packages list:
#install.packages("scales")#required for scale() function
library(scales)
#install.packages("dummies") #required to create dummy variables for categorical variable
library(dummies) 
library(ggplot2)#required to create plots
#........................................................................... Problem 1 start...................................................

#Q 1. Use ToyotaCorolla dataset. Prepare the dataset by doing the following

#Solution:

#Reading the csv files using read.csv()
toyota.dataset<-read.csv("ToyotaCorolla.csv",header = TRUE)
#Checking the dimension to know the no of rows and columns in the dataset
dim(toyota.dataset)

#1.a Summarize the dataset. (Do not just put the R output. Make observations based on(numbers)

#Solution:

summary(toyota.dataset)

#Plots and their respective inferences are  mentioned in the pdf doc.


#1.b Normalize the variable kilometers

#Data summary of column KM before normalization
summary(toyota.dataset$KM) 

toyota.dataset[,7]=scale(toyota.dataset[,7])#kms is the column 7
head(toyota.dataset$KM)

#Data summary of column KM after normalization
summary(toyota.dataset$KM) 

#1.c Create dummies for the variable Fuel Type.

#Passing specific column names to create dummies for that categorical variable
toyota.dataset <- dummy.data.frame(toyota.dataset,names = c("Fuel_Type"), sep = ".")

#check the new dimension.We will get 2 extra columns as we have three distinct values for this column
dim(toyota.dataset)

head(toyota.dataset)


#1.d Partition the data into three sets similar to what we did in class.
set.seed(1)#IT is done so that  same random sample is generated every time.
#training  Data-  (50%), 
#validation Data- (30%), 
#Test Data -      (20%)
train.rows <- sample(rownames(toyota.dataset), dim(toyota.dataset)[1]*0.5)

# sample 30% of the row IDs into the validation set, drawing only from records not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(toyota.dataset), train.rows), 
                     dim(toyota.dataset)[1]*0.3)

# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(toyota.dataset), union(train.rows, valid.rows))

#selecting all the above rows and assigning them to respective dataframes
train.data <- toyota.dataset[train.rows, ]
valid.data <- toyota.dataset[valid.rows, ]
test.data <- toyota.dataset[test.rows, ]


#........................................................................... Problem 1 end...................................................


#........................................................................... Problem 2 start...................................................

# Q 2. Use ApplianceShipments dataset. It contains quarterly shipments data on US household appliances.

#Solution:

#2.a Create a time plot.

appliance.shipments.df<-read.csv("ApplianceShipments.csv")

#Remove the date column
temp.df<-appliance.shipments.df[,2]
temp.df<-na.omit(temp.df)#Omit all the NA values if present

#Creating a ts object which contains shipment values of each quarter across a year.
quartely.shipment.ts <- ts(temp.df, start = c(1985,1), end = c(1989,4), freq = 4)#frequency 4 is used as the shipments values are given per Quarter.
plot(quartely.shipment.ts , xlab = "Year", ylab = "Shipments (in units)", ylim = c(3800, 5000))


#2.b Is there a quarterly pattern? Zoom in to the range of 3500-5000 on the y-axis

# zoom in--In window function we can zoom in shipment values for specific time period.Going with the entire duration(1985-1989)
#as nothing is being mentioned in the question.Assigning ylim argument to  the required range as per the question.
quartely.shipment.temp.ts<- window(quartely.shipment.ts, start = c(1985,1), end = c(1989,4))
plot(quartely.shipment.temp.ts, xlab = "Year", ylab = "Shipments (in units)", ylim = c(3500, 5000))


#2 c.Create 4 separate lines, one line for each of the quarters and then plot them as a separate series
#on the line graph. Zoom in to the range of 3500-5000 on the y-axis.Summarize your observations. (Hint: Try exploring seq function in R)

#getting shipment values for  quarter 1 of all the years between 1985 to 1989
shipment.quarter1<-ts(quartely.shipment.ts[c(seq(from = 1, to =20, by= 4))], start = c(1985, 1), end = c(1989, 1)) 

#getting shipment values for  quarter 2 of all the years between 1985 to 1989
shipment.quarter2<-ts(quartely.shipment.ts[c(seq(from = 2, to =20, by= 4))], start = c(1985, 1), end = c(1989, 1)) 

#getting shipment values for  quarter 3 of all the years between 1985 to 1989
shipment.quarter3<-ts(quartely.shipment.ts[c(seq(from = 3, to =20, by= 4))], start = c(1985, 1), end = c(1989, 1)) 

#getting shipment values for  quarter 4 of all the years between 1985 to 1989
shipment.quarter4<-ts(quartely.shipment.ts[c(seq(from = 4, to =20, by= 4))], start = c(1985, 1), end = c(1989, 1)) 

plot(quartely.shipment.ts, xlab = "Year", ylab = "Shipment(in units)",
     ylim = c(3800, 5000), type = "l")

#Creating the respective line graphs
lines(shipment.quarter1,type='l',col="blue")
lines(shipment.quarter2,type='l',col="red")
lines(shipment.quarter3,type='l',col="orange")
lines(shipment.quarter4,type='l',col="grey")

#Creating the legend for above
par(mfcol = c(1,1), xpd=TRUE) 
legend("topleft", inset=c(0, -0.2), 
       legend = c("Quarter1", "Quarter2","Quarter3","Quarter4"), col = c("blue", "red","orange","grey"), 
       pch = 1, cex = .7)



# 2.d Create a line graph at a yearly aggregated level.

annual.ridership.ts <- aggregate(quartely.shipment.ts, FUN = mean)
plot(annual.ridership.ts, xlab = "Year", ylab = "Average Shipment",
     ylim = c(3000, 5000))


#........................................................................... Problem 2 end...................................................



#........................................................................... Problem 3 start...................................................


#Q 3. Use RidingMowers dataset. Create a scatterplot of Lot size vs. Income, color coded byowner/non-owner.
#What do you infer? Which customer segment would you target?

#Solution:

mowers.dataset<-read.csv("RidingMowers.csv")
head(mowers.dataset)

#with ggplot2
ownership_status=mowers.dataset$Ownership
qplot(Income,Lot_Size, data = mowers.dataset, color =ownership_status ,
      xlab = "Income", ylab = "Lot size",
      main = "Lot size vs Income ")



#................................................... Problem 3  end...................................................
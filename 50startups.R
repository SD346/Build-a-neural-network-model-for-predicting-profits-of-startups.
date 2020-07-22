a<- read.csv(choose.files())
View(a)
str(a)
#applying dummies fuction on State column as it is categorical
install.packages("dummies")
library(dummies)
X <- dummies::dummy(a$State)
View(X)
#combining dummy variable after excuding the old column
Startup1 <- a[,-4]
Data <- cbind(Startup1,X)
names(Data) <- c("R.D.Spend","Administration","Marketing.Spend","Profit","StateCalifornia","StateFlorida","StateNewYork")
View(Data)
ndata <- scale(Data) #Normalizing the data

#Splitting data into training(80%) and test(20%)
part <- sample(nrow(ndata),nrow(ndata)*0.8,replace = FALSE)
Train <- data.frame(ndata[part,])
Test <- data.frame(ndata[-part,])
#building the model with train data with single neuron
library(neuralnet)
model <- neuralnet(formula=Profit~R.D.Spend+Administration+Marketing.Spend+StateCalifornia+StateFlorida+StateNewYork,data=Train)
plot(model)
#computing the accuracy of the model
pred <- predict(model,Test[,-4])
pred
accuracy <- cor(pred,Test$Profit)
accuracy #98.28%

#building the model with train data with three neurons
model1 <- neuralnet(formula=Profit~R.D.Spend+Administration+Marketing.Spend+StateCalifornia+StateFlorida+StateNewYork,data=Train, hidden = 3)
plot(model1)
#computing the accuracy of the model
pred1 <- predict(model1,Test[,-4])
pred1
accuracy1 <- cor(pred1,Test$Profit)
accuracy1 #95.94%


print("This step will first install three R packages. Please wait until the packages are fully installed.")
print("Once the installation is complete, this step will print 'Installation complete!'")

install.packages("ResourceSelection")
install.packages("pROC")
install.packages("rpart.plot")

print("Installation complete!")

heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

# Converting appropriate variables to factors  
heart_data <- within(heart_data, {
   target <- factor(target)
   sex <- factor(sex)
   cp <- factor(cp)
   fbs <- factor(fbs)
   restecg <- factor(restecg)
   exang <- factor(exang)
   slope <- factor(slope)
   ca <- factor(ca)
   thal <- factor(thal)
})

head(heart_data, 10)

print("Number of variables")
ncol(heart_data)

print("Number of rows")
nrow(heart_data)

#create the complete model
model1<-glm(target ~ age + trestbps + exang + thalach, data = heart_data, family = "binomial")

summary(model1)

library(ResourceSelection)

print("Hosmer-Lemeshow Goodness of Fit Test, g= 10")
hl10=hoslem.test(model1$y, fitted(model1), g = 10)
hl10

print("Hosmer-Lemeshow Goodness of Fit Test g= 50")
hl50=hoslem.test(model1$y, fitted(model1), g = 50)
hl50

pmin = 1
pmax = 0
for(i in 10:50){
    h = hoslem.test(model1$y, fitted(model1), g=i)$p.value
    cat("p-value of g =",i,": ",h,"\n")
    if(pmin > h){
        pmin = h
    }
    if(pmax < h){
        pmax = h
    }        
}
paste("Min p-value: ", pmin)
paste("Max p-value: ", pmax)

#predict heart disease or no heart disease for the dataset using the model
default_model_data <-heart_data[c('age','trestbps','exang','thalach')]
pred <-predict(model1, newdata = default_model_data, type = 'response')

#predict heart disease if probibility is >= 0.50
depvar_pred = as.factor(ifelse(pred >= 0.5, '1','0'))

# Create confusion matrix
conf.matrix <-table(heart_data$target,depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual",rownames(conf.matrix),sep = ": target = ")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix),sep = ": target = ")

#Print formated matrix
print("Confusion Matrix")
format(conf.matrix,justify = "centre", digit = 2)


library(pROC)

labels <- heart_data$target
predictions <-model1$fitted.values

roc <- roc(labels ~ predictions)

paste("Area Unider the Curve (AUC): ", round(auc(roc),4))

print("ROC Curve")
plot(roc,legacy.axes = TRUE)

ODDS = function(p){
    return (round(p/(1-p),4))
}
#Predictions
paste("Prediction 1: age = 50, resting blood pressure = 122, exercise indused angina = yes, maximum heart rate = 140")
newdata1 <- data.frame(age=50, trestbps=122, exang = '1',thalach = 140)
p1=round(predict(model1, newdata1, type = "response"),4)
paste("Probability:",p1)
paste("Odds: ",ODDS(p1))

paste("Prediction 2: age = 50, resting blood pressure = 130, exercise indused angina = no, maximum heart rate = 165")
newdata2 <- data.frame(age=50, trestbps=130,exang = '0',thalach = 165)
p2 = round(predict(model1, newdata2, type = "response"),4)

paste("Probability:",p2)
paste("Odds: ",ODDS(p2))

model2 <-glm(target ~ age + trestbps + cp + thalach +age:thalach + I(age^2), data = heart_data, family = "binomial")

summary(model2)

library(ResourceSelection)

print("Hosmer-Lemeshow Goodness of Fit Test, g=10")
hl2_10=hoslem.test(model2$y, fitted(model2), g = 10)
hl2_10

print("Hosmer-Lemeshow Goodness of Fit Test g=50")
hl2_50=hoslem.test(model2$y, fitted(model2), g = 50)
hl2_50

pmin = 1
pmax = 0
for(i in 10:50){
    h = hoslem.test(model2$y, fitted(model2), g=i)$p.value
    cat("p-value of g =",i,": ",h,"\n")
    if(pmin > h){
        pmin = h
    }
    if(pmax < h){
        pmax = h
    }        
}
paste("Min p-value: ", pmin)
paste("Max p-value: ", pmax)

#predict heart disease or no heart disease for the dataset using the model
default_model_data2 <-heart_data[c('age','trestbps','cp','thalach')]
pred2 <-predict(model2, newdata = default_model_data2, type = 'response')

#predict heart disease if probibility is >= 0.50
depvar_pred2 = as.factor(ifelse(pred2 >= 0.5, '1','0'))

# Create confusion matrix
conf.matrix2 <-table(heart_data$target,depvar_pred2)[c('0','1'),c('0','1')]
rownames(conf.matrix2) <- paste("Actual",rownames(conf.matrix2),sep = ": target = ")
colnames(conf.matrix2) <- paste("Prediction", colnames(conf.matrix2),sep = ": target = ")

#Print formated matrix
print("Confusion Matrix")
format(conf.matrix2,justify = "centre", digit = 2)

library(pROC)

labels2 <- heart_data$target
predictions2 <- model2$fitted.values

roc2 <- roc(labels2 ~ predictions2)

paste("Area under the Curve (AUC): ", round(auc(roc2),4))

print("ROC Curve")
plot(roc2, legacy.axes = TRUE)

ODDS = function(p){
    return (round(p/(1-p),4))
}
#Predictions
paste("Prediction 1: age = 50, resting blood pressure = 115, chest pain = no, maximum heart rate = 133")
newdata3 <- data.frame(age=50, trestbps=115, cp = '0',thalach = 133)
p3 = round(predict(model2, newdata3, type = "response"),4)
paste("Probability:",p3)
paste("Odds: ",ODDS(p3))

paste("Prediction 2: age = 50, resting blood pressure = 125, chest pain = typical angina, maximum heart rate = 155")
newdata4 <- data.frame(age=50, trestbps=125, cp = '1',thalach = 155)
p4 = round(predict(model2, newdata4, type = "response"),4)

paste("Probability:",p4)
paste("Odds: ",ODDS(p4))

set.seed(6522048)

#split data into training and testing sets
samp.size = floor(0.85*nrow(heart_data))

#training set
train_ind = sample(seq_len(nrow(heart_data)), size = samp.size)
train.data = heart_data[train_ind,]
paste("Number of rows for the training set: ", nrow(train.data))

#test set
test.data = heart_data[-train_ind,]
paste("Number of rows for the validation set: ", nrow(test.data))

set.seed(6522048)
library(randomForest)

train = c()
test = c()
trees = c()

for(i in seq(from=1, to=150, by=1)){
    #print i
    
    trees <- c(trees, i)
    
    model3_rf <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = train.data, ntree = i)
    
    train.data.predict <- predict(model3_rf, train.data, type = "class")
    conf.matrix.train <-table(train.data$target, train.data.predict)
    train_error = 1-(sum(diag(conf.matrix.train)))/sum(conf.matrix.train)
    train <-c(train, train_error)
    
    test.data.predict <- predict(model3_rf, test.data, type = "class")
    conf.matrix.test <-table(test.data$target, test.data.predict)
    test_error = 1-(sum(diag(conf.matrix.test)))/sum(conf.matrix.test)
    test <-c(test, test_error)
}

#plot training and testing errors against number of trees
plot(trees, train, type = 'l', ylim = c(0,.50), col = "green", xlab = "Number of Trees", ylab = "Classification Error")
lines(test, type = "l", col = "blue")
legend('topright', legend = c('Training Set','Testing Set'),col = c("green","blue"), lwd = 2)

set.seed(6522048)
library(randomForest)

model3_rf2 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = heart_data, ntree = 45)

#Confusion matrix for training set
paste('Confusion Matrix: TRAINING set based on random forest model built using 45 trees')
train.data.predict <- predict(model3_rf2, train.data, type = "class")

#Create confusion matrix
conf.matrix.train <-table(train.data$target, train.data.predict)[,c('0','1')]
rownames(conf.matrix.train) <- paste("Actual",rownames(conf.matrix.train),sep = ": target = ")
colnames(conf.matrix.train) <- paste("Prediction", colnames(conf.matrix.train),sep = ": target = ")

#Print matrix
format(conf.matrix.train,justify = "centre",digit = 2)

#confusion matrix for testing set
paste('Confusion Matrix: TESTING set based on random forest model built using 45 trees')
test.data.predict <- predict(model3_rf2, test.data, type = "class")

#Create confusion matrix
conf.matrix.test <-table(test.data$target,test.data.predict)[,c('0','1')]
rownames(conf.matrix.test) <- paste("Actual",rownames(conf.matrix.test),sep = ": target = ")
colnames(conf.matrix.test) <- paste("Prediction", colnames(conf.matrix.test),sep = ": target = ")

#Print matrix
format(conf.matrix.test,justify = "centre",digit = 2)

paste('Aditional te')
set.seed(6522048)
model3_rf3 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = heart_data, ntree = 25)

#Confusion matrix for training set
paste('Confusion Matrix: TRAINING set based on random forest model built using 25 trees')
train.data.predict2 <- predict(model3_rf3, train.data, type = "class")

#Create confusion matrix
conf.matrix.train2 <-table(train.data$target, train.data.predict2)[,c('0','1')]
rownames(conf.matrix.train2) <- paste("Actual",rownames(conf.matrix.train2),sep = ": target = ")
colnames(conf.matrix.train2) <- paste("Prediction", colnames(conf.matrix.train2),sep = ": target = ")

#Print matrix
format(conf.matrix.train2,justify = "centre",digit = 2)

#confusion matrix for testing set
paste('Confusion Matrix: TESTING set based on random forest model built using 25 trees')
test.data.predict2 <- predict(model3_rf3, test.data, type = "class")

#Create confusion matrix
conf.matrix.test2 <-table(test.data$target,test.data.predict2)[,c('0','1')]
rownames(conf.matrix.test2) <- paste("Actual",rownames(conf.matrix.test2),sep = ": target = ")
colnames(conf.matrix.test2) <- paste("Prediction", colnames(conf.matrix.test2),sep = ": target = ")

#Print matrix
format(conf.matrix.test2,justify = "centre",digit = 2)

paste("================================================================================")

set.seed(6522048)
model3_rf4 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = heart_data, ntree = 65)

#Confusion matrix for training set
paste('Confusion Matrix: TRAINING set based on random forest model built using 65 trees')
train.data.predict3 <- predict(model3_rf4, train.data, type = "class")

#Create confusion matrix
conf.matrix.train3 <-table(train.data$target, train.data.predict3)[,c('0','1')]
rownames(conf.matrix.train3) <- paste("Actual",rownames(conf.matrix.train3),sep = ": target = ")
colnames(conf.matrix.train3) <- paste("Prediction", colnames(conf.matrix.train3),sep = ": target = ")

#Print matrix
format(conf.matrix.train3,justify = "centre",digit = 2)

#confusion matrix for testing set
paste('Confusion Matrix: TESTING set based on random forest model built using 65 trees')
test.data.predict3 <- predict(model3_rf4, test.data, type = "class")

#Create confusion matrix
conf.matrix.test3 <-table(test.data$target,test.data.predict3)[,c('0','1')]
rownames(conf.matrix.test3) <- paste("Actual",rownames(conf.matrix.test3),sep = ": target = ")
colnames(conf.matrix.test3) <- paste("Prediction", colnames(conf.matrix.test3),sep = ": target = ")

#Print matrix
format(conf.matrix.test3,justify = "centre",digit = 2)

predicted <-predict(model3_rf2,test.data, type = 'class')
predicted_target <-data.frame(predicted)
data <-cbind('actual'=test.data$target,"predicted"=predicted_target)
print(data)

set.seed(6522048)

samp.size2 = floor(0.80*nrow(heart_data))

#training set
train_ind2 = sample(seq_len(nrow(heart_data)), size = samp.size2)
train.data2 = heart_data[train_ind2,]
paste("Number of rows for the training set: ", nrow(train.data2))

#test set
test.data2 = heart_data[-train_ind2,]
paste("Number of rows for the validation set: ", nrow(test.data2))

set.seed(6522048)
library(randomForest)

#Root mean squared error
RMSE = function(pred,obs){
    return(sqrt(sum((pred-obs)^2)/length(pred)))
}

train = c()
test = c()
trees = c()

for(i in seq(from=1, to=80, by=1)){
    trees <- c(trees, i)
    model4_rf <-randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = train.data2, ntree = i)
    
    pred <- predict(model4_rf, newdata = train.data2, type = 'response')
    rmse_train <- RMSE(pred,train.data2$thalach)
    train <-c(train,rmse_train)
    
    pred2 <- predict(model4_rf, newdata = test.data2, type = 'response')
    rmse_test <- RMSE(pred2,test.data2$thalach)
    test <-c(test, rmse_test)
}

plot(trees, train, type = "l", ylim=c(10,30), col = "red", xlab = "Number of Trees", ylab = "Root Mean Squared Error")
lines(test, type = "l", col = "blue")
legend('topright', legend = c('Training Set','Test Set'), col = c("red","blue"), lwd = 2)

set.seed(6522048)
library(randomForest)

#Root mean squared error
RMSE = function(pred,obs){
    return(sqrt(sum((pred-obs)^2)/length(pred)))
}

model4_rf2 <-randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = train.data2, ntree = 30)

pred1 <-predict(model4_rf2, newdata = train.data2, type = 'response')
paste('RMSE: TRAINING set based on random forest model built using 30 trees: ',
      RMSE(pred1,train.data2$thalach))

paste('====================================================================================================')

pred2 <-predict(model4_rf2, newdata = test.data2, type = 'response')
paste('RMSE: TESTING set based on random forest model built using 30 trees: ',
    RMSE(pred2,test.data2$thalach))

predicted <-predict(model4_rf2,test.data, type = 'response')
predicted_thalach <-data.frame(predicted)
difference <- round(test.data$thalach-predicted,0)
data2 <-cbind('actual'=test.data$thalach,'predicted'=round(predicted_thalach,0),difference)
print(data2)

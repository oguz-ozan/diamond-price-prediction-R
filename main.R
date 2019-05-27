library(ggplot2)
diamonds <- read.csv("diamonds.csv")
inds_x <- which(diamonds$x==0)
inds_y <- which(diamonds$y==0)
inds_z <- which(diamonds$z==0)
diamonds <- diamonds[-c(inds_x,inds_y,inds_z),]
diamonds1 <- read.csv("diamonds.csv")

#X is only increasing index numbers, hence we don't need them
diamonds$X <- NULL

#Factorization
diamonds$cut <- as.factor(diamonds$cut)
diamonds$color <- as.factor(diamonds$color)
diamonds$clarity <- as.factor(diamonds$clarity)
diamonds$carat <- log(diamonds$carat)
diamonds$price <- log(diamonds$price)

#z <- (diamonds$table-min(diamonds$table))/(max(diamonds$table)-min(diamonds$table))
#diamonds$table <- z
diamonds$depth <- scale(diamonds$depth, center = TRUE, scale = TRUE)
diamonds$x <- scale(diamonds$x, center = TRUE, scale = TRUE)


## at the very beginning, the residual error is 1130.
veryFirstModel <- lm(price~carat+cut+clarity+color,data=diamonds)

summary(diamonds)
#correlation between x,y,z are too high. so we can pick only x from all three.
#we will pick one of them to dismiss in modelling phase.
cor1<-cor(diamonds$x, diamonds$y)
cor2<-cor(diamonds$x, diamonds$z)
cor3<-cor(diamonds$y, diamonds$z)


## quantile operation. we saw carat,table, and depth columns have outliers. we can cut them by %5
quantile95 <- diamonds[1,]
categorical_var <- diamonds[,c(2,3,4)]
continous_var <- diamonds[,-c(2,3,4)]

# we take here 95th quantile
for(i in 1:ncol(continous_var)){
  if(is.numeric(continous_var[,i])){
    quantile95[,i] <- quantile(continous_var[,i], 0.98, na.rm = T)
  }
}

# and here equalizing the columns that are bigger than 95th quantile, to 95th quantile.
for(i in 1:ncol(continous_var)){
  if(is.numeric(continous_var[,i])){
    continous_var[which(continous_var[,i]>quantile95[,i]),i] <- quantile95[,i] 
  }
}
data_main <- cbind(continous_var,categorical_var)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#from these model we can extract again, that y and z can be dismissed from dataset, because of the big p values
model2 <- lm(price~carat+cut+clarity+color,data=diamonds)
drops <- c("y","z")
data_main <- data_main[ , !(names(data_main) %in% drops)]
# this doesn't improve our model but we do the same job with less effort, so remove it.
modelAfterDismiss <- lm(price~.,data=data_main)


## here error is 1130 to 924.. after quantile operation for all numerical columns.
modelAfterQuantile <- lm(price~carat+cut+clarity+color,data=data_main)


#we see from here the attributes for 'cut' are totally non-uniformly distributed. so we can make undersampling
cutFair <- which(diamonds$color == "D")
cutGood <- which(diamonds$color == "E")
cutIdeal <- which(diamonds$color == "F")
cutPremium <- which(diamonds$color == "G")
cutVeryGood <- which(diamonds$color == "H")
cutI <- which(diamonds$color == "I")
cutJ <- which(diamonds$color == "J")

## undersampling with color
pick_fair <- sample(cutFair,2808)
pick_good <- sample(cutGood,2808)
pick_ideal <- sample(cutIdeal,2808)
pick_premium <- sample(cutPremium,2808)
pick_verygood <- sample(cutVeryGood,2808)
pick_I <- sample(cutI,2808)
pick_J <- sample(cutJ,2808)

# Undersampling is not been used
#train_data <- data_main[c(pick_fair, pick_good, pick_ideal, pick_premium, pick_verygood,pick_I,pick_J), ]
#test_data <- data_main[-c(pick_fair, pick_good, pick_ideal, pick_premium, pick_verygood,pick_I,pick_J), ]
new_data <- sample(1:nrow(data_main), size=nrow(data_main)*0.6)
train_data <- data_main[new_data,]
test_data <- data_main[-new_data,]

model5 <- lm(price~carat+cut+clarity+color,data=train_data)

set.seed(1815851)
new_data <- train_data[sample(nrow(train_data)),]
folds <- cut(seq(1,nrow(new_data)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
minErr<-1000
test_indexes_be_used <- NA
cv_errors <- c()
cv_iters <- c(1,2,3,4,5,6,7,8,9,10)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- new_data[testIndexes, ]
  trainData <- new_data[-testIndexes, ]
  temp_model <- lm(price~carat+cut+clarity+color,data=trainData)
  predictions <- predict(temp_model, testData)
  
  errors <- abs(exp(predictions)-exp(testData$price))
  print(mean(errors))
  cv_errors[i] <- mean(errors)
  if(mean(errors) < minErr){
    minErr <- mean(errors)
    test_indexes_be_used <- testIndexes
  }
}

testData <- new_data[test_indexes_be_used,]
trainData <- new_data[-test_indexes_be_used,]
mainModel <- lm(price~carat+cut+clarity+color.,data=trainData)


last_predict <- predict(mainModel,test_data)
main_error <- abs(exp(last_predict)-exp(test_data$price))
mean(main_error)
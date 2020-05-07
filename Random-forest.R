install.packages("randomForest")
install.packages("caret")
install.packages("e1071")
install.packages("ggplot2")
install.packages("ggRandomForests")
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(ggRandomForests)
head(Cervical)

#scaling 
preprocessParams <- preProcess(Cervical[,1:59], method=c("center", "scale"))
print(preprocessParams)
Cervical <- predict(preprocessParams, Cervical[,1:59])

#restructuring data
Cervical$X <- NULL
Cervical <- as.data.frame(t(Cervical))
State <- c('N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','N','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T','T')
Cervical$State <- State

#factorizing data
Cervical$State <- as.factor(Cervical$State)

#forming classes
class1 <- Cervical[1:29,]
class2 <- Cervical[30:58, ]

#validation data
norm_validation <- class1[1:9,]
canc_validation <- class2[1:9,]
validation_set <- rbind(norm_validation, canc_validation)

#training data
norm_test <- class1[10:29,]
canc_test <- class2[10:29,]
training_set <- rbind(norm_test, canc_test)

#random forest
model1 <- randomForest(State ~ ., data = training_set, proximity = TRUE)
model1

#prediction
pred <- predict(model1, newdata = validation_set, type = "class")
pred
confusionMatrix(validation_set$State, pred)

#OOB error plot
gg_dta <- gg_error(model1)
plot(gg_dta)

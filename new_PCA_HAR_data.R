
library(caret)
library(xgboost)
library(kernlab)
library(tidyverse)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(AppliedPredictiveModeling)
library(Hmisc)
install.packages("rlist")
library(rlist)
trainingURL <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

weightLifting <-read.csv(trainingURL)
test.data <-read.csv(testingURL)
train_vars<-colnames(weightLiftingTraining)

dat<-weightLiftingTraining[,grep("^roll_", colnames(weightLiftingTraining))]

weightLiftingTraining<-weightLifting%>%
  select( 
    grep("^roll_", colnames(weightLifting)),
    grep("^pitch_", colnames(weightLifting)),
    grep("^yaw_", colnames(weightLifting)),
  
    classe
  )#%>%
 # select(roll_dumbbell, pitch_arm)

test.data<-test.data%>%
  select(
          grep("^roll_", colnames(weightLifting)),
           grep("^pitch_", colnames(weightLifting)),
           grep("^yaw_", colnames(weightLifting)),
     )


res.pca <-PCA(weightLiftingTraining[-13], scale.unit=TRUE, ncp=9, graph=FALSE)


fviz_eig(res.pca)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

f <-res.pca[[1]][[2]]
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

set.seed(123)
my.cont.var <- rnorm(20)

############### too much ovrlap give for poorer accruacy


training.samples <- weightLiftingTraining$classe %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data <- weightLiftingTraining[training.samples, ]
quiz.data <- weightLiftingTraining[-training.samples, ]



###################
preproccessTraining<-
  train.data%>%
  preProcess(method=c("center", "scale"))

train.transformed <-preproccessTraining%>%
     predict(train.data)
quiz.transformed <-preproccessTraining%>%
     predict(quiz.data)


library(MASS)
library(mda)

model <-lda(classe ~., data=train.transformed)
model

# Make predictions
predictions <- model %>% predict(quiz.transformed)
# Model accuracy
mean(predictions$class==quiz.transformed$class)





predictions <- model %>% predict(quiz.transformed)
names(predictions)


lda.data <- cbind(train.transformed, predict(model)$x)

mean(predictions$class==quiz.transformed$classe)







# Fit the model
model <- mda(classe~., data = train.transformed)
model
# Make predictions
predicted.classes.mda <- model %>% predict(quiz.transformed)
# Model accuracy
mean(predicted.classes.mda == quiz.transformed$classe)


library(nnet)

# Fit the model
model <- nnet::multinom(classe ~., data = train.data)
# Summarize the model
summary(model)
# Make predictions
predicted.classes.nnet <- model %>% predict(quiz.data)
head(predicted.classes)
# Model accuracy
mean(predicted.classes.nnet == quiz.data$classe)

#####################x boost Tree #########################
library(randomForest)
modelxb <- train(classe ~., data = train.data, method = "xgbTree",
               trControl = trainControl("CV", number = 10))
# Make predictions
predicted.classes.xb <- modelxb %>% predict(quiz.data)

predicted <-predict(modelxb, quiz.data)
# Model n accuracy



mean(predicted.classes.xb== test.data$classe)


levels(quiz.data$classe) <-c("A","B","C","D","E")



actual <-as.factor(quiz.data$classe)
levels(predicted.classes.xb) <-c("A","B","C","D","E")

predicted<-predicted.classes.xb
confusionMatrix( actual,predicted)


library(cvms)
cf <-tibble(actual, predicted)
conf_mat <- confusion_matrix(actual  = cf$targets,
                             predicted= cf$predictions)

plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]])
```

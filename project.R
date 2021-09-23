#install packages
install.packages("caret")
install.packages("e1071", dep = TRUE, type = "source")
install.packages(("gbm"))
install.packages("rsample")
install.packages("vip")


library("caret")
getNamespaceVersion("caret")
library("readxl")
library("e1071")
library(MASS)

# import data set, for WB_pheno
nonstring1 <- read_excel("Desktop/STAT/2020fall/research/NS_89ind_training_in_78_042815_pred_pheno.xls")
head(nonstring1)
set.seed(1)
nonstring2 = nonstring1[,4:196] #Three classes data set
head(nonstring2)


### Three classes, response = U A P, don't use this because when you use this the accuracy is 1, the reason is unknow so far.
#set.seed(1)
#nonstring2$response <- NA
#for (i in c(1:89)) {
#  if (nonstring2$WB_Pheno[i] == "Affected"){
#    nonstring2$response[i]= "A"
#  } else if (nonstring2$WB_Pheno[i] == "Unaffected"){
#    nonstring2$response[i]="U"
#  } else{
#    nonstring2$response[i] ="P"
#  }
#}
#nonstring2$response
#head(nonstring2)
#table(nonstring2$WB_Pheno)
#table(nonstring2$response)

# Two classes
set.seed(1)
nonstring3 = nonstring1[, 4:196]
nonstring3$response <- NA #two classes, responses are U and A
for (i in c(1:89)) {
  if (nonstring3$WB_Pheno[i]=="Unaffected"){
    nonstring3$response[i]="U"
  } else{
    nonstring3$response[i]="A"
  }
}
nonstring3$response

nonstring=nonstring3[, -1] #two classes data set
head(nonstring)
table(nonstring$response)
table(nonstring2$WB_Pheno)
#Lasso
#Scale
# Three Classes
#Leave one out
set.seed(1)
lassomodel=train(WB_Pheno~., data=nonstring2, method="glmnet", trControl = trainControl(method = "LOOCV"),
                 preProcess = c("center", "scale"),
                 tuneGrid = expand.grid(alpha =1, lambda = seq(0, 0.5, by = 0.01)))
print(lassomodel)
lassomodel$bestTune

coef(lassomodel$finalModel, lassomodel$bestTune$lambda)

final_0 = train(WB_Pheno~., data=nonstring2, method="glmnet", trControl=trainControl(method ="LOOCV"),
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(alpha = 1, lambda = 0.07))
print(final_0)
pred_0 <-as.factor(final_0$pred[1]$pred)
obs_0 <- as.factor(final_0$pred[2]$obs)
confusionMatrix(data=pred_0, reference=obs_0)

#lasso_plot--three class
pred_la = as.matrix(final_0$pred[1])
obs_la=as.matrix((final_0$pred[2]))
a_la =c(1:89)
for (i in c(1:89)) {
  if (pred_la[i]==obs_la[i]){
    a_la[i]=1
  }else {
    a_la[i]=0
  }
}
a_la
b_la=rep("red",89)
b_la
for (i in 1:89) {
  if (obs_la[i]=="Affected"){
    b_la[i]="red"
  } else if (obs_la[i]=="Partial") {
    b_la[i]="green"
  }else {
    b_la[i]="blue"
  }
}
b_la
plot(1:89, a_la, type='p',xaxt='n', yaxt='n', col=b_la, xlab='obs',ylab='predict', main='Lasso with all predictors in three classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_la[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("Affected","Partial","Unaffected"),pch=1,col=c("red","green", "blue"))


#two classes
# leave one out
library("datasets")
library("gbm")
set.seed(1)
lassomodel1=train(response~., data=nonstring, method="glmnet", trControl=trainControl(method ="LOOCV"),
                  preProcess= c("center", "scale"),
                  tuneGrid = expand.grid(alpha=1, lambda = seq(0,0.5,by=0.01)))

print(lassomodel1)
lassomodel1$bestTune

coef(lassomodel1$finalModel, lassomodel1$bestTune$lambda)

final_1 = train(response~., data = nonstring, method="glmnet", trControl=trainControl(method="LOOCV"),
                preProcessor = c("center", "scale"),
                tuneGrid = expand.grid(alpha=1, lambda=0.02))
print(final_1)
pred_1 <- as.factor(final_1$pred[1]$pred)
obs_1 <- as.factor(final_1$pred[2]$obs)
confusionMatrix(data=pred_1, reference=obs_1)

#lasso_plot--two classes
pred_la2 = as.matrix(final_1$pred[1])
obs_la2=as.matrix((final_1$pred[2]))
a_la2 =c(1:89)
for (i in c(1:89)) {
  if (pred_la2[i]==obs_la2[i]){
    a_la2[i]=1
  }else {
    a_la2[i]=0
  }
}
a_la2
b_la2=rep("red",89)
b_la2
for (i in 1:89) {
  if (obs_la2[i]=="A"){
    b_la2[i]="red"
  } else if (obs_la2[i]=="U") {
    b_la2[i]="green"
  }
}
b_la2
plot(1:89, a_la2, type='p',xaxt='n', yaxt='n', col=b_la2, xlab='obs',ylab='predict', main='Lasso with all predictors in two classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_la2[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("A","U"),pch=1,col=c("red","green"))



#Raw -- WB_Pheno_Predicted by RFA, this raw part should not be considered, which mean this variable WB_Pheno_Predicted by RFA, is not use in this analysis
data <- read_excel("Desktop/STAT/2020fall/research/NS_89ind_training_in_78_042815_pred_pheno.xls")
head(data)
data1 = data[,3:196] #Three classes data set
head(data1)

#Three Classes
### Three classes, response = U A P
set.seed(1)
data1$response <- NA
for (i in c(1:89)) {
  if (data1$`WB_Pheno_Predicted by RFA`[i] == "Affected"){
    data1$response[i]= "A"
  } else if (data1$`WB_Pheno_Predicted by RFA`[i] == "Unaffected"){
    data1$response[i]="U"
  } else{
    data1$response[i] ="P"
  }
}
data1$response
head(data1)
table(data1$WB_Pheno)
table(data1$`WB_Pheno_Predicted by RFA`)
table(data1$response)


# Two classes
set.seed(1)
data2 = data[, 3:196]
data2$response <- NA #two classes, responses are U and A
for (i in c(1:89)) {
  if (data2$`WB_Pheno_Predicted by RFA`[i]=="Unaffected"){
    data2$response[i]="U"
  } else{
    data2$response[i]="A"
  }
}
data2$response
table(data2$response)

data3=data2[, -1] #two classes data set
head(data3)
table(data3$response)

#three classes
#leave one out
set.seed(1)
lassomodel_raw=train(`WB_Pheno_Predicted by RFA`~., data=data1, method="glmnet", trControl=trainControl(method = "LOOCV"),
                     tuneGrid =expand.grid(alpha = 1,lambda = seq(0,0.5,by = 0.01)))
print(lassomodel_raw)

coef_train=coef(lassomodel$finalModel, lassomodel$bestTune$lambda)
a_0=as.matrix(coef_train$Affected)
b_0=as.matrix(coef_train$Partial)
c_0=as.matrix(coef_train$Unaffected)
d_0=cbind(a_0,b_0,c_0)
write.csv(d_0,"Desktop/STAT/2020fall/research/d_3_raw.csv")
gene1 =read.csv("Desktop/STAT/2020fall/research/d_3_raw.csv")

final_0 = train(`WB_Pheno_Predicted by RFA`~., data=data1, method="glmnet", trControl=trainControl(method ="LOOCV"),
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(alpha = 1, lambda = 0.07))
print(final_0)
pred_0 <-as.factor(final_0$pred[1]$pred)
obs_0 <- as.factor(final_0$pred[2]$obs)
confusionMatrix(data=pred_0, reference=obs_0)


#Two Classes
#leave one out
set.seed(1)
lassomodel_raw1=train(response~., data=data3, method="glmnet", trControl=trainControl(method = "LOOCV"),
                      tuneGrid =expand.grid(alpha = 1,lambda = seq(0,0.5,by = 0.01)))
print(lassomodel_raw1)
coef(lassomodel1$finalModel, lassomodel1$bestTune$lambda)

final_1 = train(response~., data = data3, method="glmnet", trControl=trainControl(method="LOOCV"),
                preProcessor = c("center", "scale"),
                tuneGrid = expand.grid(alpha=1, lambda=0.02))
print(final_1)
pred_1 <- as.factor(final_1$pred[1]$pred)
obs_1 <- as.factor(final_1$pred[2]$obs)
confusionMatrix(data=pred_1, reference=obs_1)

# logistic regression
# Helper packages
library(dplyr)     # for data wrangling
library(ggplot2)   # for awesome plotting
library(rsample)   # for data splitting

# Modeling packages
library(caret)     # for logistic regression modeling

# Model interpretability packages
library(vip)       # variable importance

##three classes- logistic regression
set.seed(1)
logisticmodel=train(WB_Pheno~., data=nonstring2, method="glmnet", 
                    family = "multinomial", type.multinomial = "grouped",
                    trControl=trainControl(method = "LOOCV"),
                     tuneGrid =expand.grid(alpha = 1,lambda = seq(0,0.5,by = 0.01)))
print(logisticmodel)
logisticmodel$bestTune

coef(logisticmodel$finalModel, logisticmodel$bestTune$lambda)

final_0_log = train(WB_Pheno~., data=nonstring2, method="glmnet", 
                family = "multinomial", type.multinomial = "grouped",
                trControl=trainControl(method ="LOOCV"),
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(alpha = 1, lambda = 0.07))
print(final_0_log)
pred_0 <-as.factor(final_0_log$pred[1]$pred)
obs_0 <- as.factor(final_0_log$pred[2]$obs)
confusionMatrix(data=pred_0, reference=obs_0)

#logistic_plot--three class
pred_log = as.matrix(final_0_log$pred[1])
obs_log=as.matrix((final_0_log$pred[2]))
a_log =c(1:89)
for (i in c(1:89)) {
  if (pred_log[i]==obs_log[i]){
    a_log[i]=1
  }else {
    a_log[i]=0
  }
}
a_log
b_log=rep("red",89)
b_log
for (i in 1:89) {
  if (obs_log[i]=="Affected"){
    b_log[i]="red"
  } else if (obs_log[i]=="Partial") {
    b_log[i]="green"
  }else {
    b_log[i]="blue"
  }
}
b_log
plot(1:89, a_log, type='p',xaxt='n', yaxt='n', col=b_log, xlab='obs',ylab='predict', main='Logistic with all predictors in three classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_log[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("Affected","Partial","Unaffected"),pch=1,col=c("red","green", "blue"))



#two classes
# leave one out
library("datasets")
library("gbm")
set.seed(1)
logisticmodel1=train(response~., data=nonstring, method="glmnet", 
                     family = "binomial", type.measure = "class",
                     trControl=trainControl(method ="LOOCV"),
                  preProcess= c("center", "scale"),
                  tuneGrid = expand.grid(alpha=1, lambda = seq(0,0.5,by=0.01)))

print(logisticmodel1)
logisticmodel$bestTune

coef(logisticmodel1$finalModel, logisticmodel1$bestTune$lambda)

final_1_log = train(response~., data = nonstring, method="glmnet", 
                family = "binomial", type.measure = "class",
                trControl=trainControl(method="LOOCV"),
                preProcessor = c("center", "scale"),
                tuneGrid = expand.grid(alpha=1, lambda=0.02))
print(final_1_log)
pred_1 <- as.factor(final_1_log$pred[1]$pred)
obs_1 <- as.factor(final_1_log$pred[2]$obs)
confusionMatrix(data=pred_1, reference=obs_1)

#logistic_plot--two classes
pred_log2 = as.matrix(final_1_log$pred[1])
obs_log2=as.matrix((final_1_log$pred[2]))
a_log2 =c(1:89)
for (i in c(1:89)) {
  if (pred_log2[i]==obs_log2[i]){
    a_log2[i]=1
  }else {
    a_log2[i]=0
  }
}
a_log2
b_log2=rep("red",89)
b_log2
for (i in 1:89) {
  if (obs_log2[i]=="A"){
    b_log2[i]="red"
  } else if (obs_log2[i]=="U") {
    b_log2[i]="green"
  }
}
b_log2
plot(1:89, a_log2, type='p',xaxt='n', yaxt='n', col=b_log2, xlab='obs',ylab='predict', main='Logistic regression with all predictors in two classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_log2[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("A","U"),pch=1,col=c("red","green"))


#Raw -- WB_Pheno_Predicted by RFA -- logistic regression
#three classes
#leave one out
logisticmodel_raw=train(`WB_Pheno_Predicted by RFA`~., data=data1, method="glmnet", 
                        family = "multinomial", type.multinomial = "grouped",
                        trControl=trainControl(method = "LOOCV"),
                     tuneGrid =expand.grid(alpha = 1,lambda = seq(0,0.5,by = 0.01)))
print(logisticmodel_raw)

coef_train=coef(logisticmodel$finalModel, logisticmodel$bestTune$lambda)
a_0=as.matrix(coef_train$Affected)
b_0=as.matrix(coef_train$Partial)
c_0=as.matrix(coef_train$Unaffected)
d_0=cbind(a_0,b_0,c_0)
write.csv(d_0,"Desktop/STAT/2020fall/research/d_3_raw_log.csv")
gene1 =read.csv("Desktop/STAT/2020fall/research/d_3_raw_log.csv")

final_0_raw_log = train(`WB_Pheno_Predicted by RFA`~., data=data1, method="glmnet", 
                family = "multinomial", type.multinomial = "grouped",
                trControl=trainControl(method ="LOOCV"),
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(alpha = 1, lambda = 0.07))
print(final_0_raw_log)
pred_0 <-as.factor(final_0_raw_log$pred[1]$pred)
obs_0 <- as.factor(final_0_raw_log$pred[2]$obs)
confusionMatrix(data=pred_0, reference=obs_0)


#Two Classes
#leave one out
logisticmodel_raw1=train(response~., data=data3, method="glmnet", 
                      family = "binomial", type.measure = "class",
                      trControl=trainControl(method = "LOOCV"),
                      tuneGrid =expand.grid(alpha = 1,lambda = seq(0,0.5,by = 0.01)))
print(logisticmodel_raw1)
coef(logisticmodel1$finalModel, logisticmodel1$bestTune$lambda)

final_1_raw_log = train(response~., data = data3, method="glmnet", 
                family = "binomial", type.measure = "class",
                trControl=trainControl(method="LOOCV"),
                preProcessor = c("center", "scale"),
                tuneGrid = expand.grid(alpha=1, lambda=0.02))
print(final_1_raw_log)
pred_1 <- as.factor(final_1_raw_log$pred[1]$pred)
obs_1 <- as.factor(final_1_raw_log$pred[2]$obs)
confusionMatrix(data=pred_1, reference=obs_1)

##SVM
#three classes
#SVMlinear with all predictors
set.seed(1)
svmlinear=train(WB_Pheno~., data=nonstring2, 
                     trControl=trainControl(method="LOOCV"),
                     method="svmLinear",
                     tuneGrid=data.frame(C=0.01))
print(svmlinear)
svmlinear$bestTune

svmlinear$pred
str(svmlinear$pred[1])
svmlinear$pred[1]
svmlinear$pred[2]
pred<- as.factor(svmlinear$pred[1]$pred)
obs<- as.factor(svmlinear$pred[2]$obs)
confusionMatrix(data=pred, reference=obs)


#linear_plot
pred = as.matrix(svmlinear$pred[1])
obs = as.matrix.POSIXlt(svmlinear$pred[2])
a = c(1:89)
for (i in c(1:89)) {
  if (pred[i]==obs[i]) {
    a[i]=1
  } else {
    a[i]= 0
  }
}
a
b = rep('red',89)
for (i in 1:89) {
  if (obs[i] == "Affected"){
    b[i]="red"
  } else if (obs[i] == "Partial") {
    b[i]="green"
  } else {
    b[i]="blue"
  }
}
b
plot(1:89,a, type ="p", xaxt = 'n', yaxt='n', col = b, xlab ="obs", ylab="predict", main = "SVM_linear with all predictors in three classes")
axis(side = 1, at=c(1:89), labels = c(1:89))
axis(side=2, at =c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a[i] ==0) {
    abline(v=i, col='grey')
  }
}
legend(x=80, y=1.4, xpd=TRUE, title="obs", legend=c("Affected", "Partial", "Unaffected"), pch=1, col=c("red", "green", "blue"))

#SVMlinear with pca - three classes
set.seed(1)
svmlinear3_pca=train(WB_Pheno~., data=nonstring2, 
                trControl=trainControl(method="LOOCV", preProcOptions = list(thresh = 0.95)),
                method="svmLinear",
                preProcess='pca',
                tuneGrid=data.frame(C=0.1))
print(svmlinear3_pca)
svmlinear3_pca$bestTune

svmlinear3_pca$pred
str(svmlinear3_pca$pred[1])
svmlinear3_pca$pred[1]
svmlinear3_pca$pred[2]
pred<- as.factor(svmlinear3_pca$pred[1]$pred)
obs<- as.factor(svmlinear3_pca$pred[2]$obs)
confusionMatrix(data=pred, reference=obs)

#linear_plot_pca
pred = as.matrix(svmlinear3_pca$pred[1])
obs = as.matrix.POSIXlt(svmlinear3_pca$pred[2])
a = c(1:89)
for (i in c(1:89)) {
  if (pred[i]==obs[i]) {
    a[i]=1
  } else {
    a[i]= 0
  }
}
a
b = rep('red',89)
for (i in 1:89) {
  if (obs[i] == "Affected"){
    b[i]="red"
  } else if (obs[i] == "Partial") {
    b[i]="green"
  } else {
    b[i]="blue"
  }
}
b
plot(1:89,a, type ="p", xaxt = 'n', yaxt='n', col = b, xlab ="obs", ylab="predict", main = "SVM_linear with PCA in three classes")
axis(side = 1, at=c(1:89), labels = c(1:89))
axis(side=2, at =c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a[i] ==0) {
    abline(v=i, col='grey')
  }
}
legend(x=80, y=1.4, xpd=TRUE, title="obs", legend=c("Affected", "Partial", "Unaffected"), pch=1, col=c("red", "green", "blue"))


#SVMradial with all predictors
set.seed(1)
svmradial=train(WB_Pheno~., data=nonstring2, 
                trControl=trainControl(method="LOOCV"),
                method="svmRadial",
                tuneGrid=data.frame(C=10, sigma=0.001))
print(svmradial)
svmradial$bestTune

svmradial$pred
str(svmradial$pred[1])
pred1<- as.factor(svmradial$pred[1]$pred)
pred1
obs1<- as.factor(svmradial$pred[2]$obs)
obs1
confusionMatrix(data=pred1, reference=obs1)


fit.svm <- train(WB_Pheno~., data=nonstring2, 
                 method = "svmRadial",
                 tuneLength=10,
                 trainControl=trainControl(method = "repeatedcv",
                                               number = 10,
                                               repeats = 3,
                                               search = "grid"))
fit.svm
imp.svm <- varImp(fit.svm)


svm_imp <- varImp(svmradial, scale = FALSE)
svm_imp$importance

coef(svmradial, svmradial$bestTune$C)

#radial_plot
pred1 =as.matrix(svmradial$pred[1])
obs1 = as.matrix(svmradial$pred[2])
a1 =c(1:89)
a1
for (i in c(1:89)) {
  if (pred1[i]==obs1[i]) {
    a1[i]=1
  } else {
    a1[i]=0
  }
}
a1
b1=rep('red', 89)
for (i in 1:89) {
  if (obs1[i]=="Affected") {
    b1[i]="red"
  } else if (obs1[i]=="Partial") {
    b1[i]="green"
  } else {
    b1[i]="blue"
  }
}
b1
plot(1:89, a1, type ="p", xaxt='n', yaxt='n', col=b1, xlab='obs', ylab='predict', main="SVM_radial with all predictors in three classes")
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a1[i]==0) {
    abline(v=i, col='grey')
  }
}
legend(x=80, y=1.4, xpd=TRUE, title='obs', legend=c("Affected", "Partial", "Unaffected"), pch=1, col=c("red", "green", "blue"))

#SVMradial with pca - three classes
set.seed(1)
svmradial3_pca=train(WB_Pheno~., data=nonstring2, 
                trControl=trainControl(method="LOOCV", preProcOptions = list(thresh = 0.95)),
                method="svmRadial",
                preProcess="pca",
                tuneGrid=data.frame(C=20, sigma=0.001))
print(svmradial3_pca)
svmradial3_pca$bestTune

svmradial3_pca$pred
str(svmradial3_pca$pred[1])
pred1<- as.factor(svmradial3_pca$pred[1]$pred)
obs1<- as.factor(svmradial3_pca$pred[2]$obs)
confusionMatrix(data=pred1, reference=obs1)

#radial_plot_pca
pred1 =as.matrix(svmradial3_pca$pred[1])
obs1 = as.matrix(svmradial3_pca$pred[2])
a1 =c(1:89)
a1
for (i in c(1:89)) {
  if (pred1[i]==obs1[i]) {
    a1[i]=1
  } else {
    a1[i]=0
  }
}
a1
b1=rep('red', 89)
for (i in 1:89) {
  if (obs1[i]=="Affected") {
    b1[i]="red"
  } else if (obs1[i]=="Partial") {
    b1[i]="green"
  } else {
    b1[i]="blue"
  }
}
b1
plot(1:89, a1, type ="p", xaxt='n', yaxt='n', col=b1, xlab='obs', ylab='predict', main="SVM_radial with PCA in three classes")
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a1[i]==0) {
    abline(v=i, col='grey')
  }
}
legend(x=80, y=1.4, xpd=TRUE, title='obs', legend=c("Affected", "Partial", "Unaffected"), pch=1, col=c("red", "green", "blue"))


# two classes
#SVMlinear with all predictors
set.seed(1)
svmlinear1=train(response~., data=nonstring, 
                trControl=trainControl(method="LOOCV"),
                method="svmLinear",
                tuneGrid=data.frame(C=0.01))
print(svmlinear1)
svmlinear1$bestTune

svmlinear1$pred
str(svmlinear1$pred[1])
pred<- as.factor(svmlinear1$pred[1]$pred)
pred
obs<- as.factor(svmlinear1$pred[2]$obs)
obs
confusionMatrix(data=pred, reference=obs)

#linear_plot
pred=as.matrix(svmlinear1$pred[1])
obs=as.matrix(svmlinear1$pred[2])
a=c(1:89)
for (i in c(1:89)) {
  if (pred[i]==obs[i]){
    a[i]=1
  } else {
    a[i]=0
  }
}
a
b=rep('red',89)
for (i in 1:89) {
  if (obs[i]=="A") {
    b[i]="red"
  } else if (obs[i]=="U") {
    b[i]="green"
  }
}
b

plot(1:89, a, type='p', xaxt='n',yaxt='n', col=b, xlab='obs', ylab='predict', main='SVM_linear with all predictors in two classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1),labels=c(0,1))
for (i in 1:89) {
  if (a[i]==0){
    abline(v=i, col="grey")
  }
}
legend(x=80, y=1.4, xpd=TRUE, title='obs', legend=c("A", "U"),pch=1, col=c("red", "green"))

## SVMlinear with pca - two classes
set.seed(1)
svmlinear_pca=train(response~., data=nonstring, 
                 trControl=trainControl(method="LOOCV", preProcOptions = list(thresh = 0.95)),
                 method="svmLinear",
                 preProcess="pca",
                 tuneGrid=data.frame(C=0.1))
print(svmlinear_pca)
svmlinear_pca$bestTune

svmlinear_pca$pred
str(svmlinear_pca$pred[1])
pred<- as.factor(svmlinear_pca$pred[1]$pred)
pred
obs<- as.factor(svmlinear_pca$pred[2]$obs)
obs
confusionMatrix(data=pred, reference=obs)

#linear_plot pca
pred=as.matrix(svmlinear_pca$pred[1])
obs=as.matrix(svmlinear_pca$pred[2])
a=c(1:89)
for (i in c(1:89)) {
  if (pred[i]==obs[i]){
    a[i]=1
  } else {
    a[i]=0
  }
}
a
b=rep('red',89)
for (i in 1:89) {
  if (obs[i]=="A") {
    b[i]="red"
  } else if (obs[i]=="U") {
    b[i]="green"
  }
}
b

plot(1:89, a, type='p', xaxt='n',yaxt='n', col=b, xlab='obs', ylab='predict', main='SVM_linear with PCA in two classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1),labels=c(0,1))
for (i in 1:89) {
  if (a[i]==0){
    abline(v=i, col="grey")
  }
}
legend(x=80, y=1.4, xpd=TRUE, title='obs', legend=c("A", "U"),pch=1, col=c("red", "green"))

#SVMradial with all predictors
set.seed(1)
svmradial1=train(response~., data=nonstring, 
                trControl=trainControl(method="LOOCV"),
                method="svmRadial",
                tuneGrid=data.frame(C=10, sigma=0.001))
print(svmradial1)
svmradial1$bestTune

svmradial1$pred
str(svmradial1$pred[1])
pred1<- as.factor(svmradial1$pred[1]$pred)
obs1<- as.factor(svmradial1$pred[2]$obs)
confusionMatrix(data=pred1, reference=obs1)

#radial_plot
pred1=as.matrix((svmradial1$pred[1]))
obs1=as.matrix(svmradial1$pred[2])
a1=c(1:89)
for (i in c(1:89)) {
  if (pred1[i]==obs1[i]){
    a1[i]=1
  }else {
    a1[i]=0
  }
}
a1
b1=rep('red',89)
for (i in 1:89){
  if (obs1[i]=="A"){
    b1[i]="red"
  } else {
    b1[i]="green"
  }
}
b1
plot(1:89, a1, type='p', xaxt='n', yaxt='n', col=b1, xlab='obs', ylab='predict', main="SVM_radial with all predictors in two classes")
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a1[i]==0){
    abline(v=i, col='grey')
  }
}
legend(x=80, y=1.4, xpd=TRUE, title='obs', legend=c("A", "U"), pch=1, col=c("red", "green"))

#SVMradial with pca - two classes
set.seed(1)
svmradial_pca=train(response~., data=nonstring, 
                 trControl=trainControl(method="LOOCV",preProcOptions = list(thresh = 0.95)),
                 preProcess="pca",
                 method="svmRadial",
                 tuneGrid=data.frame(C=10, sigma=0.001))
print(svmradial_pca)
svmradial_pca$bestTune

svmradial_pca$pred
str(svmradial_pca$pred[1])
pred1<- as.factor(svmradial_pca$pred[1]$pred)
obs1<- as.factor(svmradial_pca$pred[2]$obs)
confusionMatrix(data=pred1, reference=obs1)

#radial_plot
pred1=as.matrix((svmradial_pca$pred[1]))
obs1=as.matrix(svmradial_pca$pred[2])
a1=c(1:89)
for (i in c(1:89)) {
  if (pred1[i]==obs1[i]){
    a1[i]=1
  }else {
    a1[i]=0
  }
}
a1
b1=rep('red',89)
for (i in 1:89){
  if (obs1[i]=="A"){
    b1[i]="red"
  } else {
    b1[i]="green"
  }
}
b1
plot(1:89, a1, type='p', xaxt='n', yaxt='n', col=b1, xlab='obs', ylab='predict', main="SVM_radial with PCA in two classes")
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a1[i]==0){
    abline(v=i, col='grey')
  }
}
legend(x=80, y=1.4, xpd=TRUE, title='obs', legend=c("A", "U"), pch=1, col=c("red", "green"))



# Random Forest with all predictors
#two class
set.seed(1)
rf2=train(response~.,data=nonstring, 
          trControl=trainControl(method="LOOCV"),
          method='rf', 
          tuneGrid=data.frame(mtry=14))
print(rf2)

importance_rf2 <- varImp(rf2,scale=FALSE) #importance
importance_rf2$importance
plot(importance_rf2,top=20)
print(importance_rf2)

rf2$pred
pred_rf2=as.factor(rf2$pred[1]$pred)
obs_rf2=as.factor(rf2$pred[2]$obs)
confusionMatrix(data=pred_rf2, reference=obs_rf2)

#rf2_plot
pred_rf2 = as.matrix(rf2$pred[1])
obs_rf2=as.matrix((rf2$pred[2]))
a_rf2 =c(1:89)
for (i in c(1:89)) {
  if (pred_rf2[i]==obs_rf2[i]){
    a_rf2[i]=1
  }else {
    a_rf2[i]=0
  }
}
a_rf2
b_rf2=rep("red",89)
b_rf2
for (i in 1:89) {
  if (obs_rf2[i]=="A"){
    b_rf2[i]="red"
  } else if (obs_rf2[i]=="U") {
    b_rf2[i]="green"
  }
}
b_rf2
plot(1:89, a_rf2, type='p',xaxt='n', yaxt='n', col=b_rf2, xlab='obs',ylab='predict', main='Random Forests with all predictors in two classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_rf2[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("A","U"),pch=1,col=c("red","green"))

#Random Forset with PCA (two classes)
set.seed(1)
rf=train(response~.,data=nonstring, 
          trControl=trainControl(method="LOOCV", preProcOptions = list(thresh =0.95)),
          method='rf', 
         preProcess='pca',
          tuneGrid=data.frame(mtry=14))
print(rf)

importance_rf <- varImp(rf,scale=FALSE) #importance
importance_rf$importance
plot(importance_rf,top=20)
print(importance_rf)

rf$pred
pred_rf=as.factor(rf$pred[1]$pred)
obs_rf=as.factor(rf$pred[2]$obs)
confusionMatrix(data=pred_rf, reference=obs_rf)

#rf_plot
pred_rf = as.matrix(rf$pred[1])
obs_rf=as.matrix((rf$pred[2]))
a_rf =c(1:89)
for (i in c(1:89)) {
  if (pred_rf[i]==obs_rf[i]){
    a_rf[i]=1
  }else {
    a_rf[i]=0
  }
}
a_rf
b_rf=rep("red",89)
b_rf
for (i in 1:89) {
  if (obs_rf[i]=="A"){
    b_rf[i]="red"
  } else if (obs_rf[i]=="U") {
    b_rf[i]="green"
  }
}
b_rf
plot(1:89, a_rf, type='p',xaxt='n', yaxt='n', col=b_rf, xlab='obs',ylab='predict', main='Random Forests with PCA in two classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_rf[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("A","U"),pch=1,col=c("red","green"))


#three class
set.seed(1)
rf3=train(WB_Pheno~.,data=nonstring2, 
          trControl=trainControl(method="LOOCV"),
          method='rf', 
          tuneGrid=data.frame(mtry=14))
print(rf3)

importance_rf3 <- varImp(rf3,scale=FALSE) #importance
importance_rf3$importance
plot(importance_rf3,top=20)
print(importance_rf3)

rf3$pred
pred_rf3=as.factor(rf3$pred[1]$pred)
obs_rf3=as.factor(rf3$pred[2]$obs)
confusionMatrix(data=pred_rf3, reference=obs_rf3)

#rf3_plot
pred_rf3 = as.matrix(rf3$pred[1])
obs_rf3=as.matrix((rf3$pred[2]))
a_rf3 =c(1:89)
for (i in c(1:89)) {
  if (pred_rf3[i]==obs_rf3[i]){
    a_rf3[i]=1
  }else {
    a_rf3[i]=0
  }
}
a_rf3
b_rf3=rep("red",89)
b_rf3
for (i in 1:89) {
  if (obs_rf3[i]=="Affected"){
    b_rf3[i]="red"
  } else if (obs_rf3[i]=="Partial") {
    b_rf3[i]="green"
  }else {
    b_rf3[i]="blue"
  }
}
b_rf3
plot(1:89, a_rf3, type='p',xaxt='n', yaxt='n', col=b_rf3, xlab='obs',ylab='predict', main='Random Forests with all predictors in three classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_rf3[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("Affected","Partial","Unaffected"),pch=1,col=c("red","green", "blue"))

#Random Forest with pca (three classes)
set.seed(1)
preproc=preProcess(nonstring2, c('pca'), thresh=0.95)
preproc$rotation
preproc

rf1=train(WB_Pheno~.,data=nonstring2, 
              trControl=trainControl(method="LOOCV", preProcOptions = list(thresh =0.95)),
              method='rf', 
          preprocess="pca",
              tuneGrid=data.frame(mtry=14))
print(rf1)

importance_rf1 <- varImp(rf1,scale=FALSE) #importance
importance_rf1$importance
plot(importance_rf1,top=20)
print(importance_rf1)

rf1$pred
pred_rf1=as.factor(rf1$pred[1]$pred)
obs_rf1=as.factor(rf1$pred[2]$obs)
confusionMatrix(data=pred_rf1, reference=obs_rf1)

#rf1_plot
pred_rf1 = as.matrix(rf1$pred[1])
obs_rf1=as.matrix((rf1$pred[2]))
a_rf1 =c(1:89)
for (i in c(1:89)) {
  if (pred_rf1[i]==obs_rf1[i]){
    a_rf1[i]=1
  }else {
    a_rf1[i]=0
  }
}
a_rf1
b_rf1=rep("red",89)
b_rf1
for (i in 1:89) {
  if (obs_rf1[i]=="Affected"){
    b_rf1[i]="red"
  } else if (obs_rf1[i]=="Partial") {
    b_rf1[i]="green"
  }else {
    b_rf1[i]="blue"
  }
}
b_rf1
plot(1:89, a_rf1, type='p',xaxt='n', yaxt='n', col=b_rf1, xlab='obs',ylab='predict', main='Random Forests with PCA in three classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_rf1[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("Affected","Partial","Unaffected"),pch=1,col=c("red","green", "blue"))

#ElasticNet
#scale and center
#three classes
ENmodel=train(WB_Pheno~., data=nonstring2, method='glmnet', 
              trControl=trainControl(method="LOOCV"),
              preProcess=c('center', 'scale'),
              tuneGrid=expand.grid(alpha=seq(0.1,0.9,0.01),
                                   lambda=seq(0,0.1,by=0.01)))
print(ENmodel)
ENmodel$bestTune

coef(ENmodel$finalModel, ENmodel$bestTune$lambda)

Efinal_0=train(WB_Pheno~., data=nonstring2, method="glmnet", 
               trControl=trainControl(method = "LOOCV"),
               preProcess = c("center", "scale"),
               tuneGrid =expand.grid(alpha = 0.84,lambda = 0.08))
print(Efinal_0)
Epred_0<- as.factor(Efinal_0$pred[1]$pred)
Eobs_0<- as.factor(Efinal_0$pred[2]$obs)
confusionMatrix(data=Epred_0, reference=Eobs_0)

#enet3_plot
pred_enet3 = as.matrix(Efinal_0$pred[1])
obs_enet3=as.matrix((Efinal_0$pred[2]))
a_enet3 =c(1:89)
for (i in c(1:89)) {
  if (pred_enet3[i]==obs_enet3[i]){
    a_enet3[i]=1
  }else {
    a_enet3[i]=0
  }
}
a_enet3
b_enet3=rep("red",89)
b_enet3
for (i in 1:89) {
  if (obs_enet3[i]=="Affected"){
    b_enet3[i]="red"
  } else if (obs_enet3[i]=="Partial") {
    b_enet3[i]="green"
  }else {
    b_enet3[i]="blue"
  }
}
b_enet3
plot(1:89, a_enet3, type='p',xaxt='n', yaxt='n', col=b_enet3, xlab='obs',ylab='predict', main='ENET in three classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_enet3[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("Affected","Partial","Unaffected"),pch=1,col=c("red","green", "blue"))


#Two Classes
#leave one out
ENmodel1=train(response~., data=nonstring, method="glmnet", 
               trControl=trainControl(method = "LOOCV"),
               preProcess = c("center", "scale"),
               tuneGrid =expand.grid(alpha = seq(0.1,0.9,0.01),
                                     lambda = seq(0,0.1,by = 0.01)))
print(ENmodel1)
ENmodel1$bestTune

coef(ENmodel1$finalModel, ENmodel1$bestTune$lambda)

Efinal_1=train(response~., data=nonstring, method="glmnet", trControl=trainControl(method = "LOOCV"),
               preProcess = c("center", "scale"),
               tuneGrid =expand.grid(alpha = 0.81,lambda = 0.02))
print(Efinal_1)
Epred_1<- as.factor(Efinal_1$pred[1]$pred)
Eobs_1<- as.factor(Efinal_1$pred[2]$obs)
confusionMatrix(data=Epred_1, reference=Eobs_1)

#enet2_plot

pred_enet2 = as.matrix(Efinal_1$pred[1])
obs_enet2=as.matrix((Efinal_1$pred[2]))
a_enet2 =c(1:89)
for (i in c(1:89)) {
  if (pred_enet2[i]==obs_enet2[i]){
    a_enet2[i]=1
  }else {
    a_enet2[i]=0
  }
}
a_enet2
b_enet2=rep("red",89)
b_enet2
for (i in 1:89) {
  if (obs_enet2[i]=="A"){
    b_enet2[i]="red"
  } else if (obs_enet2[i]=="U") {
    b_enet2[i]="green"
  }
}
b_enet2
plot(1:89, a_enet2, type='p',xaxt='n', yaxt='n', col=b_enet2, xlab='obs',ylab='predict', main='ENET in two classes')
axis(side=1, at=c(1:89), labels=c(1:89))
axis(side=2, at=c(0,1), labels=c(0,1))
for (i in 1:89) {
  if (a_enet2[i]==0) {
    abline(v=i,col="grey")
  }
}
legend(x=80,y=1.4,xpd=TRUE,title="obs",legend=c("A","U"),pch=1,col=c("red","green"))






### split data training - 0.7, test- 0.3, useing training data with 10-fold to get hyperpramater, then use it in test data
library(caret)
set.seed(54321)
## three classes
index <- createDataPartition(nonstring2$WB_Pheno, times=1, p=0.7, list=FALSE)
nonstring2.train <- nonstring2[index, ]
print(nonstring2.train)
nonstring2.test <- nonstring2[-index, ]
print(nonstring2.test)

# check the ration is almost same
prop.table(table(nonstring2.train$WB_Pheno))
prop.table(table(nonstring2.test$WB_Pheno))

train.control <- trainControl(method ="repeatedcv", number = 10, repeats=1, search="grid")

#logistic
library(mlbench)
fit.glm <- train(WB_Pheno~., data=nonstring2.train, method="glmnet",
                 family="multinomial",
                 type.multinomial ="grouped",
                 trControl=train.control)

print(fit.glm)
test_pre <- predict(fit.glm, nonstring2.test)
print(test_pre)

obs <- as.factor(nonstring2.test$WB_Pheno)
print(obs)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(fit.glm)
print(imp.glm)

#lasso
library(mlbench)
lasso3 <- train(WB_Pheno~., data=nonstring2.train, method="glmnet",
                 trControl=train.control)

print(lasso3)
test_pre <- predict(lasso3, nonstring2.test)
print(test_pre)

obs <- as.factor(nonstring2.test$WB_Pheno)
print(obs)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(lasso3)
print(imp.glm)

#SVMlinear
svmlinear3 <- train(WB_Pheno~., data=nonstring2.train, method="svmLinear",
                   tuneGrid = expand.grid(C = seq(0.1, 2.1, length =10)),
                   trControl=train.control)
print(svmlinear3)
test_pre <- predict(svmlinear3, nonstring2.test)
obs <- as.factor(nonstring2.test$WB_Pheno)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(svmlinear3)
print(imp.glm)

#SVMradial
svmradial3 <- train(WB_Pheno~., data=nonstring2.train, method="svmRadial",
                    tuneGrid=data.frame(C=10, sigma=0.001),
                    trControl=train.control)
print(svmradial3)
test_pre <- predict(svmradial3, nonstring2.test)
obs <- as.factor(nonstring2.test$WB_Pheno)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(svmradial3)
print(imp.glm)

#Random forest
rf3 <- train(WB_Pheno~., data = nonstring2.train, method ="rf",
             tuneGrid=expand.grid(.mtry=c(1:15)),
             trControl=train.control)
print(rf3)
test_pre <- predict(rf3, nonstring2.test)
obs <- as.factor(nonstring2.test$WB_Pheno)
confusionMatrix(data=test_pre, reference=obs)
imp.glm <- varImp(rf3)
print(imp.glm)

#ElasticNet
en3=train(WB_Pheno~., data=nonstring2.train, method='glmnet', 
              trControl=train.control,
              preProcess=c('center', 'scale'),
              tuneGrid=expand.grid(alpha=seq(0.1,0.9,0.01),
                                   lambda=seq(0,0.1,by=0.01)))
print(en3)
test_pre <- predict(en3, nonstring2.test)
obs <- as.factor(nonstring2.test$WB_Pheno)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(en3)
print(imp.glm)


#two class
set.seed(1)
nonstring3 = nonstring1[, 4:196]
nonstring3$response <- NA #two classes, responses are U and A
for (i in c(1:89)) {
  if (nonstring3$WB_Pheno[i]=="Unaffected"){
    nonstring3$response[i]="U"
  } else{
    nonstring3$response[i]="A"
  }
}
nonstring3$response

nonstring=nonstring3[, -1] #two classes data set
head(nonstring)
table(nonstring$response)

library(caret)
set.seed(54321)
index2 <- createDataPartition(nonstring$response, times=1, p=0.7, list=FALSE)
nonstring.train <- nonstring[index2, ]
print(nonstring.train)
nonstring.test <- nonstring[-index2, ]
print(nonstring.test)

# check the ration is almost same
prop.table(table(nonstring.train$response))
prop.table(table(nonstring.test$response))

train.control <- trainControl(method ="repeatedcv", number = 10, repeats=1, search="grid")

#logistic
library(mlbench)
logistic2 <- train(response~., data=nonstring.train, method="glmnet",
                 family="multinomial",
                 type.multinomial ="grouped",
                 trControl=train.control)

print(logistic2)
test_pre <- predict(logistic2, nonstring.test)
print(test_pre)

obs <- as.factor(nonstring.test$response)
print(obs)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(logistic2)
print(logistic2)

#lasso
library(mlbench)
lasso2 <- train(response~., data=nonstring.train, method="glmnet",
                trControl=train.control)

print(lasso2)
test_pre <- predict(lasso2, nonstring.test)
print(test_pre)

obs <- as.factor(nonstring.test$response)
print(obs)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(lasso2)
print(imp.glm)

#SVMlinear
svmlinear2 <- train(response~., data=nonstring.train, method="svmLinear",
                    tuneGrid = expand.grid(C = seq(0.1, 2.1, length =10)),
                    trControl=train.control)
print(svmlinear2)
test_pre <- predict(svmlinear2, nonstring.test)
obs <- as.factor(nonstring.test$response)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(svmlinear)
print(imp.glm)

#SVMradial
svmradial2 <- train(response~., data=nonstring.train, method="svmRadial",
                    tuneGrid=data.frame(C=10, sigma=0.001),
                    trControl=train.control)
print(svmradial2)
test_pre <- predict(svmradial2, nonstring.test)
obs <- as.factor(nonstring.test$response)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(svmradial2)
print(imp.glm)

#Random forest
rf2 <- train(response~., data = nonstring.train, method ="rf",
             tuneGrid=expand.grid(.mtry=c(1:15)),
             trControl=train.control)
print(rf2)
test_pre <- predict(rf2, nonstring.test)
obs <- as.factor(nonstring.test$response)
confusionMatrix(data=test_pre, reference=obs)
imp.glm <- varImp(rf2)
print(imp.glm)

#ElasticNet
en2=train(response~., data=nonstring.train, method='glmnet', 
          trControl=train.control,
          preProcess=c('center', 'scale'),
          tuneGrid=expand.grid(alpha=seq(0.1,0.9,0.01),
                               lambda=seq(0,0.1,by=0.01)))
print(en2)
test_pre <- predict(en2, nonstring.test)
obs <- as.factor(nonstring.test$response)
confusionMatrix(test_pre, obs)
imp.glm <- varImp(en2)
print(imp.glm)


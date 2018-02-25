##status prediction

classif <- read.csv(file.choose())
summary(classif)
##   These are the columns having N/A inst ,ph.ecog          ph.karno        pat.karno         meal.cal         wt.loss
head(classif$inst)
##imputing missing values
attach(classif)
##mean / mode imputation
library(Hmisc)
classif$inst <- impute(classif$inst,mean)
classif$ph.ecog <- as.factor(classif$ph.ecog)
classif$ph.ecog <- impute(classif$ph.ecog,mode)##could see from data that there are 3 levels hence factor
classif$pat.karno <- impute(classif$pat.karno,mean)
classif$meal.cal <- impute(classif$meal.cal,mean)
classif$wt.loss <- impute(classif$wt.loss,mean)
classif$ph.karno <- impute(classif$ph.karno,mean)

summary(classif)


head(classif)


##Data splitting

classif$X <- 1:dim(classif)[1]
dtrain<-subset(classif,X <= dim(classif)[1]*0.7); 
dtest<-subset(classif,X > dim(classif)[1]*0.7)

dtrain$Y<-ifelse(dtrain$status =='1',1,0)

dtest$Y <- ifelse(dtest$status == '1',1,0)
##Converting factor level 1 & 2 as 1 & 0
 none<-glm(Y ~1,data=dtrain,family=binomial(link='logit'))
 all<-glm(Y ~ sex+age+pat.karno+meal.cal,data=dtrain,family=binomial(link='logit'))
 step(none, scope=list(lower=none, upper=all), direction="forward")
 step(all, scope=list(lower=all, upper=none), direction="backward")
##shows non covergent problem
 
 ##using rpart
 library(rpart)
 rprt <- rpart(Y ~ .,data = dtrain)
summary(rprt)
defaultSummary(rprt)
 printcp(rprt)
 plotcp(rprt)
 
  table(dtrain$Y,predict(rprt,data=dtrain))##159 rows and 138 predicted for level 0 ,21 predicted for level 1
  nrow(dtrain)
  
  predict(rprt,newdata = dtest)
  table(predict(rprt,newdata = dtest))
  
  library(e1071)
  library(caret)
  varImp(rprt)
  
  ##indicates time,status,meal.cal,sex are important factors
  
  pred = predict(rprt, newdata=dtest)
  accuracy <- table(pred, dtest[,"Y"])
  sum(diag(accuracy))/sum(accuracy)
  
  ##rprt shows 100% accuracy
  
  
  
  glm_fit <- glm(Y ~ . , data=dtrain)
  ##meal.cal+sex+ph.ecog
  summary(glm_fit)
  pred_glm = predict(glm_fit, newdata=dtest)
  accuracy_glm <- table(pred_glm, dtest[,"Y"])
  sum(diag(accuracy_glm))/sum(accuracy_glm)
  
  
  pred_glm = predict(glm_fit, newdata=dtest)
  confusionMatrix(data=pred_glm, dtest$Y)
  ##Confusion Matrix metrics shows 100% accuracy ,and 100% precision 
  ##Actual survived is labelled as survived and actual dead is labelled dead

 
  
  
  
  ###ROC curve for testing model fittnes
  library(ROCR)
  predic_glm <- prediction(pred_glm,dtest$Y)
  roc <- performance(predic_glm, "tpr", "fpr")
  plot(roc, colorize = "TRUE")
  ##Roc curve shows that the x & y axis lines are along axis otherwords a good fit
  
  

  km<- with(classif,Surv(time,status))
head(km)
##using survival fnction to predict survival a + indicates patient alive,else death

km_fit <- survfit(Surv(time, status) ~ 1, data=classif)
summary(km_fit, times = c(1,30,60,90*(1:10)))
plot(km_fit, xlab="Days", main = 'Survival Plot')






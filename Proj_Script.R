setwd("D:\\Aditya\\for group 8 - STARCH_final paper data with readership")
projdata=read.csv("Data.csv")
str(projdata)
table(projdata$v7)

summary(projdata$readersh) 

#DataExploration------------------------------

table(projdata$pagetype)
projdata<-projdata[projdata$pagetype!=19,]

table(projdata$position)
table(projdata$v7)
projdata<-projdata[projdata$v7<=5,]
table(projdata$page)
table(projdata$paper_co)
summary(projdata$v17)
projdata$v18<-NULL
projdata$v17<-NULL
str(projdata)


which(is.na(projdata$readersh))
#unique (unlist (lapply (projdata, function (projdata) which (is.na (projdata)))))
projdata<-projdata[!is.na(projdata$readersh),]
projdata<-projdata[!is.na(projdata$type),]
summary(projdata$readersh)


nrow(projdata)
table(projdata$type)
summary(projdata$area)
which(projdata$area == 0)
projdata<-subset(projdata,projdata$area!=0)
nrow(projdata)

table(projdata$pagetype)
table(projdata$city)
projdata$chennai<-NULL
projdata$madurai<-NULL
projdata$namakkal<-NULL
str(projdata)
summary(projdata$length)
table(projdata$length)

#Imputing the Lenght and breadth


impute<-function(x){
 for(i in 1:nrow(x)){
  if(x$area[i]>0 & is.na(x$length[i]) & is.na(x$breadth[i])){ 
    x$length[i]<-sqrt(x$area[i])
    x$breadth[i]<-sqrt(x$area[i])
  }
}
return(x)
}


projdata<-impute(projdata)
summary(projdata$length)
str(projdata)
table(projdata$position)
table(projdata$positio2)
projdata<-projdata[!is.na(projdata$v7recode),]
table(projdata$v7recode)
nrow(projdata)
is.na(projdata$positio2)

projdata$positio2[is.na(projdata$positio2)] = 0
projdata$category[is.na(projdata$category)] = 0
projdata$topic[is.na(projdata$topic)] = 0
summary(projdata)
str(projdata)
projdata<-subset(projdata,projdata$topic<=15)


#splitting after bucketting into three classes

factor_readersh<-function(data){
  for(i in 1:nrow(data)){
    if(data$readersh[i] < 22.86){
      data$ReaderShClass[i]<-"LOW"
    }
    else if(data$readersh[i] >= 22.86 & data$readersh[i] <= 66.67){
      data$ReaderShClass[i]<-"MED"
    }
    else{
      data$ReaderShClass[i]<-"HIGH"
    }
  }
  return(data)
}
summary(projdata$readersh)

#Bucketting the Readership into Two classes.

projdata$ReaderShClass<-ifelse(projdata$readersh>=50,"High","Low")
str(projdata$ReaderShClass)
#projdata<-factor_readersh(projdata)

#Distribution of The new bucketted variable.
table(projdata$ReaderShClass)

projdataTrain<-NULL
projdataTest<-NULL

#Splitting in training and test set.
library(caTools)
set.seed(100)
split=sample.split(projdata$ReaderShClass,SplitRatio=0.75)
projdataTrain=subset(projdata,split==TRUE)
projdataTest=subset(projdata,split==FALSE)
nrow(projdataTrain)
nrow(projdataTest)
table(projdataTrain$ReaderShClass)
table(projdataTest$ReaderShClass)
#Training set preparation


projdataTrain$master_d<-NULL
projdataTrain$s.no<-NULL
projdataTrain$brand<-NULL
projdataTrain$ReaderShClass<-as.factor(projdataTrain$ReaderShClass)
projdataTrain$readersh<-NULL
projdataTrain$paper_co<-as.factor(projdataTrain$paper_co)
projdataTrain$city<-as.factor(projdataTrain$city)
projdataTrain$citydow<-as.factor(projdataTrain$citydow)
projdataTrain$category<-as.factor(projdataTrain$category)
projdataTrain$article<-as.factor(projdataTrain$article)
projdataTrain$pagetype<-as.factor(projdataTrain$pagetype)
projdataTrain$all3var<-as.factor(projdataTrain$all3var)
projdataTrain$areacode<-as.factor(projdataTrain$areacode)
projdataTrain$v7<-as.factor(projdataTrain$v7)
projdataTrain$position<-as.factor(projdataTrain$position)
projdataTrain$positio2<-as.factor(projdataTrain$positio2)
projdataTrain$nppr<-as.factor(projdataTrain$nppr)
projdataTrain$dow<-as.factor(projdataTrain$dow)
projdataTrain$topic<-as.factor(projdataTrain$topic)
projdataTrain$type<-as.factor(projdataTrain$type)
projdataTrain$pprcity<-as.factor(projdataTrain$pprcity)
projdataTrain$pprdow<-as.factor(projdataTrain$pprdow)

projdataTrain$v7recode<-NULL
projdataTrain$article<-NULL

#Test set preparation

projdataTest$master_d<-NULL
projdataTest$s.no<-NULL
projdataTest$brand<-NULL
projdataTest$ReaderShClass<-as.factor(projdataTest$ReaderShClass)
projdataTest$readersh<-NULL
projdataTest$paper_co<-as.factor(projdataTest$paper_co)
projdataTest$city<-as.factor(projdataTest$city)
projdataTest$citydow<-as.factor(projdataTest$citydow)
projdataTest$category<-as.factor(projdataTest$category)
projdataTest$article<-as.factor(projdataTest$article)
projdataTest$pagetype<-as.factor(projdataTest$pagetype)
projdataTest$all3var<-as.factor(projdataTest$all3var)
projdataTest$areacode<-as.factor(projdataTest$areacode)
projdataTest$v7<-as.factor(projdataTest$v7)
projdataTest$position<-as.factor(projdataTest$position)
projdataTest$positio2<-as.factor(projdataTest$positio2)
projdataTest$nppr<-as.factor(projdataTest$nppr)
projdataTest$dow<-as.factor(projdataTest$dow)

projdataTest$topic<-as.factor(projdataTest$topic)
projdataTest$type<-as.factor(projdataTest$type)
projdataTest$pprcity<-as.factor(projdataTest$pprcity)
projdataTest$pprdow<-as.factor(projdataTest$pprdow)

projdataTest$v7recode<-NULL
projdataTest$article<-NULL
str(projdataTrain)
str(projdataTest)

#Modelling----------------------
library(rpart)
library(rpart.plot)
library(tree)
library(rattle)
library(RColorBrewer)

r.ctrl = rpart.control(minsplit=35, minbucket = 10, cp = 0, xval = 10)
modelCT = rpart(projdataTrain$ReaderShClass ~ ., data=projdataTrain[,-1],control = r.ctrl)
#Plotting the tree
prp(modelCT)
fancyRpartPlot(modelCT)

#Evaluating the value of complexity parameter to prune tree.
printcp(modelCT)
plotcp(modelCT)

#prune tree
mytree<-prune(modelCT,cp=0.015,"CP")
fancyRpartPlot(mytree, uniform=TRUE)




projdataTrain$predictCT.Class<-predict(mytree,projdataTrain, type="class")
projdataTrain$predictCT.Score<-predict(mytree,projdataTrain)

# 
# ## deciling code
# decile <- function(x){
#   deciles <- vector(length=10)
#   for (i in seq(0.1,1,.1)){
#     deciles[i*10] <- quantile(x, i, na.rm=T)
#   }
#   return (
#     ifelse(x<deciles[1], 1,
#            ifelse(x<deciles[2], 2,
#                   ifelse(x<deciles[3], 3,
#                          ifelse(x<deciles[4], 4,
#                                 ifelse(x<deciles[5], 5,
#                                        ifelse(x<deciles[6], 6,
#                                               ifelse(x<deciles[7], 7,
#                                                      ifelse(x<deciles[8], 8,
#                                                             ifelse(x<deciles[9], 9, 10
#                                                             ))))))))))
# }

## deciling
#projdataTrain$deciles <- decile(projdataTrain$predictCT.Score[,2])

library(ROCR)
pred <- prediction(projdataTrain$predictCT.Score[,2], projdataTrain$ReaderShClass)
perf <- performance(pred, "tpr", "fpr")
plot(perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
#install.packages("ineq")
library(ineq)
library("caret")
library("e1071")
gini = ineq(projdataTrain$predictCT.Score[,2], type="Gini")

#Confusion MAtrix

with(projdataTrain, table(ReaderShClass, predictCT.Class))

confusionMatrix(projdataTrain$predictCT.Class,projdataTrain$ReaderShClass)

auc
KS
gini


#Eliminating the levels in category variable in the test set which may not be present in the test set.
projdataTest1<-projdataTest
projdataTest1<-subset(projdataTest,projdataTest$category != 24)
projdataTest1<-subset(projdataTest1,projdataTest1$category != 36)
table(projdataTest1$category)

projdataTest1$predictCT.Class<-predict(mytree,projdataTest1, type="class")
projdataTest1$predictCT.Score<-predict(mytree,projdataTest1)

with(projdataTest1,table(ReaderShClass,predictCT.Class))  


confusionMatrix(projdataTest1$predictCT.Class,projdataTest1$ReaderShClass)


#Confusion Matrix

table(projdataTest1$ReaderShClass,predictCT)
(95+152)/nrow(projdataTest1)
library("caret")
library("e1071")
confusionMatrix(predictCT,projdataTest1$ReaderShClass)


#ROC

library(ROCR)
?prediction
ROCRTest=prediction(predictCT,projdataTest1$ReaderShClass)
ROCR_Perf=performance(ROCRTest,"tpr","fpr")
ROCR_Perf
plot(ROCR_Perf,colorize=TRUE)
plot(ROCR_Perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))





### Random Forests 

?rpart

library(randomForest)


set.seed(1)
projdataTrain.Forest<-projdataTrain
projdataTest.Forest<-projdataTest
projdataTrain.Forest$article<-NULL
projdataTest.Forest$article<-NULL
ModelFrst <- randomForest(ReaderShClass~., data=projdataTrain.Forest,type="class")



## Test data prediction accuacy

(projdataTrain)

#to make only levels of training data be in test
projdataTest.Forest<-factor(projdataTest.Forest, levels=levels(projdataTrain.Forest))


predictFrst <- predict(ModelFrst,newdata=projdataTest.Forest,type="class")
table(predictFrst,test1$SharesClass)

##>



## to check importance of various variables in RF model
importance(ModelFrst)




#removing vars
projdataTrain_2<-projdataTrain
projdataTrain_2$paper_co<-NULL
projdataTrain_2$pprcity<-NULL
projdataTrain_2$pprdow<-NULL
projdataTrain_2$citydow<-NULL
projdataTrain_2$all3var<-NULL
projdataTrain_2$v7recode<-NULL
str(projdataTrain_2)

modelCT_2 = rpart(projdataTrain_2$ReaderShClass~., data=projdataTrain_2, method="class",minbucket=3,maxdepth=30)

prp(modelCT_2)
fancyRpartPlot(modelCT_2)
printcp(modelCT_2)
plotcp(modelCT_2)
cor(test)
test$length<-NULL
test$breadth<-NULL
test$v7recode<-NULL
str(test)
table(test$v7)
test_article<-subset(test,test$v7==1)
test_article$v7<-NULL
test_ad<-subset(test,test$v7==2)
test_ad$v7<-NULL
test_classified<-subset(test,test$v7==3)
fit1<-lm(readersh ~ .,data=projdata)
summary(fit1)
fit2<-lm(readersh ~ . -pagetype,data=test)
summary(fit2)
test$readersh<-as.factor(test$readersh)



test.dist<-dist(test)

test.hclust<-hclust(test.dist)
plot(test.hclust)
str(test)
test[-23]
test.matrix<-as.matrix(test)
test.matrix<-scale(test[-23])

kmeans.fit<-kmeans(test,3)
kmeans.fit$cluster
kmeans.fit$size
library(cluster)
install.packages("fpc")
library(fpc)
plotcluster(test,kmeans.fit$cluster)
clusplot(test, kmeans.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=2, lines=0)
hclustfunc <- function(x, method = "complete", dmeth = "euclidean") {    
  hclust(dist(x, method = dmeth), method = method)
}
trials<-hclustfunc(projdata)
rect.hclust(trials, 5)
plot(trials,hang=-1)
hist(projdata$v7)

########################################################################


imputeNA<-function(y,var)
{
  for(i in 1:nrow(y)) 
  {
    if(is.na(y[i,var])){
      y[i,var]<-0
    } 
  }
  return(y)
}
imputeNA<-NULL
imputeNA2<-function(u,var1){
  return(ifelse((!is.na(u$var1)),u$var1,0))
}




projdata$ReaderShClass=ifelse(projdata$readersh > 50 , 1,0)
table(projdata$ReaderShClass)
library(caTools)
set.seed(100)
split=sample.split(projdata$ReaderShClass,SplitRatio=0.75)
projdataTrain=subset(projdata,split==TRUE)
projdataTest=subset(projdata,split==FALSE)
str(projdataTrain)
table(projdataTrain$ReaderShClass)
table(projdataTest$ReaderShClass)
summary(projdataTrain)
modelTrain=glm(ReaderShClass ~ v7+area+page+type+position+positio2+category+topic+brand+city+nppr+dow+v7recode+pagetype ,data=projdataTrain,family=binomial)

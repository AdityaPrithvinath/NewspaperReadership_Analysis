
# Data Loading #

setwd("D:\\Aditya\\for group 8 - STARCH_final paper data with readership")
projdata=read.csv("Data.csv")


# Data Exploration #

---Columns check -----
names(projdata)

--Overall view -----
str(projdata)

--Top and bottom rows ----------

head(projdata)
tail(projdata)

--Summary Data View ---------

summary(projdata)
str(projdata)

sum(is.na(projdata))

#  Variable analysis  #

---- Individual

hist(projdata$paper_co)
plot(projdata$paper_co)
table(projdata$paper_co)
sum(is.na(projdata$paper_co))

var(projdata$page)
sum(is.na(projdata$page))
hist(projdata$page) -- S-1, S-4, S-8, s1, s8 supplement paper data 
plot(projdata$page))
table(projdata$page)

var(projdata$readersh)
sum(is.na(projdata$readersh)) --1726
hist(projdata$readersh)
plot(projdata$readersh)
table(projdata$readersh)

var(projdata$category)
sum(is.na(projdata$category))
hist(projdata$category)
plot(projdata$category)
table(projdata$category)

var(projdata$v7)
sum(is.na(projdata$v7))
hist(projdata$v7)
plot(projdata$v7)
table(projdata$v7)

var(projdata$v7)
sum(is.na(projdata$v7))
hist(projdata$v7)
plot(projdata$v7)
table(projdata$v7)

var(projdata$v7)
sum(is.na(projdata$v7))
hist(projdata$v7)
plot(projdata$v7)
table(projdata$v7)



## DATA PREPERATION ##

----V7 unknown values removal -----------------------------

table(projdata$v7)
projdata<-projdata[projdata$v7<=5,]

--- Supplement value replace and covrt to numeric for  page --------------------

table(projdata$page)

install.packages("plyr")
#-----Creating another data set to be used for logistic reg----- 
projdata_logistic<-projdata

data(projdata_logistic)
 library(plyr)
 revalue(projdata_logistic$page, c("s1" = "41")) -> projdata_logistic$page
 revalue(projdata_logistic$page, c("S-1" = "41")) -> projdata_logistic$page
 revalue(projdata_logistic$page, c("S-4" = "42")) -> projdata_logistic$page
 revalue(projdata_logistic$page, c("s8" = "43")) -> projdata_logistic$page
 revalue(projdata_logistic$page, c("S-8" = "43")) -> projdata_logistic$page

projdata_logistic$page<-as.numeric(levels(projdata_logistic$page))[projdata_logistic$page]

#--------- Removing unused columns -------------------------------

summary(projdata_logistic$v17)
projdata_logistic$v18<-NULL
projdata_logistic$v17<-NULL
str(projdata_logistic)
#----Already prepared.Need not execute this.------------
------- Removing all NA valus columns for Readership --------------------

which(is.na(projdata$readersh))
#unique (unlist (lapply (projdata, function (projdata1) which (is.na (projdata)))))
projdata<-projdata[!is.na(projdata$readersh),]
projdata<-projdata[!is.na(projdata$type),]

str(projdata$readersh)
nrow(projdata)

------ Modifying dataframe for area = 0 -----------------

summary(projdata$area)
which(projdata$area == 0)
projdata<-subset(projdata,projdata$area!=0)
nrow(projdata)

----- Removing redundant columns for city ---------------

table(projdata$city)
projdata$chennai<-NULL
projdata$madurai<-NULL
projdata$namakkal<-NULL
str(projdata)

----- Replacing NA with area calculation -------------------------

summary(projdata$length)
table(projdata$length)
test<-subset(projdata,is.na(projdata$length))
test1<-subset(projdata,is.na(projdata$breadth))

nrow(test1)
nrow(test)


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

nrow(projdata)


------------- extra page type handling -----------

table(projdata$pagetype)
projdata<-projdata[projdata$pagetype!=19,]

------------ Modifying position fields -------------

table(projdata$position)
table(projdata$positio2)
projdata<-projdata[!is.na(projdata$v7recode),]
table(projdata$v7recode)
nrow(projdata)

is.na(projdata$positio2)
test<-NULL
test<-projdata
projdata$positio2[is.na(projdata$positio2)] = 0
projdata$category[is.na(projdata$category)] = 0
projdata$topic[is.na(projdata$topic)] = 0
is.na(test$positio2)
summary(projdata)
str(projdata)
test<-NULL

---------- Topic column outliers remooval ------------

table(projdata$topic)
projdata<-subset(projdata,projdata$topic<=15)

table(projdata$readersh)

#----start execution again from hre---------------

##  CORRELATION CHECK ##
table(projdata_logistic$ReaderShClass)
projdata_logistic$readersh<-NULL

names(projdata_logistic)
str(projdata_logistic)

impute_ClassAsBinary<-function(x){
  for(i in 1:nrow(x)){
    x$readersh<-ifelse(x$ReaderShClass=="Low",0,1)
  }
  return(x)
}
projdata_logistic<-impute_ClassAsBinary(projdata_logistic)
corr<-projdata_logistic

corr$article<-NULL
corr$brand<-NULL
corr$master_d<-NULL
corr$s.no<-NULL

cor(corr)

 install.packages("ellipse")
library(ellipse)

ctab <- cor(corr)
round(ctab, 2)
plotcorr(ctab, mar = c(0.1, 0.1, 0.1, 0.1))
colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
plotcorr(ctab, col=rgb(colorfun((ctab+1)/2), maxColorValue=255),
         mar = c(0.1, 0.1, 0.1, 0.1))


########### REGRESSION ################

-------------------------------------- Linear --------------------------

Lin_Sin<-lm(readersh~v7+type+position+areacode+city+page,data=projdata_logistic)
summary(Lin_Sin)


Lin_Int<-lm(readersh~paper_co+v7+type+position+category+topic+areacode+all3var+pagetype+page,data=projdata_logistic)
summary(Lin_Int)


-------------------------------------- Logistic ---------------------------

summary(projdata_logistic$readersh)

# splitting after bucketing

library(caTools)
factor_readersh<-function(data){
    for(i in 1:nrow(data)){
        if(data$readersh[i] < 50){
            data$readershClass[i]<-0
        }
        else{
            data$readershClass[i]<-1
        }
    }
    return(data)
}

projdata$readershclass<-ifelse(projdata$readersh<50,0,1)

str(projdata)


sub <- sample(nrow(projdata), floor(nrow(projdata) * 0.70))
training <- projdata[sub, ]
testing <- projdata[-sub, ]

str(testing)

----------- Individual -------------------

Log_reg<-glm(readershclass~paper_co+v7+type+position+category+areacode+page,family=binomial(logit),data=training)
summary(Log_reg)

# Log_reg<-glm(readershclass~master_d+s.no+paper_co+page+v7+area+length+breadth+type+position+positio2
# +category+topic+topic+areacode+city+nppr+dow+pprcity+pprdow+citydow+all3var+v7recode+pagetype,family=binomial(logit),data=training)

# step(Log_reg,direction="both")
# names(projdata)

exp(Log_reg$coefficients)

exp(confint(Log_reg))
#Log_reg
pred = predict(Log_reg,newdata=testing,type="response")
#length(pred.SharesClass)
pred.SharesClass=round(pred)
#table(pred.SharesClass)
table(pred.SharesClass, testing$readershclass)

#install.packages("caret")
#install.packages("e1071")
library("caret")
library("e1071")
confusionMatrix(pred.SharesClass,testing$readershclass)


--- Iteration 2 -----------

Log_reg<-glm(readershclass~v7+position+areacode+page,family=binomial(logit),data=training)
summary(Log_reg)

# Log_reg<-glm(readershclass~master_d+s.no+paper_co+page+v7+area+length+breadth+type+position+positio2
# +category+topic+topic+areacode+city+nppr+dow+pprcity+pprdow+citydow+all3var+v7recode+pagetype,family=binomial(logit),data=training)

# step(Log_reg,direction="both")
# names(projdata)

exp(Log_reg$coefficients)

exp(confint(Log_reg))
#Log_reg
pred = predict(Log_reg,newdata=testing,type="response")
#length(pred.SharesClass)
pred.SharesClass=round(pred)
#table(pred.SharesClass)
table(pred.SharesClass, testing$readershclass)

#install.packages("caret")
#install.packages("e1071")
library("caret")
library("e1071")
confusionMatrix(pred.SharesClass,testing$readershclass)


------------ Interaction --------------------


Log_reg<-glm(readershclass~paper_co+v7+type+areacode+all3var+pagetype+page,family=binomial(logit),data=training)
summary(Log_reg)


 Log_reg<-glm(formula = readershclass ~ paper_co + page + v7 + 
    type + position + category + areacode, family = binomial(logit), 
    data = training)

--------------- Segmentation -----------------------------------------

Article<-projdata[projdata$v7==1,]
Ad<-projdata[projdata$v7==2,]

table(Article$v7)
table(Ad$v7)



---------------- ROC CURVE -------------------------------------------


prob=predict(Log_reg,type=c("response"))
training$prob=prob

# install.packages("ROCR")
library(pROC)
g <- roc(readershclass ~ prob, data = training)
plot(g)

roc(Log_reg)


yhat <- predict(Log_reg, testing, type = "response")

library(ROCR)
score <- prediction(yhat, testing$readershclass)
plot(performance(score, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")


#CART

setwd("C:\\Users\\udatla\\Desktop\\GLIM\\Capstone")
projdata=read.csv("data.csv")
str(projdata)
table(projdata$v7)

summary(projdata$readersh) 

#DataExploration------------------------------
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

plot(perf,colorize=TRUE)


KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
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
projdataTest1<-NULL
projdataTest1<-projdataTest
str(projdataTest1)
projdataTest1<-subset(projdataTest,projdataTest$category != 9)
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















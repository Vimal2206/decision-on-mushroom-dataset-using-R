##descion tree-> if else conditon  (probability approach) or axis parellel model(geometric identification)
# descion tree also known as (CART) classifcation reg tree
##  whenever final node came is called as terminal node or leaf node
#in between the branch came is called branch node
# start point is root node or descison node
## prouning is two types 1. prepruning 2.post pruning
#methods of pruning 1.gini index 2.information gain 3.entropy
# Decision tree is:
# if else condition or axis parallel model 
# probabilistic approach or geometric approach 
# DT is also known as cart algorithm
# pre Pruning and post pruning
# Gini index, entropy, information gain 

female<- (0.2)^2 + (0.8)^2
female

male<- (0.65)^2 + (0.35)^2
male

Gender_gini <- (10/30)*0.68 + (20/30)*0.545
Gender_gini


nineth<- (0.43)^2 +(0.57)^2
nineth

tenth <- (0.56)^2 + (0.44)^2
tenth

class_gini<- (14/30)*0.5098 +(16/30)*0.5072
class_gini
Gender_gini
# ks test (sup|diff Q - diff p|)
# back propagation only works when data is differnciable
# difference between ks test and kl diverngence is ks test is not divernciable because it have supreme function  and kl divergence is differnciable(because it have  a log function )

female <- (2/10)*log2(2/10) - (8/10)*log2(8/10)
female

male<- (13/20)*log2(13/20) - (7/20)*log2(7/20)
male

gender_entropy <- (10/30)*0.7219281 + (20/30)*0.9340681
gender_entropy


### Class

ninth <- -(6/14)*log2 (6/14)- (8/14)*log2(8/14)
ninth 

tenth<- -(9/16)*log2(9/16)- (7/16)*log2(7/16)
tenth

class_entropy<- (14/30)*0.9852 +(16/30)*0.9886
class_entropy
gender_entropy 


# project mushroom
# import dataset
mydata<-read.csv(choose.files())
head(mydata)
str(mydata)
# dep variable= class
table(mydata$class)
dim(mydata)
# binary class problem and balanced dataset given
mydata$class<-ifelse(mydata$class=='e',1,0)
table(mydata$class)
dim(mydata)
colSums(is.na(mydata))
# encoding part required
#library(caret)
#dummy<-dummyVars('~ .',data = mydata)
#dummy
#str(mydata)
#table(mydata$stalk.root)
#mydata<- data.frame(pre)
table(mydata$cap.shape)
table(mydata$cap.surface)
table(mydata$cap.color)
table(mydata$bruises)
table(mydata$odor)
table(mydata$gill.attachment)
table(mydata$gill.spacing)
table(mydata$gill.size)
table(mydata$stalk.root)# in this we have a ? in the datset so we have to change
mydata$stalk.root<- ifelse(mydata$stalk.root=='?','b',mydata$stalk.root)
mydata$stalk.root
table(mydata$stalk.surface.above.ring)
table(mydata$stalk.surface.below.ring)
table(mydata$stalk.color.above.ring)
table(mydata$stalk.color.below.ring)
table(mydata$veil.type)# it have only one value in the column
mydata$veil.type<-ifelse(mydata$veil.type=='p',0,0)
table(mydata$veil.color)
table(mydata$ring.number)
table(mydata$ring.type)
table(mydata$spore.print.color)
table(mydata$population)
table(mydata$habitat)
# encoding part required
library(caret)
dummy<-dummyVars('~ .',data = mydata)
dummy
mydata<-data.frame(predict(dummy,newdata=mydata))
head(mydata)
str(mydata)
names(mydata)
dim(mydata)
mydata<-mydata[,-c(2,8,12,22,24,33,51,53,57,61,65,74,83,84,88,91,96,105,111)]
mydata
# split the data into training# split the data into train and test for building the model 

library(caTools)
set.seed(123)
split<- sample.split(mydata$class, SplitRatio = 0.75)
split
table(split)
training<- subset(mydata, split==TRUE)
test<- subset(mydata, split==FALSE)
print(table(split))

print(nrow(training))
print(nrow(test))

# model building 

install.packages("rpart")
library(rpart)
dtree<-rpart(class~.,data=mydata)
summary(dtree)
plot(dtree)
text(dtree)

install.packages("rattle")
library(rattle)
fancyRpartPlot(dtree)

#predict the model by using test dataset
ypred<- predict(dtree, newdata= test)
ypred
ypred_value<- ifelse(ypred>=0.5,1,0)
ypred_value


#building confusion matrix 
cm<-table(test$class, ypred_value)
library(caret)
confusionMatrix(cm)
# test accuracy:99.2 
dtree_pred_train<-predict(dtree,newdata = training)
dtree_pred_train<-ifelse(dtree_pred_train>=0.5,1,0)
cm1<-table(training$class,dtree_pred_train)
confusionMatrix(cm1)
# training accuracy:98.8
# test accuracy:99.2
# there is neither higher variance nor high bias problem
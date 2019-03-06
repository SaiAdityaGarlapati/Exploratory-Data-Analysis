# assignment 2
library(dplyr)

train=read.csv("D:/assignments/asses2/train.csv")
test=read.csv("D:/assignments/asses2/train.csv")
nfl=read.csv("D:/assignments/asses2/NFL.csv")
titanic=read.csv("D:/assignments/asses2/Titanic.csv")
View(nfl)

## Qustion 1

## 1)      What is the probability for someone age 35 with bmi of 32, to be diabetic?

train=read.csv("D:/assignments/asses2/train.csv")
test=read.csv("D:/assignments/asses2/train.csv")

model1=glm(type~age+bmi,data=train,family=binomial("logit"))

age=c(35)

bmi=c(32)


model_ab=cbind(age,bmi)

model_ab=as.data.frame(model_ab)

predict(model1,model_ab,type="response")


#  Answer :- 0.347

##2)       According to your model what is the probability that a woman in your sample is diabetic

###   given age 55, a bmi 37, bp 68 and npreg of 2?

model2=glm(type~age+bmi+bp+npreg,data=train,family=binomial("logit"))

age=c(55)

bmi=c(37)

bp=c(68)

npreg=c(2)

model_abcd=as.data.frame(cbind(age,bmi,bp,npreg))

predict(model2,model_abcd,type="response")



# Answer :- 0.611



##3)       For a woman aged 35 and mother of 2 children, by how much does the probability of diabetes increase,

##    if her bmi was 35 instead of 25 according to the model?

model3=glm(type~age+bmi+npreg,data=train,family=binomial("logit"))

age=c(35)

bmi=c(35)

npreg=c(2)

model_abn=as.data.frame(cbind(age,bmi,npreg))

a1=predict(model3,model_abn,type="response")

a1

#model=glm(type~age+bmi+npreg,data=train,family=binomial("logit"))

age=c(35)

bmi=c(25)

npreg=c(2)

model_abn1=as.data.frame(cbind(age,bmi,npreg))

a2=predict(model3,model_abn1,type="response")

a2

a1-a2

# for bmi 35  : 0.3789903

# for bmi 25  : 0.1771995

# Difference  : 0.2017908





##4)       What is the accuracy of Models?



test$Pred1 = predict(model1, test[,c(5,7)], type="response")

#test$Pred = round(test$Pred, 3)

test_result1=as.factor(ifelse(test$Pred1>0.5,"Yes","No"))

confusion1=table(test$type,test_result1)

confusion1

##     test_result1

#         No Yes

#     No  195  28

#     Yes  67  42

Accuracy=(195+42)/332

Accuracy

# Accuracy 0.7138554



test$Pred2 = predict(model2, test[,c(1,3,5,7)], type="response")

#test$Pred = round(test$Pred, 3)

test_result2=as.factor(ifelse(test$Pred2>0.5,"Yes","No"))

confusion2=table(test$type,test_result2)

confusion2

#test_result2

#     No Yes

#No  197  26

#Yes  67  42

accu=(197+42)/332

accu

#Accuracy 0.7198795



test$Pred3 = predict(model3, test[,c(1,5,7)], type="response")

#test$Pred = round(test$Pred, 3)

test_result3=as.factor(ifelse(test$Pred3>0.5,"Yes","No"))

confusion3=table(test$type,test_result3)

confusion3



#test_result3

#     No Yes

#No  193  30

#Yes  67  42

accur=(193+42)/332

accur

# Accuracy 0.7078313
..............................................................................
########################## 2nd Question###########################
data=read.csv("D:/assignments/asses2/Titanic.csv")
View(data)

levels(data$Sex)

data=select(data,-c(PassengerId,Name,Ticket,Fare,Cabin))
View(data)


data$Sex=as.factor(data$Sex)

data$Sex = ifelse(data$Sex=="male",1,0)

data$Age=as.integer(data$Age)

data=select(data,-c(Parch,Embarked))

View(data)

#standardise the data
sd_data=scale(data)
View(sd_data)

data3=as.data.frame(sd_data)
View(data3)

# setting the seed value
set.seed(1234)

# kmeans for 4 clusters
model=kmeans(sd_data,4,nstart = 20)
model

Sex=as.list(data$Sex)
View(sex)
Sex=unlist(Sex)

Survived=as.list(data$Survived)
View(survived)
survived=unlist(Survived)

# creating a contingency table
T=table(Sex,survived,model$cluster)
T

# 2 Q)	Describe, in words, each of the clusters.
  # ANS) 
        # here sex(0) is female,sex(1) is male
        # here survived(0) is killed,survived(1) is survived
        # cluster 1:
                # In cluster 1 ,More number of male are killed than female
                 # 7 female are survived
        # cluster 2:
                # In cluster 2,More number of both male and female are killed
                # No single female is survived
        # cluster 3:
                # In cluster 3,More number of people are survived (203)
                # 190 female are survived
        # cluster 4:
                # In cluster 4,this purely consists of male .
                # 86 male are killed and 41 are survived
             

# 3 Q)	Now, 4 clusters was an arbitrary choice. What seems like a reasonable number of clusters?

# ANS) To know the reasonable number of clusters we need to plot the elbow graph,Then see the distortion
         # where it is similar and find the value and do n-1

# Creating the clusters
cluster=kmeans(data3,4)
cluster

cluster$centers 

cluster$tot.withinss

#cluster 1 to 10
cluster1=kmeans(data3,1)
cluster2=kmeans(data3,2)
cluster3=kmeans(data3,3)
cluster4=kmeans(data3,4)
cluster5=kmeans(data3,5)
cluster6=kmeans(data3,6)
cluster7=kmeans(data3,7)
cluster8=kmeans(data3,8)
cluster9=kmeans(data3,9)
cluster10=kmeans(data3,10)

y=c(cluster1$tot.withinss,cluster2$tot.withinss,cluster3$tot.withinss,
    cluster4$tot.withinss,cluster5$tot.withinss,cluster6$tot.withinss,cluster7$tot.withinss,
    cluster8$tot.withinss,cluster9$tot.withinss,cluster10$tot.withinss)
x=c(1,2,3,4,5,6,7,8,9,10)

plot(x,y,xlab="No of clusters",ylab="Distortion",type="b",col="Red")

# ANS)  We see a distortion in the graph from 6.
      # The reasonable number of clusters are (6-1)=5
.......................................................................................
#############################  3rd Question  ###########################
###3a)	Get the number of missing data points per column.
nfl=read.csv("D:/assignments/asses2/NFL.csv")
columncount=sapply(nfl, function(x) sum(is.na(x)))
columncount= as.data.frame(columncount)
View(columncount)
###	3b)Percent of data that is missing in terms of cells
library(dplyr)
per = columncount %>% summarise(percentage =(sum(columncount)/(407688*102))*100)
per
###3c)the number of rows after removing all the rows that contain a missing value

numrow= na.omit(nfl)
numrow
###3d) the number of columns after removing all columns with at least one missing val
library(dplyr)
nflcol=columncount %>% filter(columncount==0)
dim(nflcol)
# 3E) 	Replace all missing value by the mean value of that column
nfl[] <- lapply(nfl, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
nfl[]

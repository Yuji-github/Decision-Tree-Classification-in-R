# Decision Tree Classification
# install.packages('rpart')
library(caTools)
library(rpart) # for Decision Tree Classification classifier

# import data 
dataset = read.csv('Social_Network_Ads.csv')

#remove unnecessary data 
dataset = dataset[2:5]

# encoding gender 
dataset$Gender = factor(dataset$Gender, levels = c('Male', 'Female'), 
                        labels = c(0, 1))

# check data type 
typeof(dataset$Age)
typeof(dataset$EstimatedSalary)
typeof(dataset$Gender)
# all integers, but must convert to integer for feature scaling 
dataset$Gender = as.integer(dataset$Gender)
dataset$Purchased = as.factor(dataset$Purchased) #for showing both outcomes NOT purchase[0] and Purchase[1]

# splitting dataset 
seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)

# no need to do feature scaling as this is decision tree
# feature scaling '-' means except 
# train[-4] = scale(train[-4])
# test[-4] = scale(test[-4])

# training/fit Decision Tree Classification
classifier = rpart(formula = Purchased ~ ., data = train)
# no need to have -> control = rpart.control(minsplit = 1) because this is a classification 


# prediction NO Need for Decision Tree Classification with probabilities / without factor is okay
y_pred = predict(classifier, newdata=test[-4]) 

# without probability and MUST encode as factor before this 
y_pred_class = predict(classifier, newdata=test[-4], type ='class') 

# confusion matrix
# cm = table(test[, 4], y_pred) # must do factor before this
cm = table(test[, 4], y_pred_class)
print(cm)

# plotting decision tree 
plot(classifier)
text(classifier)
# select the above at the same time when it runs

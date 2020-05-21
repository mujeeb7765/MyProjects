rm(list=ls())
setwd("C:/Users/Mujeeb/Downloads/DATA SCIENTIST/PROJECT")

### L0ading Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
lapply(x, require, character.only = TRUE)

## Loading data set

#loading train data
train = read.csv("train.csv", header = T)

#Dimension of train data
dim(train)

#converting target variable  into factor
train$target = as.factor(train$target)

##target variable count with visualizations
table(train$target)

#Bar plot for count of target variable
barplot1 = ggplot(train,aes(target))+theme_bw()+geom_bar(stat='count',fill='orange')

grid.arrange(barplot1, ncol=2)

#target variable Violin with jitter plots 
vj_plot1 = ggplot(train,aes(x=target,y=1:nrow(train)))+theme_bw()+geom_violin(fill='pink')+
  facet_grid(train$target)+geom_jitter(width=0.03)+labs(y='Index')
grid.arrange(vj_plot1,ncol=2)

#Loading test data
test = read.csv("test.csv", header = T)

##Mean value distribution in test &train data columnwise

#creating function to avoid intermediate output
train_mean = apply(train[,-(1:2)],MARGIN=2,FUN=mean)
test_mean = apply(test[,-1],MARGIN=2,FUN=mean)

# Plot for Mean value distribution
ggplot()+geom_density(aes(x=train_mean),kernel='gaussian',show.legend=TRUE,color='red')+theme_classic()+geom_density(aes(x=test_mean),kernel='gaussian',show.legend=TRUE,color='green')+
  labs(x='columnwise mean ',title="Mean value Distribution in train and test dataset")

##Std. deviation value distribution in test &train data columnwise

#creating function to avoid intermediate output

train_sd = apply(train[,-(1:2)],MARGIN=1,FUN=sd)
test_sd = apply(test[,-1],MARGIN=1,FUN=sd)

# Plot for Std. deviation value distribution
ggplot()+geom_density(aes(x=train_sd),kernel='gaussian',show.legend=TRUE,color='yellow')+theme_classic()+geom_density(aes(x=test_sd),kernel='gaussian',show.legend=TRUE,color='blue')+
  labs(x='columnwise standard deviation ',title="standard deviation value Distribution in train and test dataset")

## Missing Value analysis on test & train data
mv_train = data.frame(apply(train,2,function(x){sum(is.na(x))}))
names(mv_train)[1] = "Missing Values"
mv_train = sum(mv_train)
# Missing Values in train data = 0
mv_test = data.frame(apply(test,2,function(x){sum(is.na(x))}))
names(mv_test)[1] = "Missing Values"
mv_test = sum(mv_test)

##Correlations in train data
#converting factor to numeric
train$target = as.numeric(train$target)
train_corr = cor(train[,(2:202)])
train_corr

##Using simple random sampling for splitting training data
train.index = sample(1:nrow (train), 0.70*nrow(train))
train_data = train[train.index, ]
valid_data = train[-train.index, ]


##Training & Validation Dataset
#Training dataset
X_t = as.matrix(train_data[,(1:2)])
y_t = as.matrix(train_data$target)
#validation dataset
X_v = as.matrix(valid_data[,-(1:2)])
y_v = as.matrix(valid_data$target)
#test dataset
test_df = as.matrix(test[,-1])

#Logistic regression model
set.seed(814) 
lr_model = glm(X_t,y_t, family = "binomial")
summary(lr_model)

#Cross validation prediction
set.seed(7818)
cv_lr_model = cv.glm(X_t,y_t,family = "binomial", type.measure = "class")
cv_lr_model

#Model performance on validation dataset
set.seed(6173)
cv_predict.lr_model = predict(cv_lr_model,X_v,s = "lambda.min", type = "class")
cv_predict.lr_model

#ROC_AUC score and curve
set.seed(993)
cv_predict.lr_model = as.numeric(cv_predict.lr_model)
roc(data=valid_data[,-(1:2)],response=target,predictor=cv_predict.lr_model,auc=TRUE,plot=TRUE)

#predict the model
set.seed(863)
lr_pred = predict(lr_model,test,type='response')

##Decision Tree Model
#Develop Model on training data
c50_model = C5.0(target ~., train, trials = 100, rules = TRUE)

#Summary of DecsionTree model
summary(c50_model)

# prediction for test cases
c50_Predictions = predict(c50_model, test_df, type = "class")

#ROC_AUC score and curve
roc(data=test_df,response=target,predictor=c50_Predictions,auc=TRUE,plot=TRUE)

##COnverting probabilities of lr_model
lr_pred = ifelse(lr_pred > 0.5, 1, 0)

#writing file 
my_project = data.frame(ID_code=test$ID_code, lr_pred)

write.csv(my_project,'my_project_work.CSV',row.names=F) 
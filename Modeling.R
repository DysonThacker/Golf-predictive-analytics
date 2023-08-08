
############# Load Packages ################
library(readr)
library(tidyverse)
library(faraway)
library(ROCR)
library(gains)
library(rpart)
library(maptree)
library(xgboost)
library(fastDummies)
library(corrplot)

############## Load Data #################

mydata <- read_csv("Career_Data.csv")

########### Data Manipulation ###############



# Create Flag Variable for top 10 percent of players #################

mydata <- mydata %>%
  group_by(Year) %>% 
  mutate(good_golfer = case_when(Yearly_Earnings >= quantile(mydata$Yearly_Earnings, .9) ~ 1, TRUE ~ 0)) %>% 
  ungroup()

mean(mydata$good_golfer)

mydata <- mydata %>%
  group_by(Year) %>% 
  mutate(good_putter = case_when(SG_PUTT >= quantile(mydata$SG_PUTT, .9) ~ 1, TRUE ~ 0)) %>% 
  ungroup()

mean(mydata$good_putter)


set.seed(7)
smp_size <- floor(0.7 * nrow(mydata))

train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
build <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

############## Strokes Gained ################


## Logistic Model


mylogit <- glm(good_golfer ~  SG_Off_The_Tee + SG_Aproach + SG_Around_Green + SG_PUTT + Driving_Distance + Driving_Accuracy + Greens_In_Regulation + Scrambling_Percentage ,
               family = "binomial", data = build)

mylogit <- glm(good_golfer ~  good_putter ,
               family = "binomial", data = build)

mylogit <- glm(good_golfer ~  SG_Off_The_Tee + SG_Aproach + SG_Around_Green + SG_PUTT,
               family = "binomial", data = build)

mylogit <- glm(good_golfer ~ .-Year - Player_Name - Yearly_Earnings,
               family = "binomial", data = build)

mylogit <- step(mylogit)

summary(model1)

summary(mylogit)

build$golf_predictions <- predict(mylogit)

test$golf_logprobs <- ilogit(predict(mylogit,test))


logpred <- prediction(test$golf_logprobs, test$good_golfer)
log.perf = performance(logpred, measure = "tpr", x.measure = "fpr")
plot(log.perf, main = "Golf Logistic ROC Curve")
abline(a=0, b= 1)


##  Tree Model

tree <- rpart(good_golfer ~  SG_Off_The_Tee + SG_Aproach + SG_Around_Green + SG_PUTT,
              data=build, minbucket=25 ,cp=.001)

tree <- rpart(good_golfer ~  .-Year - Player_Name - Yearly_Earnings,
              data=build, minbucket=25 ,cp=.001)

prune <- clip.rpart(tree, best = 10)

draw.tree (prune, cex=.8, 
           nodeinfo=TRUE,units="good",
           cases="obs",
           digits=2, print.levels=TRUE,
           new=TRUE)

test$golf_treeprobs <- predict(prune,test)
mydata$golf_treeprobs <- predict(prune,mydata)

treepred <- prediction(test$golf_treeprobs, test$good_golfer)
tree.perf = performance(treepred, measure = "tpr", x.measure = "fpr")
plot(tree.perf, main = "Golf Tree ROC Curve")
abline(a=0,b=1)





# XG Boostin up in here

booster <- mydata %>% 
  select("good_golfer", "SG_Off_The_Tee", "SG_Aproach", "SG_Around_Green", "SG_PUTT")

good_golfer = booster$good_golfer
label = as.integer(good_golfer)
booster$good_golfer = NULL

n = nrow(booster)
train.index = sample(n,floor(0.7*n))
train.data = as.matrix(booster[train.index,])
train.label = label[train.index]
test.data = as.matrix(booster[-train.index,])
test.label = label[-train.index]

xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

num_class = length(levels(good_golfer))
params = list(
  set.seed = 7,
  eval_metric = "auc",
  objective = "reg:logistic"
)

xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=1000,
  nthreads=1,
 early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

xgb.fit

test$golf_xgprobs <- predict(xgb.fit,test.data,reshape=T)
xgpred <- prediction(test$golf_xgprobs, test$good_golfer)
xg.perf = performance(xgpred, measure = "tpr", x.measure = "fpr")
plot(xg.perf, main = "Golf XGBoost ROC Curve")
abline(a=0, b= 1)





####### Putting ############

# Logistical Model

putting_logit <- glm(good_putter ~  Putting_Inside_5 + Putting_5_10 + putting_10_15 + putting_15_20 + putting_20_25 + putting_Outside_25,
                family = "binomial", data = build)

summary(putting_logit)

build$putt_predictions <- predict(putting_logit)

test$putt_logprobs <- ilogit(predict(putting_logit,test))


logpred <- prediction(test$putt_logprobs, test$good_putter)
log.perf = performance(logpred, measure = "tpr", x.measure = "fpr")
plot(log.perf, main = "Putting Logistic ROC Curve")
abline(a=0, b= 1)

# Tree Model

tree <- rpart(good_putter ~  Putting_Inside_5 + Putting_5_10 + putting_10_15 + putting_15_20 + putting_20_25 + putting_Outside_25,
              data=build, minbucket=25 ,cp=.001)

prune <- clip.rpart(tree, best = 50)

draw.tree (prune, cex=.55, 
           nodeinfo=TRUE,units="good",
           cases="obs",
           digits=2, print.levels=TRUE,
           new=TRUE)

test$putt_treeprobs <- predict(prune,test)
mydata$putt_treeprobs <- predict(prune,mydata)

treepred <- prediction(test$putt_treeprobs, test$good_putter)
tree.perf = performance(treepred, measure = "tpr", x.measure = "fpr")
plot(tree.perf, main = "Putting Tree ROC Curve")
abline(a=0,b=1)






# XG Boostin up in here

booster <- mydata %>% 
  select("good_putter","Putting_Inside_5", "Putting_5_10", "putting_10_15", "putting_15_20", "putting_20_25", "putting_Outside_25")

good_putter = booster$good_putter
label = as.integer(good_putter)
booster$good_putter = NULL

n = nrow(booster)
train.index = sample(n,floor(0.7*n))
train.data = as.matrix(booster[train.index,])
train.label = label[train.index]
test.data = as.matrix(booster[-train.index,])
test.label = label[-train.index]

xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

num_class = length(levels(good_putter))
params = list(
  set.seed = 7,
  eval_metric = "auc",
  objective = "binary:logistic"
)

xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads = 1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

xgb.fit

test$golf_xgprobs <- ilogit(predict(xgb.fit,test.data,reshape=T))
xgpred <- prediction(test$golf_xgprobs, test$good_putter)
xg.perf = performance(xgpred, measure = "tpr", x.measure = "fpr")
plot(xg.perf, main = "Putting XGBoost ROC Curve")
abline(a=0, b= 1)






############### Cool plots ##################

corr <- build %>% 
  select("SG_PUTT", "Putting_Inside_5", "Putting_5_10", "putting_10_15", "putting_15_20", "putting_20_25", "putting_Outside_25")
corr <- cor(corr)
corrplot(corr, method = "number")


corr <- build %>% 
  select("good_golfer", "SG_Off_The_Tee", "SG_Aproach", "SG_Around_Green", "SG_PUTT")
corr <- cor(corr)
corrplot(corr, method = "number")


# SHAP (SHapley Additive exPlnation) values is claimed to be the most advanced method 
# to interpret results from tree-based models. It was based on Shaply values from game theory.












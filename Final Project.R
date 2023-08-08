library(readr)
library(ggplot2)
library(dplyr)
library(fields)
library(rpart)
library(maptree)
library(faraway)
library(gains)
library(ROCR)
library(pROC)
library(earth)

mydata <- read.csv("~/Documents/School/2020 Spring/Predictive Analytics/Final Project/prosper.csv")
mydata <- mydata %>%
  filter(!AmountDelinquent == "NA")

#Remove unnecessary columns
mydata <- mydata[,-c(1,2,3,4,5,8,10,12,28)]

# Let's mess with dates
mydata$FirstRecordedCreditLine <- as.character(mydata$FirstRecordedCreditLine)
mydata$FirstRecordedCreditLine <- substr(mydata$FirstRecordedCreditLine,1,nchar(mydata$FirstRecordedCreditLine)-9)
mydata$FirstRecordedCreditLine <- as.Date(mydata$FirstRecordedCreditLine, "%Y-%m-%d")
# The latest recorded first recorded credit line is about halfway through 2008. Let's find dates between first recorded
# credit line and January 1st 2009.
mydata$January2009 <- as.Date("2009-01-01")
mydata$DaysOfCredit <- mydata$January2009 - mydata$FirstRecordedCreditLine
mydata <- mydata[,-c(24)]

View(mydata)

#Create test and train data
smp_size <- floor(0.6 * nrow(mydata))
set.seed(777)
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

mylogit <- glm(Bad ~  DebtToIncomeRatio
               + IsBorrowerHomeowner + AmountBorrowed + CurrentDelinquencies +
                 DelinquenciesLast7Years + PublicRecordsLast10Years + TotalCreditLines + 
                 InquiriesLast6Months + AmountDelinquent + PublicRecordsLast12Months + 
                 CurrentCreditLines + OpenCreditLines + RevolvingCreditBalance + BankcardUtilization +
                 LengthStatusMonths + Income + DaysOfCredit, family = binomial, data = train)
summary(mylogit)
#Logistic gives 6 variables with Pr(>|z|) between 0 and 0.01.



#Summaryplots
mydata$Bad <- as.factor(mydata$Bad)

ggplot(mydata, aes(x=AmountBorrowed,fill=mydata$Bad,group=mydata$Bad)) + geom_histogram(binwidth = 1000) + 
  ggtitle("Amount Borrowed") + xlab("Amount Borrowed") +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad"))

ggplot(mydata, aes(x=CurrentDelinquencies,fill=mydata$Bad,group=mydata$Bad)) + geom_histogram(binwidth = 1) + 
  ggtitle("Current Deliquencies") + xlab("Current Deliquencies") +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad")) + scale_x_continuous(limits = c(2,10))

ggplot(mydata, aes(x=PublicRecordsLast10Years,fill=mydata$Bad,group=mydata$Bad)) + geom_histogram(binwidth = 1) + 
  ggtitle("Public Records in the Last 10 Years") + xlab("Public Records in the Last 10 Years") +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad")) + scale_x_continuous(limits = c(2,20)) + scale_y_continuous(limits = c(0,300))

ggplot(mydata, aes(x=InquiriesLast6Months,fill=mydata$Bad,group=mydata$Bad)) + geom_histogram(binwidth = 1) + 
  ggtitle("Loan Inquiries in the last 6 Months") + xlab("Loan Inquiries in the last 6 Months") +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad")) + scale_x_continuous(limits = c(5,20))

ggplot(mydata, aes(x=BankcardUtilization,fill=mydata$Bad,group=mydata$Bad)) + geom_histogram(binwidth = .1) + 
  ggtitle("Bankcard Utilization") + xlab("Bankcard Utilization") +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad")) + scale_x_continuous(limits = c(0,2))

ggplot(mydata, aes(x=Income,fill=mydata$Bad,group=mydata$Bad)) + geom_histogram(binwidth = 1) + 
  ggtitle("Income") + xlab("Income") +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad"))





#Logistic Model
mylogit2 <- glm(Bad ~  AmountBorrowed + CurrentDelinquencies + PublicRecordsLast10Years +
                  InquiriesLast6Months + BankcardUtilization + Income, family = binomial, data = train)
summary(mylogit2)
test$logprobs <- ilogit(predict(mylogit2,test))
mydata$logprobs <- ilogit(predict(mylogit2,mydata))



#Tree Model

tree <- rpart(Bad ~  AmountBorrowed + CurrentDelinquencies + PublicRecordsLast10Years +
                InquiriesLast6Months + BankcardUtilization + Income, data=train, minbucket=50,cp=.001)

prune <- clip.rpart(tree, best = 10)

draw.tree (prune, cex=.55, 
           nodeinfo=TRUE,units="bad",
           cases="obs",
           digits=2, print.levels=TRUE,
           new=TRUE)

test$treeprobs <- predict(prune,test)
mydata$treeprobs <- predict(prune,mydata)


#MARS Model

earth <- earth(Bad ~  AmountBorrowed + CurrentDelinquencies + PublicRecordsLast10Years +
                 InquiriesLast6Months + BankcardUtilization + Income, data=train)

summary(earth)
test$MARSprobs <- predict(earth,test)
mydata$MARSprobs <- predict(earth,mydata)

View(train)
View(test)
View(mydata)



#Log Model Tests
#KS
n <- nrow(test)
index <- 1:n
n
order(test$logprobs,decreasing = T)
test <- test[order(test$logprobs, decreasing = T),]
cumsum_1 <- cumsum(test$Bad)
cumsum_0 <- index - cumsum_1
percent_1 <- cumsum_1/cumsum_1[n]
percent_0 <- cumsum_0/cumsum_0[n]
difference <- percent_1 - percent_0
difference
ks <- max(difference)
ks
#
percent_tot <- index / (cumsum_1[n] + cumsum_0[n])
plot(percent_tot, percent_1, main = "Logistic KS Curve", xlab = "Cumulative Depth of File (Population Percentile)", ylab = "Capture Rate", type = "l", col = "black")
lines(percent_tot, percent_0, type = "l", col = "black")

#ROC
logpred <- prediction(test$logprobs, test$Bad)
log.perf = performance(logpred, measure = "tpr", x.measure = "fpr")
plot(log.perf, main = "Logistic ROC Curve")
abline(a=0, b= 1)


#TreeModelTests
#KS
n <- nrow(test)
index <- 1:n
n
order(test$treeprobs,decreasing = T)
test <- test[order(test$treeprobs, decreasing = T),]
cumsum_1 <- cumsum(test$Bad)
cumsum_0 <- index - cumsum_1
percent_1 <- cumsum_1/cumsum_1[n]
percent_0 <- cumsum_0/cumsum_0[n]
difference <- percent_1 - percent_0
difference
ks <- max(difference)
ks
#
percent_tot <- index / (cumsum_1[n] + cumsum_0[n])
plot(percent_tot, percent_1, main = "Tree KS Curve", xlab = "Cumulative Depth of File (Population Percentile)", ylab = "Capture Rate", type = "l", col = "black")
lines(percent_tot, percent_0, type = "l", col = "black")

#ROC
treepred <- prediction(test$treeprobs, test$Bad)
tree.perf = performance(treepred, measure = "tpr", x.measure = "fpr")
plot(tree.perf, main = "Tree ROC Curve")
abline(a=0,b=1)



#MARS Tests
#KS
n <- nrow(test)
index <- 1:n
n
order(test$MARSprobs,decreasing = T)
test <- test[order(test$MARSprobs, decreasing = T),]
cumsum_1 <- cumsum(test$Bad)
cumsum_0 <- index - cumsum_1
percent_1 <- cumsum_1/cumsum_1[n]
percent_0 <- cumsum_0/cumsum_0[n]
difference <- percent_1 - percent_0
difference
ks <- max(difference)
ks
#
percent_tot <- index / (cumsum_1[n] + cumsum_0[n])
plot(percent_tot, percent_1, main = "MARS KS Curve", xlab = "Cumulative Depth of File (Population Percentile)", ylab = "Capture Rate", type = "l", col = "black")
lines(percent_tot, percent_0, type = "l", col = "black")

#ROC
marspred <- prediction(test$MARSprobs, test$Bad)
mars.perf = performance(marspred, measure = "tpr", x.measure = "fpr")
plot(mars.perf, main = "MARS ROC Curve")
abline(a=0,b=1)

#test <- test %>%
  #filter(!AmountDelinquent == "NA")



#Gainz Tables
loggains <- gains(test$Bad, test$logprobs)
plot(loggains, main = "Log Gains")
par(mfrow=c(1,1))
plot(x=loggains$depth, y=loggains$cume.lift, col='blue', main='Log Lift Plots',ylab='Lift', xlab='Depth', ylim=c(0,200))
lines(x=loggains$depth, y=loggains$cume.lift, col='blue')
points(x=loggains$depth, y=loggains$lift, col='red')
lines(x=loggains$depth, y=loggains$lift, col='red')
abline(h=100)
loggains

treegains <- gains(test$Bad, test$treeprobs)
plot(treegains, main = "Tree Gains")
plot(x=treegains$depth, y=treegains$cume.lift, col='blue', main='Tree Lift Plots',ylab='Lift', xlab='Depth', ylim=c(0,200))
lines(x=treegains$depth, y=treegains$cume.lift, col='blue')
points(x=treegains$depth, y=treegains$lift, col='red')
lines(x=treegains$depth, y=treegains$lift, col='red')
abline(h=100)
treegains

marsgains <- gains(test$Bad, test$MARSprobs)
plot(marsgains, main = "MARS Gains")
plot(x=marsgains$depth, y=marsgains$cume.lift, col='blue', main='MARS Lift Plots',ylab='Lift', xlab='Depth', ylim=c(0,200))
lines(x=marsgains$depth, y=marsgains$cume.lift, col='blue')
points(x=marsgains$depth, y=marsgains$lift, col='red')
lines(x=marsgains$depth, y=marsgains$lift, col='red')
abline(h=100)
marsgains

str(mydata)
summary(mydata)
table(mydata$logprobs)
mydata$loprobs <- as.numeric(mydata$logprobs)

# Binned Logistic Model
mydata$logbins <- cut(mydata$logprobs, breaks = c(-Inf,.1,.2,.3,.4,.5,.6,.7,.8,.9,Inf),right=FALSE)
mydata$treebins <- cut(mydata$treeprobs, breaks = c(-Inf,.1,.2,.3,.4,.5,.6,.7,.8,.9,Inf),right=FALSE)
mydata$marsbins <- cut(mydata$MARSprobs, breaks = c(-Inf,.1,.2,.3,.4,.5,.6,.7,.8,.9,Inf),right=FALSE)
ggplot(data=mydata, aes(x=logbins, y = Bad, fill = Bad)) + geom_bar(stat="identity") + theme_minimal() + ylab("Number of Humans")  +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad")) + xlab("Logistic Model's Probability of Bad")

ggplot(data=mydata, aes(x=treebins, y = Bad, fill = Bad)) + geom_bar(stat="identity") + theme_minimal() + ylab("Number of Humans")  +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad")) + xlab("Tree Model's Probability of Bad")

ggplot(data=mydata, aes(x=marsbins, y = Bad, fill = Bad)) + geom_bar(stat="identity") + theme_minimal() + ylab("Number of Humans")  +
  scale_fill_discrete(name="Legend", labels=c("Good", "Bad")) + xlab("MARS Model's Probability of Bad")


write.csv(mydata, file="mydata.csv", row.names=F)

## Data Read
library(party)
train <- read.csv("train.csv", header=TRUE, as.is=TRUE )
test  <- read.csv("test.csv", header=TRUE,  as.is=TRUE )

## Data preprocessing

## Cabin needs to be processed later, not now
drop.columns <- c("name", "ticket", "sibsp", "parch", "cabin")
clean.train <- train[, !(names(train) %in% drop.columns)]
clean.test <- test[, !(names(test) %in% drop.columns)]

clean.train$alone[clean.train$alone >= 2] <- 1
clean.train$alone[clean.train$alone == 2] <- 1
clean.test$alone[clean.test$alone >= 2] <- 1
clean.test$alone[clean.test$alone == 2] <- 1

## Convert factors to integers
clean.train$sex <- as.integer(factor(clean.train$sex))
clean.test$sex <- as.integer(factor(clean.test$sex))
clean.train$embarked <- as.integer(factor(clean.train$embarked))
clean.test$embarked <- as.integer(factor(clean.test$embarked))

## Dealing with NA in columns 'age', 'fare'

## Missing fares replaced by median value of that class
ctrain.pclass.medians <- tapply(clean.train$fare, clean.train$pclass, median, na.rm = T)
ctest.pclass.medians <- tapply(clean.test$fare, clean.test$pclass, median, na.rm = T)
clean.train$fare[is.na(clean.train$fare)] <- ctrain.pclass.medians[clean.train$pclass[which(is.na(clean.train$fare))]]
clean.test$fare[is.na(clean.test$fare)] <- ctest.pclass.medians[clean.test$pclass[which(is.na(clean.test$fare))]]

# Missing values in 'age'
# Change it to values from (uniform or normal) distribution
set.seed(9)
ctest.median.age <- median(test$age, na.rm = T)
ctrain.median.age <- median(train$age, na.rm = T)

ctest.rnorm.ages <- floor(rnorm(length(which(is.na(test$age))), mean = ctest.median.age, sd = ctest.median.age))
ctrain.rnorm.ages <- floor(rnorm(length(which(is.na(train$age))), mean = ctrain.median.age, sd = ctrain.median.age))

clean.train$age[is.na(clean.train$age)] <- ctrain.rnorm.ages
clean.test$age[is.na(clean.test$age)] <- ctest.rnorm.ages

## Subset for cross validation
titanic.full  <- clean.train
titanic.predict <- clean.test

n <- nrow(titanic.full)
nex <- 200
set.seed(1234)
subs <- split(titanic.full, sample(rep(1:2, times=c(n-nex, nex))))
titanic.train <- subs[[1]]
titanic.test  <- subs[[2]]


## Modeling Begin

## formula1 for 'gender' model
##formula1 <- as.formula(as.factor(survived) ~ pclass + sex)
## formula2 for rest features model
##formula2 <- as.formula(as.factor(survived) ~ pclass + alone + fare + age)
predictions <- NULL
NT <- 1000
## formula3 for 'gender' model using SVM
formula3 <- as.factor(survived) ~ pclass + sex
## formula1 and formula2 both for rest features without gender model
formula1 <- as.formula(as.factor(survived) ~ pclass  + alone + fare + age)
formula2 <- as.formula(          survived  ~ pclass  + alone + fare + age)

## Train SVM(only for gender model) and Predict
library(e1071)
formula3 <- as.factor(survived) ~ pclass + sex
tune <- tune.svm(formula3, data=titanic.train, gamma=10^(-4:-1), cost=10^(1:4))
# summary(tune)
tune$best.parameters

model.svm <- svm(formula3, 
               data=titanic.train, 
               type="C-classification", 
               kernel="radial", 
               probability=T, 
               gamma=tune$best.parameters$gamma, 
               cost=tune$best.parameters$cost)
predictions$svm <- as.numeric(predict(model.svm, newdata=titanic.test))-1
ans3 <- as.numeric(predict(model.svm, newdata=clean.test))-1
tb <- table(titanic.test$survived, predictions$svm)
# tb
results$svm <- (tb[1,1]+tb[2,2])/sum(tb)
results$svm


## Train cForest and Predict
model.cforest <- cforest(formula2, data=titanic.train, 
                           control=cforest_unbiased(ntree=NT, trace=F))
predictions$cforest<-predict(model.cforest, titanic.test,OOB=T)
predictions$cforest[,1][predictions$cforest[,1]<=0.5] <- 0
predictions$cforest[,1][predictions$cforest[,1]>=0.5] <- 1
ans1 <- predictions$cforest[,1]

##predictions$cforest <- as.numeric(predict(model.cforest, newdata=clean.test))-1

ps$cforest <- as.numeric(predict(model.cforest, newdata=titanic.test, OOB = T))-1
tb <- table(titanic.test$survived, ps$cforest)
# tb
results$cforest <- (tb[1,1]+tb[2,2])/sum(tb)

##write.csv(ans, file="submission-cf.csv", row.names=F)

## Train ctree and Predict
##model.ctree <- ctree(formula1.cf, data=sub.train)
# plot(titanic.ctree)
##predictions$ctree <- as.numeric(predict(model.ctree, newdata=sub.test))-1

##ans <- as.numeric(predict(model.ctree, newdata=clean.test))-1
##write.csv(ans, file="submission-ctree.csv", row.names=F)

## Train gbm and Predict
library(gbm)
model.gbm <- gbm(formula2, data=clean.train, n.trees=NT, interaction.depth=2)
predictions$gbm<-predict(model.gbm,clean.test, type = "response", n.trees = NT)
predictions$gbm <- round(predictions$gbm, 0)
tb <- table(titanic.test$survived, predictions$gbm)
# tb
results$gbm <- (tb[1,1]+tb[2,2])/sum(tb)

ans2 <- predictions$gbm

##predictions$gbm <- predict(model.gbm, newdata=clean.test, type="response", n.trees=NT)
##ans2 <- as.numeric(predict(model.gbm, newdata=clean.test, n.trees = NT))-1
##write.csv(ans, file="submission-gbm.csv", row.names=F)

##model.svmfm <- attr(terms(model.svm),"terms.labels")
##model.cforestfm <- attr(terms(model.cforest), "terms.labels")
##model.fm <- as.formula(paste("y ~ ", paste(c(model.svmfm, model.cforestfm), collapse = "+")))
##model <- lm(model.fm, clean.train)

#
# show results
#
as.data.frame(results)

psr <- as.data.frame(ps)
psr$MEDIAN <- round(apply(psr, 1, median), 0)
psr$RES <- as.numeric(titanic.test$survived)

psr$ALL1 <- apply(psr[,1:6],1,mean)==1
psr$ALL0 <- apply(psr[,1:6],1,mean)==0


## Ensembling models
ans <- (ans1 + ans2 + ans3)
ans[ans <= 1] <- 0
ans[ans == 1] <- 0
ans[ans >= 2] <- 1
ans[ans == 2] <- 1
write.csv(ans, file="submission06.csv", row.names=F)


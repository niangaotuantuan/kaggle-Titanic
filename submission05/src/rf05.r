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

## Missing values in age with age.model
n <- nrow(clean.train)
nex <- 200
set.seed(37)
subs <- split(clean.train, sample(rep(1:2, times=c(n-nex, nex))))
sub.train <- subs[[1]]
sub.test  <- subs[[2]]

sapply(sub.train, function(x) sum(is.na(x)))

subs <- split(sub.train, is.na(sub.train$age))
sub.train.age <- subs[[1]]
sub.train.not.age <- subs[[2]]

subs <- split(sub.test, is.na(sub.test$age))
sub.test.age <- subs[[1]]
sub.test.not.age <- subs[[2]]

## modeling for age
##
##
train.age.model <- function(df){
  stopifnot(is.data.frame(df))
  
  glm(age~pclass:sex+alone+embarked, data=df)
}

##
##
predict.age.model <- function(df, model){
  stopifnot(is.data.frame(df))
  stopifnot(class(model)[1]=="glm")
  
  ret <- predict(model, newdata=df)
  ret[ret<0] <- 0.5
  
  return (ret)
}

## model for age implementation 
model.age <- train.age.model(sub.train.age)
summary(model.age)

page <- predict.age.model(sub.test.age, model.age)
cor(sub.test.age$age, page)

## set ages
sub.train$age[is.na(sub.train$age)] <- predict.age.model(subset(sub.train, is.na(sub.train$age)), model.age)

sub.test$age[is.na(sub.test$age)] <- predict.age.model(subset(sub.test, is.na(sub.test$age)), model.age)

clean.test$age[is.na(clean.test$age)] <- predict.age.model(subset(clean.test, is.na(clean.test$age)), model.age)

summary(sub.train$age)


## Modeling Begin

## formula1 for 'gender' model
formula1 <- as.formula(as.factor(survived) ~ pclass + sex)
## formula2 for rest features model
formula2 <- as.formula(as.factor(survived) ~ pclass + alone + fare + age)
predictions <- NULL
NT <- 1000
## formula.cf for cforest
formula1.cf <- as.formula(as.factor(survived) ~ pclass + sex + alone + fare + age)
formula2.cf <- as.formula(          survived  ~ pclass + sex + alone + fare + age)

## Train cForest and Predict
model.cforest <- cforest(formula2.cf, data=sub.train, 
                           control=cforest_unbiased(ntree=NT, trace=F))
predictions$cforest <- as.numeric(predict(model.cforest, newdata=sub.test))-1
sapply(clean.test, function(x) sum(is.na(x)))

ans <- as.numeric(predict(model.cforest, newdata=clean.test))-1
write.csv(ans, file="submission-cf.csv", row.names=F)

## Train ctree and Predict
model.ctree <- ctree(formula1.cf, data=sub.train)
# plot(titanic.ctree)
predictions$ctree <- as.numeric(predict(model.ctree, newdata=sub.test))-1

ans <- as.numeric(predict(model.ctree, newdata=clean.test))-1
write.csv(ans, file="submission-ctree.csv", row.names=F)

## Train gbm and Predict
library(gbm)
model.gbm <- gbm(formula2.cf, data=sub.train, n.trees=NT, interaction.depth=2)
predictions$gbm <- predict(model.gbm, newdata=sub.test, type="response", n.trees=NT)
ans <- as.numeric(predict(model.gbm, newdata=clean.test))-1
write.csv(ans, file="submission-gbm.csv", row.names=F)

## Train SVM and Predict
library(e1071)
formula3 <- as.factor(survived) ~ pclass + sex + fare + age + alone
tune <- tune.svm(formula3, data=sub.train, gamma=10^(-4:-1), cost=10^(1:4))
# summary(tune)
tune$best.parameters

model.svm <- svm(formula3, 
               data=sub.train, 
               type="C-classification", 
               kernel="radial", 
               probability=T, 
               gamma=tune$best.parameters$gamma, 
               cost=tune$best.parameters$cost)
predictions$svm <- as.numeric(predict(model.svm, newdata=sub.test))-1
ans <- as.numeric(predict(model.svm, newdata=clean.test))-1
write.csv(ans, file="submission-svm.csv", row.names=F)

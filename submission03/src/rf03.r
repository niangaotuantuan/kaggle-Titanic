# Data Read
library(party)
train <- read.csv("train.csv", header=TRUE, as.is=TRUE )
test  <- read.csv("test.csv", header=TRUE,  as.is=TRUE )

# Data preprocessing

# Cabin needs to be processed later, not now
drop.columns <- c("name", "ticket", "cabin")
clean.train <- train[, !(names(train) %in% drop.columns)]
clean.test <- test[, !(names(test) %in% drop.columns)]

# Dealing with NA in columns 'age', 'fare', 'embarked' 

# Missing fares replaced by median value of that class
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

# Missing values in 'embarked'
ctest.embarked.most.frequent <- names(rev(sort(table(test$embarked)))[1])
ctrain.embarked.most.frequent <- names(rev(sort(table(train$embarked)))[1])
clean.train$embarked[clean.train$embarked == ""] <- ctrain.embarked.most.frequent
clean.test$embarked[clean.test$embarked == ""] <- ctest.embarked.most.frequent

# Convert factors to integers
clean.train$sex <- as.integer(factor(clean.train$sex))
clean.test$sex <- as.integer(factor(clean.test$sex))
clean.train$embarked <- as.integer(factor(clean.train$embarked))
clean.test$embarked <- as.integer(factor(clean.test$embarked))

# Train randomForest
cf <- cforest(survived ~ ., data = clean.train, controls = cforest_unbiased())

# Predict
cf.predictions<-predict(cf,clean.test,OOB=T)
cf.predictions[,1][cf.predictions[,1]<=0.5] <- 0
cf.predictions[,1][cf.predictions[,1]>=0.5] <- 1
rf.predictions <- predict(rf.model, clean.test)
write(cf.predictions[,1], file = "submission03.csv", ncolumns = 1)

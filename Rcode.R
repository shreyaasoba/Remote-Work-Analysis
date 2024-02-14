#Required library
library(caret)
library(rsample)
library(RWeka)
library(rpart)
library(rpart.plot)
library(MASS)
library(kernlab)
library(pROC)
# Load the data into a data frame
data_2020 <-read.csv('C:/Users/Admin/Downloads/DataMiningProject/2020_rws.csv', fileEncoding = "Latin1", check.names = F)
# Check for missing values
sum(is.na(data_2020))

# Remove rows with missing values
data_2020 <- na.omit(data_2020)

# Check for duplicates
sum(duplicated(data_2020))

# Remove duplicates
data_2020 <- unique(data_2020)

write.csv(data_2020,'C:/Users/Admin/Downloads/DataMiningProject/preproc.csv')

#Run Python code to do label encoding:
#Loading the data after encoding
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class <- factor(data$class)
write.csv(data,'C:/Users/Admin/Downloads/DataMiningProject/FinalPreProcData.csv')

#splitting Data into train and test:

set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)

#10 fold cross Validation with 5 different models
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
## rpart

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)

#############################################################################
## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

########################################################################
##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

########################################################################
## random forest
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)

ctrl <- trainControl(method = "CV",
classProbs = TRUE,
savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train[, -13], 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp

pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm

###############################################################################
##nnet
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(31)
nnetFit <- train(x = data[, -10], 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

################################################################################
###Feature selection
#Recursive Feature Elimination (RFE)

data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class <- factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
model <- rpart(class ~ ., data = data)

# Apply RFE
rfe_control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_result <- rfe(X, Y, rfeControl = rfe_control, method = "rpart")
rfe_result
data<-subset(data,select=c(Imagine.that.COVID.19.is.cured.or.eradicated..Going.forward..how.much.of.your.time.would.you.prefer.to.work.remotely., How.much.of.your.time.would.you.have.preferred.to.work.remotely.last.year., How.much.of.your.time.would.you.have.preferred.to.work.remotely.in.the.last.3.months., Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others, Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.could.easily.collaborate.with.colleagues.when.working.remotely,class))
write.csv(data,'C:/Users/Admin/Downloads/DataMiningProject/feat1.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)

## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
cm <- confusionMatrix(test_pred, test$class)
cm

f1_score <- cm$byClass["F1"]
precision <- cm$byClass["Pos Pred Value"]
f1_score
precision

## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

##nnet
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
model <- rpart(class ~ ., data = data)

# Apply RFE
rfe_control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_result <- rfe(X, Y, rfeControl = rfe_control, method = "rpart")
rfe_result
data<-subset(data,select=c(Imagine.that.COVID.19.is.cured.or.eradicated..Going.forward..how.much.of.your.time.would.you.prefer.to.work.remotely., How.much.of.your.time.would.you.have.preferred.to.work.remotely.last.year., How.much.of.your.time.would.you.have.preferred.to.work.remotely.in.the.last.3.months., Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others, Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.could.easily.collaborate.with.colleagues.when.working.remotely,class))
write.csv(data,'C:/Users/Admin/Downloads/DataMiningProject/feat1.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp

pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm


########################################################################
#Wrapper-based Feature Selection:
library(caret)
library(glmnet)
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class <- factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class

#model
model <- train(x = X, y = Y, method = "glmnet")

# Apply wrapper-based feature selection
glmnet_wrapper_result <- varImp(model, scale = FALSE)
glmnet_wrapper_result
data<-subset(data,select=c(Do.you.manage.people.as.part.of.your.current.occupation.,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....My.organisation.was.well.prepared.for.me.to.work.remotely,What.is.your.gender.,Compare.remote.working.to.working.at.your.employer.s.workplace..Select.the.worst.aspect.of.remote.working.for.you...Managing.my.family.responsibilities...My.working.relationships...Preparing.for.work.and.commuting...My.daily.expenses...My.personal.relationships...My.job.satisfaction,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.could.easily.collaborate.with.colleagues.when.working.remotely,From.the.following..please.select.the.least.significant.barrier.to.doing.your.work.remotely...IT.equipment..computer..printer..etc.....Difficulty.collaborating.remotely...Caring.responsibilities...Lack.of.remote.working.skills...My.living.situation..e.g..location..home.size..who.I.live.with....Management.discourages.remote.working,How.much.of.your.time.would.you.have.preferred.to.work.remotely.in.the.last.3.months.,From.the.following..please.select.the.most.significant.barrier.to.doing.your.work.remotely...Cyber.security...Lack.of.motivation...Lack.of.motivation...Lack.of.remote.working.skills...My.living.situation..e.g..location..home.size..who.I.live.with....Management.discourages.remote.working,From.the.following..please.select.the.most.significant.barrier.to.doing.your.work.remotely...IT.equipment..computer..printer..etc.....Difficulty.collaborating.remotely...Caring.responsibilities...Cyber.security...Lack.of.motivation...Lack.of.motivation,From.the.following..please.select.the.most.significant.barrier.to.doing.your.work.remotely...Connectivity..internet.connection....Feeling.left.out.and.or.isolated...Poor.management...Cyber.security...Lack.of.motivation...Lack.of.motivation,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....My.organisation.encouraged.people.to.work.remotely,Compare.remote.working.to.working.at.your.employer.s.workplace..Select.the.best.aspect.of.remote.working.for.you...Managing.my.personal.commitments...My.opportunities.to.socialise...My.mental.wellbeing...My.daily.expenses...My.personal.relationships...My.job.satisfaction,On.a.day.when.you.attend.your.employer.s.workplace..how.many.hours.would.you.spend.doing.the.following.activities....Preparing.for.work.and.commuting,From.the.following..please.select.the.least.significant.barrier.to.doing.your.work.remotely...Connectivity..internet.connection....Feeling.left.out.and.or.isolated...Poor.management...My.organisation.s.software.and.systems...My.workspace..e.g..suitable.chair..lighting..noise.levels..facilities....I.have.tasks.that.can.t.be.done.remotely,From.the.following..please.select.the.least.significant.barrier.to.doing.your.work.remotely...IT.equipment..computer..printer..etc.....Difficulty.collaborating.remotely...Caring.responsibilities...Cyber.security...Lack.of.motivation...Lack.of.motivation,Thinking.about.remote.working.last.year..how.strongly.do.you.agree.or.disagree.with.the.following.statements....It.was.common.for.people.in.my.organisation.to.work.remotely,Compare.remote.working.to.working.at.your.employer.s.workplace..Select.the.worst.aspect.of.remote.working.for.you...The.number.of.hours..I.work...My.work.life.balance...My.on.the.job.learning.opportunities...My.daily.expenses...My.personal.relationships...My.job.satisfaction,Thinking.about.remote.working.last.year..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others,On.a.day.when.you.do.remote.work..how.many.hours.would.you.spend.doing.the.following.activities....Caring.and.domestic.responsibilities,class))
write.csv(data,'C:/Users/Admin/Downloads/DataMiningProject/feat2.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)

## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)


## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

##nnet
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class

#model
model <- train(x = X, y = Y, method = "glmnet")

# Apply wrapper-based feature selection
glmnet_wrapper_result <- varImp(model, scale = FALSE)
glmnet_wrapper_result
data<-subset(data,select=c(Do.you.manage.people.as.part.of.your.current.occupation.,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....My.organisation.was.well.prepared.for.me.to.work.remotely,What.is.your.gender.,Compare.remote.working.to.working.at.your.employer.s.workplace..Select.the.worst.aspect.of.remote.working.for.you...Managing.my.family.responsibilities...My.working.relationships...Preparing.for.work.and.commuting...My.daily.expenses...My.personal.relationships...My.job.satisfaction,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.could.easily.collaborate.with.colleagues.when.working.remotely,From.the.following..please.select.the.least.significant.barrier.to.doing.your.work.remotely...IT.equipment..computer..printer..etc.....Difficulty.collaborating.remotely...Caring.responsibilities...Lack.of.remote.working.skills...My.living.situation..e.g..location..home.size..who.I.live.with....Management.discourages.remote.working,How.much.of.your.time.would.you.have.preferred.to.work.remotely.in.the.last.3.months.,From.the.following..please.select.the.most.significant.barrier.to.doing.your.work.remotely...Cyber.security...Lack.of.motivation...Lack.of.motivation...Lack.of.remote.working.skills...My.living.situation..e.g..location..home.size..who.I.live.with....Management.discourages.remote.working,From.the.following..please.select.the.most.significant.barrier.to.doing.your.work.remotely...IT.equipment..computer..printer..etc.....Difficulty.collaborating.remotely...Caring.responsibilities...Cyber.security...Lack.of.motivation...Lack.of.motivation,From.the.following..please.select.the.most.significant.barrier.to.doing.your.work.remotely...Connectivity..internet.connection....Feeling.left.out.and.or.isolated...Poor.management...Cyber.security...Lack.of.motivation...Lack.of.motivation,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....My.organisation.encouraged.people.to.work.remotely,Compare.remote.working.to.working.at.your.employer.s.workplace..Select.the.best.aspect.of.remote.working.for.you...Managing.my.personal.commitments...My.opportunities.to.socialise...My.mental.wellbeing...My.daily.expenses...My.personal.relationships...My.job.satisfaction,On.a.day.when.you.attend.your.employer.s.workplace..how.many.hours.would.you.spend.doing.the.following.activities....Preparing.for.work.and.commuting,From.the.following..please.select.the.least.significant.barrier.to.doing.your.work.remotely...Connectivity..internet.connection....Feeling.left.out.and.or.isolated...Poor.management...My.organisation.s.software.and.systems...My.workspace..e.g..suitable.chair..lighting..noise.levels..facilities....I.have.tasks.that.can.t.be.done.remotely,From.the.following..please.select.the.least.significant.barrier.to.doing.your.work.remotely...IT.equipment..computer..printer..etc.....Difficulty.collaborating.remotely...Caring.responsibilities...Cyber.security...Lack.of.motivation...Lack.of.motivation,Thinking.about.remote.working.last.year..how.strongly.do.you.agree.or.disagree.with.the.following.statements....It.was.common.for.people.in.my.organisation.to.work.remotely,Compare.remote.working.to.working.at.your.employer.s.workplace..Select.the.worst.aspect.of.remote.working.for.you...The.number.of.hours..I.work...My.work.life.balance...My.on.the.job.learning.opportunities...My.daily.expenses...My.personal.relationships...My.job.satisfaction,Thinking.about.remote.working.last.year..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others,On.a.day.when.you.do.remote.work..how.many.hours.would.you.spend.doing.the.following.activities....Caring.and.domestic.responsibilities,class))
write.csv(data,'C:/Users/Admin/Downloads/DataMiningProject/feat2.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp

pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm

##############################################################################
#Filter Method:
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
cors.data<-cor(data)
highly_correlated <- findCorrelation(cors.data, cutoff = 0.7)
data <- data[, -highly_correlated]
data$class <- factor(data$class)
write.csv(data,'C:/Users/Admin/Downloads/DataMiningProject/feat3.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)


## 12) KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

#nnet
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
cors.data<-cor(data)
highly_correlated <- findCorrelation(cors.data, cutoff = 0.7)
data <- data[, -highly_correlated]
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp

pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm
##############################################################################
#Embedded Method:
library(glmnet)
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
X<- model.matrix(class ~ ., data = data)
Y<-data$class
cv.out <- cv.glmnet(X, Y, alpha = 1)
lambda <- cv.out$lambda.min
lasso_coef <- predict(cv.out, type = "coef", s = lambda)[,1]
data <- data[, -which(lasso_coef == 0)]
data$class <- factor(data$class)
write.csv(data,'C:/Users/Admin/Downloads/DataMiningProject/feat.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)


## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm
#nnet
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
X<- model.matrix(class ~ ., data = data)
Y<-data$class
cv.out <- cv.glmnet(X, Y, alpha = 1)
lambda <- cv.out$lambda.min
lasso_coef <- predict(cv.out, type = "coef", s = lambda)[,1]
data <- data[, -which(lasso_coef == 0)]
data$class[data$class==0]<-'N'
data$class[data$class==2]<-'Y'
data$class[data$class==1]<-'M'
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp

pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm
############################################################################
#Boruta Feature Selection
library(Boruta)
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class <- factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
# Perform Boruta feature selection
set.seed(123)
boruta_res <- Boruta(X, Y)

# Get selected features from Boruta
selected_features <- getSelectedAttributes(boruta_res, withTentative = FALSE)
data<-data[, selected_features]
data$class<-Y
write.csv(data,'C:/Users/Admin/Downloads/DataMiningProject/feat5.csv')
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
## rpart
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)

modelLookup("rpart")
model <- train(class ~ ., data = train, method = "rpart", trControl = train_control,
               tuneLength = 10)
model
plot(model)
test_pred <- predict(model, newdata = test)
confusionMatrix(test_pred, test$class)


## KNN
knnModel <- train(class ~., data = train, method = "knn",
                  trControl=train_control,
                  preProcess = c("center", "scale"),
                  tuneLength = 100)

knnModel
plot(knnModel)

test_pred <- predict(knnModel, newdata = test)
confusionMatrix(test_pred, test$class)

##SVM

set.seed(31)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                              summaryFunction = defaultSummary)
model <- train(class ~ ., data = train, method = "svmRadial",
               preProc = c("center", "scale"),
               trControl = train_control)
model
plot(model)

pred <- predict(model, test)
cm <- confusionMatrix(pred, test$class)
cm

#nnet
data<-read.csv('C:/Users/Admin/Downloads/DataMiningProject/preproc1.csv')
data$class<-data$This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.
data <- subset(data, select = -c(X,Response.ID,Unnamed..0,This.question.is.about.your.productivity..Productivity.means.what.you.produce.for.each.hour.that.you.work..It.includes.the.amount.of.work.you.achieve.each.hour..and.the.quality.of.your.work.each.hour....Please.compare.your.productivity.when.you.work.remotely.to.when.you.work.at.your.employer.s.workplace....Roughly.how.productive.are.you..each.hour..when.you.work.remotely.))
data$class <- factor(data$class)
X<-subset(data, select = -c(class))
Y<-data$class
# Perform Boruta feature selection
set.seed(123)
boruta_res <- Boruta(X, Y)
boruta_res
# Get selected features from Boruta
selected_features <- getSelectedAttributes(boruta_res, withTentative = FALSE)
selected_features
data<-subset(data,select = c(Thinking.about.remote.working.last.year..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others,How.much.of.your.time.would.you.have.preferred.to.work.remotely.last.year.,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.could.easily.collaborate.with.colleagues.when.working.remotely,Thinking.about.remote.working.in.the.last.3.months..how.strongly.do.you.agree.or.disagree.with.the.following.statements....I.would.recommend.remote.working.to.others,How.much.of.your.time.would.you.have.preferred.to.work.remotely.in.the.last.3.months.,Imagine.that.COVID.19.is.cured.or.eradicated..Going.forward..how.much.of.your.time.would.you.prefer.to.work.remotely.,On.a.day.when.you.attend.your.employer.s.workplace..how.many.hours.would.you.spend.doing.the.following.activities....Preparing.for.work.and.commuting,On.a.day.when.you.do.remote.work..how.many.hours.would.you.spend.doing.the.following.activities....Working,From.the.following..please.select.the.most.significant.barrier.to.doing.your.work.remotely...My.organisation.s.software.and.systems...My.workspace..e.g..suitable.chair..lighting..noise.levels..facilities....I.have.tasks.that.can.t.be.done.remotely...Lack.of.remote.working.skills...My.living.situation..e.g..location..home.size..who.I.live.with....Management.discourages.remote.working,class))
temp<-data
temp$newclass[temp$class==0]<-'N'
temp$newclass[temp$class==1]<-'M'
temp$newclass[temp$class==2]<-'Y'
temp$class<-temp$newclass
temp<-subset(temp,select=-c(newclass))
data<-temp
data$class <- as.factor(data$class)
set.seed(31)
split <- initial_split(data, prop = 0.66, strata = class)
train <- training(split)
test <- testing(split)
set.seed(31)
ctrl <- trainControl(method = "CV", number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
nnetFit <- train(x = data, 
                 y = data$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 trace = FALSE,
                 maxit = 100,
                 MaxNWts = 1000,
                 trControl = ctrl)

nnetFit
# nnetFit$bestTune
plot(nnetFit)
# nnetFit$pred
nnetFit$pred <- merge(nnetFit$pred,  nnetFit$bestTune)
# nnetFit$pred
nnetCM <- confusionMatrix(nnetFit)
nnetCM

#random forest
ctrl <- trainControl(method = "CV",
                     classProbs = TRUE,
                     savePredictions = TRUE)

set.seed(31)
rfFit <- train(x = train, 
               y = train$class,
               method = "rf",
               ntree = 500,
               importance = TRUE,
               metric = "ROC",
               trControl = ctrl)
rfFit
plot(rfFit)

## variable importance
imp <- varImp(rfFit)
imp

pred <- predict(rfFit, test)
cm <- confusionMatrix(pred, test$class)
cm


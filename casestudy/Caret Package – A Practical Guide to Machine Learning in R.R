library('RYLin')
loadPackages()
loadPackages(c('OpenML', 'mlr', 'h2o', 'caret', 'skimr'))


# global settings ---------------------------------------------------------
theme_set(theme_RYLin())
set.seed(787)


# load data ---------------------------------------------------------------
# Data: Orange Juice Data, https://rdrr.io/cran/ISLR/man/OJ.html
dt <- fread('data/orange_juice_withmissing.csv',
            # na.strings = c("NA", "#DIV/0!", ""),
            verbose = TRUE)
# train[, lbl := 'train']
# test <- fread(
#   'data/pml-testing.csv',
#   na.strings = c("NA", "#DIV/0!", ""),
#   verbose = TRUE
# )
# test <- test[, 1:159, with = FALSE][, ':='(classe = NA_character_,
#                                            lbl = 'test')][]
#
# combine <- bind_rows(train, test)


# summary -----------------------------------------------------------------
dim(dt)
str(dt)

# response variable
describe(dt$Purchase)

# predictors

# descriptive statistics
skimr::skim_to_list(train)



# correlation -------------------------------------------------------------
library(corrplot)
par(mar = c(4, 2, 2, 2))
M <- cor(dt[, -c('Purchase')])
corrplot(
  M,
  tl.col = 'grey50',
  type = "lower",
  method = "square",
  tl.cex = .6
)



# preprocess --------------------------------------------------------------
# colnames
cols_all <- names(dt)
cols_y <- 'Purchase'
dt[, (cols_y) := factor(get(cols_y))]
# cols_lbl <- ''
cols_x <- setdiff(cols_all, c(cols_y))
# cols_cat <-
#   c('Purchase',
#     'StoreID',
#     'Store7',
#     'STORE',
#     'SpecialCH',
#     'SpecialMM')
# cols_num <- setdiff(cols_x, cols_cat)

X = dt[, cols_x, with = FALSE]
Y = dt[[cols_y]]

# dt[, (cols_cat) := lapply(.SD, as.factor), .SDcols = cols_cat]
# dt[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num]
# skim_to_wide(dt)



# missing values
#Identify the variables with majority NA, and remove
cols_na <- dt %>% showNAInfo() %>% '['(prop > 0.5, col)
if (length(cols_na) > 0L) {
  dt[, (cols_na) := NULL]
}


# impute missing values using preProcess()
preprocess_model_na <- preProcess(dt, method = 'knnImpute')
dt <- predict(preprocess_model_na, newdata = dt)
skim_to_wide(dt)

# # categorical variables
# for (col in cols_cat) {
#   fillNAs(dt, col)
# }
# skim_to_wide(dt)


# One-Hot Encoding
preprocess_model_dummies <- dummyVars(Purchase ~ ., data = dt)
dt <- predict(preprocess_model_dummies, newdata = dt)
dt <- data.table(dt)


# Normalization
preprocess_model_range <- preProcess(dt, method = 'range')
dt <- predict(preprocess_model_range, newdata = dt)
dt <- data.table(dt)



# Append the Y variable
dt$Purchase <- Y








# split train & test ------------------------------------------------------
# # Take training and break into validation and training.
# cols_y <- 'Purchase'
# cols_x <- setdiff(names(dt), cols_y)
# trainX <- dt[, cols_x, with = FALSE]
# trainY <- dt[, cols_y, with = FALSE]
set.seed(787)
inTrain <-
  createDataPartition(y = dt[[cols_y]], p = 0.8, list = FALSE)
train <- dt[inTrain,]
valid <- dt[-inTrain,]


# build model -------------------------------------------------------------
set.seed(787)

fitMARS = caret::train(Purchase ~ ., data = train, method = 'earth')
fitMARS
# fitted <- predict(fitMARS)
# plot(fitMARS, main="Model Accuracies with MARS")
# plot(varImp(fitMARS), main="Variable Importance with MARS")
# confusionMatrix(reference = fitted, data = dt$Purchase, mode='everything', positive='MM')


# hyperparameter tuning ---------------------------------------------------
set.seed(787)

# Define the training control
fitControl <- trainControl(
  method = 'cv',
  # k-fold cross validation
  number = 5,
  # number of folds
  savePredictions = 'final',
  # saves predictions for optimal tuning parameter
  classProbs = T,
  # should class probabilities be returned
  summaryFunction = twoClassSummary  # results summary function
)

fitMARS = caret::train(
  Purchase ~ .,
  data = train,
  method = 'earth',
  tuneLength = 5,
  metric = 'ROC',
  trControl = fitControl
)
fitMARS

predicted <- predict(fitMARS, valid)
confusionMatrix(
  reference = valid[[cols_y]],
  data = predicted,
  mode = 'everything',
  positive = 'MM'
)


# Hyper Parameter Tuning using tuneGrid
marsGrid <-  expand.grid(nprune = c(2, 4, 6, 8, 10),
                         degree = c(1, 2, 3))

fitMARS2 = caret::train(
  Purchase ~ .,
  data = train,
  method = 'earth',
  metric = 'ROC',
  tuneGrid = marsGrid,
  trControl = fitControl
)
fitMARS2
predicted <- predict(fitMARS2, valid)
confusionMatrix(
  reference = valid[[cols_y]],
  data = predicted,
  mode = 'everything',
  positive = 'MM'
)




# model comparision -------------------------------------------------------
# Adaboost
fitAdaboost = train(
  Purchase ~ .,
  data = train,
  method = 'adaboost',
  tuneLength = 2,
  trControl = fitControl
)
fitAdaboost

# Random Forest
fitRF = train(
  Purchase ~ .,
  data = train,
  method = 'rf',
  tuneLength = 5,
  trControl = fitControl
)
fitRF


# xgBoost Dart
fitXGBDART = train(
  Purchase ~ .,
  data = train,
  method = 'xgbDART',
  tuneLength = 5,
  trControl = fitControl
)
fitXGBDART

# SVM
fitSVM = train(
  Purchase ~ .,
  data = train,
  method = 'svmRadial',
  tuneLength = 5,
  trControl = fitControl
)
fitSVM


# compare models
models_compare <-
  resamples(list(
    ADABOOST = fitAdaboost,
    RF = fitRF,
    XGBDART = fitXGBDART,
    MARS = fitMARS,
    SVM = fitSVM
  ))
summary(models_compare)




# Ensembling --------------------------------------------------------------
library(caretEnsemble)
# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  savePredictions = TRUE,
  classProbs = TRUE
)

algorithmList <-
  c('rf', 'adaboost', 'earth', 'xgbDART', 'svmRadial')

set.seed(100)
models <-
  caretList(
    Purchase ~ .,
    data = train,
    trControl = trainControl,
    methodList = algorithmList
  )
results <- resamples(models)
summary(results)



# combine the predictions of multiple models to form a final prediction
set.seed(101)
stackControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  savePredictions = TRUE,
  classProbs = TRUE
)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <-
  caretStack(models,
             method = "glm",
             metric = "Accuracy",
             trControl = stackControl)
print(stack.glm)

# Predict on testData
stack_predicteds <- predict(stack.glm, newdata = testData4)
head(stack_predicteds)


# # Prepare the test dataset and predict -----------------------------------
# # Step 1: Impute missing values
# testData2 <- predict(preProcess_missingdata_model, testData)
# # Step 2: Create one-hot encodings (dummy variables)
# testData3 <- predict(dummies_model, testData2)
# # Step 3: Transform the features to range between 0 and 1
# testData4 <- predict(preProcess_range_model, testData3)
# # View
# head(testData4[, 1:10])
# # Predict on testData
# predicted <- predict(model_mars, testData4)
# head(predicted)



# Get rid of variables with near zero variance ----------------------------
nsv <- nearZeroVar(dt[], saveMetrics = TRUE,)
cols_nzv <- rownames(nsv[nsv$nzv == TRUE,])
if (length(cols_nzv) > 0L) {
  dt[, (cols_na) := NULL]
}



# feture engering ---------------------------------------------------------
# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(Purchase ~ ., data = dt)
dt <- predict(dummies_model, newdata = dt)
dt <- data.table(dt, Purchase = trainY$Purchase)
skim_to_list(dt)


# featurePlot(
#   x = dt[, 1:32],
#   y = dt$Purchase,
#   plot = "box",
#   strip = strip.custom(par.strip.text = list(cex = .7)),
#   scales = list(
#     x = list(relation = "free"),
#     y = list(relation = "free")
#   )
# )









# Center & Scale ----------------------------------------------------------
trainPreProcValues <-
  preProcess(trainX, method = c("center", "scale"))



# Predict the preProcValues model on trainX -------------------------------
trainScaled <- predict(trainPreProcValues, trainX)




# Look for, and removed, highly correlated variables ----------------------
correlations <- cor(trainScaled)
high_corr <- findCorrelation(correlations, cutoff = .9)
cols_cor <- rownames(correlations)[high_corr]
cols_X <- setdiff(cols_X, cols_cor)

trainFiltered <- trainScaled[, -high_corr, with = FALSE]



# Bind the classe variable back onto the transformed dataset --------------
train <- bind_cols(classe = trainY, trainFiltered)




# # pre processing with pca
set.seed(787)
preProc <- preProcess(train[, -1], method = "pca", thresh = 0.90)
trainPC <- predict(preProc, train[, -1])
# testPC <- predict(preProc, test[, -53])
# pml_testingPC <- predict(preProc, pml_testing2)



# build model -------------------------------------------------------------
# trainPC$classe <- train$classe
# rf_model <- caret::train(classe ~.,method = "rf",
#                   , data = trainPC[1:5000]
#                   , trControl = trainControl(method = "cv",  number = 4)
#                   , importance = TRUE)


# cross-validation controller ---------------------------------------------
ctrl <-
  trainControl(method = "repeatedcv",
               repeats = 1,
               savePred = TRUE)




# Support Vector Machines (SVM) -------------------------------------------

fitSVM <- caret::train(
  train$classe ~ .,
  data = trainPC,
  method = "svmRadial",
  tuneGrid = data.frame(.C = c(.25, .5, 1), .sigma = .05),
  trControl = ctrl
)

# model results
predSVM <- suppressMessages(predict(fitSVM, train))
confusionMatrix(predSVM, train$classe)




# Generalized Boosting Model (GBM -----------------------------------------
fitGBM <- train(
  classe ~ .,
  data = train,
  method = "gbm",
  trControl = ctrl,
  verbose = FALSE
)





data = read.csv("pml-training.csv", header = TRUE)
data <- data[, !apply(data , 2 , function(x)
  any(is.na(x)))]
classe <- data$classe
data <-
  data[, grepl("^roll|^pitch|^yaw|^gyros", names(data))]  #|^accel|^magnet|^total
data <- cbind(classe, data)

inTrain <- createDataPartition(data$classe, p = .7, list = FALSE)
training <- data[inTrain, ]
testing <- data[-inTrain, ]

fitControl <- trainControl(method = "cv", number = 10)
modelFit <-
  train(
    training$classe ~ .,
    method = "rf",
    trControl = fitControl,
    data = training
  )
pred <- predict(modelFit, testing)
confusionMatrix(pred, testing$classe)

#preProc <- preProcess(training[,-1], method="pca", pcaComp=10)
#trainPC <- predict(preProc, training[,-1])
#modelFit <- train(training$classe ~. ,method="rf", data= trainPC )
#testPC <- predict(preProc, testing[,-1])
#confusionMatrix(testing$classe, predict(modelFit,testPC))

#fitControl <- trainControl(method = "cv",number = 10)
#gbmFit <- train(classe ~ ., data = training,
#		method = "gbm",
#               trControl = fitControl,
#               verbose = FALSE)
#pred <- predict(fitControl, testing)

#testex <- read.csv("pml-testing.csv", header = TRUE)
#testex <- testex[, grepl( "^roll|^pitch|^yaw|^gyros", names(testex))]
#pml_write_files = function(x){
#  n = length(x)
#  for(i in 1:n){
#    filename = paste0("problem_id_",i,".txt")
#    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#  }
#}

#   ____________________________________________________________________________
#   initializing                                                            ####

library('RYLin')
loadPackages()
loadPackages(c('OpenML', 'mlr', 'h2o', 'caret', 'skimr'))


#   ____________________________________________________________________________
#   global settings                                                         ####

theme_set(theme_RYLin())
set.seed(787)



#   ____________________________________________________________________________
#   loading data                                                               ####
# Data: Orange Juice Data, https://rdrr.io/cran/ISLR/man/OJ.html
dt <- fread('data/orange_juice_withmissing.csv',
            # na.strings = c("NA", "#DIV/0!", ""),
            stringsAsFactors = TRUE,
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


##  ............................................................................
##  colunm info                                                             ####
cols_all <- names(dt)
cols_y <- 'Purchase'
dt[, (cols_y) := factor(get(cols_y))]
# cols_lbl <- ''
cols_x <- setdiff(cols_all, c(cols_y))
# cols_cat <-
#   c('Purchase',
#     'Store7')
# cols_num <- setdiff(cols_x, cols_cat)





#   ____________________________________________________________________________
#   summary info                                                            ####
# structure
dim(dt)
str(dt)

# response variable
# describe(dt$Purchase)

# independent variables

# descriptive statistics
# skimr::skim_to_list(dt)


# # correlation
# library(corrplot)
# par(mar = c(4, 2, 2, 2))
# M <- cor(dt[, cols_num, with = FALSE] %>% na.delete())
# corrplot(
#   M,
#   tl.col = 'grey30',
#   type = "lower",
#   method = "square",
#   tl.cex = .6
# )



#   ____________________________________________________________________________
#   preprocess                                                              ####


##  ............................................................................
##  missing values                                                          ####
##Identify variables with majority NA (prop > 0.5) and remove
cols_na <- dt %>% showNAInfo() %>% '['(prop > 0.5, col)
if (length(cols_na) > 0L) {
  dt[, (cols_na) := NULL]
}

##impute missing values using preProcess()
loadPackages('RANN')
preprocess_model_na <- preProcess(dt, method = 'knnImpute')
dt <- predict(preprocess_model_na, newdata = dt)
skim_to_wide(dt)

X = dt[, cols_x, with = FALSE]
Y = dt[[cols_y]]

##  ............................................................................
##  one-hot encoding                                                        ####
preprocess_model_dummies <- dummyVars( ~ ., data = X)
X <- predict(preprocess_model_dummies, newdata = X)
X <- data.table(X)


# ##  ............................................................................
# ##  normalization                                                           ####
# preprocess_model_range <- preProcess(X, method = 'range')
# X <- predict(preprocess_model_range, newdata = X)
# dt <- data.table(X)



##  ............................................................................
##  append target variable                                                  ####
dt$Purchase <- Y



# # Get rid of variables with near zero variance
# nsv <- nearZeroVar(dt[], saveMetrics = TRUE,)
# cols_nzv <- rownames(nsv[nsv$nzv == TRUE,])
# if (length(cols_nzv) > 0L) {
#   dt[, (cols_na) := NULL]
# }
#
#
# # Look for and removed highly correlated variables
# # correlations <- cor(trainScaled)
# # high_corr <- findCorrelation(correlations, cutoff = .9)
# # cols_cor <- rownames(correlations)[high_corr]
# # cols_X <- setdiff(cols_X, cols_cor)
# #
# # trainFiltered <- trainScaled[, -high_corr, with = FALSE]
#
#
#
# # pre processing with pca
# set.seed(787)
# preProc <- preProcess(trainX, method = "pca", thresh = 0.95)
# trainPC <- predict(preProc, trainX)
# # testPC <- predict(preProc, test[, -53])




#   ____________________________________________________________________________
#   split data                                                              ####
set.seed(787)
inTrain <-
  createDataPartition(y = dt[[cols_y]], p = 0.8, list = FALSE)
trainData <- dt[inTrain, ]
validData <- dt[-inTrain, ]



#   ____________________________________________________________________________
#   build model                                                             ####
fitControl <- trainControl(
  method = 'cv',
  number = 10,
  savePredictions = 'final',
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter  = TRUE
)


##  ............................................................................
##  Logistic                                                                ####

fitLogistic <- train(
  Purchase ~ .,
  data = trainData,
  method = 'glm',
  family = binomial(link = "logit"),
  # tuneLength = 5,
  # preProcess = c('center', 'scale'),
  # verbose = FALSE,
  metric = 'ROC',
  # tuneGrid = fitGrid,
  trControl = fitControl
)
fitLogistic


##  ............................................................................
##  SVM                                                                     ####

## svmLinear
set.seed(787)
fitGrid <- expand.grid(C = seq(0.1, by = 0.5, length.out = 10))

fitLinearSVM <- train(
  Purchase ~ .,
  data = trainData,
  method = 'svmLinear',
  tuneLength = 5,
  # preProcess = c('center', 'scale'),
  verbose = FALSE,
  metric = 'ROC',
  tuneGrid = fitGrid,
  trControl = fitControl
)
fitLinearSVM
plot(fitLinearSVM)

## svmRadial
fitRadialSVM <- train(
  Purchase ~ .,
  data = trainData,
  method = 'svmRadial',
  tuneLength = 10,
  # preProcess = c('center', 'scale'),
  verbose = FALSE,
  metric = 'ROC',
  tuneGrid = expand.grid(C = 2^seq(-2, 2, 0.5),
                         sigma = 2^seq(-10, 0, 2)),
  allowParallel = TRUE,
  trControl = fitControl
)
fitRadialSVM
plotCaretModel(fitRadialSVM)


# make predictions on the test data
svmRadialFitted <- fitRadialSVM %>% predict(validData)
confusionMatrix(validData$Purchase, svmRadialFitted, mode = 'everything', positive = 'MM')



##  ............................................................................
##  XGBoost                                                                 ####
# Step 1: Number of Iterations and the Learning Rate
# Step 2: Maximum Depth and Minimum Child Weight
# Step 3: Column and Row Sampling
# Step 4: Gamma
# Step 5: Reducing the Learning Rate

fitControl <- caret::trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final',
  verboseIter = TRUE,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

xgbGrid <- expand.grid(
  nrounds = seq(from = 400, to = 600, 30),
  eta = c(0.01),
  max_depth = c(2, 3),
  gamma = 0,
  colsample_bytree = c(0.8),
  min_child_weight = c(1, 5),
  subsample = c(1.0)
)

fitXGBoost <- caret::train(
  Purchase ~ .,
  data = trainData,
  trControl = fitControl,
  tuneGrid = xgbGrid,
  metric = 'ROC',
  method = "xgbTree",
  verbose = TRUE
)

fitXGBoost
plot(fitXGBoost)

# feature importance
plot(varImp(fitXGBoost), main = "Variable Importance with MARS")

# predicted
xgbFitted <- fitXGBoost %>% predict(validData)
confusionMatrix(validData$Purchase, xgbFitted, mode = 'everything', positive = 'MM')



##  ............................................................................
##  Adaboost                                                                ####
fitAdaboost <- caret::train(
  Purchase ~ .,
  data = trainData,
  trControl = fitControl,
  # tuneGrid = xgbGrid,
  tuneLength = 3,
  metric = 'ROC',
  method = "adaboost",
  verbose = TRUE
)
plot(fitAdaboost)


##  ............................................................................
##  Random Forest                                                           ####
fitRF <- caret::train(
  Purchase ~ .,
  data = trainData,
  trControl = fitControl,
  # tuneGrid = xgbGrid,
  tuneLength = 3,
  metric = 'ROC',
  method = "rf",
  verbose = TRUE
)
plot(fitRF)



##  ............................................................................
##  GBM                                                                     ####
set.seed(787)
getModelInfo()$gbm$parameters
gbmGrid <-  expand.grid(interaction.depth = c(3),
                        n.trees = c(500),
                        shrinkage = c(0.01),
                        n.minobsinnode = c(5))

fitGBM <- caret::train(
  Purchase ~ .,
  data = trainData,
  trControl = fitControl,
  tuneGrid = gbmGrid,
  # tuneLength = 3,
  metric = 'ROC',
  method = "gbm",
  verbose = TRUE
)
fitGBM
plot(fitGBM)



#   ____________________________________________________________________________
#   compare models                                                          ####

# compare models
modelComparision <-
  resamples(list(
    Logistic = fitLogistic,
    Adaboost = fitAdaboost,
    # RF = fitRF,
    XGBoost = fitXGBoost,
    svmLinear = fitLinearSVM,
    svmRadial = fitRadialSVM
  ))
summary(modelComparision)


#   ____________________________________________________________________________
#   ensembling                                                              ####
loadPackages('caretEnsemble')

# Stacking Algorithms - Run multiple algos in one call.
algorithmList <-
  c('rf', 'adaboost', 'earth', 'xgbDART', 'svmRadial')

set.seed(100)
models <-
  caretList(
    Purchase ~ .,
    data = trainData,
    trControl = fitControl,
    methodList = algorithmList
  )
results <- resamples(models)
summary(results)



# combine the predictions of multiple models to form a final prediction
set.seed(101)
stackControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1,
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
stack_predicteds <- predict(stack.glm, newdata = validData)
head(stack_predicteds)



##  ............................................................................
##  Parallel Processing                                                     ####

library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

## All subsequent models are then run in parallel
fitAdaboost <- caret::train(
  Purchase ~ .,
  data = trainData,
  trControl = fitControl,
  # tuneGrid = xgbGrid,
  tuneLength = 3,
  metric = 'ROC',
  method = "adaboost",
  verbose = TRUE
)
plot(fitAdaboost)

## When you are done:
stopCluster(cl)





#   ____________________________________________________________________________
#   h2o                                                                     ####
# https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/
library(h2o)
h2o.init(nthreads = -1)



##  import the data
df <- as.h2o(dt)
# df <- h2o.importFile(path = "http://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/titanic.csv")
dim(df)

## pick a response for the supervised problem
response <- "Purchase"
## the response variable is an integer, we will turn it into a categorical/factor for binary classification
df[[response]] <- as.factor(df[[response]])
## use all other columns (except for the name) as predictors
predictors <- setdiff(names(df), c(response, "name"))


# splits <- h2o.splitFrame(
#   data = df,
#   ratios = c(0.6, 0.2),   ## only need to specify 2 fractions, the 3rd is implied
#   destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 787
# )
# train <- splits[[1]]
# valid <- splits[[2]]
# test  <- splits[[3]]

splits <- h2o.splitFrame(
  data = df,
  ratios = c(0.8),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex"), seed = 787
)
trainData <- splits[[1]]
validData <- splits[[2]]




##establish baseline performance
## We only provide the required parameters, everything else is default
gbm <- h2o.gbm(x = predictors, y = response, training_frame = trainData)
## Show a detailed model summary
gbm
## Get the AUC on the validation set
h2o.auc(h2o.performance(gbm, newdata = validData))


## h2o.rbind makes a copy here, so it's better to use splitFrame with `ratios = c(0.8)` instead above
gbm <- h2o.gbm(x = predictors, y = response, training_frame = h2o.rbind(trainData), nfolds = 5, seed = 0xDECAF)



hyper_params = list(
  ## restrict the search to the range of max_depth established above
  max_depth = seq(5, 5, 1),

  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.9),

  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.8, 0.8, 0.1),

  ## search a large space of column sampling rates per tree
  # col_sample_rate_per_tree = seq(0.2, 1, 0.1),

  ## search a large space of how column sampling per split should change as a function of the depth of the split
  # col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),

  ## search a large space of the number of min rows in a terminal node
  min_rows = 2 ^ seq(0, 3, 1),

  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2 ^ seq(5, 5, 1),

  ## search a large space of the number of bins for split-finding for categorical columns
  # nbins_cats = 2 ^ seq(4, 12, 1),

  ## search a few minimum required relative error improvement thresholds for a split to happen
  # min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
  min_split_improvement = c(1e-3)

  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  # histogram_type = c("UniformAdaptive", "QuantilesGlobal", "RoundRobin")
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",

  ## limit the runtime to 30 minutes
  max_runtime_secs = 1800,

  ## build no more than 100 models
  max_models = 100,

  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,
  stopping_tolerance = 1e-3,
  stopping_metric = "AUC",

  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234
)

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,

  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,

  ## which algorithm to run
  algorithm = "gbm",

  ## identifier for the grid, to later retrieve it
  grid_id = "finalGrid",

  ## standard model parameters
  x = cols_x,
  y = cols_y,
  training_frame = trainData,
  validation_frame = validData,

  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 1000,

  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,

  ## learning rate annealing: learning_rate shrinks by 1% after every tree
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,

  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,

  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 787
)

## Sort the grid models by AUC
sortedGrid <-
  h2o.getGrid("finalGrid", sort_by = "auc", decreasing = TRUE)
sortedGrid




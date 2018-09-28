library('RYLin')
loadPackages()
loadPackages(c('OpenML', 'mlr', 'h2o', 'caret', 'skimr'))


# global settings ---------------------------------------------------------
theme_set(theme_RYLin())
set.seed(787)


# load data ---------------------------------------------------------------
# Data: Orange Juice Data
# https://rdrr.io/cran/ISLR/man/OJ.html
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


# exploring the data ------------------------------------------------------
dim(dt)
str(dt)

# response variable
describe(dt$Purchase)


# predictors


# descriptive statistics
skimr::skim_to_list(train)



# # correlation -------------------------------------------------------------
# library(corrplot)
# par(mar = c(4, 2, 2, 2))
# M <- cor(train[, -c('classe')])
# corrplot(
#   M,
#   tl.col = 'grey50',
#   type = "lower",
#   method = "square",
#   tl.cex = .6
# )



#
# # split train & test ------------------------------------------------------
# #Take training and break into validation and training.
# set.seed(787)
# inTrain <- createDataPartition(y = dt$classe, p = 0.6, list = FALSE)
# train <- dt[inTrain,]
# test <- dt[-inTrain,]



# preprocess --------------------------------------------------------------
# colnames ----------------------------------------------------------------
cols_all <- names(dt)
cols_y <- 'Purchase'
cols_lbl <- ''
cols_x <- setdiff(cols_all, c(cols_y, cols_lbl))
cols_cat <-
  c('Purchase',
    'StoreID',
    'Store7',
    'STORE',
    'SpecialCH',
    'SpecialMM')
cols_num <- setdiff(cols_x, cols_cat)

# valid_names <-
#   grep("^roll_|^pitch|^yaw_|^total_accel|^gyros_|^accel_|^magnet|classe",
#        names,
#        value = T)
# train <- train[, .SD, .SDcols = subsetnames]
dt[, (cols_cat) := lapply(.SD, as.factor), .SDcols = cols_cat]
dt[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num]
skim_to_wide(dt)



# missing values ----------------------------------------------------------
#Identify the variables with majority NA, and remove
cols_na <- dt %>%
  showNAInfo() %>%
  '['(prop > 0.5, col)

if (length(cols_na) > 0L) {
  dt[, (cols_na) := NULL]
}


# impute missing values using preProcess()
# Create the knn imputation model on the training data
missingvalue_model <- preProcess(dt, method = 'knnImpute')
missingvalue_model

# Use the imputation model to predict the values of missing data points
# library(RANN)  # required for knnInpute
dt <- predict(missingvalue_model, newdata = dt)
skim_to_wide(dt)

# categorical variables
for (col in cols_cat) {
  fillNAs(dt, col)
}
skim_to_wide(dt)




# Get rid of variables with near zero variance ----------------------------
nsv <- nearZeroVar(dt[], saveMetrics = TRUE, )
cols_nzv <- rownames(nsv[nsv$nzv == TRUE, ])
if (length(cols_nzv) > 0L) {
  dt[, (cols_na) := NULL]
}


# Remove the first six variables. They have very little value in prediction --
# cols_tbr <- cols_all[1:6]


cols_Y <- 'Purchase'
cols_X <- setdiff(names(dt), cols_Y)
trainX <- dt[, cols_X, with = FALSE]
trainY <- dt[, cols_Y, with = FALSE]


# feture engering ---------------------------------------------------------
# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(Purchase ~ ., data = dt)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
dt <- predict(dummies_model, newdata = dt)

# # Convert to dataframe
dt <- data.table(dt, Purchase = trainY$Purchase)

# # See the structure of the new dataset
skim_to_list(dt)




featurePlot(
  x = dt[, 1:32],
  y = dt$Purchase,
  plot = "box",
  strip = strip.custom(par.strip.text = list(cex = .7)),
  scales = list(
    x = list(relation = "free"),
    y = list(relation = "free")
  )
)









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
training <- data[inTrain,]
testing <- data[-inTrain,]

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

## A Data Science Case Study With R and mlr
## source: http://www.alpha-epsilon.de/r/2018/05/08/a-data-science-case-study-with-r-and-mlr/
##
loadPackages('RYLin')
loadPackages()
loadPackages('OpenML', 'mlr')


# global settings ---------------------------------------------------------
theme_set(theme_RYLin())


# Q: how to approximate a subject’s height by measuring his/her arms length?
dt <- fread('data/nhgh', select = c("sex", "ht", "arml")) %>%
  na.omit()





# eda ---------------------------------------------------------------------
dt %>%
  ggplot(aes(x = arml, y = ht, color = sex)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = F) +
  # scale_color_viridis_d()
  scale_colour_manual(values = mycolors)

dt[, sex := factor(sex)]


# caret -------------------------------------------------------------------

library(caret)
set.seed(787)

trainID <- createDataPartition(y = dt$ht, p = 0.6, list = FALSE)
train <- dt[trainID, ]
test <-  dt[-trainID, ]
validID <- createDataPartition(y = test$ht, p = 0.5, list = FALSE)
valid <- test[validID, ]
test <- test[-validID, ]
rm(trainID, validID)


ctrl <-
  trainControl(method = 'repeatedcv',
               number = 10,
               repeats = 5)
grid <- expand.grid(maxdepth = seq(1, 10, 1))
cart <-
  train(
    ht ~ arml + sex,
    data = train,
    method = 'rpart2',
    tuneGrid = grid,
    metric = 'RMSE',
    trControl = ctrl
  )
cart



# h2o ---------------------------------------------------------------------
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "6g")

hdt  <- as.h2o(dt)
rand  <- h2o.runif(hdt)
htrain <- hdt[rand$rnd <= 0.8,]
hvalid <- hdt[rand$rnd > 0.8,]

response <- c("ht")
features <- setdiff(colnames(hdt), c(response))

gbm50 <- h2o.gbm(
  x = features,
  y = response,
  training_frame = htrain,
  validation_frame = hvalid,
  nfolds = 10,
  keep_cross_validation_predictions = FALSE,
  keep_cross_validation_fold_assignment = FALSE,
  score_each_iteration = TRUE,
  fold_assignment = "Random",
  ntrees = 100,
  max_depth = 50,
  min_rows = 1,
  nbins = 10,
  stopping_rounds = 5,
  stopping_metric = "RMSE",
  stopping_tolerance = 0.0001,
  seed = 787,
  learn_rate = 0.3,
  learn_rate_annealing = 1,
  distribution = "quantile",
  quantile_alpha = 0.5,
  huber_alpha = 0.9,
  sample_rate = 0.7,
  col_sample_rate = 1,
  col_sample_rate_change_per_level = 1,
  col_sample_rate_per_tree = 1,
  min_split_improvement = 1e-05,
  histogram_type = "Random",
  max_abs_leafnode_pred = Inf,
  pred_noise_bandwidth = 0,
  categorical_encoding = "Binary"
)



df <- as.h2o(dt)
summary(df, exact_quantiles = TRUE)

## pick a response for the ml problem
response <- "ht"

## the response variable is an integer, we will turn it into a categorical/factor for binary classification
# df[[response]] <- as.factor(df[[response]])

## use all other columns (except for the name) as predictors
predictors <- setdiff(names(df), c(response))


# Split the data for Machine Learning -------------------------------------

splits <- h2o.splitFrame(
  data = df,
  ratios = c(0.6, 0.2),
  ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"),
  seed = 787
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]



# Establish baseline performance ------------------------------------------

## We only provide the required parameters, everything else is default
gbm <- h2o.gbm(x = predictors,
               y = response,
               training_frame = train)

## Show a detailed model summary
gbm

## Get the AUC on the validation set
# h2o.auc(h2o.performance(gbm, newdata = valid))



gbm <-
  h2o.gbm(
    x = predictors,
    y = response,
    training_frame = h2o.rbind(train, valid),
    nfolds = 10,
    histogram_type = 'UniformAdaptive',
    max_depth = 3,
    min_rows = 32,
    nbins = 128,
    sample_rate = 0.7,
    seed = 787
  )
gbm@model$cross_validation_metrics_summary


gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors,
  y = response,
  training_frame = train,
  validation_frame = valid,

  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 1000,

  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate = 0.01,

  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  # stopping_tolerance = 1e-4,
  # stopping_metric = "AUC",

  ## sample 80% of rows per tree
  sample_rate = 0.7,

  ## sample 80% of columns per split
  # col_sample_rate = 0.8,

  histogram_type = "UniformAdaptive",

  max_depth = 5,
  nbins = 128,

  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,


  ## fix a random number generator seed for reproducibility
  seed = 787
)

## Get the AUC on the validation set
# h2o.auc(h2o.performance(gbm, valid = TRUE))
gbm




## Depth 10 is usually plenty of depth for most datasets, but you never know
hyper_params = list(max_depth = seq(1, 10, 1))
# hyper_params = list(max_depth = c(2,4,6,8,12,16,20) ) ##faster for larger datasets

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,

  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),

  ## which algorithm to run
  algorithm = "gbm",

  ## identifier for the grid, to later retrieve it
  grid_id = "depth_grid",

  ## standard model parameters
  x = predictors,
  y = response,
  training_frame = train,
  validation_frame = valid,

  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,

  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,

  ## learning rate annealing: learning_rate shrinks by 1% after every tree
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,

  ## sample 80% of rows per tree
  sample_rate = 0.8,

  ## sample 80% of columns per split
  # col_sample_rate = 0.8,

  ## fix a random number generator seed for reproducibility
  seed = 787,

  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-2,
  stopping_metric = "MSE",

  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid

## sort the grid models by decreasing AUC
sortedGrid <-
  h2o.getGrid("depth_grid", sort_by = "RMSE", decreasing = FALSE)
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))






# mlr ---------------------------------------------------------------------
tsk <- makeRegrTask(data = data.frame(dt), target = "ht")
lrn <- makeLearner('regr.glm')
mdl <- mlr::train(lrn, tsk)

performance(mdl, measures = list(rsq, expvar, mae, rmse, mse, medse, medae, rrse))

## A Data Science Case Study With R and mlr
## source: http://www.alpha-epsilon.de/r/2018/05/08/a-data-science-case-study-with-r-and-mlr/
##

loadPackages('OpenML', 'mlr')

dt <- getOMLDataSet(data.id = 31) %>%
  '$'(data) %>%
  as.data.table()


### Generate the task
tsk <- makeClassifTask(data = as.data.frame(dt), target = "class")

### Generate the learner
lrn.glm <- makeLearner("classif.binomial")

### Train the learner
mdl.glm = train(lrn.glm, tsk)
mdl.glm


# Running machine learning models
n <- nrow(dt)
set.seed(787)
train.id <- sample(n, size = 2 / 3 * n)
test.id <- setdiff(1:n, train.id)

mdl.glm <- train(lrn.glm, tsk, subset = train.id)
pred <- predict(mdl.glm, tsk, subset = test.id)

table(pred$data[, 2:3])
performance(pred, measures = list(acc, tpr, fpr, fnr, auc))

# Predicting probability instead of class
lrn.glm.prob <-
  makeLearner("classif.binomial", predict.type = "prob")
mdl.glm.prob <- train(lrn.glm.prob, tsk, subset = train.id)
pred <- predict(mdl.glm.prob, tsk, subset = test.id)
g <- generateThreshVsPerfData(pred, measures = list(fpr, fnr, mmce))
plotThreshVsPerf(g)


# Tuning a random forest
library("parallelMap")
parallelStartSocket(3)

lrn.ranger <-
  makeLearner("classif.ranger", predict.type = "prob")

ps <- makeParamSet(
  makeIntegerParam("num.trees", lower = 100, upper = 200),
  makeIntegerParam("mtry", lower = 2, upper = 10)
)
ctrl <- makeTuneControlRandom(maxit = 100)
rdesc <- makeResampleDesc("CV", iters = 3)
res <-
  tuneParams(
    "classif.ranger",
    task = tsk,
    resampling = rdesc,
    par.set = ps,
    control = ctrl,
    measure = acc
  )
parallelStop()
res$x


# Benchmarking different models
tuned.ranger.learner <-
  makeLearner(
    "classif.ranger",
    predict.type = "prob",
    num.trees = best.n,
    mtry = best.mtry
  )

learnersList <- list(glm.prob.learner,
                     tuned.ranger.learner)

rdesc <- makeResampleDesc("CV", iters = 10, predict = "test")
bmr <- benchmark(learnersList,
                 task,
                 rdesc,
                 measures = list(fpr, fnr, acc, mmce, timetrain))

getBMRAggrPerformances(bmr, as.df = TRUE)


# Apply different probability thresholds
tuned.ranger.learner <-
  setPredictThreshold(tuned.ranger.learner, 0.70)
tuned.glm.learner <- setPredictThreshold(glm.prob.learner, 0.70)

learnersList <- list(tuned.glm.learner,
                     tuned.ranger.learner)
bmr <- benchmark(learnersList,
                 task,
                 rdesc,
                 measures = list(fpr, fnr, acc, mmce, timetrain))

getBMRAggrPerformances(bmr, as.df = TRUE)



# demo --------------------------------------------------------------------

tsk <- makeClassifTask(data = iris, target = "Species")
lrn <- makeLearner('classif.ksvm')
rdesc = makeResampleDesc("CV", iters = 3L)
ps = makeParamSet(makeDiscreteParam("C", values = c(0.5, 1.0, 1.5, 2.0)),
                  makeDiscreteParam("sigma", values = c(0.5, 1.0, 1.5, 2.0)))
ctrl = makeTuneControlGrid()
tuneParams(
  learner = lrn,
  task = tsk,
  resampling = rdesc,
  par.set = ps,
  control = ctrl
)

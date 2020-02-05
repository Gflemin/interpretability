



df_list = loader()

df = combiner(df_list) 

#sapply(df, function(x) length(unique(x[!is.na(x)])))

### Make our task and check its characteristics

# make the task
home_price = TaskRegr$new(id = "houses", backend = df, target = "sale_price")

# see the data
glimpse(home_price$data()) # drop alley, pool_qc, misc_feature, fence, fireplace_qu, or just convert NAs to factor levels? 

# check the # of cols/rows
home_price$nrow # 1094
home_price$ncol # 75

# see the "role" each column plays
home_price$col_roles

# feature names
home_price$feature_names

# filer rows/select columns
# home_price$select(c("alley", "bldg_type", "gr_liv_area"))

### Learners 

# view learners
mlr_learners

# save a learner
ex = mlr_learners$get("regr.featureless")
ex2 = mlr_learners$get("regr.glmnet")
ex3 = lrn("regr.rpart", minsplit = 15, maxdepth = 15) # alternative, shorter notation

# learn about a learner's params
ex2$param_set
ex3$param_set

# choose values of certain params and drop others
ex2$param_set$values = list(lambda = 0.08, family = "gaussian")
ex2 # see that the parameters are now included 

### Train/test split

# do our train/test split
set.seed(1)
train = sample(home_price$nrow, 0.8*home_price$nrow)
test = setdiff(seq_len(home_price$nrow), train)

### Training the model

# at first, there's nothing in the learner
ex3$model

# put something there by running the $train method on the learner and inputing a task
ex3$train(home_price, row_ids = train)

# now look at the learner's model
ex3$model

### Predictions and scores

# make the learner make predictions
preds3 = ex3$predict(home_price, row_ids = test)
preds3$data$tab # view our predictions

# view our measures
mlr_measures

# save a measure of interest 
score = msr('regr.mape')

# score our predictions
preds3$score(score)

##### The above was our most basic workflow. Now, lets take that and do performance comparisons, resampling, and model tuning (using same task as last time)

### Save a list of learners and instantiate them

# save list
learners_list = c("regr.rpart", "regr.lm", "regr.ranger")

# instantiate
learners = lapply(learners_list, lrn,
                  predict_sets = c("train", "test")) # hyperparameter for lrn

### Decide upon a resampling method to handle crossvalidation

# view resample methods
mlr_resamplings

# save one
set.seed(1)
resampl = rsmp("cv", folds = 4)

### Create a benchmark design that will frame our "experiment", instantitate

# create the design
design = benchmark_grid(home_price, learners, resampl)
design

# instantiate
bmr = benchmark(design)

### Choose and calculate measures

# choose measures 
measures = list(
  msr("regr.mse", id = "train_MSE", predict_sets = "train"),
  msr("regr.mse", id = "test_MSE"),
  msr("regr.mape", id = "train_MAPE", predict_sets = "train"),
  msr("regr.mape", id = "test_MAPE")
)

# calculate measures
bmr$aggregate(measures) # how do I break out the individual results?


# if we have multiple tasks, we can also aggregate and rank our learners according to which ones do best overall 

### Building tuning precursors

# lm params
## none!

# rpart params
rpart_ps = ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1, upper = 20)
))

# ranger params 
ranger_ps = ParamSet$new(list(
  ParamInt$new("mtry", lower = 1, upper = 10),
  ParamInt$new("num.trees", lower = 200, upper = 800)
))

# terminators
terminator = term("evals", n_evals = 20)

# tuner
tuner = tnr("random_search")

### Build the tuners

# rpart tuner
at_rpart = AutoTuner$new(
  learner = lrn("regr.rpart"),
  resampling = resampl,
  measures = msr("regr.mape"),
  tune_ps = rpart_ps,
  terminator = terminator,
  tuner = tuner
)
at_rpart # view params selected

# ranger tuner
at_ranger = AutoTuner$new(
  learner = lrn("regr.ranger"),
  resampling = resampl,
  measures = msr("regr.mape"),
  tune_ps = ranger_ps,
  terminator = terminator,
  tuner = tuner
)
at_ranger # view params selected

### Benchmark our new tuners against the defaults 

# build a new benchmark design
design2 = benchmark_grid(
  task = home_price,
  learner = c(at_rpart, at_ranger, learners_list),
  resampling = resampl
)

# instantiate the benchmark
bmr2 = benchmark(design2) 

# aggregate the benchmark over our measures
bmr2$aggregate(msr("regr.mape"))
bmr2$learners$learner
hey = mlr_learners$get("regr.ranger")

### Train our models

# ranger
at_ranger$train(home_price, row_ids = train)
at_ranger$model
##### IML Time!

### Put a model(s) in IML Predictor() objects

# throwing ranger into predictor 
pred = Predictor$new(at_ranger$model$learner, data = df %>% select(-sale_price), y = df$sale_price) # need to drop the y in here 
pred

at_ranger$model$learner
trained_learners[[1]]
### Use the Predictor() object to calculate and plot feature importance

# calculate feature importance
imp = FeatureImp$new(pred, loss = "mae") # use a different metric for importance than we used for model performace for funsies

# filter down to a smaller set of features so it isn't crazy when we plot it 
imp_smaller = imp$results %>% 
  top_n(20)
imp_smaller

# plot feature importance
imp_plot = ggplot(imp_smaller, aes(importance, reorder(feature, importance))) + geom_point(size = 3) + 
  geom_errorbarh(aes(xmin = importance.05, xmax = importance.95), size = 0.3)
imp_plot

### Generate global feature effects calculations

# ale
effs = FeatureEffects$new(pred, method = "ale")
effs$effects$gr_liv_area$results

# pdp
effs2 = FeatureEffects$new(pred, method = "pdp")
effs2$effects$gr_liv_area$results

# pdp + ice 
effs3 = FeatureEffects$new(pred, method = "pdp+ice")
effs3$effects$gr_liv_area$results

### Build and view our plots for our various feature effects 

# pdp 
pdp = ggplot(effs2$effects$gr_liv_area$results, aes(gr_liv_area, .y.hat)) + geom_line()
pdp

# pdp + ice
ice_frame = effs3$effects$gr_liv_area$results %>% 
  filter(.type == "ice") %>% 
  mutate(.y.hat = .y.hat/1000)
pdp_frame = effs3$effects$gr_liv_area$results %>% 
  filter(.type == "pdp") %>% 
  mutate(.y.hat = .y.hat/1000)
pdpice = ggplot() + 
  geom_line(ice_frame, mapping = aes(gr_liv_area, .y.hat, group = .id), alpha = 0.2) + 
  geom_line(pdp_frame, mapping = aes(gr_liv_area, .y.hat), color = "red", size = 3) +
  geom_rug(alpha = 0.05)
pdpice

# ale 
ale = ggplot(effs$effects$gr_liv_area$results, aes(gr_liv_area, .ale)) + geom_line()
ale

### Generate local feature effects calculations 

# lime 
lime.explain = LocalModel$new(pred, x.interest = df %>% select(-sale_price) %>% slice(3), k = 30)
lime.explain$results
plot(lime.explain)

# shapley 
shap.explain = Shapley$new(pred, x.interest = df %>% select(-sale_price) %>% slice(30))
shap.explain$results
plot(shap.explain)
class(df[1, ])
class(df %>% select(-sale_price) %>% slice(3))
?Shapley 

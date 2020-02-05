##### Drake plan

# the plan
plan = drake_plan(
  df_raw = loader(),
  df = combiner(df_raw, list = c("overall_cond", "overall_qual", "mo_sold", "kitchen_abv_gr", "half_bath", 
                                 "garage_cars", "full_bath", "fireplaces", "bsmt_half_bath", "bsmt_full_bath", "bedroom_abv_gr"), 
                0.2, 0.1),
  task = tasker(df, "house", target = "sale_price"),
  learners = learn_gener(c("regr.rpart", "regr.ranger")),
  datasets = splitter(task, 0.8, 2),
  trained_learners = trainer(learners, task, datasets),
  resample = resampler("cv", 4, 2),
  benchmarks = benchmarker(task, learners, resample, c("regr.mse", "regr.mape")),
  preds = predictor(trained_learners, task, datasets[[2]]),
  data_clean = fixer(task, "sale_price"), # correct
  global_plots = imler_global(trained_learners, data_clean[[1]], data_clean[[2]]), # correct
  local_plots = imler_local(trained_learners, data_clean[[1]], data_clean[[2]], 131)
)

# configure our drake plan into a visualizable object
config = drake_config(plan)

# overall_cond, overall_qual, mo_sold, kitchen_abv_gr, half_bath, garage_cars, full_bath, fireplaces, bsmt_half_bath,
# bsmt_full_bath, bedroom_abv_gr



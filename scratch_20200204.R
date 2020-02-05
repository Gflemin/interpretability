preds = lapply(trained_learners, function(x) Predictor$new(x, data = as_tibble(data_clean[[1]]), 
                                                           y = as_tibble(data_clean[[2]])))
imps = lapply(preds, function(x) FeatureImp$new(x, loss = "mae"))
smol_imps = lapply(imps, function(x) x$results %>% top_n(20))
smol_imps = 
  imp_plots = lapply(smol_imps, function(x) ggplot(x, aes(importance, reorder(feature, importance))) + 
                       geom_point(size = 3) +
                       geom_errorbarh(aes(xmin = importance.05, xmax = importance.95), size = 0.3))



imler_local = function(trained_learners, data, target, observation) {
  preds = lapply(trained_learners, function(x) Predictor$new(x, data = as_tibble(data), 
                                                             y = target))
  limes = lapply(preds, function(x) LocalModel$new(x, x.interest = as_tibble(data) %>% slice(observation), k = 20))
  shaps = lapply(preds, function(x) Shapley$new(x, x.interest = as_tibble(data) %>% slice(observation)))
  list(lapply(limes, function(x) plot(x)), lapply(shaps, function(x) plot(x)))
}

imler_local(trained_learners, data_clean[[1]], data_clean[[2]], 131)


preds = lapply(trained_learners, function(x) Predictor$new(x, data = as_tibble(data_clean[[1]]), 
                                                           y = data_clean[[2]]))
limes = lapply(preds, function(x) LocalModel$new(x, x.interest = as_tibble(data_clean[[1]]) %>% slice(41), k = 20))
shaps = lapply(preds, function(x) Shapley$new(x, x.interest = as_tibble(data_clean[[1]]) %>% slice(41)))
lime_plots = lapply(limes, function(x) plot(x))
shap_plots = lapply(shaps, function(x) plot(x))



imler_global = function(trained_learners, data, target) {
  preds = lapply(trained_learners, function(x) Predictor$new(x, data = as_tibble(data), 
                                                             y = target))
  imps = lapply(preds, function(x) FeatureImp$new(x, loss = "mae"))
  smol_imps = lapply(imps, function(x) x$results %>% top_n(20))
  imp_plots = lapply(smol_imps, function(x) ggplot(x, aes(importance, reorder(feature, importance))) + 
                       geom_point(size = 3) +
                       geom_errorbarh(aes(xmin = importance.05, xmax = importance.95), size = 0.3))
  imp_plots2 = list()
  for (i in 1:2) {
    imp_plots_2[[i]] = global_plots[[i]] + 
      labs(x = "Importance", y = "Feature", title = paste("Permutation importance for", lrn_list[[i]]))
  }
  imp_plots2
}

imler_local = function(trained_learners, data, target, observation) {
  preds = lapply(trained_learners, function(x) Predictor$new(x, data = as_tibble(data), 
                                                             y = target))
  limes = lapply(preds, function(x) LocalModel$new(x, x.interest = as_tibble(data) %>% slice(observation), k = 20))
  shaps = lapply(preds, function(x) Shapley$new(x, x.interest = as_tibble(data) %>% slice(observation)))
  list(lapply(limes, function(x) plot(x)), lapply(shaps, function(x) plot(x)))
}

preds = lapply(trained_learners, function(x) Predictor$new(x, data = as_tibble(data_clean[[1]]), 
                                                           y = data_clean[[2]]))
limes = lapply(preds, function(x) LocalModel$new(x, x.interest = as_tibble(data_clean[[1]]) %>% slice(45), k = 20))
lime_plots = lapply(limes, function(x) ggplot(as_tibble(x$results), aes(reorder(x$results$feature.value, x$results$effect), x$results$effect)) +
                      geom_bar(stat = "identity") +
                      coord_flip())
lime_plots
class(limes[[2]]$results)
limes[[2]]$results
shaps = lapply(preds, function(x) Shapley$new(x, x.interest = as_tibble(data_clean[[1]]) %>% slice(45)))

View(limes[[2]])
list(lapply(limes, function(x) plot(x)), lapply(shaps, function(x) plot(x)))

loadd(local_plots)
local_plots
imler_global2(trained_learners, data_clean[[1]], data_clean[[2]])

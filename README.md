README
================
TatsumotoH
6/13/2021

## R Markdown

Quick installation

``` r
# install.packages("remotes")
remotes::install_github("TatsumotoH/addstepr")
```

Getting started

``` r
library(workflows)
library(workflowsets)

library(tidymodels)
library(embed)

library(addstepr)

#for parallel processing
doParallel::registerDoParallel()

data(ames)

data_split = initial_split(ames, strata = "Sale_Price", prop = 0.75)
data_train = training(data_split)
data_test  = testing(data_split)


tune_folds = vfold_cv(data_train, v=5)


data_rec = 
  recipe(Sale_Price ~ ., data = data_train ) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_umap(all_numeric_predictors(),
            neighbors = tune(),
            min_dist = tune()) %>% 
  step_dbscan(umap_1, umap_2, 
                eps = tune(),
              minPts = tune())


mod_spec =  linear_reg() %>% 
    set_engine('lm') %>% # adds lm implementation of linear regression
    set_mode('regression')
    
    
tune_wf = workflow() %>% 
   add_recipe(data_rec) %>% 
   add_model(mod_spec)
   
# check parameters to tune
tune_args(tune_wf)

# make the paramater grid
tune_params = parameters(tune_wf)
param_finalized = finalize(tune_params, x=data_train)
param_grid = grid_random(param_finalized, size=5)
    
# tuning
tune_res = tune_grid(
   tune_wf,
   resamples = tune_folds,             
   grid = param_grid                   
 )
 
# show tuning results
tune_res %>% 
    collect_metrics() 

tune_res %>% 
   show_best(metric = "rmse")
 
# best model
best_rmse = tune_res %>% 
   select_best("rmse")

# finalize workflow
final_wf = finalize_workflow(
  tune_wf,
  best_rmse
)


# fit
final_model = workflows:::fit.workflow(final_wf, ames)

pred = predict(final_model, new_data= ames)

# evaluate the performance of final model
final_res = final_wf %>%
  last_fit(data_split)
  
final_res %>%
  collect_metrics()
```

Remove package

``` r
remove.packages("addstepr")
```

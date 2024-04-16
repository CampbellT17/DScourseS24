library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)
library(knitr)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income %<>% select(-native.country, -fnlwgt, education.num)
# Make sure continuous variables are formatted as numeric
income %<>% mutate(across(c(age,hours,education.num,capital.gain,capital.loss), as.numeric))
# Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
# Combine levels of factor variables that currently have too many levels
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
),
marital.status = fct_collapse(marital.status,
                              Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                              Divorced     = c("Divorced","Separated"), 
                              Widowed      = c("Widowed"), 
                              NeverMarried = c("Never-married")
), 
race = fct_collapse(race,
                    White = c("White"), 
                    Black = c("Black"), 
                    Asian = c("Asian-Pac-Islander"), 
                    Other = c("Other","Amer-Indian-Eskimo")
), 
workclass = fct_collapse(workclass,
                         Private = c("Private"), 
                         SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                         Gov     = c("Federal-gov","Local-gov","State-gov"), 
                         Other   = c("Without-pay","Never-worked","?")
), 
occupation = fct_collapse(occupation,
                          BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                          WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                          Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
)
)


######################
# tidymodels time!
######################
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)




#####################
# logistic regression
#####################
print('Starting LOGIT')
# set up the task and the engine
tune_logit_spec <- logistic_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

# define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf_logit <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res_logit <- rec_wf_logit %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid,
    metrics = metric_set(accuracy)
  )

# what is the best value of lambda?
top_acc  <- show_best(rec_res_logit, metric = "accuracy")
best_acc <- select_best(rec_res_logit, metric = "accuracy")
final_logit_lasso <- finalize_workflow(rec_wf_logit,
                                       best_acc
)

# Train the model with the optimal parameters
trained_logit_lasso <- final_logit_lasso %>%
  fit(data = income_train)

# Generate predictions on the test set
logit_preds <- predict(trained_logit_lasso, income_test) %>%
  bind_cols(income_test) %>%
  select(high.earner, .pred_class)

# Compute accuracy
logit_accuracy <- logit_preds %>%
  accuracy(truth = high.earner, estimate = .pred_class)

print('*********** LOGISTIC REGRESSION **************')
logit_test <- last_fit(final_logit_lasso,income_split) %>%
  collect_metrics()  %>% mutate(alg="logit")

logit_test %>% print(n = 1)
top_acc %>% print(n = 1)
best_acc %>% print(n = 1)
print(logit_accuracy)

# combine results into a nice tibble (for later use)
logit_accuracy <- logit_test %>%
  filter(.metric == "accuracy") %>%
  select(alg, .metric, .estimate)

#####################
# tree model
#####################
print('Starting TREE')
# set up the task and the engine
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune(), # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,50,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df <- tree_parm_df1 %>%
  cross_join(tree_parm_df2) %>%
  cross_join(tree_parm_df3)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf_tree <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res_tree <- rec_wf_tree %>%
  tune_grid(
    resamples = rec_folds,
    grid = tree_parm_df,
    metrics = metric_set(accuracy)
  )

# What are the best parameters?
best_params_tree <- rec_res_tree %>%
  select_best(metric = "accuracy")

# Finalize workflow with the best parameters
final_tree <- finalize_workflow(rec_wf_tree, best_params_tree)

# Train the model with the optimal parameters
trained_tree <- final_tree %>%
  fit(data = income_train)

# Generate predictions on the test set
tree_preds <- predict(trained_tree, income_test) %>%
  bind_cols(income_test) %>%
  select(high.earner, .pred_class)

# Compute accuracy
tree_accuracy <- tree_preds %>%
  accuracy(truth = high.earner, estimate = .pred_class) %>%
  mutate(alg = "tree") %>%
  select(alg, .metric, .estimate)

print('*********** DECISION TREE **************')
print(tree_accuracy)

#####################
# neural net
#####################
print('Starting NNET')
# set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (penalty)
lambda_grid <- grid_regular(penalty(), levels = 10)
nnet_parm_df1 <- tibble(hidden_units = seq(1, 10))
nnet_parm_df <- expand_grid(nnet_parm_df1, lambda_grid)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf_nnet <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res_nnet <- rec_wf_nnet %>%
  tune_grid(
    resamples = rec_folds,
    grid = nnet_parm_df,
    metrics = metric_set(accuracy)
  )

# What are the best parameters?
best_params_nnet <- rec_res_nnet %>%
  select_best(metric = "accuracy")

# Finalize workflow with the best parameters
final_nnet <- finalize_workflow(rec_wf_nnet, best_params_nnet)

# Train the model with the optimal parameters
trained_nnet <- final_nnet %>%
  fit(data = income_train)

# Generate predictions on the test set
nnet_preds <- predict(trained_nnet, income_test) %>%
  bind_cols(income_test) %>%
  select(high.earner, .pred_class)

# Compute accuracy
nnet_accuracy <- nnet_preds %>%
  accuracy(truth = high.earner, estimate = .pred_class) %>%
     mutate(alg = "nnet") %>%
     select(alg, .metric, .estimate)

print('*********** NEURAL NETWORK **************')
print(nnet_accuracy)

#####################
# knn
#####################
print('Starting KNN')
# set up the task and the engine
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # tuning parameter
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
knn_parm_df <- tibble(neighbors = seq(1,30))

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf_knn <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res_knn <- rec_wf_knn %>%
  tune_grid(
    resamples = rec_folds,
    grid = knn_parm_df,
    metrics = metric_set(accuracy)
  )

# What is the best value of the hyperparameter?
best_params_knn <- rec_res_knn %>%
  select_best(metric = "accuracy")

# Finalize workflow with the best parameters
final_knn <- finalize_workflow(rec_wf_knn, best_params_knn)

# Train the model with the optimal parameters
trained_knn <- final_knn %>%
  fit(data = income_train)

# Generate predictions on the test set
knn_preds <- predict(trained_knn, income_test) %>%
  bind_cols(income_test) %>%
  select(high.earner, .pred_class)

# Compute accuracy
knn_accuracy <- knn_preds %>%
  accuracy(truth = high.earner, estimate = .pred_class) %>%
     mutate(alg = "knn") %>%
     select(alg, .metric, .estimate)

print('*********** k-NEAREST NEIGHBORS **************')
print(knn_accuracy)

#####################
# SVM
#####################
print('Starting SVM')
# set up the task and the engine
tune_svm_spec <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
svm_parm_df1 <- tibble(cost = c(2^(-2), 2^(-1), 2^0, 2^1, 2^2, 2^10))
svm_parm_df2 <- tibble(rbf_sigma = c(2^(-2), 2^(-1), 2^0, 2^1, 2^2, 2^10))
svm_parm_df <- expand_grid(svm_parm_df1, svm_parm_df2)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

# Workflow
rec_wf_svm <- workflow() %>%
  add_model(tune_svm_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res_svm <- rec_wf_svm %>%
  tune_grid(
    resamples = rec_folds,
    grid = svm_parm_df,
    metrics = metric_set(accuracy)
  )

# What are the best parameters?
best_params_svm <- rec_res_svm %>%
  select_best(metric = "accuracy")

# Finalize workflow with the best parameters
final_svm <- finalize_workflow(rec_wf_svm, best_params_svm)

# Train the model with the optimal parameters
trained_svm <- final_svm %>%
  fit(data = income_train)

# Generate predictions on the test set
svm_preds <- predict(trained_svm, income_test) %>%
  bind_cols(income_test) %>%
  select(high.earner, .pred_class)

# Compute accuracy
svm_accuracy <- svm_preds %>%
  accuracy(truth = high.earner, estimate = .pred_class) %>%
     mutate(alg = "svm") %>%
     select(alg, .metric, .estimate)

print('*********** SUPPORT VECTOR MACHINE **************')
print(svm_accuracy)

#####################
# combine answers
#####################
all_ans <- bind_rows(logit_accuracy, tree_accuracy, nnet_accuracy, knn_accuracy, svm_accuracy)
all_ans

# Convert the tibble to LaTeX format
latex_table <- kable(all_ans, format = "latex", booktabs = TRUE)

# Print the LaTeX table
cat(latex_table)
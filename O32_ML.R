load.libraries <- c( 'ggplot2','tidymodels','recipes','dplyr','tidyverse','rsample','xgboost','keras')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
column_names = c('age', 'workclass', 'USERID', 'education', 'educational-num','marital-status', 'occupation', 'relationship', 'race', 
                'gender','capital-gain', 'capital-loss', 'hours-per-week', 'native-country','income')
df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),na.string = " ?",header=FALSE,col.names = column_names)
##just for testing
col_n<-dim(df)[2]

##show head
head(df,10)
# Check for missing values and return column names
miss_col<-colnames(df[,sapply(df, function(x) any(is.na(x)))])

sapply(df, function(x) sum(is.na(x)))
sapply(df, function(x) round(sum(is.na(x))/length(x),3))

#replace mising value with 0 for all miss_col
#df<-mutate(df, across(miss_col, ~ifelse(is.na(.x),0,.x)))

##check column with single value
col_single<-names(df)[sapply(df, function(x) length(unique(x))==1)]
##check column with totally unique value
col_unique<-names(df)[sapply(df, function(x) length(unique(x))==dim(df)[1])]
##check column got too many values
col_id<-names(df)[sapply(df, function(x) length(unique(x))>dim(df)[1]/10)]

df[col_id]<-NULL
df$educational.num<-NULL
##check missing value
str(df)
#tidyr::replace_na(df,0)
mlr::summarizeColumns(df)

##create column lists
num_col<-names(df)[unlist(lapply(df, is.numeric))]
cat_col<-setdiff(names(df), num_col)
##convert call categorical into factor
df[,cat_col]<-lapply(df[,cat_col] , factor)
cat_col<-cat_col[cat_col!= "income"]  
save(num_col,file="data/num_col.RData")
save(cat_col,file="data/cat_col.RData")


##create new variable
df<- df %>% 
  mutate(worktype = case_when( 
    hours.per.week < 31 ~ "parttime",
    hours.per.week >= 30 ~ "fulltime",
  )) %>% 
  mutate(worktype = as.factor(worktype)) 
#%>%  select(-hours.per.week)

###EDA##############
print_boxplot <- function(.y_var){
  # convert strings to variable
  y_var <- sym(.y_var) 
  # unquote variables using {{}}
  df %>% 
    ggplot(aes(x = income, y = {{y_var}},
               fill = income, color = income)) +
    geom_boxplot(alpha=0.4)}  

y_var <- 
  df %>% 
  select(where(is.numeric)) %>% 
  variable.names() 


map(y_var, print_boxplot)

#train/test split
set.seed(123)
df_split <- initial_split(df, prop = 0.8)
df_train <- training(df_split)
df_test <- testing(df_split)

save(df,file="data/data.RData")
save(df_train,file="data/train.RData")
save(df_test,file="data/test.RData")

##preprocessing pipeline

##data cleaning
df_recipe <- recipe(df_train) %>%
  update_role(everything(), new_role = "support") %>% 
  update_role(income, new_role = "outcome") %>%
  update_role(
    c(cat_col,num_col),
    new_role = "predictor"
  ) %>%
  step_impute_median(num_col)%>%
  step_unknown(cat_col,new_level = "Unknown") %>%
  step_other(native.country, threshold = 0.01)%>%
  step_string2factor(all_nominal(), -all_outcomes())%>%
  themis::step_smote(income)

##data cleaning + dummy +smote
df_recipe <- recipe(income~.,data=df_train) %>%
  step_impute_median(num_col)%>%
  step_unknown(cat_col,new_level = "Unknown") %>%
  step_other(native.country, threshold = 0.01)%>%
  step_string2factor(all_nominal(), -all_outcomes())%>%
  step_dummy(all_nominal(), -income) %>%
  themis::step_smote(income)

summary(df_recipe)

prepped_data <- 
  df_recipe%>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

##5-fold cross validation
set.seed(123)
cv_folds <- vfold_cv(df_train, v = 5,strata=income)


##Define classifiers https://www.tidymodels.org/find/parsnip/
##logistic regression
log_spec <- # your model specification
  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

##random forest 
rf_spec <- rand_forest(
  trees = tune(),
  mtry = tune(),
  min_n=10#tune()
) %>%
  set_engine("ranger") %>% 
  set_mode("classification")

##XGboost
xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

##knn
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification") 

##MLP
nnet_spec <-
  mlp() %>%
  set_mode("classification") %>% 
  set_engine("keras", verbose = 0) 


##define the model pipeline
model<-log_spec

clf_workflow <- workflow() %>% 
  add_recipe(df_recipe) %>% 
  add_model(model)

metrics_list <- yardstick::metric_set(roc_auc, yardstick::accuracy, sensitivity, specificity)

##model training and cross validation
get_model <- function(x) {
  extract_fit_parsnip(x) %>% tidy()
}

clf_res <-clf_workflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metrics_list,
    control = control_resamples(save_pred = TRUE,extract = get_model))

#clf_res$.extracts[[1]][[1]]
collect_metrics(clf_res,summarize = TRUE)

pred <- clf_res %>% collect_predictions()

pred %>% 
conf_mat(income,.pred_class)%>% 
  autoplot(type = "heatmap")

pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(income, `.pred_ <=50K`) %>% 
  autoplot()

pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(income, `.pred_ <=50K`) %>% 
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

##plot predicted probability
pred %>% 
  ggplot() +
  geom_density(aes(x = `.pred_ >50K`, 
                   fill = income), 
               alpha = 0.5)



last_fit_clf <- last_fit(clf_workflow, 
                         split = df_split,
                         metrics = metrics_list)

##result
collect_metrics(last_fit_clf)

members_final %>%
  pull(.workflow) %>%
  pluck(1) %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(estimate, fct_reorder(term, estimate))) +
  geom_vline(xintercept = 0, color = "gray50", lty = 2, size = 1.2) +
  geom_errorbar(aes(
    xmin = estimate - std.error,
    xmax = estimate + std.error
  ),
  width = .2, color = "gray50", alpha = 0.7
  ) +
  geom_point(size = 2, color = "#85144B") +
  labs(y = NULL, x = "Coefficent from logistic regression")
##hyperparameter tuning
#clf_grid <- expand_grid(mtry = 3:5, trees = seq(500, 1500, by = 200),min_n=seq(5,30,by=5))
clf_grid <- expand_grid(mtry = 3:4, trees = seq(200, 1000, by = 200))



clf_grid_results <- clf_workflow %>% 
  tune_grid(
    resamples = cv_folds,
    grid = clf_grid,
    metrics=metric_set(pr_auc)
  )


collect_metrics(clf_grid_results) %>%
  arrange(mean) %>%
  head() %>% 
  knitr::kable()

autoplot(clf_grid_results)

show_best(clf_grid_results) %>% knitr::kable()



##model fit
fitted_model <- clf_workflow %>% 
  finalize_workflow(select_by_pct_loss(clf_grid_results, metric = "pr_auc", limit = 5, trees)) %>% 
  fit(df_train)

pred<-clf_res  %>%
  collect_predictions()

metric_set(pr_auc)(df_test$income,.pred_class)

##########################BKP############################


##preprocessing
prep_rec<-recipe(~ ., data = train) %>% 
  #Transform numeric skewed predictors
  step_YeoJohnson(all_numeric()) %>%
  # Lump factor levels that occur in <= 10% of data as "other"
  step_other(native.country, threshold = 0.1,other =999)%>% 
  prep(data = train,retain = TRUE )

tidy(prep_rec)
df_train<-bake(prep_rec,train)
df_test<-bake(prep_rec,test)





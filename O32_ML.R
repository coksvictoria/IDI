load.libraries <- c( 'ggplot2', 'dplyr','tidyverse','caret','rsample')
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

##create column lists
num_col<-names(df)[unlist(lapply(df, is.numeric))]
cat_col<-setdiff(names(df), num_col)
cat_col<-cat_col[cat_col!= "income"]  
save(num_col,file="data/num_col.RData")
save(cat_col,file="data/cat_col.RData")

c(cat_col,num_col)

df[,cat_col]<-lapply(df[,cat_col] , factor)

#train/test split
set.seed(123)
df_split <- initial_split(df, prop = 0.8)
df_train <- training(df_split)
df_test <- testing(df_split)

##preprocessing pipeline
df_recipe <- recipe(df_train) %>%
  update_role(everything(), new_role = "support") %>% 
  update_role(income, new_role = "outcome") %>%
  update_role(
    c(cat_col,num_col),
    new_role = "predictor"
  ) %>%
  step_string2factor(all_nominal(), -all_outcomes()) %>%
  step_knnimpute(workclass,
                 impute_with = imp_vars(
                   num_col
                 )
  ) %>%
  step_knnimpute(occupation,
                 impute_with = imp_vars(
                   num_col
                 )
  ) %>%
  step_unknown(native.country,new_level = "unknown") %>%
  step_other(native.country, threshold = 0.01)

%>%
  step_normalize(all_numeric(), -all_outcomes())

#https://mdneuzerling.com/post/machine-learning-pipelines-with-tidymodels-and-targets/

df_recipe

df_recipe %>% 
  prep(df_train) %>%
  bake(df_test)







##create data dictionary
data_dict<-lapply(df,function(x) as.data.frame(prop.table(table(x))))
save(data_dict,file="data/data_dict.RData")

transform_data <- function(train_data) {
    train_data <- train_data %>%
      ##check if categorical data with levels
      dplyr::mutate_if(is.factor, ~ levels(.x)[.x]) %>%
      ##check if logical data eg: T or TRUE FALSE
      dplyr::mutate_if(is.logical, as.character) %>%
      ##chek if character and replace missing value with unkown
      dplyr::mutate_if(is.character, ~ ifelse(is.na(.x), "Unkown", .x)) %>%
      ##chek if character and replace missing value with mode
      #dplyr::mutate_if(is.character, ~ ifelse(is.na(.x),getmode(.x), .x)) %>%
      
      assertr::assert(assertr::not_na, dplyr::everything())
    
    col_classes <- lapply(train_data, class)
    
    ##check if variable type is not in following three and add it to bad_cols list
    bad_cols <- col_classes %>%
      purrr::discard(~ .x %in% c("numeric", "integer", "character"))
    
    ##if bad_cols are not empty then print variable names
    if (length(bad_cols)) {
      stop(glue::glue("The following columns have unsupported types:
                         {paste0(names(bad_cols), ' (', bad_cols, ')',
                      collapse = ',')}"),
           call. = FALSE
      )
    }
    
    ## A recipe is a description of the steps to be applied to a data set in order to prepare it for data analysis.
    if (any(unlist(col_classes) == "character")) {
      rec <- recipes::recipe(train_data, ~.) %>%
      recipes::step_integer(recipes::all_nominal(),zero_based = FALSE)
      
      trained_rec <- recipes::prep(rec, train_data, retain = FALSE)
      
      col_info <- trained_rec$var_info %>%
        dplyr::select(.data$variable, .data$type)
      
      categorical_levels <- trained_rec$orig_lvls %>%
        purrr::keep(~ length(.x$values) > 1) %>%
        purrr::map("values")
    
      
      train_data <- recipes::bake(trained_rec, train_data)
        } else {
      col_info <- tibble::tibble(variable = names(train_data),type = "numeric")
      categorical_levels <- NULL
        }
    
  list(train_data = train_data,
       categorical_encoded= lapply(trained_rec$steps[[1]]$key,function(x) as.data.frame(x)),
       col_classes=col_classes,
    metadata = list(col_info = col_info,categorical_levels = categorical_levels))
}


cleaned_df<-transform_data(df)

data_ori<-cleaned_df$train_data
data<-data_ori

column_number<-dim(data)[2]
##convert V16 to factor
data$income<-as.factor(data$income)
data[cat_col] <- lapply(data[cat_col] , factor)

mlr::summarizeColumns(data)

save(data,file="data/data.RData")

# create training set indices with 80% of data
set.seed(100)  # For reproducibility
#################################METHOD 2###################
train.index <- createDataPartition(data$income, p = .8, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]

print(dim(train)); print(dim(test))
save(train,file="data/train.RData")
save(test,file="data/test.RData")
############################################################

#impute missing values by mean and mode
imp <- impute(train, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")
imp_train <- imp$data

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
save(df_train,file="data/df_train.RData")
save(df_test,file="data/df_test.RData")

# NonGau<- preProcess(as.data.frame(train$age), method = "BoxCox")
# trainBC <- predict(NonGau, as.data.frame(train$age))
# testBC <-  predict(NonGau, as.data.frame(test$age))


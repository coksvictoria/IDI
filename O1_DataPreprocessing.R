load.libraries <- c('dplyr','tidyverse','caret','mlr','recipes')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

column_names = c('age', 'workclass', 'USERID', 'education', 'educational-num','marital-status', 'occupation', 'relationship', 'race', 
                'gender','capital-gain', 'capital-loss', 'hours-per-week', 'native-country','income')
df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),header=FALSE,col.names = column_names)
##just for testing
col_n<-dim(df)[2]

##show head
head(df)
# Check for missing values
map(df, ~sum(is.na(.)))

##check column with single value
col_single<-names(df)[sapply(df, function(x) length(unique(x))==1)]
##check column with totally unique value
col_unique<-names(df)[sapply(df, function(x) length(unique(x))==dim(df)[1])]
##check column got too many values
col_id<-names(df)[sapply(df, function(x) length(unique(x))>dim(df)[1]/10)]

df[col_id]<-NULL

col_highcor<-function(df){
  ## Convert data.frame to a matrix with a convenient structure
  ## (have a look at m to see where this is headed)
  l <- lapply(df, function(X) as.numeric(factor(X, levels=unique(X))))
  m <- as.matrix(data.frame(l))
  
  ## Identify pairs of perfectly correlated columns    
  M <- (cor(m,m)>0.9)
  M[lower.tri(M, diag=TRUE)] <- FALSE
  
  ## Extract the names of the redundant columns
  col_highcor<-colnames(M)[colSums(M)>0]
}

col_highcor<-col_highcor(df)
df[col_highcor]<-NULL

##check missing value
sum(is.na(df))
str(df)

##create column lists
num_col<-names(df)[unlist(lapply(df, is.numeric))]
cat_col<-setdiff(names(df), num_col)
cat_col<-cat_col[cat_col!= "income"]  
save(num_col,file="num_col.RData")
save(cat_col,file="cat_col.RData")

##create data dictionary
data_dict<-lapply(df,function(x) as.data.frame(prop.table(table(x))))
save(data_dict,file="data_dict.RData")

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
      recipes::step_integer(recipes::all_nominal(),zero_based = TRUE)
      
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

save(data,file="data.RData")

# create training set indices with 80% of data
set.seed(100)  # For reproducibility
#################################METHOD 2###################
train.index <- createDataPartition(data$income, p = .8, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]

print(dim(train)); print(dim(test))
save(train,file="train.RData")
save(test,file="test.RData")
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
save(df_train,file="df_train.RData")
save(df_test,file="df_test.RData")

# NonGau<- preProcess(as.data.frame(train$age), method = "BoxCox")
# trainBC <- predict(NonGau, as.data.frame(train$age))
# testBC <-  predict(NonGau, as.data.frame(test$age))

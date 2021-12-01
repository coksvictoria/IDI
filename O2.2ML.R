load.libraries <- c('data.table','sm','ggpubr','plyr','Hmisc','RVAideMemoire','car','MASS','ggthemes','DataExplorer','assertr',
                    'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'dplyr','tidyverse','caret','ggplot2','corrplot','caTools','torch',
                    'xgboost','mlr','parallel','parallelMap','FSelector')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"),header=FALSE)

##just for testing
dim(df) 
##show head
head(df)
# Check for missing values
map(df, ~sum(is.na(.)))

##convert V2 to numeric
df$V2 <- as.numeric(df$V2)

# Check for missing values
map(df, ~sum(is.na(.)))

#impute missing values by mean and mode
imp <- impute(df, classes = list(character = imputeMode(), numeric = imputeMean()))

df<-imp$data

##create data dictionar
dict<-lapply(df,function(x) table(x))

# `%||%` <- function(x, y) {
#   if (is.null(x)) y else x
# }

#' @importFrom dplyr .data
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
    metadata = list(col_info = col_info,
      categorical_levels = categorical_levels))
}

cleaned_df<-transform_data(df)

cleaned_df$metadata

cleaned_df$metadata

data<-cleaned_df$train_data
glimpse(data)

# Check data types
map(data, class)

##convert V16 to factor
data$V16<-as.factor(data$V16)

mlr::summarizeColumns(data)
dplyr::count(data,V1, sort = TRUE)

filter(data,V1==1 & V2>30)

filter(data,V11 %in% c(2,4,5))

# Correlation 
colNames <- names(df)[1:15]
for(i in colNames){
  plt<-ggplot(data,aes_string(i,'V16')) +
  geom_point(color= "blue", alpha = 0.3) +
  ggtitle("Plot dataset") +
  xlab(i) +
  ylab("V16") +
  theme(plot.title = element_text(color="darkred",
                                  size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))
  print(plt)
  Sys.sleep(2)
  }

correlations = cor(data)
corrplot(correlations, method="color")

# create training set indices with 80% of data
set.seed(100)  # For reproducibility

#################################METHOD 1###################
# Create index for testing and training data
inTrain <- createDataPartition(y = data$V16, 
                               p = 0.8, list = FALSE)
# subset power_plant data to training
training <- data[inTrain,]
# subset the rest to test
testing <- data[-inTrain,]

print(dim(training)); print(dim(testing))
#################################METHOD 2###################
library(caTools)

spl = sample.split(data$V16, SplitRatio = 0.8)
train = subset(data, spl==TRUE)
test = subset(data, spl==FALSE)

print(dim(train)); print(dim(test))

############################################################

#impute missing values by mean and mode
imp <- impute(train, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")
imp_train <- imp$data
##preprocessing
data_clean <- recipe(V16 ~ ., data = train ) %>%
  #Transform numeric skewed predictors
  step_YeoJohnson(all_numeric()) %>%
  # standardize the data 
  step_center(all_numeric(), -all_outcomes()) %>%
  #scale the data
  step_scale(all_numeric(), -all_outcomes()) %>%
  #step_kpca a specification of a recipe step that will convert numeric data into one or more principal components using a kernel basis expansion.
  #step_kpca(all_numeric(), num=6)%>%
  #step_log(Label, base = 10)
  # Lump factor levels that occur in <= 10% of data as "other"
  step_other(V14, threshold = 0.1) %>%
  # Create dummy variables for all nominal predictor factor variables except the response
  step_dummy(all_nominal(), -all_outcomes())%>%
  prep(data = train,retain = TRUE )


######LOGISTIC REGRESSION###
model_glm = glm(V16 ~ . , family="binomial", data = train)
##The significance code '***' in the above output shows the relative importance of the feature variables.
summary(model_glm)

#Baseline Accuracy =  Majority percentage
prop.table(table(train$V16))


# Predictions on the training set
predictTrain = predict(model_glm, data = train, type = "response")

# Confusion matrix on training data
table(train$V16, predictTrain >= 0.5)
480/nrow(train)##show acc

#Predictions on the test set
predictTest = predict(model_glm, newdata = test, type = "response")

# Confusion matrix on test set
table(test$V16, predictTest >= 0.5)
123/nrow(test) #Accuracy - 89%


#HYPERPARAMETER TUNING
library('mlr')

install.packages("xgboost")
library(xgboost)
library(parallelMap)

##Xtreme GB trees using the "mlr" package - allows for grid search in xgb
#Define the learner
lrn <- makeLearner(cl = "classif.xgboost", predict.type = "prob")
#Define grid search parameters
params <- makeParamSet(
  makeDiscreteParam("booster",values = c("gbtree","gblinear")),
  makeIntegerParam("max_depth",lower = 3L,upper = 10L),
  makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
  makeNumericParam("subsample",lower = 0.5,upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
  makeNumericParam("eta", lower = 0.01, upper = 0.1),
  makeDiscreteParam("nrounds", values = seq(100L, 1000L, by = 50L))
)

train=as.data.frame(train)
test=as.data.frame(test)

#Create train and test tasks to fit and evaluate model
traintask <- makeClassifTask(data = train, target ="V16")
testtask <- makeClassifTask(data = test,target = "V16")

cv.xgboost <- crossval(learner = lrn,task = traintask,iters = 3,stratify = TRUE,measures = auc,show.info = F,nrounds=10)
cv.xgboost$aggr
cv.xgboost$measures.test

#Set type of resampling
rdesc <- makeResampleDesc("CV", iters = 10L)
ctrl <- makeTuneControlRandom(maxit = 50L)

#Tune the model based on the hyperparameters
set.seed(42)
#Enable parallel processing, after automatic detection of CPU cores
parallelStop()
parallelStartSocket(cpus = detectCores())
lrn$par.vals <- list(
  objective= "binary:logistic",
  eval_metric= "aucpr")

mytune <- tuneParams(learner = lrn,
                     task = traintask,
                     resampling = rdesc,
                     measures = auc,
                     par.set = params,
                     control = ctrl,
                     show.info = TRUE)

#Set parameters as defined by grid search in previous step
lrn_tune <- setHyperPars(lrn,
                         par.vals = mytune$x,
                         print_every_n = 100)
#Fit the X-gradient boosted model
set.seed(42)
xgmodel <- mlr::train(learner = lrn_tune,task = traintask)
xgpred <- predict(xgmodel,testtask)
xg_predData <- xgpred$data


##performance metrics
r = calculateROCMeasures(xgpred)

performance(xgpred, measures = list(fpr, fnr, mmce))

#Plot ROC curves
#Since we want to plot ROC curves we calculate the false and true positive rates (fpr and tpr)
#Additionally, we also compute error rates (mmce).
df = generateThreshVsPerfData(xgpred, measures = list(fpr, tpr, mmce))
plotROCCurves(df)
mlr::performance(xgpred, mlr::auc)
plotThreshVsPerf(df)

mlr::performance(xgpred, mlr::prauc)


##feature importance
im_feat <- generateFilterValuesData(traintask, method = c("FSelector_information.gain"))
plotFilterValues(im_feat,n.show = 20)

# install.lib <- load.libraries[!load.libraries %in% installed.packages()]
# for(libs in install.lib) install.packages(libs, dependences = TRUE)
load.libraries <- c('dplyr','tidyverse','caret','mlr','recipes')
sapply(load.libraries, require, character = TRUE)

column_names = c('age', 'workclass', 'USERID', 'education', 'educational-num','marital-status', 'occupation', 'relationship', 'race', 
                'gender','capital-gain', 'capital-loss', 'hours-per-week', 'native-country','income')
df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),header=FALSE,col.names = column_names)
##just for testing
col_n<-dim(df)[2]

#https://r4ds.had.co.nz/transform.html

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

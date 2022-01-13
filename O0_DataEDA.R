# install.lib <- load.libraries[!load.libraries %in% installed.packages()]
# for(libs in install.lib) install.packages(libs, dependences = TRUE)
load.libraries <- c('dplyr','tidyverse','caret','mlr','recipes')
sapply(load.libraries, require, character = TRUE)

column_names = c('age', 'workclass', 'USERID', 'education', 'educational-num','marital-status', 'occupation', 'relationship', 'race', 
                'gender','capital-gain', 'capital-loss', 'hours-per-week', 'native-country','income')
df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),header=FALSE,col.names = column_names)


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


##dplyr for data manipulation
#https://www.listendata.com/2016/08/dplyr-tutorial.html

sample_n(df,2)
sample_frac(df,0.001)

df2<-distinct(df,.keep_all= TRUE)


##get number of columns
col_n<-dim(df)[2]

X<-select(df2,-col_n)
y<-select(df2,col_n)

##The grepl function is used to search for pattern matching. 
filter(df2,race==" White" & gender==" Male" & age !=39 & grepl("gov",workclass))

##Summarize Multiple Variables
summarise_at(df2, vars(age, hours.per.week), list(n=~n(), mean=mean, median=median))

##custom function
summarise_at(df2, vars(age, hours.per.week), function(x) var(x-mean(x)))

##stats for all numeric data
summarise_if(df2, is.numeric, list(n=~n(),mean=mean,median=median))

##stats for all categorical data
summarise_if(df2$race, funs(nlevels(.), nmiss=sum(is.na(.))))

##sort datatable
arrange(df2,desc(age))


##Pipe line %>%
df2%>%
  group_by(education,marital.status)%>%
  summarise_at(vars(hours.per.week), funs(n(), mean(., na.rm = TRUE),median(.,na.rm=TRUE)))
#summarise_at(vars(hours.per.week),list(n=~n(),mean=mean,median=median))

df2%>%
  filter(relationship %in% c(" Husband"," Unmarried"))%>%
  group_by(education)%>%
do(head( . , 2))

#Summarize, Group and Sort Together

df2%>%
  filter(relationship %in% c(" Husband"," Unmarried"))%>%
  group_by(education)%>%
  summarise(mean_age=mean(age,na.rm=TRUE))%>%
  arrange(desc(mean_age))


##Combine Data Vertically
intersect(x, y)
#Rows that appear in both x and y.
union(x, y) # union_all () allows duplicate rows
#Rows that appear in either or both x and y.
setdiff(x, y)
#Rows that appear in x but not y.

##create new variable using mutate xxx
##divide the data into N bins.
mutate(df2, pos = ntile(df2$age,8))

group_by(df2,gender)

##Number of levels in factor variable
summarise_if(df, is.factor, funs(nlevels(.)))

##convert " ?" to NA
na_if(df2," ?")


##How to deal with Quotation (turn string to var_name)
filter_df <- function(df, colname, val){
  colname = enquo(colname)
  filter(df, !!colname == val)
}
filter_df(df2, gender, " Male")

df2%>%
na_if(" ?")%>%##remove NA rows
filter(!is.na(workclass)) 

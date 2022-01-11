load.libraries <- c('dplyr','tidyverse','reshape2','recipes','smotefamily','themis','synthpop')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


load("data/data_dict.RData")
load("data/data.RData")
load("data/df_train.RData")
load("data/df_test.RData")
load("data/num_col.RData")
load("data/cat_col.RData")

data[num_col]
data[cat_col]

column_names<-c(num_col,cat_col,"income")

n_samples<-dim(data)[1]

data_distribution<-list()

df_distribution<-function(df,c_col){
for (i in column_names){
  if (i %in% num_col) {
    temp<-melt(list(mean(df[[i]]),sd(df[[i]]),quantile(df[[i]], c(0, .25, 0.5,.75,1))))
    data_distribution[[i]]<-temp[["value"]]
  }
  else
  data_distribution[[i]]<-as.data.frame(dplyr::count(df,df[[i]], sort = TRUE))
  }
  return(data_distribution)
  }

df_dis<-df_distribution(data,c_col)

#######METHOD 1 Perturbation###############

##Categorical columns#
cat_generator<-function(data,i){
  temp<-sample(as.vector(sort(unique(data[[i]]))),n_samples, replace=TRUE, prob=data_dict[[i]]$Freq)
  return(temp)}

cat_output <- matrix(ncol=length(cat_col), nrow=dim(data)[1])

for(i in 1:length(cat_col)){
  cat_output[,i]<- cat_generator(data,cat_col[i])
}
cat_output<-as.data.frame(cat_output)
colnames(cat_output)<-cat_col

##continuous columns#
num_generator<-function(count,mean,std,min,p25,p50,p75,max){
  #Positions of the percentiles
  P25_pos = floor(0.25 * count)
  # print(P25_pos)
  P50_pos = floor(0.5 * count)
  # print(P50_pos)
  P75_pos = floor(0.75 * count)
  # print(P75_pos)
  MAX_pos = count
  # print(MAX_pos)
  v = matrix(ncol=1, nrow=dim(data)[1])
  #Min requirement
  v[1] = min
  #Max requirement
  v[MAX_pos] = max
  
  #This will satisfy the 25th percentile requirement
  for (i in 1:P25_pos){
    #We could also interpolate the value from P25 to P50, even adding a bit of randomness.
    v[i] = runif(1,min,p25)}
  v[P25_pos] = p25
  #Actually pandas does some linear interpolation (https://stackoverflow.com/questions/39581893/pandas-find-percentile-stats-of-a-given-column)
  #when calculating percentiles but we can simulate that by letting the next value be also P25
  if (P25_pos + 1 != P50_pos){
    v[P25_pos + 1] = p25}
  #We do something extremely similar with the other percentiles
  for(i in P25_pos:P50_pos){
    v[i] =runif(1,p25,p50)}
  v[P50_pos] = p50
  if(P50_pos + 1 != P75_pos){
    v[P50_pos + 1] = p50}
  
  for (i in P50_pos:P75_pos){
    v[i] = runif(1,p50,p75)}
  v[P75_pos] = p75
  if (P75_pos + 1 != max){
    v[P75_pos + 1] = p75}
  # print(length(v))
  for(i in P75_pos :MAX_pos){
    v[i] = runif(1,p75,max)}
  
  return(v)
}

num_output <- matrix(ncol=length(num_col), nrow=dim(data)[1])

for(i in 1:length(num_col)){
  t<-dist[[num_col[i]]]
  num_output[,i]<- round(num_generator(n_samples,t[1],t[2],t[3],t[4],t[5],t[6],t[7]))
}
num_output<-as.data.frame(num_output)
colnames(num_output)<-num_col

##Target column - y###
income<-cat_generator(data,"income")

#bind all columns together##
perturbation_df<-cbind(cat_output,num_output,income)

perturbation_df_num <- as.data.frame(sapply(perturbation_df, as.numeric))
save(perturbation_df_num,file="data/syn1_num.RData")

##Convert it back to original format
for(i in cat_col){
  print(i)
  look<-data_dict[[i]][1]
  perturbation_df[[i]]<-look$x[match(perturbation_df[[i]],seq_along(look$x))]
}
perturbation_df$income<-as.factor(perturbation_df$income)
save(perturbation_df,file="data/syn1.RData")


##METHOD 2 - SMOTE#####

data_ori <- as.data.frame(sapply(data, as.numeric))
sort(table(data_ori$income, useNA = "always"))

df_real<-data_ori
df_fake<-data_ori
df_real['y']<-1
df_fake['y']<-0
df_rff=rbind(df_real,df_fake,df_fake)
df_rff$y <- as.factor(df_rff$y)

synthetic_df<-recipe(y~., data = df_rff) %>%
  step_smote(y, over_ratio = 1) %>%
  prep()%>%
  bake(new_data = NULL)

synthetic_df$y<-NULL


synthetic_df<-as.data.frame(sapply(tail(synthetic_df,n_samples),as.integer))

##Convert it back to original format
for(i in cat_col){
  print(i)
  look<-data_dict[[i]][1]
  synthetic_df[[i]]<-look$x[match(synthetic_df[[i]],seq_along(look$x))]
}
synthetic_df$income<-as.factor(synthetic_df$income)

save(synthetic_df,file="data/syn2.RData")


##METHOD 3 Synthpop##
# apply rules to ensure consistency
rules.list <- list(
  marital.status = "age < 18")

rules.value.list <- list(
  marital.status = 5)

# synthesise data
synth.obj <- syn(data, rules = rules.list, rvalues = rules.value.list, seed = 1)

syn_df<-synth.obj$syn
save(syn_df,file="data/syn3.RData")

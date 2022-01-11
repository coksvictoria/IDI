load.libraries <- c('dplyr','tidyverse','DMwR','reshape2','compareDF')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

setwd('//corp.ssi.govt.nz/usersx/xwang004/Documents/Github/IDI')

load("perturbation_df.RData")
load("data.RData")
load("cat_col.RData")

column_names=colnames(data)

##looping through all columns and plot one by one
lapply(data[,column_names], function(x) ggplot(data = data, aes(y = data$income, x = x))+ geom_col())


#https://stackoverflow.com/questions/52436487/show-multiple-plots-from-ggplot-on-one-page-in-r
# create a list with a specific length 
plot_lst <- vector("list", length = 12)

for (i in 1:12) {
  print(i)
  real <- data.frame(num = data[,i])
  fake <- data.frame(num = perturbation_df[,i])
  
  real$s <- 'real'
  fake$s <- 'fake'
  
  plot_data <- rbind(real, fake)
  g<-ggplot(plot_data, aes_string(x =column_names[i], fill = 's')) + geom_density(alpha = 0.2)
  plot_lst[[i]] <- g
}

for (i in 1:12) {
  print(i)
  real <- data.frame(num = data[,i])
  fake <- data.frame(num = perturbation_df[,i])
  
  real$s <- 'real'
  fake$s <- 'fake'
  
  plot_data <- rbind(real, fake)
  g<-ggplot(plot_data,aes_string(x =column_names[i], fill = 's')) +
    geom_bar(alpha=0.2, position="identity") +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    ggtitle(column_names[i]) +
    theme(plot.title = element_text(size=5))
  plot_lst[[i]] <- g
}

    
##method 1 plot all in one page
cowplot::plot_grid(plotlist = plot_lst, nrow = 4)

##method 2 plot all in multiple pages
library(gridExtra)
ml1 <- marrangeGrob(plot_lst, nrow = 2, ncol = 2)
ml1


summary(data)

summary(perturbation_df)

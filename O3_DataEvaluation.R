       
load.libraries <- c('dplyr','tidyverse','reshape2','recipes','smotefamily','themis','dagitty','patchwork','synthpop')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


load("data/data_dict.RData")
load("data/data.RData")
load("data/num_col.RData")
load("data/cat_col.RData")
load("data/syn1.RData")
load("data/syn1_num.RData")
load("data/syn2.RData")
load("data/syn3.RData")


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
       
       
       

mycols <- c("darkmagenta", "turquoise")
# compare the synthetic and original data frames
compare(perturbation_df_num,as.data.frame(sapply(data, as.numeric)), nrow = 3, ncol = 4, cols = mycols)

summary(perturbation_df_num)
summary(data,10)


sort(table(data$race, useNA = "always"))
sort(table(synth.obj$syn$race, useNA = "always"))







#perturbation_df[cat_col] <- lapply(perturbation_df[cat_col] , factor)

summary(synthetic_df)
summary(data)


p = list()
for (i in 1:ncol(data)) p[[i]] <- qplot(data[,i], xlab=names(data)[[i]])
do.call(grid.arrange, p)

for (i in 1:ncol(data)){
hgA <- hist(data[[i]]) # Save first histogram data
hgB <- hist(perturbation_df[[i]]) # Save 2nd histogram data

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

plot(hgA, col = c1) # Plot 1st histogram using a transparent color
plot(hgB, col = c2, add = TRUE)} # Add 2nd histogram using different color
  

plot(data)

plot1 <- ggplot(data, aes(x = age)) +
  geom_histogram(boundary = 1)

plot2 <- ggplot(data, aes(x = age)) +
  geom_density()

plot1+plot2

plot_r<-ggplot(data, aes(x = age, fill = income)) +
  geom_histogram(binwidth = 5, color = "white", boundary = 0) +
  facet_wrap(vars(income), ncol = 1)

plot_f<-ggplot(synthetic_df, aes(x = age, fill = income)) +
  geom_histogram(binwidth = 5, color = "white", boundary = 0) +
  facet_wrap(vars(income), ncol = 1)

plot_r+plot_f


library(corrplot)
library(RColorBrewer)
M <-cor(data)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

M <-cor(synthetic_df[num_col])
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


UncertCoef(synthetic_df$income,synthetic_df$gender,direction = "column")
UncertCoef(synthetic_df$gender,synthetic_df$income,direction = "column")
UncertCoef(data$gender,data$income,direction = "column")


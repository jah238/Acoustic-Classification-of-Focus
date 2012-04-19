




#Generating dataframes with different imputations for NAs

###### MEAN IMPUTE################
#load library for function impute()
library(e1071)

# Read in output from Praat script (--undefined-- has already been replaced by NA in a text editor)
w1 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/results_with_NAs/results_all.txt",header=TRUE)
# Remove any undesired parameters or observations
w1 <- w1[-9,-c(90:97,128:135)]
w1$duration_ratio <- w1$duration_V3/w1$duration_V2
write.table(w1,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/results_with_NAs/thanIdid1_results_withNAs.dataframe")
w1 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/results_with_NAs/thanIdid1_results_withNAs.dataframe")

w2 <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_results_withNAs.txt",header=TRUE)
w2 = w2[,-c(90:97,128:135)]
w2$duration_ratio <- w2$duration_V3/w2$duration_V2
write.table(w2,"/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_results_withNAs.dataframe")
w2 <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_results_withNAs.dataframe")

thanIdidlab <- read.table("/Users/Jonathan/Documents/Current/speech/Lab/17_thanidid/6_2_jah_truncated/04_results/results_withNAs.txt",header=TRUE)
thanIdidlab <- thanIdidlab[,-c(90:97,128:135)]
thanIdidlab$duration_ratio <- thanIdidlab$duration_V3/thanIdidlab$duration_V2
categories <- read.table("/Users/Jonathan/Documents/Current/speech/Lab/17_thanidid/6_2_jah_truncated/05_results_withzeros/categories_thanIdid_lab.txt", header=T)
categories <- categories[,2:5]
thanIdidlab <- as.data.frame(cbind(thanIdidlab,categories))
#fof1 <- as.data.frame(subset(thanIdidlab,thanIdidlab$occurrence=="FOF"))
fof <- as.data.frame(subset(thanIdidlab[,-c(311,313:315)],thanIdidlab$occurrence== "FOF"))



## Calculate mean impute  (need to exclude non numeric columns)
w1_mean <- impute(w1[,2:310], what = "mean")
w2_mean <- impute(w2[,2:310], what = "mean")
fof_mean <- impute(fof[,2:310], what = "mean")


w1_mean <- as.data.frame(w1_mean)
w1_mean <- cbind(w1[,1],w1_mean)
colnames(w1_mean)[1] <- colnames(w1)[1]
category <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/categories_s_vs_nons.txt",header=TRUE,fill=TRUE)
focus <- category[-9,3]
w1_mean <- cbind(w1_mean,focus)
#w1_mean$focus <- ifelse(w1_mean$focus==1, "ns", "s")
w1_mean$focus <- as.factor(w1_mean$focus)

write.table(w1_mean,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_mean_impute.dataframe")
w1_mean <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_mean_impute.dataframe")


w2_mean <- as.data.frame(w2_mean)
w2_mean <- cbind(w2[,1],w2_mean)
colnames(w2_mean)[1] <- colnames(w2)[1]
category=read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/categories_thanIdid2.txt",header=TRUE,fill=TRUE)
focus<-category[,2]
w2_mean <- cbind(w2_mean,focus)
#w2_mean$focus <- ifelse(w2_mean$focus==1, "ns", "s")
w2_mean$focus <- as.factor(w2_mean$focus)


write.table(w2_mean,"/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_mean_impute.dataframe")
w2_mean <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_mean_impute.dataframe")


fof_mean <- as.data.frame(fof_mean)
fof_mean <- cbind(fof[,1],fof_mean,fof[,311])
colnames(fof_mean)[c(1,311)] <- colnames(fof)[c(1,311)]
write.table(fof_mean,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_mean_impute.dataframe")
fof_mean <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_mean_impute.dataframe",header=TRUE)







library(imputation)
# knn impute (full dataset)
w1_kNN <- kNNImpute(w1[,2:309],2,verbose=F)
objects(w1_kNN)
w1_kNN <- w1_kNN$x

# knn impute (divided dataset)
all_f0 <- c(15:32,215:220)
all_nonf0 <- c(2:14,33:214,221:309)
w1_kNN_f0 <- kNNImpute(w1[,c(all_f0)],2,verbose=T)
w1_kNN_nonf0 <- kNNImpute(w1[,c(all_nonf0)],2,verbose=T)
category=read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/categories_s_vs_nons.txt",header=TRUE,fill=TRUE)
focus=category[,3]
w1_kNN_f0 <- w1_kNN_f0$x
w1_kNN_nonf0 <- w1_kNN_nonf0$x
w1_kNN <- cbind(w1[,1],w1_kNN_f0,w1_kNN_nonf0,focus)
write.table(w1_kNN,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_kNN2divided_impute.dataframe")

w1_kNN <- cbind(w1[,1],w1_kNN)
colnames(w1_kNN)[1] <- colnames(w1)[1]
category=read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/categories_s_vs_nons.txt",header=TRUE,fill=TRUE)
focus=category[,3]
w1_kNN <- cbind(w1_kNN,focus)
w1_kNN <- as.data.frame(w1_kNN)
write.table(w1_kNN,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_kNN2_impute.dataframe")
w1_kNN <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_kNN2_impute.dataframe")






w2_kNN <- kNNImpute(w2[,2:309],2,verbose=F)
objects(w2_kNN)
w2_kNN <- w2_kNN$x

w2_kNN <- cbind(w2[,1],w2_kNN)
colnames(w2_kNN)[1] <- colnames(w2)[1]
category=read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/categories_thanIdid2.txt",header=TRUE,fill=TRUE)
focus=category[,2]
w2_kNN_f0 <- w2_kNN_f0$x
w2_kNN_nonf0 <- w2_kNN_nonf0$x
w2_kNN <- as.data.frame(w2_kNN)
write.table(w2_kNN,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid2_kNN2_impute.dataframe")
w2_kNN <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid2_kNN2_impute.dataframe")

# knn impute (divided dataset)
all_f0 <- c(15:32,215:220)
all_nonf0 <- c(2:14,33:214,221:309)
w2_kNN_f0 <- kNNImpute(w2[,c(all_f0)],2,verbose=T)
w2_kNN_nonf0 <- kNNImpute(w2[,c(all_nonf0)],2,verbose=T)
category=read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/categories_thanIdid2.txt",header=TRUE,fill=TRUE)
focus=category[,2]
w2_kNN_f0 <- w2_kNN_f0$x
w2_kNN_nonf0 <- w2_kNN_nonf0$x
w2_kNN <- cbind(w2[,1],w2_kNN_f0,w2_kNN_nonf0,focus)
write.table(w2_kNN,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid2_kNN2divided_impute.dataframe")




thanIdidlab <- read.table("/Users/Jonathan/Documents/Current/speech/Lab/17_thanidid/6_2_jah_truncated/04_results/results_withNAs.txt",header=TRUE)
thanIdidlab <- thanIdidlab[,-c(90:97,128:135)]
categories <- read.table("/Users/Jonathan/Documents/Current/speech/Lab/17_thanidid/6_2_jah_truncated/05_results_withzeros/categories_thanIdid_lab.txt", header=T)
categories <- categories[,2:5]
thanIdidlab <- cbind(thanIdidlab,categories)
fof <- thanIdidlab[thanIdidlab$occurrence== "FOF",-c(311:313)]

fof_kNN <- kNNImpute(fof[,2:309],2,verbose=F)
fof_kNN <- fof_kNN$x
fof_kNN <- cbind(fof[,1],fof_kNN,fof[,310])
fof_kNN <- as.data.frame(fof_kNN)
colnames(fof_kNN)[c(1,310)] <- colnames(fof)[c(1,310)]
write.table(fof_kNN,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN_impute.dataframe")
fof_kNN <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN_impute.dataframe",header=T)


fof_kNN5 <- kNNImpute(fof[,2:309],5,verbose=F)
fof_kNN5 <- fof_kNN5$x
fof_kNN5 <- cbind(fof[,1],fof_kNN5,fof[,310])
fof_kNN5 <- as.data.frame(fof_kNN5)
colnames(fof_kNN5)[c(1,310)] <- colnames(fof)[c(1,310)]
write.table(fof_kNN5,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN5_impute.dataframe")
fof_kNN5 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN5_impute.dataframe",header=T)


fof_kNN25 <- kNNImpute(fof[,2:309],25,verbose=F)
fof_kNN25 <- fof_kNN25$x
fof_kNN25 <- cbind(fof[,1],fof_kNN25,fof[,310])
fof_kNN25 <- as.data.frame(fof_kNN25)
colnames(fof_kNN25)[c(1,310)] <- colnames(fof)[c(1,310)]
write.table(fof_kNN25,"/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN25_impute.dataframe")
fof_kNN5 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN25_impute.dataframe",header=T)




library(mi)
w1 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/results_with_NAs/thanIdid1_results_withNAs.dataframe")
w2 <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_results_withNAs.dataframe")

mi.w1 <- mi(w1[,2:309])


# More playing around


library(VIM)

#Get counts of missings
a <- aggr(w1)

barMiss(w1[,2:309])
histMiss(w1[,3:309])
matrixplot(w1[,2:309],sortby="f0_ratio")

w1 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/results_with_NAs/thanIdid1_results_withNAs.dataframe")
w2 <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_results_withNAs.dataframe")

hotdeck.w1<- hotdeck(w1,2:309)
hotdeck.w2<- hotdeck(w2,2:309)

category=read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/categories_s_vs_nons.txt",header=TRUE,fill=TRUE)
focus=category[,3]
hotdeck.w1<- cbind(hotdeck.w1[,1:309],focus)
category=read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/categories_thanIdid2.txt",header=TRUE,fill=TRUE)
focus=category[,2]
hotdeck.w2<- cbind(hotdeck.w2[,1:309],focus)






library(Amelia)
amelia.imputed.w1 <- amelia(w1[,])
Amelia Error Code:  34 
The number of observations is too low to estimate the number of 
parameters.  You can either remove some variables, reduce 
the order of the time polynomial, or increase the empirical prior. 


# Nov 7, 2011  Jonathan Howell


setwd("/Users/Jonathan/Documents/Current/Comparatives/")
setwd("/Users/Jonathan/Documents/Current/Comparatives/images_pval/LabFOFtoLabSOF_lda_5000")


# Read in training set web1 (aka thanIdid1)
thanIdid1 <- read.table("/your/path/thanIdid1_with_zeros.dataframe",header=TRUE)
#thanIdid1 <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/results_with_zeros/august2010/thanIdid1_with_zeros.dataframe",header=TRUE)
w1_mean <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_mean_impute.dataframe")
w2_mean <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_mean_impute.dataframe")
w1_kNN <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_kNN2_impute.dataframe")
w2_kNN <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid2_kNN2_impute.dataframe")
fof_kNN <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN_impute.dataframe",header=T)
fof_kNN5 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN5_impute.dataframe",header=T)
fof_kNN25 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN25_impute.dataframe",header=T)
fof_kNN50 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/FOF_kNN50_impute.dataframe",header=T)



# Remove file 102 (index 9) which is a duplicate of file 88
thanIdid1c <- thanIdid1[-9,]

# Read in test set web2 (aka thanIdid2)
thanIdid2 <- read.table("/your/path/thanIdid2_with_zeros.dataframe",header=TRUE)
#thanIdid2 <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/results_with_zeros/august2010/thanIdid2_with_zeros.dataframe",header=TRUE)


thanIdidlab <- read.table("/Users/Jonathan/Documents/Current/speech/Lab/17_thanidid/6_2_jah_truncated/05_results_withzeros/thanIdidlab_with_zeros2.dataframe", header =T)
FOF <- thanIdidlab[thanIdidlab$occurrence== "FOF",-c(311:313)]
SOF <- thanIdidlab[thanIdidlab$occurrence== "SOF",-c(311:313)]

decl <- thanIdidlab[thanIdidlab$occurrence== "FOF"& thanIdidlab$condition!="q",-c(311:313)]
q <- thanIdidlab[thanIdidlab$occurrence== "FOF"& thanIdidlab$condition=="q",-c(311:313)]


##
## Load packages, including e1071 which has the svm() function
## and MASS which has the lda() function
library(e1071)
library(MASS)
#library(foreign) #not sure if you need this


################  FUNCTION get.values.svm() #############
#arguments should have 310 columns
get.values.svm <- function(name,train,test,features,kernel){
  
  model <- svm(focus ~ ., data = train[,c(features,310)],kernel=kernel)
  pred <- predict(model,test[,c(-1,-310)])
  table <- table(pred,test$focus)
  accuracy <- round((dim(test)[1]-(table[2]+table[3]))/dim(test)[1]*100,1)
  BER <- round(0.5*((table[3]/(table[1]+table[3]))+(table[2]/(table[2]+table[4])))*100,1)
  
  #	result = cat("SVM-RBF","\n",accuracy.rbf," (",BER.rbf,")","\n","SVM-lin","\n",accuracy.lin," (",BER.lin,")","\n","LDA","\n",accuracy.lda," (",BER.lda,")","\n",sep="")
  #	print(result);print(model.lm.summary)
  
  stats <- data.frame(name,accuracy,BER)
  colnames(stats) <- c("name","stat_accuracy", "stat_BER")
  stats$name <- as.character(stats$name)
  stats
  
}


###############################################

################  FUNCTION get.values.lda() #############

get.values.lda <- function(name,train,test,features){
  
  model <- lda(train[,features], train[,310])
  pred <- predict(model, test[,features])
  table <- table(pred$class, test$focus)
  accuracy <- round((dim(test)[1]-(table[2]+table[3]))/dim(test)[1]*100,1)
  
  BER <- round(0.5*((table[3]/(table[1]+table[3]))+(table[2]/(table[2]+table[4])))*100,1)
  stats <- data.frame(name,accuracy,BER)
  colnames(stats) <- c("name","stat_accuracy", "stat_BER")
  stats$name <- as.character(stats$name)
  stats
  
}

###############################################



#############  FUNCTION permute.svm()  #################
permute.svm <- function(train,test,features,samples){
  
  ## Create dummy matrices for achieved accuracy and achieved balanced error
  ACA <- matrix()
  ACE <- matrix()
  
  ## Specify training set, test set and features
  #train <- thanIdid1c
  #features <- c(2:309)
  #test <- thanIdid2
  
  
  ## For-loop with permutations
  ## WARNING: large loops will take a long time and your machine may become unresponsive for a period of time
  
  for (i in 1:samples){
    
    # create a classification model (svm) using permuted classes
    model <- svm(sample(focus) ~ ., data = train[,c(features,310)])
    # predict classes of test set using classification model
    pred <- predict(model,test[,c(-1,-310)])
    # create a confusion table
    table <- table(pred,test$focus)
    # calculate accuracy and balanced error rate from the confusion table
    accuracy <- round((dim(test)[1]-(table[2]+table[3]))/dim(test)[1]*100,1)
    BER <- round(0.5*((table[3]/(table[1]+table[3]))+(table[2]/(table[2]+table[4])))*100,1)
    # write the accuracy and balanced error to a variable
    ACA[i] <- accuracy
    ACE[i] <- BER
  }
  
  permStats <- cbind(ACA/100,ACE/100)
  colnames(permStats) <- c("PermAccuracy","PermError")
  permStats
}


#################  end function permute.svm()  #######################



#############  FUNCTION permute.lda()  #################
permute.lda <- function(train,test,features,samples){
  
  ## Create dummy matrices for achieved accuracy and achieved balanced error
  ACA <- matrix()
  ACE <- matrix()
  
  ## Specify training set, test set and features
  #train <- thanIdid1c
  #features <- c(2:309)
  #test <- thanIdid2
  
  
  ## For-loop with permutations
  ## WARNING: large loops will take a long time and your machine may become unresponsive for a period of time
  
  for (i in 1:samples){
    
    # create a classification model (svm) using permuted classes
    model <- lda(train[,features], sample(train[,310]))
    # predict classes of test set using classification model
    pred <- predict(model,test[,features])
    # create a confusion table
    table <- table(pred$class,test$focus)
    # calculate accuracy and balanced error rate from the confusion table
    accuracy <- round((dim(test)[1]-(table[2]+table[3]))/dim(test)[1]*100,1)
    BER <- round(0.5*((table[3]/(table[1]+table[3]))+(table[2]/(table[2]+table[4])))*100,1)
    # write the accuracy and balanced error to a variable
    ACA[i] <- accuracy
    ACE[i] <- BER
  }
  
  permStats <- cbind(ACA/100,ACE/100)
  colnames(permStats) <- c("PermAccuracy","PermError")
  permStats
}


#################  end function permute.lda()  #######################




# Plot ecdf for BER
# If writing to a pdf file, uncomment pdf() and dev.off	

#pdf("/Users/Jonathan/Documents/Current/Comparatives/ecdf_test.pdf")

#plot ecdf and draw lines connecting the points
#plot.ecdf(permStats[,1],xlim=c(0,1),xlab="Accuracy",main="ECDF for Classifier A",sub="kernel k, n permutations, features f")
#sortACE <- sort(permStats[,1])
#ecdfACE <- (1:length(sortACE))/length(sortACE)
#lines(sortACE,ecdfACE)

# add a red line corresponding to the nonpermuted test statistic (BER)
# (I added the value manually)
#abline(v=0.155,col=2)

# add a blue line corresponding to a test statistic (BER) at p=0.5
#q <- quantile(ACE, probs=.05)/100
#abline(v=q,col=4)

#legend("bottomright",lty=1,c("BER","BER at p=0.05"),col=c(2,4))

#dev.off()
# end plot


#ecdf1 <- ecdf(ACE/100)


#min(which(ACE/100>=0.28))
#goo <- ACE/100
#which(goo>0.48)


########### PValue function #################################
PValue.BER<-function(stat, statPerm){
  ###Get the length of the ECFD
  L<-length(statPerm)
  i<-min(which(sort(statPerm)>=stat))
  ###Return the 3 values that bracket the p value
  #c((i-1)/L, i/L, (i+1)/L)
  i/L
}

PValue.acc<-function(stat, statPerm){
  ###Get the length of the ECFD
  L<-length(statPerm)
  i<-min(which(sort(statPerm,decreasing=T)<=stat))
  ###Return the 3 values that bracket the p value
  #c((i-1)/L, i/L, (i+1)/L)
  i/L
}


############ end PValue function #############################


############### start confidence interval function ###########


conf.acc <- function(statPerm){
  
  acc.95 <-	quantile(statPerm,0.95,na.rm=T)*100
  acc.99 <- quantile(statPerm,0.99,na.rm=T)*100
  
  conf.acc <- cbind(acc.95,acc.99)
  colnames(conf.acc) <- c("acc.95","acc.99")
  conf.acc
}

conf.BER <- function(statPerm){
  
  BER.01 <- quantile(statPerm,0.01,na.rm=T)*100
  BER.05 <- quantile(statPerm,0.05,na.rm=T)*100
  
  conf.BER <- cbind(BER.01,BER.05)
  colnames(conf.BER) <- c("BER.01","BER.05")
  conf.BER
  
}
############### end confidence interval function ###########


##########################################################################################
##########################################################################################
###############################classifier.results()#######################################
##########################################################################################
##########################################################################################

classifier.results <-function(name="test",train=thanIdid1c,test=thanIdid2,features=c(2:3),samples=10,kernel="radial basis"){
  
  stats <- get.values.svm(name,train,test,features,kernel)
  permStats <- permute.svm(train,test,features,samples)
  MACA <- mean(permStats[,1]*100)
  MACE <- mean(permStats[,2]*100)
  p.value.acc <- PValue.acc(stats[,2]/100,permStats[,1])
  p.value.BER <- PValue.BER(stats[,3]/100,permStats[,2])
  conf.acc <- conf.acc(permStats[,1])
  conf.BER <- conf.BER(permStats[,2])
  
  #feat.num0 <- paste(features)
  #feat.num <- paste(feat.num0,collapse = ',')
  
  imagename <- paste(getwd(),"/",name,sep="")
  
  ################
  pdf(paste(imagename,"_BER.pdf",sep=""))
  
  #plot ecdf and draw lines connecting the points
  plot.ecdf(permStats[,2],xlim=c(0,1),xlab="Balanced Error Rate",ylab="ecdf",main=paste(name,"\n(BER=", stats[,3],"%, p=",round(p.value.BER,5),")",sep=""),sub=paste("kernel=",kernel,", permutations=",samples,sep=""));
  sortACE <- sort(permStats[,2])
  ecdfACE <- (1:length(sortACE))/length(sortACE)
  lines(sortACE,ecdfACE)
  
  # add a red line corresponding to the nonpermuted test statistic (BER)
  # (I added the value manually)
  abline(v=stats[,3]/100,col=2)
  
  # add a blue line corresponding to a test statistic (BER) at p=0.5
  q <- quantile(permStats[,2], probs=.05,na.rm=T)
  abline(v=q,col=4)
  
  legend("bottomright",lty=1,c("observed BER","BER at p=0.05"),col=c(2,4))
  
  dev.off()
  
  
  pdf(paste(imagename,"_acc.pdf",sep=""))
  
  #plot ecdf and draw lines connecting the points
  plot.ecdf(permStats[,1],xlim=c(0,1),xlab="Generalization Accuracy",ylab="ecdf",main=paste(name,"\n(Accuracy=", stats[,2],"%, p=",round(p.value.acc,5),")",sep=""),sub=paste("kernel=",kernel,", permutations=",samples,sep=""));
  sortACA <- sort(permStats[,1])
  ecdfACA <- (1:length(sortACA))/length(sortACA)
  lines(sortACA,ecdfACA)
  
  # add a red line corresponding to the nonpermuted test statistic
  # (I added the value manually)
  abline(v=stats[,2]/100,col=2)
  
  # add a blue line corresponding to a test statistic (BER) at p=0.5
  q <- quantile(permStats[,1], probs=.95,na.rm=T)
  abline(v=q,col=4)
  
  legend("topleft",lty=1,c("observed accuracy","accuracy at p=0.05"),col=c(2,4))
  
  dev.off()
  
  
  
  
  ################## end plot
  
  
  
  results <- data.frame(stats[,1],kernel, samples, round(stats[,2],digits=1),conf.acc[,1],conf.acc[,2],p.value.acc,round(stats[,3],digits=1),conf.BER[,1],conf.BER[,2],p.value.BER,round(MACA,digits=1),round(MACE,digits=1))
  
  
  #colnames(results) <- c("name", "kernel","samples", "acc","acc.95","acc.99", "p.value.acc","BER","BER.01","BER.05", "p.value.BER", "mean.perm.acc", "mean.perm.BER")
  
  results
}

##########################################################################################
##########################################################################################
#################################### end of classifier.results()##########################      
##########################################################################################
########################################################################################## 




##########################################################################################
##########################################################################################
###############################classifier.results.lda()####################################
##########################################################################################
##########################################################################################

classifier.results.lda <-function(name="test",train=thanIdid1c,test=thanIdid2,features=c(2:3),samples=10){
  
  stats <- get.values.lda(name,train,test,features)
  permStats <- permute.lda(train,test,features,samples)
  MACA <- mean(permStats[,1]*100)
  MACE <- mean(permStats[,2]*100)
  p.value.acc <- PValue.acc(stats[,2]/100,permStats[,1])
  p.value.BER <- PValue.BER(stats[,3]/100,permStats[,2])
  conf.acc <- conf.acc(permStats[,1])
  conf.BER <- conf.BER(permStats[,2])
  
  #feat.num0 <- paste(features)
  #feat.num <- paste(feat.num0,collapse = ',')
  
  imagename <- paste(getwd(),"/",name,sep="")
  
  ################
  pdf(paste(imagename,"_BER.pdf",sep=""))
  
  #plot ecdf and draw lines connecting the points
  plot.ecdf(permStats[,2],xlim=c(0,1),xlab="Balanced Error Rate",ylab="ecdf",main=paste(name,"\n(BER=", stats[,3],"%, p=",round(p.value.BER,5),")",sep=""),sub=paste("LDA",", permutations=",samples,sep=""));
  sortACE <- sort(permStats[,2])
  ecdfACE <- (1:length(sortACE))/length(sortACE)
  lines(sortACE,ecdfACE)
  
  # add a red line corresponding to the nonpermuted test statistic (BER)
  # (I added the value manually)
  abline(v=stats[,3]/100,col=2)
  
  # add a blue line corresponding to a test statistic (BER) at p=0.5
  q <- quantile(permStats[,2], probs=.05,na.rm=T)
  abline(v=q,col=4)
  
  legend("bottomright",lty=1,c("observed BER","BER at p=0.05"),col=c(2,4))
  
  dev.off()
  
  
  pdf(paste(imagename,"_acc.pdf",sep=""))
  
  #plot ecdf and draw lines connecting the points
  plot.ecdf(permStats[,1],xlim=c(0,1),xlab="Generalization Accuracy",ylab="ecdf",main=paste(name,"\n(Accuracy=", stats[,2],"%, p=",round(p.value.acc,5),")",sep=""),sub=paste("LDA",", permutations=",samples,sep=""));
  sortACA <- sort(permStats[,1])
  ecdfACA <- (1:length(sortACA))/length(sortACA)
  lines(sortACA,ecdfACA)
  
  # add a red line corresponding to the nonpermuted test statistic
  # (I added the value manually)
  abline(v=stats[,2]/100,col=2)
  
  # add a blue line corresponding to a test statistic (BER) at p=0.5
  q <- quantile(permStats[,1], probs=.95,na.rm=T)
  abline(v=q,col=4)
  
  legend("topleft",lty=1,c("observed accuracy","accuracy at p=0.05"),col=c(2,4))
  
  dev.off()
  
  
  
  
  ################## end plot
  
  
  
  results <- data.frame(stats[,1],"LDA", samples, round(stats[,2],digits=1),conf.acc[,1],conf.acc[,2],p.value.acc,round(stats[,3],digits=1),conf.BER[,1],conf.BER[,2],p.value.BER,round(MACA,digits=1),round(MACE,digits=1))
  
  
  #colnames(results) <- c("name", "kernel","samples", "acc","acc.95","acc.99", "p.value.acc","BER","BER.01","BER.05", "p.value.BER", "mean.perm.acc", "mean.perm.BER")
  
  results
}

##########################################################################################
##########################################################################################
#################################### end of classifier.results.lda()######################      
##########################################################################################
########################################################################################## 







#######################  Dataset loop #######################


thanIdid1c$duration_ratio <- thanIdid1c$duration_V3/thanIdid1c$duration_V2

thanIdid2$duration_ratio <- thanIdid2$duration_V3/thanIdid2$duration_V2
FOF$duration_ratio <- FOF$duration_V3/FOF$duration_V2
SOF$duration_ratio <- SOF$duration_V3/SOF$duration_V2
q$duration_ratio <- q$duration_V3/q$duration_V2
decl$duration_ratio <- decl$duration_V3/decl$duration_V2
w1_mean$duration_ratio <- w1_mean$duration_V3/w1_mean$duration_V2
w2_mean$duration_ratio <- w2_mean$duration_V3/w2_mean$duration_V2
fof_mean$duration_ratio <- fof_mean$duration_V3/fof_mean$duration_V2
w1_kNN$duration_ratio <- w1_kNN$duration_V3/w1_kNN$duration_V2
w2_kNN$duration_ratio <- w2_kNN$duration_V3/w2_kNN$duration_V2
fof_kNN$duration_ratio <- fof_kNN$duration_V3/fof_kNN$duration_V2
fof_kNN5$duration_ratio <- fof_kNN5$duration_V3/fof_kNN5$duration_V2
fof_kNN25$duration_ratio <- fof_kNN25$duration_V3/fof_kNN25$duration_V2
fof_kNN50$duration_ratio <- fof_kNN50$duration_V3/fof_kNN50$duration_V2

#VarSelRF values#
all <- c(2:309,311)
best <- c(2,178)
all_f0 <- c(15:32,215:220)
best_f0 <- c(17,20,30)
all_nonf0 <- c(2:14,33:214,221:309,311)
best_nonf0 <- c(2,178)
syntag <- c(8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,311)
best_syntag <- c(17,20,27)
paradig <-c(2:7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42,43,45,46,48,49,51,52,54,55,57,58,60:309)
best_paradig <- c(2,178)
expA <- c(2,178, 17, 5)
expB <- c(2,178, 20, 5)
expC <- c(2,178, 5)

# Boruta values #
all <- c(2:309,311)
best <- c(6, 17, 20, 26, 29, 30, 32, 50, 122, 126, 128, 130, 132, 134, 136, 138, 140, 176, 177, 178, 179, 180)
all_f0 <- c(15:32,215:220)
best_f0 <- c(7, 20, 24, 26, 27, 29, 30, 32)
all_nonf0 <- c(2:14,33:214,221:309,311)
best_nonf0 <- c(2, 6, 8, 50, 56, 63, 86, 122, 126, 128, 130, 132, 134, 136, 138, 140, 176, 177, 178, 179, 180)
syntag <- c(8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,311)
best_syntag <- c(8, 17, 20, 23, 26, 29)
paradig <-c(2:7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42,43,45,46,48,49,51,52,54,55,57,58,60:309)
best_paradig <- c(73, 120, 122, 126, 128, 130, 132, 134, 136, 138, 140, 176, 177, 178, 179, 180)
expA <- c(2,178, 17, 5)
expB <- c(2,178, 20, 5)
expC <- c(2,178, 5)





dsets <- list(all,best,all_f0,best_f0,all_nonf0,best_nonf0,syntag,best_syntag,paradig,best_paradig,expA,expB,expC)

names(dsets) <- c("all","best","all_f0","best_f0","all_nonf0","best_nonf0","syntag","best_syntag","paradig","best_paradig","expA","expB","expC")

all_results <- data.frame()
for (i in 1:length(dsets)){
  
  
  d.train <- thanIdid1
  d.test <- thanIdid2
  d.name <- paste("Web1_Web2_Mean_Boruta",names(dsets)[i],sep="")
  d.features <- unlist(dsets[i])
  d.samples = 5000
  d.kernel = "linear"
  
  ###### SVM #####	
  #	class_results <-classifier.results(d.name,d.train,d.test,d.features,d.samples,d.kernel)
  
  ###### LDA #####	
  class_results <-classifier.results.lda(d.name,d.train,d.test,d.features,d.samples)	
  #	print(dsets[i])
  #	hist(permStats[,1])
  
  all_results <- rbind(all_results,class_results)
  
}

colnames(all_results) <- c("name", "kernel","permutations", "acc","acc.95","acc.99", "p.value.acc","BER","BER.01","BER.05", "p.value.BER", "mean.perm.acc", "mean.perm.BER")

write.table(all_results,"/Users/Jonathan/Documents/Current/Comparatives/Web1toWeb2MeanBoruta_LDA_5000_1to13.dataframe")



FOFw2_kNN.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/FOFkNNtoWeb2kNN_RBF_5000_1to13.dataframe",header=TRUE)









### getting funny error
# Error in quantile.default(statPerm, 0.05) : 
#  missing values and NaN's not allowed if 'na.rm' is FALSE

a <- classifier.results("Web1toWeb2_best_syntag",thanIdid1c,thanIdid2,best_syntag,1000,"linear")
b <- classifier.results("Web1toWeb2_paradig",thanIdid1c,thanIdid2,paradig,30,"linear")



w1w2.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toWeb2_RBF_1000_2to13.dataframe",header=T)

coo <- cbind()

plot(y=w1w2.rbf$acc,x=w1w2.rbf$name)

sort.w1w2.rbf <- w1w2.rbf[order(w1w2.rbf$acc),]
sort.w1w2.rbf

plot(x=sort.w1w2.rbf$name,y=sort.w1w2.rbf$acc)


## This script first builds a set of classifiers from a training set and tests them on a test set.  Then, through multiple iterations of permuting categories in the training set and repeating the training-testing paradigm, the script calculates an empirical distribution of accuracy and error and a p-value for the observed accuracy and error.



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

# Specify working directory
# Plots of the permutation-achieved empirical distribution will be written here
setwd("/Users/Jonathan/Documents/Current/Comparatives/images_pval/LabFOFtoLabSOF_lda_5000")

# Set path for results dataframes
results_path <- "/Users/Jonathan/Documents/Current/Comparatives/"

# Read in training set web1 (aka thanIdid1)
w1_mean <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_mean_impute.dataframe")
w2_mean <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_mean_impute.dataframe")
decl <- thanIdidlab[thanIdidlab$occurrence== "FOF"& thanIdidlab$condition!="q",-c(311:313)]
q <- thanIdidlab[thanIdidlab$occurrence== "FOF"& thanIdidlab$condition=="q",-c(311:313)]


train <- w1_mean
train.name <- "w1"
test <- w2_mean
test.name <- "w2"
samples <- 5000
samples.name <- "5000"


# Boruta values #
# see select.R #
dsets <- list(all,best,all_f0,best_f0,all_nonf0,best_nonf0,all_syntag,best_syntag,all_paradig,best_paradig,expA,expB,expC)

features <- unlist(dsets[i])

names(dsets) <- c("all","best","all_f0","best_f0","all_nonf0","best_nonf0","all_syntag","best_syntag","all_paradig","best_paradig","expA","expB","expC")


algorithms <- c("svm-rbf","svm-lda","lda")

for (j in 1:length(algorithms)){

all_results <- data.frame()
for (i in 1:length(dsets)){

  if (algorithms[j]=="svm-rbf"){
    d.name <- paste(train.name,test.name,names(dsets)[i],sep="_")
    kernel <- "radial"
    class_results <-classifier.results(d.name,train,test,features,samples,kernel)
  }
  
  if (algorithms[j]=="svm-lin"){
    d.name <- paste(train.name,test.name,algorithms[j],names(dsets)[i],sep="_")
    kernel <- "linear"
    class_results <-classifier.results(d.name,train,test,features,samples,kernel)
  }
  
  if (algorithms[j]=="lda"){
    d.name <- paste(train.name,test.name,algorithms[j],names(dsets)[i],sep="_")
    class_results <-classifier.results.lda(d.name,train,test,features,samples)
  }
  
  all_results <- rbind(all_results,class_results)
  
}

colnames(all_results) <- c("name", "kernel","permutations", "acc","acc.95","acc.99", "p.value.acc","BER","BER.01","BER.05", "p.value.BER", "mean.perm.acc", "mean.perm.BER")

write.table(all_results,paste(results_path,train.name,"_",test.name,"_",algorithms[j],"_",samples.name,".dataframe",sep=""))
}




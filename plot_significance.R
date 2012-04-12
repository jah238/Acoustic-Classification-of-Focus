

#Playing with ggplot2

library(ggplot2)

## Prepare data

w1w2.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toWeb2_RBF_5000_1to13.dataframe",header=T)
w1w2.lin <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toWeb2_linear_5000_1to13.dataframe",header=T)
w1w2.lda <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toWeb2_LDA_5000_1to13.dataframe",header=T)

w1FOF.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabFOF_RBF_5000_1to13.dataframe",header=T)
w1FOF.lin <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabFOF_lin_5000_1to13.dataframe",header=T)
w1FOF.lda <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabFOF_LDA_5000_1to13.dataframe",header=T)

w1q.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabInterrogative_RBF_5000_1to13.dataframe",header=T)
w1q.lin <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabInterrogative_lin_5000_1to13.dataframe",header=T)
w1q.lda <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabInterrogative_lda_5000_1to13.dataframe",header=T)

w1SOF.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabSOF_RBF_5000_1to13.dataframe",header=T)
w1SOF.lin <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabSOF_lin_5000_1to13.dataframe",header=T)
w1SOF.lda <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabSOF_lda_5000_1to13.dataframe",header=T)

w1decl.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabDeclarative_RBF_5000_1to13.dataframe",header=T)
w1decl.lin <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabDeclarative_lin_5000_1to13.dataframe",header=T)
w1decl.lda <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1toLabDeclarative_lda_5000_1to13.dataframe",header=T)

FOFw2.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/LabFOFtoWeb2_RBF_5000_1to13.dataframe",header=T)
FOFw2.lin <- read.table("/Users/Jonathan/Documents/Current/Comparatives/LabFOFtoWeb2_lin_5000_1to13.dataframe",header=T)
FOFw2.lda <- read.table("/Users/Jonathan/Documents/Current/Comparatives/LabFOFtoWeb2_lda_5000_1to13.dataframe",header=T)

FOFSOF.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/LabFOFtoLabSOF_rbf_5000_1to13.dataframe",header=T)
FOFSOF.lin <- read.table("/Users/Jonathan/Documents/Current/Comparatives/LabFOFtoLabSOF_lin_5000_1to13.dataframe",header=T)
FOFSOF.lda <- read.table("/Users/Jonathan/Documents/Current/Comparatives/LabFOFtoLabSOF_lda_5000_1to13.dataframe",header=T)

w1Meanw2Mean.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1MeantoWeb2Mean_RBF_5000_1to13.dataframe",header=T)
w1w2_kNN.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1kNNtoWeb2kNN_RBF_5000_1to13.dataframe",header=T)
w1FOF_kNN.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1kNNtoFOFkNN_RBF_5000_1to13.dataframe",header=TRUE)
w1FOF_kNN5.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/Web1kNNtoFOFkNN5_RBF_5000_1to13.dataframe",header=TRUE)

FOFw2_kNN.rbf <- read.table("/Users/Jonathan/Documents/Current/Comparatives/FOFkNNtoWeb2kNN_RBF_5000_1to13.dataframe",header=TRUE)





# Rename classifier names, prepend classifier number, order by classifier number
new.name <- function(data){
  data$name <- c("All","Best","All F0","Best F0","All non-F0","Best non-F0","All syntag", "Best syntag","All paradig", "Best paradig", "Exp A", "Exp B", "Exp C")
  
  while(levels(data$kernel)=="radial basis"){
    levels(data$kernel)="SVM (radial)"
  }
  while(levels(data$kernel)=="linear"){
    levels(data$kernel)="SVM (linear)"
  }
  data	
}

new.order <- function(data){
  ord <- length(data):1
  data <- cbind(ord,data)
  data <- data[order(ord),]
  data
}


w1w2.rbf <-new.name(w1w2.rbf)
w1w2.rbf <-new.order(w1w2.rbf)

w1w2.lin <-new.name(w1w2.lin)
w1w2.lin <-new.order(w1w2.lin)

w1w2.lda <-new.name(w1w2.lda)
w1w2.lda <-new.order(w1w2.lda)


w1w2 <- rbind(w1w2.rbf,w1w2.lin,w1w2.lda)



# Function that takes three dataframes (.rbf,.lin,.lda) and produces a pdf plot

plot.sig <- function(rbf,lin,lda){
  rbf <- new.name(rbf)
  rbf <- new.order(rbf)
  lin <- new.name(lin)
  lin <- new.order(lin)
  lda <- new.name(lda)
  lda <- new.order(lda)
  
  data <- rbind(rbf,lin,lda)
  
  p <- ggplot(data, aes(acc,reorder(name,ord),group=1)) + geom_path(aes(acc))  + facet_grid(. ~ kernel) + geom_path() + geom_point()
  
  p <- p + geom_point(aes(BER,reorder(name,ord),group=1)) + geom_path(aes(BER))
  
  p <- p + geom_point(aes(BER.05,reorder(name,ord),group=1,color="p=.05")) + geom_path(aes(BER.05,color="p=.05"))
  
  p <- p + geom_point(aes(BER.01,reorder(name,ord),group=1,color="p=.01")) + geom_path(aes(BER.01,color="p=.01"))
  
  p <- p + geom_point(aes(acc.95,reorder(name,ord),group=1,color="p=.05")) + geom_path(aes(acc.95,color="p=.05"))
  
  p <- p + geom_point(aes(acc.99,reorder(name,ord),group=1,color="p=.01")) + geom_path(aes(acc.99,color="p=.01"))
  
  p <- p + labs(x="Accuracy and Balanced Error Rate", y="Feature Set",colour="Permutation \nAchieved \nStatistic") + opts(axis.title.x = theme_text(hjust=0.7,vjust=0,size=11),axis.title.y = theme_text(size=11,angle=90),axis.text.y = theme_text(size=8,colour="grey30",hjust=1),legend.text= theme_text(size=8),legend.title = theme_text(size=8,face="bold",hjust=0)) 
  
  p <- p + geom_hline(yintercept=c(3.5,7.5,11.5),linetype=2,alpha=0.5)
  
  print(p)
  
  
  #pdf(paste(outname,".pdf",sep=""), width= unit(6.5,"inch"), height = unit(3.5,"inch"))
  p
  #dev.off()
  
}




obj <- plot.sig(FOFSOF.rbf,FOFSOF.lin,FOFSOF.lda)


pdf("FOFSOF_significance.pdf", width= unit(6.5,"inch"), height = unit(3.5,"inch"))
obj
dev.off()

















p <- ggplot(w1w2, aes(acc,reorder(name,ord),group=1)) + geom_path(aes(acc))  + facet_grid(. ~ kernel) + geom_path() + geom_point()

p <- p + geom_point(aes(BER,reorder(name,ord),group=1)) + geom_path(aes(BER))

p <- p + geom_point(aes(BER.05,reorder(name,ord),group=1,color="p=.05")) + geom_path(aes(BER.05,color="p=.05"))

p <- p + geom_point(aes(BER.01,reorder(name,ord),group=1,color="p=.01")) + geom_path(aes(BER.01,color="p=.01"))

p <- p + geom_point(aes(acc.95,reorder(name,ord),group=1,color="p=.05")) + geom_path(aes(acc.95,color="p=.05"))

p <- p + geom_point(aes(acc.99,reorder(name,ord),group=1,color="p=.01")) + geom_path(aes(acc.99,color="p=.01"))

p <- p + labs(x="Accuracy and Balanced Error Rate", y="Feature Set",colour="Permutation \nAchieved \nStatistic") + opts(axis.title.x = theme_text(hjust=0.7,vjust=0,size=11),axis.title.y = theme_text(size=11,angle=90),axis.text.y = theme_text(size=8,colour="grey30",hjust=1),legend.text= theme_text(size=8),legend.title = theme_text(size=8,face="bold",hjust=0)) 

p <- p + geom_hline(yintercept=c(3.5,7.5,11.5),linetype=2,alpha=0.5)

p


pdf("/Users/Jonathan/Documents/Current/Comparatives/images_confidence/w1w2_confidence.pdf", width= unit(6.5,"inch"), height = unit(3.5,"inch"))
p
dev.off()



pdf("/Users/Jonathan/Documents/Current/Comparatives/images_confidence/w1w2_confidence2.pdf", width= unit(8,"inch"), height = unit(6,"inch"))
p
dev.off()


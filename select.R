

#install.packages("Boruta",dependencies=T)
library("Boruta")


train <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_mean_impute.dataframe")

all <- c(2:310)
all_f0 <- c(15:32,215:220)
all_nonf0 <- c(2:14,33:214,221:310)
all_syntag <- c(8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,310)
all_paradig <-c(2:7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42,43,45,46,48,49,51,52,54,55,57,58,60:309)

sub.all <- train[,c(all,311)]
all.Boruta <- Boruta(focus ~ ., data=sub.all, light=F)
best <- subset(names(all.Boruta$finalDecision),all.Boruta$finalDecision=="Confirmed")

sub.all_f0 <- train[,c(all_f0,311)]
all_f0.Boruta <- Boruta(focus ~ ., data=sub.all_f0, light=F)
best_f0 <- subset(names(all_f0.Boruta$finalDecision),all_f0.Boruta$finalDecision=="Confirmed")

sub.all_nonf0 <- train[,c(all_nonf0,311)]
all_nonf0.Boruta <- Boruta(focus ~ ., data=sub.all_nonf0, light=F)
best_nonf0 <- subset(names(all_nonf0.Boruta$finalDecision),all_nonf0.Boruta$finalDecision=="Confirmed")

sub.all_syntag <- train[,c(all_syntag,311)]
all_syntag.Boruta <- Boruta(focus ~ ., data=sub.all_syntag, light=F)
best_syntag <- subset(names(all_syntag.Boruta$finalDecision),all_syntag.Boruta$finalDecision=="Confirmed")

sub.all_paradig <- train[,c(all_paradig,311)]
all_paradig.Boruta <- Boruta(focus ~ ., data=sub.all_paradig, light=F)
best_paradig <- subset(names(all_paradig.Boruta$finalDecision),all_paradig.Boruta$finalDecision=="Confirmed")

expA <- c(2,178, 17, 5)
expB <- c(2,178, 20, 5)
expC <- c(2,178, 5)





#Play


levels(thanIdid1$focus)[levels(thanIdid1$focus)=="s"] <- 1
levels(thanIdid1$focus)[levels(thanIdid1$focus)=="ns"] <- -1

svm.fs(thanIdid1[,c(2:309)],thanIdid1[,310],fs.method = "scad")



control <- rfeControl(functions = selectSize, method = "boot", verbose = FALSE, returnResamp = "final", number = 50)

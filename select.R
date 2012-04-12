

install.packages("Boruta",dependencies=T)
library("Boruta")


thanIdid1 <- read.table("/Users/Jonathan/Documents/Current/speech/Data/wav_big/thanIdid1_mean_impute.dataframe")
thanIdid2 <- read.table("/Users/Jonathan/Documents/Current/speech/Harvest/thanidid2/processed/thanIdid2_mean_impute.dataframe")




thanIdid1$duration_ratio <- thanIdid1$duration_V3/thanIdid1$duration_V2
thanIdid2$duration_ratio <- thanIdid2$duration_V3/thanIdid2$duration_V2


thanIdid1.310 <- thanIdid1[,2:311]
Boruta(focus ~ ., data=thanIdid1.310, light=F)
all_f0 <- c(15:32,215:220)
thanIdid1.f0 <- thanIdid1[,c(all_f0,310)]
Boruta(focus ~ ., data=thanIdid1.f0, light=F)
all_nonf0 <- c(2:14,33:214,221:309,311)
thanIdid1.nonf0 <- thanIdid1[,c(all_nonf0,310)]
Boruta(focus ~ ., data=thanIdid1.nonf0, light=F)
syntag <- c(8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,311)
thanIdid1.syntag <- thanIdid1[,c(syntag,310)]
Boruta(focus ~ ., data=thanIdid1.syntag, light=F)
paradig <-c(2:7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42,43,45,46,48,49,51,52,54,55,57,58,60:309)
thanIdid1.paradig <- thanIdid1[,c(paradig,310)]
Boruta(focus ~ ., data=thanIdid1.paradig, light=F)


attributes <- c("duration_V2", "pulses_V2", "minf0Time_V2_percent", "rangef0_V2", "f1f2Time10_V2", "f1Time20_V2", "f2Time20_V2", "f2Time30_V2", "f1Time40_V2", "f2Time40_V2", "f1Time50_V2", "f2Time50_V2", "f1Time60_V2", "f2Time60_V2", "f1Time70_V2", "f1f2Time20_V2", "f1f2Time30_V2", "f1f2Time40_V2", "f1f2Time50_V2", "f1f2Time60_V2")

attribute.indices <- vector()
for (i in 1:length(attributes)){
  attribute.indices[i] <- which(colnames(thanIdid1)==attributes[i])
}

attribute.indices

length(attributes)

# Boruta from all
# 73, 120, 122, 126, 128, 130, 132, 134, 136, 138, 140, 176, 177, 178, 179, 180
# "duration_V2", "pulses_V2", "pulses_ratio", "f0_ratio", "maxf0_ratio", "minf0Time_ratio", "rangef0_V2", "rangef0_ratio", "energy_ratio", "f2Time30_V2", "f1Time40_V2", "f2Time40_V2", "f1Time50_V2", "f2Time50_V2", "f1Time60_V2", "f2Time60_V2", "f1Time70_V2", "f1f2Time20_V2", "f1f2Time30_V2", "f1f2Time40_V2", "f1f2Time50_V2", "f1f2Time60_V2"

# Boruta from f0
# 17, 20, 23, 24, 25, 26, 27, 29, 30, 32
# "f0_ratio", "maxf0_ratio", "minf0_ratio", "maxf0Time_V2_percent", "maxf0Time_V3_percent", "maxf0Time_ratio", "minf0Time_V2_percent", "minf0Time_ratio", "rangef0_V2", "rangef0_ratio"

# Boruta from non-f0
# 2, 6, 8, 38, 50, 56, 86, 120, 122, 126, 128, 130, 132, 134, 136, 138, 140, 176, 177, 178, 179, 180
# "duration_V2", "pulses_V2", "pulses_ratio", "maxIntensity_ratio", "energy_ratio", "amp_ratio", "f1f2Time10_V2", "f1Time20_V2", "f2Time20_V2", "f2Time30_V2", "f1Time40_V2", "f2Time40_V2", "f1Time50_V2", "f2Time50_V2", "f1Time60_V2", "f2Time60_V2", "f1Time70_V2", "f1f2Time20_V2", "f1f2Time30_V2", "f1f2Time40_V2", "f1f2Time50_V2", "f1f2Time60_V2"

# Boruta from syntag
#  8, 17, 20, 23, 26, 29, 32, 38, 50, 53, 56, 311
# "pulses_ratio", "f0_ratio", "maxf0_ratio", "minf0_ratio", "maxf0Time_ratio", "minf0Time_ratio", "rangef0_ratio", "maxIntensity_ratio", "energy_ratio", "power_ratio", "amp_ratio", "duration_ratio"

# Boruta from paradig
# 2, 6, 27, 30, 86, 120, 122, 126, 128, 130, 132, 134, 136, 138, 140, 176, 177, 178, 179, 180  
# "duration_V2", "pulses_V2", "minf0Time_V2_percent", "rangef0_V2", "f1f2Time10_V2", "f1Time20_V2", "f2Time20_V2", "f2Time30_V2", "f1Time40_V2", "f2Time40_V2", "f1Time50_V2", "f2Time50_V2", "f1Time60_V2", "f2Time60_V2", "f1Time70_V2", "f1f2Time20_V2", "f1f2Time30_V2", "f1f2Time40_V2", "f1f2Time50_V2", "f1f2Time60_V2"



levels(thanIdid1$focus)[levels(thanIdid1$focus)=="s"] <- 1
levels(thanIdid1$focus)[levels(thanIdid1$focus)=="ns"] <- -1

svm.fs(thanIdid1[,c(2:309)],thanIdid1[,310],fs.method = "scad")



control <- rfeControl(functions = selectSize, method = "boot", verbose = FALSE, returnResamp = "final", number = 50)

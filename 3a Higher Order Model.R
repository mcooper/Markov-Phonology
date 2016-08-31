setwd('D:/Documents and Settings/mcooper/Google Drive/Markov Phonology')
load('allphones.Rdata')

singles0 <- unique(allphones)
singles <- singles0[!singles0=='0']

model <- array(data=NA, dim=c(40,39,40))

dimnames(model)[[1]] <- singles0
dimnames(model)[[2]] <- singles
dimnames(model)[[3]] <- singles0

for (a in singles0){
  for (b in singles){
    for (c in singles0){
      samp <- paste(c(a,b,c), collapse='.')
      model[a,b,c] <- str_count(allphones, samp)
    }
  }
}

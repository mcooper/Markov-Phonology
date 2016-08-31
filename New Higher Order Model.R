library(markovchain)
library(reshape2)
library(data.table)
library(stringr)
library(plyr)

setwd('D:/Documents and Settings/mcooper/Google Drive/Markov Phonology')

#Add in parsed words
dict <- read.csv('AllParsed.csv', stringsAsFactors=F)

##Add in frequency data 
freq <- read.csv('freq.csv', stringsAsFactors=F)
freq$Word <- toupper(freq$Word)

dict <- merge(dict, freq, by.x='name', by.y='Word')


getwords <- function(i){
  out <- i[paste0('p', seq(1,32))]
  out <- out[!is.na(out)]
  out <- paste(c('0',out,'0'), collapse='.')
  return(out)
}

for (i in 1:nrow(dict)){
  dict$word[i] <- getwords(dict[i,])
}

#create array of 2-order markov model
singles0 <- unique(allphones)#Have to find a way to do this without depricated object 'allphones'
singles <- singles0[!singles0=='0']

model <- array(data=NA, dim=c(40,39,40))

dimnames(model)[[1]] <- singles0
dimnames(model)[[2]] <- singles
dimnames(model)[[3]] <- singles0

for (a in singles0){
  for (b in singles){
    for (c in singles0){
      samp <- paste(c(a,b,c), collapse='.')
      model[a,b,c] <- sum(str_count(dict$word, samp)*dict$Frequency)
    }
  }
}

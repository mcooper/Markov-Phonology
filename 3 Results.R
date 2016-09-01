words3 <- as.list(freqChain[freqChain!=0])
names(words3) <- chain[freqChain!=0]

getNext <- function(x, n){
  #Takes a sequence of phones as "X.X.X.X..."
  #returns the possibility of that word with a new letter
  phones <- strsplit(n, '.', fixed=T)[[1]]
  last <- length(phones)
  nextPhones <- freqChain[phones[last-1], phones[last], ]
  nextPhones <- nextPhones[nextPhones > 0]
  if(sum(nextPhones) > 0 ){
    freqNames <- paste(n, names(nextPhones), sep='.')
    freqs <- x[[1]]*nextPhones
    names(freqs) <- freqNames
    return(freqs)
  }else{
    return(NULL)
  }
}

words4 <- NULL
for(w in 1:length(words3)){
  words4 <- c(words4, getNext(words3[w], names(words3[w])))
}

words5 <- NULL
for(w in 1:length(words4)){
  words5 <- c(words5, getNext(words4[w], names(words4[w])))
}

words6 <- NULL
for(w in 1:length(words5)){
  words6 <- c(words6, getNext(words5[w], names(words5[w])))
}

words7 <- NULL
for(w in 1:length(words6)){
  words7 <- c(words7, getNext(words6[w], names(words6[w])))
}

words8 <- NULL
for(w in 1:length(words7)){
  words8 <- c(words8, getNext(words7[w], names(words7[w])))
}






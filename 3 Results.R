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

words4 <- mapply(getNext, words3, names(words3))

words5 <- mapply(getNext, words4, names(words4))

words6 <- mapply(getNext, words5, names(words5))

words7 <- mapply(getNext, words6, names(words6))

words8 <- mapply(getNext, words7, names(words7))



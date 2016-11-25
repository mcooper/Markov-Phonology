#'This is a script to generate a example words
#'in a language with PHONEMIC ORTHOGRPAHY

#load('kiswahili_chain.Rdata')

#Get all of the possible word-inital phones, as well as all of the probability of each phone following
words2 <- as.list(freqChain[1, , ][freqChain[1, , ]!=0])
names(words2) <- chain[1, , ][freqChain[1, , ]!=0]

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

countPhones <- function(word) {
  word2 <- gsub('.',"",word, fixed=T)
  return (nchar(word) - nchar(word2)-1)
}

words3 <- mapply(getNext, words2, names(words2))
names(words3) <- NULL
words3 <- unlist(words3)
words2stop <- words3[grepl('.0', names(words3))]

words3sel <- words3[!grepl('.0', names(words3))]
words4 <- mapply(getNext, words3sel, names(words3sel))
names(words4) <- NULL
words4 <- unlist(words4)
words3stop <- words4[grepl('.0', names(words4))]

words4sel <- words4[!grepl('.0', names(words4))]
words5 <- mapply(getNext, words4sel, names(words4sel))
names(words5) <- NULL
words5 <- unlist(words5)
words4stop <- words5[grepl('.0', names(words5))]

words5sel <- words5[!grepl('.0', names(words5))]
words6 <- mapply(getNext, words5sel, names(words5sel))
names(words6) <- NULL
words6 <- unlist(words6)
words5stop <- words6[grepl('.0', names(words6))]

words6sel <- words6[!grepl('.0', names(words6))]
words7 <- mapply(getNext, words6sel, names(words6sel))
names(words7) <- NULL
words7 <- unlist(words7)
words6stop <- words6[grepl('.0', names(words6))]
words7stop <- words7[grepl('.0', names(words7))]

words <- data.frame(combo_probability=c(words2stop, words3stop, words4stop, words5stop, words6stop, words7stop),
                    W.0.R.D=c(names(words2stop), names(words3stop), names(words4stop), names(words5stop), names(words6stop), names(words7stop)),
                    stringsAsFactors = FALSE)

words$phonecount <- sapply(words$W.ER.D, countPhones)

words <- words[order(words$combo_probability, decreasing=T), ]

write.csv(words, paste0(filename, '_words_probability.csv'), row.names=F)

getGibberish <- function(x){
  c2probs <- rowSums(freqChain['0',,])
  c1 <- '0'
  c2 <- sample(names(c2probs), 1, prob=c2probs)
  word <- c(c1, c2)
  while(sum(word=='0') < x+1){
    c1 <- word[length(word) - 1]
    c2 <- word[length(word)]
    if(c2 == '0'){
      c3 <- sample(names(c2probs), 1, prob=c2probs)
    } else{
      c3 <- sample(names(freqChain[c1, c2, ]), 1, prob=freqChain[c1, c2, ])
    }
    word <- c(word, c3)
  }
  paste(word, collapse='.')
}

out <- getGibberish(10)
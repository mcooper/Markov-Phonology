#'This is a script to generate a 2nd-order markov model
#'of phonotactics in a language with PHONEMIC ORTHOGRPAHY
#'
#'specify the name of a csv file with headers 'word' and 'freq', 
#'as well as any digraphs in your language

filename <- 'kiswahili'
digraphs <- digraphs <- c('DH', 'TH', 'SH', 'NY', 'KH')

#Load Data & packages
dict <- read.csv(paste0(filename, '.csv'), stringsAsFactors = F)
dict$word <- toupper(dict$word)

library(dplyr)

#Define functions
parseWords <- function(str){
  newstr <- '0'
  pass <- FALSE
  for (i in 1:nchar(str)){
    if (pass){
      pass <- FALSE
    }
    else if (!substr(str, i, i+1) %in% digraphs){
      newstr <- paste0(newstr, '.', substr(str, i, i))
    }else{
      newstr <- paste0(newstr, '.', substr(str, i, i+1))
      pass <- TRUE
    }
  }
  newstr <- paste0(newstr, '.0')
  return(newstr)
}

getFreq <- function(x){
  Freq <- sum(grepl(x, dict$W.O.R.D)*dict$freq/total)
  return(Freq)
}

checkLETTERS <- function(){
  accum <- NULL
  for (i in c(LETTERS, digraphs)){
    if(any(sapply(X = dict$word, FUN=grepl, pattern=i))){
      accum <- c(accum, i)
    }
  }
  return(accum)
}

#Data Prep

dict$W.O.R.D <- sapply(X=dict$word, FUN=parseWords)

total <- sum(dict$freq)

uniquePhones <- c('0', checkLETTERS())

#2nd-Order Markov Chain
combos <- expand.grid(uniquePhones, uniquePhones, uniquePhones) %>%
  apply(MARGIN=1, FUN=paste, collapse='.')

chain <- array(combos, dim = rep(length(uniquePhones), 3), dimnames=list(first=uniquePhones, second=uniquePhones, third=uniquePhones))

freqChain <- apply(chain, c(1,2,3), getFreq)

save(list=c('chain', 'freqChain', 'filename'), file=paste0(filename, '_chain.Rdata'))

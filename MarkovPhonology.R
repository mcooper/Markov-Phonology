library(markovchain)
library(plyr)
library(stringr)

setwd('D:/Documents and Settings/mcooper/Google Drive/Markov Phonology')

text <- readLines('D:/Documents and Settings/mcooper/Google Drive/Markov Phonology/CMUDICT.txt')

parselines <- function(line){
  loc <- str_locate_all(line, ' ')[[1]]
  word <- str_sub(line, 0, loc[2,1])
  word <- str_replace_all(word, '[ 0123456789]', '')
  if(nrow(loc)>2){
    for (i in 2:(nrow(loc)-1)){
      newword <- str_sub(line, loc[i,1], loc[i+1,1])
      newword <- str_replace_all(newword, '[ 0123456789]', '')
      word <- append(word, newword)
    }
  }
  lastword <- str_sub(line, loc[nrow(loc),1], nchar(line))
  word <- append(word, lastword)
  word <- append(word, nrow(loc)-1)
  word <- as.list(word)
  names(word) <- c('name', paste0('p', seq(1, nrow(loc)-1)), 'count')
  return(word)
}

parsed <- lapply(text, parselines)

allparsed <- rbind.fill(lapply(parsed, as.data.frame))

write.csv(allparsed, 'AllParsed.csv', row.names=F)










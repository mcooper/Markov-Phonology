library(dplyr)
library(stringr)

#Read in English phonolical dictionary from Carnegie Mellon University
text <- readLines('CMUDICT.txt')

parselines <- function(line){
  #Function to parse each line of the text file
  loc <- str_locate_all(line, ' ')[[1]]
  word <- str_sub(line, 0, loc[2,1])
  if(nrow(loc)>2){
    for (i in 2:(nrow(loc)-1)){
      newword <- str_sub(line, loc[i,1], loc[i+1,1])
      newword <- str_replace_all(newword, '[ 0123456789]', '')
      word <- c(word, newword)
    }
  }
  lastword <- str_sub(line, loc[nrow(loc),1], nchar(line))
  lastword <- str_replace_all(lastword, '[ 0123456789]', '')
  word <- c(word, lastword)
  word <- c(word, nrow(loc)-1)
  word <- c(word, paste0('0.', paste(word[2:(length(word)-1)], collapse='.'), '.0'))
  word <- as.list(word)
  names(word) <- c('name', paste0('p', seq(1, nrow(loc)-1)), 'count', 'W.ER.D')
  return(word)
}

#Parse the entire dictionary
parsed <- lapply(text, parselines)  

#Combine all parsed lines into data frame form list.
allparsed <- bind_rows(lapply(parsed, as.data.frame))

#remove duplicate names
allparsed <- allparsed[!grepl('(1)', allparsed$name),]

#Clean word names
allparsed$name <- str_replace_all(allparsed$name, '[ 0123456789]', '')

write.csv(allparsed, 'AllParsed.csv', row.names=F)







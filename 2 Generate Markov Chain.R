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

#Get frequency of each word length
freq_sum <- ddply(dict, .(count), .fun=summarize, Freq=sum(Frequency))
total <- sum(freq_sum$Freq)
freq_sum$Freq <- freq_sum$Freq/total
write.csv(freq_sum, 'WordLengthFrequencies.csv', row.names=F)


#Get list of all phonemes
dict$p0 <- '0'

getwords <- function(i){
   out <- i[paste0('p', seq(0,32))]
   out <- out[!is.na(out)]
   f <- i['Frequency']
   print(out)
   rep(out, round(f/100,0))
}
 
allphones <- c()
for (i in 1:nrow(dict)){
  allphones <- c(getwords(dict[i,]), allphones)
  print(dict$name[i])
}

save(allphones, file='allphones.Rdata')

mcFit <- fitHigherOrder(allphones, order=2)

save(mcFit, file='mc-higher-order.Rdata')




##Add in frequency data 
freq <- read.csv('freq.csv', stringsAsFactors=F)
freq$Word <- toupper(freq$Word)

dict <- merge(allparsed, freq, by.x='name', by.y='Word')

#Get frequency of each word length
freq_sum <- ddply(dict, .(count), .fun=summarize, Freq=sum(Frequency))
total <- sum(freq_sum$Freq)
freq_sum$Freq <- freq_sum$Freq/total
write.csv(freq_sum, 'WordLengthFrequencies.csv', row.names=F)

uniquePhones <- c(dict$p1, dict$p2, dict$p3, dict$p4, dict$p5, dict$p7, dict$p8, dict$p9, dict$p10) %>% 
  unique %>% na.omit

getFreq <- function(x){
  Freq <- sum(grepl(x, dict$W.ER.D)*dict$Frequency/total)
  return(Freq)
}

#2nd-Order Markov Chain
combos <- expand.grid(uniquePhones, uniquePhones, uniquePhones) %>%
  apply(MARGIN=1, FUN=paste, collapse='.')

chain <- array(combos, dim = rep(length(uniquePhones), 3), dimnames=list(first=uniquePhones, second=uniquePhones, third=uniquePhones))

freqChain <- apply(chain, c(1,2,3), getFreq)

save(list=c('chain', 'freqChain'), file='chain.Rdata')

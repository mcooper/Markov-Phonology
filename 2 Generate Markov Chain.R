library(markovchain)
library(reshape2)
library(data.table)
library(stringr)

setwd('D:/Documents and Settings/mcooper/Google Drive/Markov Phonology')

#Add in parsed words
dict <- read.csv('AllParsed.csv', stringsAsFactors=F)

##Add in frequency data 
freq <- read.csv('freq.csv', stringsAsFactors=F)
freq$Word <- toupper(freq$Word)

dict <- merge(dict, freq, by.x='name', by.y='Word', all.y=T)

dict$p0 <- '0'
dict[is.na(dict)] <- '0'

#get all possible transitions
transitions <- data.frame(word=dict$name, freq=dict$Frequency, t0=paste(dict$p0, dict$p1, sep='1'),
                          t1=paste(dict$p1, dict$p2, sep='1'), t2=paste(dict$p2, dict$p3, sep='1'), t3=paste(dict$p3, dict$p4, sep='1'),
                          t4=paste(dict$p4, dict$p5, sep='1'), t5=paste(dict$p5, dict$p6, sep='1'), t6=paste(dict$p6, dict$p7, sep='1'),
                          t7=paste(dict$p7, dict$p8, sep='1'), t8=paste(dict$p8, dict$p9, sep='1'), t9=paste(dict$p9, dict$p10, sep='1'),
                          t10=paste(dict$p10, dict$p11, sep='1'), t11=paste(dict$p11, dict$p12, sep='1'), t12=paste(dict$p12, dict$p13, sep='1'),
                          t13=paste(dict$p13, dict$p14, sep='1'), t14=paste(dict$p14, dict$p15, sep='1'), t15=paste(dict$p15, dict$p16, sep='1'), 
                          t16=paste(dict$p16, dict$p17, sep='1'), t17=paste(dict$p17, dict$p18, sep='1'), t18=paste(dict$p18, dict$p19, sep='1'), 
                          t19=paste(dict$p19, dict$p20, sep='1'), t20=paste(dict$p20, dict$p21, sep='1'), t21=paste(dict$p21, dict$p22, sep='1'), 
                          t22=paste(dict$p22, dict$p23, sep='1'), t23=paste(dict$p23, dict$p24, sep='1'), t24=paste(dict$p24, dict$p25, sep='1'), 
                          t25=paste(dict$p25, dict$p26, sep='1'), t26=paste(dict$p26, dict$p27, sep='1'), t27=paste(dict$p27, dict$p28, sep='1'), 
                          t28=paste(dict$p28, dict$p29, sep='1'), t29=paste(dict$p29, dict$p30, sep='1'), t30=paste(dict$p30, dict$p31, sep='1'), 
                          t31=paste(dict$p31, dict$p32, sep='1'), t32=paste(dict$p32, dict$p0, sep='1'))

#melt transitions, remove null transitions
tf <- melt(transitions[,2:35], id.vars='freq')
tf <- tf[tf$value!='010',]

#parse transitions back into separate columns
mids <- str_locate(tf$value, '1')[,1]
tfdt$start <- substr(tf$value, 0, mids-1)
tfdt$end <- substr(tf$value, mids+1, nchar(tfdt$value))

##Generate change matrix, weighted by word frequencies
mat <- acast(tfdt, start~end, value.var="freq", fun.aggregate=sum)
rSums <- rowSums(mat)
mat <- apply(mat, MARGIN=2, FUN=function(x){x/rSums})

##Generate Markov Chain 

mc<-new("markovchain", transitionMatrix=mat)
save(mc, file='markovchain.Rdata')

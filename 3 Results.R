library(markovchain)

setwd('D:/Documents and Settings/mcooper/Google Drive/Markov Phonology')

load('markovchain.Rdata')

step <- function(state, bawal){
  p <- firstPassage(mc, state=state, n=1)
  mx <- max(p[!p %in% bawal])
  val <- colnames(p)[p==mx]
  return(list(mx, val))
}

simlatelength(2)
  bawal1<-NULL
  phn1 <- NULL
  for (z in seq(1,10)){
    out <- step('0', bawal1)
    bawal1 <- c(out[[1]], bawal1)
    phn1 <- c(phn1, out[[2]])
  }
  bawal2 <- NULL
  phn2 <- NULL
  for (i in phn1){
    for (z in seq(1,10)){
      out <- step(i, bawal2)
      bawal2 <- c(out[[1]], bawal2)
      phn2 <- list(phn2, list(i, out[[2]]))
    }
  }

  
### So its looking like we will need a higher order markov chain, as well as better methods for exploring possibility space




##AH is
mc['0','AH']*mc['AH','0']

##AH N

mc['0','AH']*mc['AH', 'N']*mc['N','0']

##AH N D

mc['0','AH']*mc[

##AH N T

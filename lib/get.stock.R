get.stocks <- function(start,end,tckk,type=type)
{
  require(tseries)
  numtk <-length(tckk); 
  p <- get.hist.quote(instrument = tckk[1], start = start,end = end, quote = type, retclass = "zoo", quiet = T)
  dimnames(p)[[2]] <- as.character(tckk[1])
  for (i in  2:numtk) {
    ## display progress by showing the current iteration step
    cat("Downloading ", i, " out of ", numtk, '(',tckk[i],')', "\n")
    
    result <- try(x <- get.hist.quote(instrument = tckk[i], start=start, end=end,quote = type,provider = "yahoo", compression = "d",method='auto',retclass='zoo', quiet = T))
    if(class(result) == "try-error") {
      next
    }
    else {
      dimnames(x)[[2]] <- as.character(tckk[i])
      
      ## merge with already downloaded data to get assets on same dates 
      p <- merge(p, x)                      
      
    }
  }
  p
}

get.stocks.quandl <- function(start,end,tckk)
{
  require(Quandl)
  Quandl.auth("X5ws5JEoYdP6VFefbPmQ")

  numtk <-length(tckk); 
  p <- Quandl(paste('YAHOO/',tckk[1],sep=''),start_date = start,end_date = end,type='zoo',column=6)
  colnames(p) <- as.character(tckk[1])
  for (i in  2:numtk) {
    ## display progress by showing the current iteration step
    cat("Downloading ", i, " out of ", numtk, '(',tckk[i],')', "\n")
    
    #result <- try(
      x <- Quandl(paste('YAHOO/',tckk[i],sep=''),start_date = start,end_date = end,type='zoo',column=6)
    #if(class(result) == "try-error") {
    #  next
    #}
    #else {
      names(x) <- as.character(tckk[i])
      
      ## merge with already downloaded data to get assets on same dates 
      p <- merge(p, x)                      
      
    }
  
  p
}

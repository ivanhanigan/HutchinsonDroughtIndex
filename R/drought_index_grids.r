
#' @name drought_index_grids
#' @title drought index using grids
#' @param rasterbrick a stack of grids
#' @param startyear the start year
#' @param endyear the end year
#' @param droughtThreshold the level of dryness
#' @return dataframe with droughtIndices
#' @export

drought_index_grids <- function(rasterbrick,startyear,endyear,droughtThreshold=.375){
    
  b<-getValuesBlock(rasterbrick, row=500, nrows=5, col=500, ncols=5)
  # TODO estimate the max and min date from the data filenames
  x<-apply(b, 1, function(x) ts(x,start=c(startyear, 01),end=c(endyear,12),frequency=12))
  sixmnthtot<-apply(x, 2, function(x) c(rep(NA,5),x+lag(x,1)+lag(x,2)+lag(x,3)+lag(x,4)+lag(x,5)))
  # TODO it might be faster to use zoo::rollapply,
  # and also we can make the lag length variable
   
  ##rank
  # TODO select for each month ie all Januarys are ranked seperate from Febs etc
  rank <- apply(x, 2, function(x) {return((rank(x)-1)/(length(x)-1))})
  index <- apply(rank, 2, function(x) 8*(x-.5)) #to be a brick
  # .375 is refering to palmer's benchmark but we could let the user vary this
  drought <- apply(x, 2, function(x) x<=quantile(x,droughtThreshold)) 
  indexBelowThreshold <- index*drought #to be a  brick
   
  ##count
  x1 <- index<=-1
  x2 <- apply(x1, 2, function(x) (cumsum(!x) + 1) * x )
  seq <- apply(x1, 2, function(x) seq_along(x))
  match <- apply(x2, 2, function(x) match(x,x))
  count<- (seq - match + 1) * x1 #double check #to be a brick
  return(count)
}

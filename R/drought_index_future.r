#' @name drought_index_future
#' @title Drought Index For Stations for future projected rainfall
#' @param data a dataframe with date (future), year (future), month, rain (past) and rain_projected
#' @param years the number of years in the time series
#' @param droughtThreshold the level of dryness below which a drought begins
#' @return dataframe with droughtIndices
#' @export
#'
drought_index_future <- function(data,years,droughtThreshold=.375){
# a drought index based on integrated six-monthly rainfall percentiles.
# based on Professor Mike Hutchinson's work described in
# Smith D, Hutchinson M, McArthur R. Climatic and Agricultural Drought: Payments and Policy.
# Canberra, ACT: Centre for Resource and Environmental Studies, Australian National University. 1992.

# Ivan C Hanigan
# Nov 2015.
# GPL2
# for updates please see https://github.com/ivanhanigan/HutchinsonDroughtIndex.

# my input data are always a data.frame with 5 columns 'date (future','year(future)','month','rain(past)' 'rain_projected'

#### PAST DISTRIBUTION  
#calculate M month totals
# started with 6 (current and prior months)
  
# ASSUMES PAST RAIN IS IN FOURTH COLUMN  
x<-ts(data[,4],start=1,end=c(years,12),frequency=12)
x<-c(rep(NA,5),x+lag(x,1)+lag(x,2)+lag(x,3)+lag(x,4)+lag(x,5))
data$sixmnthtot<-x
#data<-na.omit(data)
#### FUTURE RAIN
# ASSUMES FUTURE IS IN COL 5
x2<-ts(data[,5],start=1,end=c(years,12),frequency=12)
x2<-c(rep(NA,5),x2+lag(x2,1)+lag(x2,2)+lag(x2,3)+lag(x2,4)+lag(x2,5))
data$sixmnthtot2<-x2
data<-na.omit(data)
#head(data)
#tail(data)
# rank in percentage terms with respect to the rainfall totals
# for the same sequence of 6-months over all years of record
dataout_final=matrix(nrow=0,ncol=7)

for(i in 1:12){
  #i =1
  # col sixmnthto is the past rain, sixmnthtot2 is the future rain
x<-data[data$month==i,'sixmnthtot']
x2<-data[data$month==i,'sixmnthtot2']

#x<-na.omit(x)
# get distribution of FUTURE RAIN
y <- (rank(x2)-1)/(length(x2)-1)
# checkpct<-cbind(data[data$month==i,],y)
# plot(checkpct$sixmnthtot,checkpct$y)
# rescale between -4 and +4 to replicate palmer index
z <- 8 * (y - .5)
# defaults set the threshold at -1 which is upper limit of mild drought in palmer index (3/8ths, or the 37.5th percentile)
# use future rain < past rain threshold
drought <- x2 <= quantile(x,droughtThreshold)

# calculate the drought index for any months that fall below the threshold
zd <- z * drought
# save out to the data
dataout<-data[data$month==i,]
dataout$index<-z
dataout$indexBelowThreshold<-zd
dataout_final=rbind(dataout_final,dataout)
}

data<-dataout_final[order(dataout_final$date),]

# now calculate the indices
data$count<-as.numeric(0)

for(j in 2:nrow(data)){
  data$count[j] <- ifelse(data$indexBelowThreshold[j]==0,0,
    ifelse(data$indexBelowThreshold[j-1]!=0,1+data$count[j-1],1)
  )
}

# enhanced drought revocation threshold
# In the enhanced version rather than stop counting when the rescaled percentiles rise above -1.0,
# we keep counting the months (or adding the negative anomalies)
# if the rescaled percentile is below 0.0 AND the drought threshold has already been reached.
# If the threshold has not been reached, then stop counting (or adding) as before
# if the rescaled percentile rises above -1.0.

data$count2<-data$count
# j=1080 # 1980-06
# data[j,]

for(j in 2:nrow(data)){
  data$count2[j] <- if(data$count2[j-1] >= 5 & data$index[j] <= 0){
    data$count2[j-1] + 1
  } else {
# ifelse(data$count[j-1] > 0 & data$index[j] < 0, 1+data$count[j-1],
    data$count2[j]
  }
}


data$sums<-as.numeric(0)

for(j in 2:nrow(data)){
data$sums[j]<-ifelse(data$indexBelowThreshold[j]==0,0,
ifelse(data$indexBelowThreshold[j-1]!=0,
data$indexBelowThreshold[j]+data$sums[j-1],
data$indexBelowThreshold[j]))
}


data$sums2<-data$sums
# j=1069 # 1980-06
# data[j,]

for(j in 2:nrow(data)){
data$sums2[j] <- if(data$sums2[j-1] <= -17.5 & data$index[j] <= 0){
data$sums2[j-1] + data$index[j]
} else {
# ifelse(data$count[j-1] > 0 & data$index[j] < 0, 1+data$count[j-1],
data$sums2[j]
}
}
#plot(data$date, data$count, type = "l")
#abline(5,0)
droughtIndices<-data

return(droughtIndices)
}

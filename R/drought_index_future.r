#' @name drought_index_future
#' @title Drought Index For Stations for future projected rainfall
#' @param data a dataframe with date , year, month, rain
#' @param years the number of years in the time series
#' @param droughtThreshold the level of dryness below which a drought begins
#' @return dataframe with droughtIndices
#' @export
#'
drought_index_future <- function(data,years,baseline,droughtThreshold=.375){
# a drought index based on integrated six-monthly rainfall percentiles.
# based on Professor Mike Hutchinson's work described in
# Smith D, Hutchinson M, McArthur R. Climatic and Agricultural Drought: Payments and Policy.
# Canberra, ACT: Centre for Resource and Environmental Studies, Australian National University. 1992.

# Ivan C Hanigan
# June 2011.
  
################################################################################
## Copyright 2011, Ivan C Hanigan <ivan.hanigan@gmail.com> and Michael F Hutchinson
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
################################################################################


# my input data are always a data.frame with 4 columns
# 'date','year','month','rain'
# we want to only use the baseline to get our percentile values  
data_baseline <- data[data$year >= min(baseline) & data$year <= max(baseline),]
#summary(data_baseline)
nyears <- length(names(table(data_baseline$year)))
#calculate M month totals
# started with 6 (current and prior months)

x<-ts(data_baseline[,4],start=1,end=c(nyears,12),frequency=12)
x<-c(rep(NA,5),x+lag(x,1)+lag(x,2)+lag(x,3)+lag(x,4)+lag(x,5))
# TASK need to use rollapply?
data_baseline$sixmnthtot <- x
data_baseline <- na.omit(data_baseline)

nyears2 <- length(names(table(data$year)))
x2<-ts(data[,4],start=1,end=c(nyears2,12),frequency=12)
x2<-c(rep(NA,5),x2+lag(x2,1)+lag(x2,2)+lag(x2,3)+lag(x2,4)+lag(x2,5))
# TASK need to use rollapply?
data$sixmnthtot <- x2
data <- na.omit(data)



# now rank in percentage terms with respect to the rainfall totals 
# for the same sequence of 6-months over all years of record
dataout_final=matrix(nrow=0,ncol=7)

for(i in 1:12){
#  i = 1
        x<-data_baseline[data_baseline$month==i,"sixmnthtot"]
        x2<-data[data$month==i,"sixmnthtot"]
        #x<-na.omit(x)
        # TODO but this is the distribution of the entire series, in and out of the baseline
        y<-(rank(x2)-1)/(length(x2)-1)
        # checkpct<-cbind(data[data$month==i,],y)
        # plot(checkpct$sixmnthtot,checkpct$y)
        # rescale between -4 and +4 to replicate palmer index 
        z<-8*(y-.5)
        # defualts set the threshold at -1 which is upper limit of
        # mild drought in palmer index
        # (3/8ths, or the 37.5th percentile) OF THE BASELINE X
        # TODO so the threshold is on the baseline, but the x2 series is everything
        drought <- x2 <= quantile(x,droughtThreshold)
        # calculate the drought index for any months that fall below the threshold
        # TODO but z is on whole series, but drought is based on exceeding the baseline threshold?
        zd<-z*drought
        # save out to the data
        dataout<-data[data$month==i,]
        dataout$index<-z
        dataout$indexBelowThreshold<-zd
        dataout_final=rbind(dataout_final,dataout)
        }
                
data<-dataout_final[order(dataout_final$date),]

# now calculate the indices
# newnode COUNTS
data$count<-as.numeric(0)
# OLD and SLOW
# for(j in 2:nrow(data)){
        # data$count[j]<-ifelse(data$indexBelowThreshold[j]==0,0,
        # ifelse(data$indexBelowThreshold[j-1]!=0,1+data$count[j-1],
        # 1)
        # )
        # }

# NEW and FAST
# counts can be done with this funky bit of code 
x<-data$index<=-1
xx <- (cumsum(!x) + 1) * x 
x2<-(seq_along(x) - match(xx, xx) + 1) * x 
data$count<-x2

# OLD and SLOW enhanced drought revocation threshold 
# TASK make NEW and FAST? or add as an option?
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
############################################################
# newnode SUMS
# NEW and FAST? or add as an option?
data$sums<-as.numeric(0)
y <- ifelse(data$index >= -1, 0, data$index)
f <- data$index < -1
f <- (cumsum(!f) + 1) * f 
z <- unsplit(lapply(split(y,f),cumsum),f)
data$sums <- z
# OLD and SLOW
# for(j in 2:nrow(data)){
        # data$sums[j]<-ifelse(data$indexBelowThreshold[j]==0,0,
        # ifelse(data$indexBelowThreshold[j-1]!=0,
        # data$indexBelowThreshold[j]+data$sums[j-1],
        # data$indexBelowThreshold[j]))
        # }
        
# OLD and SLOW
# TASK make NEW and FAST
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

droughtIndices<-data
return(droughtIndices)
}

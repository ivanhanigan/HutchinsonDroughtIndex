###############################################################################
# newnode get stations within Vitoria study region
	
 
 load(file.path(absDir,'vicsd.Rdata'))
 epsg <- make_EPSG()
 
 ## Treat data frame as spatial points
 pt.stations <- SpatialPointsDataFrame(cbind(d2$V3,d2$V2),d2,
   proj4string=CRS(epsg$prj4[epsg$code %in% '4283']))
 

 # get distances
 coords <- centroid(sd2[sd2@data$SD_NAME_2006 == 'Goulburn' &  sd2@data$STATE_CODE_2006 == 2,])
 summary(pt.stations)
 dist2pt <- distVincentyEllipsoid(pt.stations,coords)
 
 d <- pt.stations[which(dist2pt<150000),]
 head(d@data)
 
 
###############################################################################
# newnode plot the Victorian SD and stations
	
 d@data 
 
 # make a map of the region
 png(file.path(rootdir,'vicsd.png'),res=200,width = 1500, height = 1000)
 plot(sd2, col = 'grey', xlim = c(140,155))
 box();axis(1);axis(2)
 plot(pt.stations, add = T)
 points(coords)
 # plot(sd2, col = 'darkgrey', add= T)
 plot(d, pch = '*', cex = 2.5, add = T, col = 'red')
 # text(pt.stations$V3,pt.stations$V2,pt.stations$V5,cex=0.5)
 points(coords, pch = 16)
 dev.off()
 
 
###############################################################################
# newnode SD wide average
	

 setwd(bomDir)
 df4v <- matrix(nrow=0,ncol=4)
 for(i in 1:nrow(d@data)){
  # i <- 1
  filename <- paste('0',as.character(d@data[i,1]),sep='')
  if(!file.exists(paste('IDCJAC0001_', filename,'_Data1.csv',sep=''))){ 
   dlMonthly(filename, getwd())
   }
  df <- read.csv(paste('IDCJAC0001_', filename,'_Data1.csv',sep=''))
  df$date <- as.Date(paste(df$Year,df$Month,1,sep='-'))
  df<-subset(df,Quality == 'Y',select=c(date,Year,Month,Monthly.Precipitation.Total..millimetres.))
  head(df)
  fulldaterange <- as.data.frame(seq(min(df$date),max(df$date),1))
  fulldaterange$day <- substring(fulldaterange[,1],9,10)
  fulldaterange <- subset(fulldaterange, day == '01')
  names(fulldaterange) <- c('date','day')
  df2 <- merge(fulldaterange,df,all.x=T)
  df2 <- subset(df2, select = c(date,Year, Month,Monthly.Precipitation.Total..millimetres.))
  # what happens with NAs?
  # stupid impute
  df2$rain <- as.numeric(0)
  # subset(df2,is.na(df2$Monthly.Precipitation.Total..millimetres.))
  df2$month <- substring(df2[,'date'],6,7)
  df2$year  <- substring(df2[,'date'],1,4)
  
    for(i in 1:nrow(df2)){
      # i <- 1
      mm <- df2[i,'month']
      df2$rain[i] <- ifelse(is.na(df2$Monthly.Precipitation.Total..millimetres.[i]), 
      mean(subset(df2, month == mm,select = c(Monthly.Precipitation.Total..millimetres.)),na.rm=T),
    df2$Monthly.Precipitation.Total..millimetres.[i])
      }
  
    tail(df2)
 
  table(df2$year)
  df3 <- subset(df2, year > min(year) & year < max(year), select = c(date, year, month, rain))
  df3$year <- as.numeric(df3$year)
  df3$month <- as.numeric(df3$month)
  
  df4v <- rbind(df4v, df3)
  }
 setwd(rootdir) 
 # newnode average for entire sd
 df5v <- ddply(df4v, c('date','year', 'month'), function(df)mean(df$rain))
 names(df5v) <- c('date',  'year' , 'month' ,'rain')

 
###############################################################################
# newnode Seymour drought index
	
  

 drt <- droughtIndex(data=df5v,years=length(names(table(df5v$year))))
 qc3=drt[drt$year>=1966 & drt$year < 1986,]
 
 png(file.path(rootdir,'SeymourDrought6686.png'),res=200,width = 1500, height = 1000)
 par(mfrow=c(4,1),mar=c(2.5,2,1.5,1))
 plot(qc3$date,qc3$rain,type='l',main='Seymour VIC: raw monthly rainfall')
 #points(qc3$date,qc3$rain)
 axis(1,at=as.Date(paste(1966:1985,1,1,sep='-')), labels = 1966:1985)
 lines(qc3$date,qc3$sixmnthtot/6, lwd = 2) #,type='l',main='6-monthly total rainfall')
 points(qc3$date,qc3$sixmnthtot/6)
 axis(1,at=as.Date(paste(1966:1985,1,1,sep='-')), labels = 1966:1985) 
 plot(qc3$date,qc3$index,type='l',main='rescaled percentiles -4 to +4, -1 is Palmer Index Mild Drought',ylim=c(-4,4))
 points(qc3$date,qc3$index)
 segments(min(qc3$date),-1,max(qc3$date),-1)
 segments(min(qc3$date),0,max(qc3$date),0,lty=2)
 plot(qc3$date,qc3$sums,type='l',main='sums below -1 threshold, sums of -17.5 or less is a drought')
 points(qc3$date,qc3$sums)
 segments(min(qc3$date),-17.5,max(qc3$date),-17.5)
 axis(1,at=as.Date(paste(1966:1985,1,1,sep='-')), labels = 1966:1985) 
 plot(qc3$date,qc3$sums2,type='l',main='enhanced sums of months if already passed threshold of -17.5 and percentiles less than 50%')
 points(qc3$date,qc3$sums2)
 segments(min(qc3$date),-17.5,max(qc3$date),-17.5)
 axis(1,at=as.Date(paste(1966:1985,1,1,sep='-')), labels = 1966:1985)
 dev.off()

 
###############################################################################
# newnode Integration by Conditional Summation
	

 # when is there an example of the enhancement making a drought longer?
  tail(drt[drt$sums2!=drt$sums,])
 # plot this one
  qc3=drt[drt$year>=1994 & drt$year < 1999,]
 
 png(file.path(rootdir,'SeymourDrought9499enhanced.png'),res=200,width = 2100, height = 1000)
 par(mfrow=c(4,1),mar=c(2.5,2,1.5,1))
 plot(qc3$date,qc3$rain,type='l',main='Seymour VIC: raw monthly rainfall')
 #points(qc3$date,qc3$rain)
 axis(1,at=as.Date(paste(1994:1998,1,1,sep='-')), labels = 1994:1998)
 lines(qc3$date,qc3$sixmnthtot/6, lwd = 2) #,type='l',main='6-monthly total rainfall')
 points(qc3$date,qc3$sixmnthtot/6)
 axis(1,at=as.Date(paste(1994:1998,1,1,sep='-')), labels = 1994:1998) 
 plot(qc3$date,qc3$index,type='l',main='rescaled percentiles -4 to +4, -1 is Palmer Index Mild Drought',ylim=c(-4,4))
 points(qc3$date,qc3$index)
 segments(min(qc3$date),-1,max(qc3$date),-1)
 segments(min(qc3$date),0,max(qc3$date),0,lty=2)
 plot(qc3$date,qc3$sums,type='l',main='sums below -1 threshold, sums of -17.5 or less is a drought')
 points(qc3$date,qc3$sums)
 segments(min(qc3$date),-17.5,max(qc3$date),-17.5)
 axis(1,at=as.Date(paste(1994:1998,1,1,sep='-')), labels = 1994:1998) 
 plot(qc3$date,qc3$sums2,type='l',main='enhanced sums of months if already passed threshold of -17.5 and percentiles less than 50%')
 points(qc3$date,qc3$sums2)
 segments(min(qc3$date),-17.5,max(qc3$date),-17.5)
 axis(1,at=as.Date(paste(1994:1998,1,1,sep='-')), labels = 1994:1998)
 dev.off()
 
 

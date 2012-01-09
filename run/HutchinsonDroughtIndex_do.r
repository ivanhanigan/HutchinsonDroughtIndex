###############################################################################
# newnode Calculate the drought index
	
  

 drt <- droughtIndex(data=df5,years=length(names(table(df5$year))))
 qc3=drt[drt$year>=1979 & drt$year < 1984,]
 
 png(file.path(rootdir,'CentralWestDrought8283.png'),res=200,width = 2100, height = 1000)
 par(mfrow=c(4,1),mar=c(2.5,2,1.5,1))
 plot(qc3$date,qc3$rain,type='l',main='Central West NSW: raw monthly rainfall')
 #points(qc3$date,qc3$rain)
 
 lines(qc3$date,qc3$sixmnthtot/6, lwd = 2) #,type='l',main='6-monthly total rainfall')
 points(qc3$date,qc3$sixmnthtot/6)
 
 plot(qc3$date,qc3$index,type='l',main='Rescaled percentiles -4 to +4, -1 is Palmer Index Mild Drought',ylim=c(-4,4))
 points(qc3$date,qc3$index)
 segments(min(qc3$date),-1,max(qc3$date),-1)
 segments(min(qc3$date),0,max(qc3$date),0,lty=2)
 plot(qc3$date,qc3$count,type='l',main='Counts below -1 threshold, count of 5 or more is a drought')
 points(qc3$date,qc3$count)
 segments(min(qc3$date),5,max(qc3$date),5)
 
 plot(qc3$date,qc3$count2,type='l',main='Enhanced counts of months if already passed count of 5 and percentiles less than 50%')
 points(qc3$date,qc3$count2)
 segments(min(qc3$date),5,max(qc3$date),5)
 dev.off()
 
 
###############################################################################
# newnode replicate Fig3.5 from Hutchinson
	


###############################################################################
# newnode Seymour drought index
	
  

 drt <- droughtIndex(data=df5,years=length(names(table(df5$year))))
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
 
 

###############################################################################
# newnode Calculate the drought index
	
  

 drt <- droughtIndex(data=df5,years=length(names(table(df5$year))))
 qc3=drt[drt$year>=1979 & drt$year < 1984,]
 
 png(file.path(rootdir,'reports','CentralWestDrought8283.png'),res=200,width = 2100, height = 1000)
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
	
 setwd(rootdir)
 source('run/HutchinsonDroughtIndex_load_vic.r')
 

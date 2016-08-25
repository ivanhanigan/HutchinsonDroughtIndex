library(HutchinsonDroughtIndex)
projdir <- "~/projects/HutchinsonDroughtIndex"
setwd(projdir)
dir()
indir  <- "data/ad_hoc"
infile <- "databag.csv"

dat  <- read.delim(file.path(indir,infile), sep = ";")
str(dat)
dat$date  <- as.Date(paste(dat$YEAR, dat$MONTH, 1, sep = "-"))
plot(dat$date, dat$precip, type = "l")
# check there are 12 obs per year
table(dat$YEAR)



names(dat)
dat <- dat[,c("date","YEAR","MONTH","precip")]
names(dat) <- c("date","year","month","rain")
tail(dat)
head(dat)

##############################################
# do the drought algorithm
drt <- drought_index_stations(data=dat,years=length(names(table(dat$year))))
head(drt)
par(mfrow = c(2,1))
plot(drt$date, drt$rain, type = "l")
plot(drt$date, drt$count2, type = "l")
segments(min(drt$date), 5, max(drt$date), 5)

write.csv(drt, file.path('data/ad_hoc/databag_drought_20160825.csv'), row.names = F)

# QC
# http://www.cbsnews.com/news/droughts-the-next-great-threat-to-iraq/
# this says drought from 2007 to 2010
qc <- drt[drt$year>=2000 & drt$year < 2013,]
qc


png(file.path('data/ad_hoc','databag.png'),res=200,width = 2100, height = 1000)
par(mfrow=c(4,1),mar=c(2.5,2,1.5,1))
plot(qc$date,qc$rain,type='l',main='Data bag: raw monthly rainfall')
#points(qc$date,qc$rain)

lines(qc$date,qc$sixmnthtot/6, lwd = 2) #,type='l',main='6-monthly total rainfall')
points(qc$date,qc$sixmnthtot/6)

plot(qc$date,qc$index,type='l',main='Rescaled percentiles -4 to +4, -1 is Palmer Index Mild Drought',ylim=c(-4,4))
points(qc$date,qc$index)
segments(min(qc$date),-1,max(qc$date),-1)
segments(min(qc$date),0,max(qc$date),0,lty=2)
plot(qc$date,qc$count,type='l',main='Counts below -1 threshold, count of 5 or more is a drought')
points(qc$date,qc$count)
segments(min(qc$date),5,max(qc$date),5)

plot(qc$date,qc$count2,type='l',main='Enhanced counts of months if already passed count of 5 and percentiles less than 50%')
points(qc$date,qc$count2)
segments(min(qc$date),5,max(qc$date),5)
dev.off()

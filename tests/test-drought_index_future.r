
################################################################
# name:drought_index_stations
analyte <- read.table("~/data/HutchinsonDroughtIndex/inst/extdata/prcphq.046037.month.txt", quote="\"", skip = 1, nrows = 1440)

# clean
str(analyte)
head(analyte);tail(analyte)

analyte <- data.frame(analyte[,1], substr(analyte[,1], 1,4) , substr(analyte[,1],5,6), analyte[,3])
names(analyte) <- c('date',  'year' , 'month' ,'rain')
str(analyte)
analyte$year <- as.numeric(as.character(analyte$year))
analyte$month <- as.numeric(as.character(analyte$month))
str(analyte)
subset(data.frame(table(na.omit(analyte)[,"year"])), Freq < 12)
# are all months present?

# do
drt <- drought_index_stations(data=analyte,years=length(names(table(analyte$year))),droughtThreshold=.375)

# report
summary(drt)
with(drt, plot(as.Date(date), count, "l"))
abline(5,0)
par(new=T)
with(drt, plot(as.Date(date), -1*sums, col= "red", type="l"))

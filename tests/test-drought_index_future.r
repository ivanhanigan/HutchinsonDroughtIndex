
################################################################
# name:drought_index_stations
# for info see
# https://github.com/ivanhanigan/GARNAUT_CLIMATE_CHANGE_REVIEW
# drought futures sub project

## dat <- read.csv("~/projects/GARNAUT_CLIMATE_CHANGE_REVIEW/drought_futures/data/rain_future_estimated_dry.csv", stringsAsFactors = F)

## names(dat)
## head(dat)
## tail(dat)
## dat$date <- as.Date(paste(dat$year, dat$month, 1, sep = "-"))

## sds <- names(table(dat$sd_group))
## sds

## # save a test dataset for developing the fucntion with, transfer to
## # hutch package
## sd_i <- c("Central West", "Murrumbidgee")
## dat2 <- dat[dat$year > 1890 & dat$sd_group %in% sd_i, c('sd_group','date','year','month','avrain')]
## summary(dat2)
## table(dat2$sd_group)
## head(dat2, 24)
## par(mfrow = c(2,1))
## for(sdi in sd_i){
##   with(dat2[dat2$sd_group == sdi,],
##        plot(date, avrain, type = "l")
##        )
##   title(sdi)
## }
## write.csv(dat2, "~/projects/HutchinsonDroughtIndex/inst/extdata/GARNAUT_CLIMATE_CHANGE_drought_futures_dry_southwest_slopes_sd07.csv", row.names = F)

library(HutchinsonDroughtIndex)

analyte <- read.csv("~/projects/HutchinsonDroughtIndex/inst/extdata/GARNAUT_CLIMATE_CHANGE_drought_futures_dry_southwest_slopes_sd07.csv")

# clean
str(analyte)
head(analyte);tail(analyte)

analyte  <- analyte[analyte$sd_group == "Murrumbidgee", c("date", "year", "month","avrain")]

# do
## drt <- drought_index_future(
##   data=analyte
##   ,
##   baseline = c(1891, 2008)
##   ,
##   years=length(names(table(analyte$year)))
##   ,
##   droughtThreshold=.375
##   )

## # report
## par(mfrow = c(2,1))
## summary(drt)
## with(drt[drt$year > 1980 & drt$year <2010,], plot(as.Date(date), count, "l"))
## abline(5,0)

analyte2 <- analyte[analyte$year < 2009,]
drt2 <- drought_index_stations(
  data=analyte2
  ,
  years=length(names(table(analyte2$year)))
  ,
  droughtThreshold=.375
  )
with(drt2[drt2$year > 1980 & drt2$year <2010,], plot(as.Date(date), count, "l"))
abline(5,0)

dev.off()
#par(new=T)
#with(drt, plot(as.Date(date), -1*sums, col= "red", type="l"))


################################################################
# name:drought_index_stations
# for info see
# https://github.com/ivanhanigan/GARNAUT_CLIMATE_CHANGE_REVIEW
# drought futures sub project

## dat <- read.csv("~/projects/GARNAUT_CLIMATE_CHANGE_REVIEW/drought_futures/data/rain_future_estimated_dry.csv", stringsAsFactors = F)

## # drop the first year as only half
## names(dat)

## head(dat)
## dat$date <- as.Date(paste(dat$year_future, dat$month, 1, sep = "-"))

## sds <- names(table(dat$sd_group))
## sds

## # save a test dataset for developing the fucntion with, transfer to
## # hutch package
## sd_i <- "Central West"
## dat2 <- dat[dat$year > 1890 & dat$sd_group == sd_i, c('date','year_future','month','avrain','rain_projected')]
## summary(dat2)
## head(dat2, 24)
## plot(dat2$date, dat2$avrain, type = "l")
# write.csv(dat2, "~/projects/HutchinsonDroughtIndex/inst/extdata/GARNAUT_CLIMATE_CHANGE_drought_futures_dry_central_west_sd07.csv", row.names = F)


analyte <- read.csv("~/projects/HutchinsonDroughtIndex/inst/extdata/GARNAUT_CLIMATE_CHANGE_drought_futures_dry_central_west_sd07.csv")

# clean
str(analyte)
head(analyte);tail(analyte)

# do
drt <- drought_index_future(data=analyte,years=length(names(table(analyte$year_future))),droughtThreshold=.375)

# report
summary(drt)
with(drt, plot(as.Date(date), count, "l"))
abline(5,0)
par(new=T)
with(drt, plot(as.Date(date), -1*sums, col= "red", type="l"))

###############################################################################
# newnode create working dir
	
 outdir <- '~/data/drought/HutchinsonDroughtIndex'
 dir.create(outdir, recursive = T)
 setwd(outdir)
 
###############################################################################
# newnode create download directories
	
 
 # create a data storage directory to store downloaded data
 
  bomDir <- file.path('data/bom_HQ_monthly_prcp')
  dir.create(bomDir, recursive = T)
  
  absDir <- file.path('data/abs_sd')
  dir.create(absDir, recursive = T)
  
  # and remember the project root directory
  rootdir <- getwd()
  
 
###############################################################################
# newnode Download spatial data
	

  # newnode data download notes.
  # newnode change work dir to download area
 if(!file.exists(file.path(absDir,'aussd.Rdata'))){ 
  setwd(absDir)
  
  download.file('http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&SD06aAUST.zip&1259.0.30.002&Data%20Cubes&56AEC033DFC11A5CCA2571BF007E5185&0&2006&04.08.2006&Latest', 'SD06aAUST.zip', mode = 'wb')
  unzip('SD06aAUST.zip',junkpaths=T)
  
  sink('readme.txt')
  cat(paste('Australian Bureau of Statistics Statistical Divisions 2006 
  downloaded on', Sys.Date(),
  '
  from http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1259.0.30.0022006?OpenDocument')
  )
  sink()
  
  # and load
  sd <- readOGR('SD06aAUST.mif', layer = 'SD06aAUST')
  # might take a while
  head(sd@data)
  plot(sd)
  dev.off()
  save.image('aussd.Rdata')

 } else {
  # OR if already loaded 
  setwd(absDir)
  load('aussd.Rdata')
}
# NB You may want to change the code to calculate the index for another type of ABS spatial unit.  If so you can find data at http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1259.0.30.0022006?OpenDocument

 
###############################################################################
# newnode subset the SDs to NSW
	
 # sd@data$SD_NAME_2006 == 'Central West' & 
 sd2 <-  sd[ sd@data$STATE_CODE_2006 == 1,]
 plot(sd2)
 axis(1);axis(2); box()
 plot(sd, add = T)
 names(sd2@data)
 # writeOGR(sd2,'centralwestsd.shp','centralwestsd','ESRI Shapefile')
 # test <- readShapePoly('centralwestsd.sd')
 # not work? ignore
 rm(sd)

 # newnode get the centroid of the Central West
 coords <- centroid(sd2[sd2@data$SD_NAME_2006 == 'Central West' &  sd2@data$STATE_CODE_2006 == 1,])

 save.image('nswsd.Rdata')

 
###############################################################################
# newnode subset the SDs to Vic
	
 load('aussd.Rdata')

 sd2 <-  sd[ sd@data$STATE_CODE_2006 == 2,]
 plot(sd2)
 axis(1);axis(2); box()
 # Look up Seymour coordinates from Wikipedia
 points(145.13, -37.03, pch = 16)
 names(sd2@data)
 sd3 <- sd2[sd2@data$SD_NAME_2006 == 'Goulburn' &  sd2@data$STATE_CODE_2006 == 2,]
 plot(sd3, add = T, col = 'grey')
 points(145.13, -37.03, pch = 16)

 rm(sd)

 
 # newnode get the centroid of the Seymour SD
 coords <- centroid(sd3)
 
 save.image('vicsd.Rdata')
  
 setwd(rootdir)
 
###############################################################################
# newnode Download the Rainfall Station location data
	
if(!file.exists(file.path(bomDir,'HQ_monthly_prcp_stations.csv'))){ 
     
 setwd(bomDir)
 sink('readme.txt')
 cat(paste('Bureau of Meteorology High Quality Monthly precipitation data 
 downloaded on', Sys.Date(),
 '
 from ftp://ftp.bom.gov.au/anon/home/ncc/www/change/HQmonthlyR/HQ_monthly_prcp_txt.tar')
 )
 sink()
 download.file('ftp://ftp.bom.gov.au/anon/home/ncc/www/change/HQmonthlyR/HQ_monthly_prcp_txt.tar','HQ_monthly_prcp_txt.tar',mode='wb')
 untar('HQ_monthly_prcp_txt.tar', exdir= 'HQ_monthly_prcp_txt')

 # check
 d <- read.table('HQ_monthly_prcp_txt/HQMR_stations.txt',header=F,skip=0,nrow=1,as.is=T)
 d
 # ok fixed width
 nchar(d)
 
 # V1 V2 V3 V4 V5 V6 
  # 4  6  6  2  9 11 
 # actually not correct
 widths <- c(7,7,7,7,41)
 
 d2 <- read.fwf('HQ_monthly_prcp_txt/HQMR_stations.txt',widths=widths,header=F,skip=0,as.is=T,comment.char='|',strip.white=T)
 str(d2)
 head(d2)
 tail(d2)
 write.csv(d2,'HQ_monthly_prcp_stations.csv', row.names = F)
 } else {
 setwd(bomDir)
 d2 <- read.csv('HQ_monthly_prcp_stations.csv')
 }
 
 
###############################################################################
# newnode revert to project root dir
	
 setwd(rootdir)
 
###############################################################################
# newnode get stations within study region
	
 
 load(file.path(absDir,'nswsd.Rdata'))
 epsg <- make_EPSG()
 
 d2 <- read.csv(file.path(bomDir,'HQ_monthly_prcp_stations.csv'))
 ## Treat data frame as spatial points
 pt.stations <- SpatialPointsDataFrame(cbind(d2$V3,d2$V2),d2,
   proj4string=CRS(epsg$prj4[epsg$code %in% '4283']))
 

 # get distances
 coords <- centroid(sd2[sd2@data$SD_NAME_2006 == 'Central West' &  sd2@data$STATE_CODE_2006 == 1,])
 summary(pt.stations)
 dist2pt <- distVincentyEllipsoid(pt.stations,coords)
 
 d <- pt.stations[which(dist2pt<150000),]
 head(d@data)

 
###############################################################################
# newnode go for a SD wide average rainfall using these stations
	

 setwd(bomDir)
 df4 <- matrix(nrow=0,ncol=4)
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
  
  df4 <- rbind(df4, df3)
  }
 setwd(rootdir) 
 # newnode average for entire sd
 df5 <- ddply(df4, c('date','year', 'month'), function(df)mean(df$rain))
 names(df5) <- c('date',  'year' , 'month' ,'rain')

 

###############################################################################
# newnode Drought tools
	

 if (!require(rgdal)) install.packages('rgdal'); require(rgdal)
 if (!require(geosphere)) install.packages('geosphere'); require(geosphere)
 if (!require(plyr)) install.packages('plyr'); require(plyr)

 # the drought index
 source('src/HutchinsonDroughtIndex_tools_droughtIndex.r')

 
###############################################################################
# newnode dlMonthly
	
 
 dlMonthly <- function(site, dataDir){
  # a function designed to download up to date rainfall station data from bom website
  # site = filename
  wd <- getwd()
  setwd(dataDir)
  # the following appears broken 2012-10-02
  #readLines(sprintf('http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=139&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=%s',site),
  #n=1)
#   download.file(sprintf('http://www.bom.gov.au/tmp/cdio/IDCJAC0001_%s.zip',site),
#   sprintf('IDCJAC0001_%s.zip',site))
   # system(sprintf('sh getZipContents.sh IDCJAC0001_%s.zip',site))
   #unzip(sprintf('IDCJAC0001_%s.zip',site),junkpaths=T)  
   if(!file.exists(sprintf('prcphq.%s.month.txt',site))){
   system(sprintf('zcat prcphq.%s.month.txt.Z > prcphq.%s.month.txt',site,site))
   }
   # calculate nrows
   n <-  read.table(sprintf('prcphq.%s.month.txt',site), header=F, sep="", skip=0, nrows = 1)
   strtyy <- substr(n$V3, 1, 4)
   strtmm <- substr(n$V3, 5, 6)
   endyy <- substr(n$V4, 1, 4)
   endmm <- substr(n$V4, 5, 6)
   daily <- seq(as.Date(paste(strtyy, strtmm, 1, sep = '-')),as.Date(paste(endyy, endmm, 1, sep = '-')), 1)
   n <- length(names(table(paste(format(daily, format= '%Y'), format(daily, format= '%m'),sep = '-'))))
   df <- read.table(sprintf('prcphq.%s.month.txt',site), header=F, sep="", skip=1, nrows = n)
   df$Year <- substr(df$V1, 1, 4)
   df$Month <- substr(df$V1, 5, 6)
   df$Monthly.Precipitation.Total..millimetres. <- df$V3
   write.csv(df[,c('Year', 'Month', 'Monthly.Precipitation.Total..millimetres.')], file.path(wd,sprintf('IDCJAC0001_%s_Data1.csv',site)), row.names=F)
   setwd(wd)
  }

 

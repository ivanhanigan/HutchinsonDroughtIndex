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
  #download.file(sprintf('http://www.bom.gov.au/tmp/cdio/IDCJAC0001_%s.zip',site),
  # sprintf('IDCJAC0001_%s.zip',site))
   # system(sprintf('sh getZipContents.sh IDCJAC0001_%s.zip',site))
   #unzip(sprintf('IDCJAC0001_%s.zip',site),junkpaths=T)  
  
   system(sprintf('uncompress prcphq.%s.month.txt.Z',site)) 
   setwd(wd)
  }

 

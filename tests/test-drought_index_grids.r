
################################################################
# name:drought_index_grids
wd <- getwd()
setwd("~/data/AWAP_GRIDS/data")
##Lu 13-14 Jan 2014
require(raster); require(rgdal)
##path?
awap.grids = dir(pattern = "grid$", full.names=T)
#  list.files('AWAP_GRIDS', pattern=glob2rx('totals*.grid'), full.names=T)
for(i in 1:12){
  #i = 1
  #file.copy(awap.grids[i], sprintf("foo%s.grid", i))}
  r <- raster(awap.grids[i])
  #str(r)
  #image(r)
  fname <- gsub(".grid",".tif", awap.grids[i])
  # TODO project this please lu!
  writeRaster(r, filename= fname, type = "GTiff")
  #file.remove(awap.grids[i])
}
## for some reason brick or stack only don't work, both together do
awap.grids <- dir(pattern = 'tif')[1:12]
rb <- brick(stack(awap.grids)) #takes too l

## I'm not sure what's more efficient, if changing the drought function 
## to do the cal on matrices or just running the function on the vectors

##option 1 modif function
ct <- drought_index_grids(rasterbrick = rb,startyear = 1900, endyear=1900, droughtThreshold=.375)
plot(ct[,1], type = "l")

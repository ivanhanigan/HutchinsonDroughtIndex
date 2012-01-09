###############################################################################
# newnode Plot the NSW SD and stations
	
 
 d@data 
 
 # make a map of the region
 png(file.path(rootdir,'nswsds.png'),res=200,width = 1500, height = 1000)
 plot(sd2, col = 'grey', xlim = c(140,155))
 box();axis(1);axis(2)
 plot(pt.stations, add = T)
 points(coords)
 # plot(sd2, col = 'darkgrey', add= T)
 plot(d, pch = '*', cex = 2.5, add = T, col = 'red')
 # text(pt.stations$V3,pt.stations$V2,pt.stations$V5,cex=0.5)
 points(coords, pch = 16)
 dev.off()
 
 

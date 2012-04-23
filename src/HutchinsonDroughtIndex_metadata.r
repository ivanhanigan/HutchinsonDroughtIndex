###############################################################################
# newnode metadata
	
 source('~/My Dropbox/tools/df2ddi/df2ddi.r')
 source('~/My Dropbox/tools/connectDelphe.r')
 # ch <- connectDelphe('130.56.102.41','delphe','ivan_hanigan')  
 oracle <- connectOracle(hostip='150.203.74.97',user='ivan') 
 idno <- 'HUTCHINSONDROUGHTINDEX'
 if(!exists('s')){ s <- dbGetQuery(oracle, paste("select * from stdydscr where idno = '",idno,"'", sep = ''))
 idno <- s$IDNO
 }
 
###############################################################################
# newnode initialise study id
	
 # initialise study id
 dir.create('metadata')
 s <- add_stdydscr(ask=T)
 s$PRODDATEDOC <- '9-JAN-12' # TASK set to take oracle format
 idno <- s$IDNO
 write.table(s,'metadata/stdydscr.csv',sep=',',row.names=F)
 dbWriteTable(oracle,  'NUSTDY', s)
 inserts()

 dbSendUpdate(oracle,  
 '
 insert into ivan.stdydscr (TITL, IDNO, PRODUCER, PRODDATEDOC, BIBLCITDOC, AUTHENTY, COPYRIGHT, PRODDATESTDY, FUNDAG, DISTRBTR, SERNAME, VERSION, BIBLCITSTDY, TIMEPRD, COLLDATE, GEOGCOVER, GEOGUNIT, ANLYUNIT, UNIVERSE, DATAKIND, CLEANOPS, CONFDEC, SPECPERM, RESTRCTN, NOTES, ABSTRACT)
 select TITL, IDNO, PRODUCER, to_date(PRODDATEDOC), BIBLCITDOC, AUTHENTY, COPYRIGHT, to_date(PRODDATESTDY), FUNDAG, DISTRBTR, SERNAME, VERSION, BIBLCITSTDY, TIMEPRD, COLLDATE, GEOGCOVER, GEOGUNIT, ANLYUNIT, UNIVERSE, DATAKIND, CLEANOPS, CONFDEC, SPECPERM, RESTRCTN, NOTES, ABSTRACT from nustdy
 ')
 
 dbSendUpdate(oracle,'drop table nustdy')

 
###############################################################################
# newnode add metadata for the files
	


 f <- add_filedscr(fileid = 1, idno = s$IDNO, ask=T)
 f$FILELOCATION <- 'I:/data/Drought/HutchinsonDroughtIndex' 
 write.table(f,'metadata/filedscr.csv',sep=',',row.names=F)
 
 f <- add_filedscr(fileid = 2, idno = s$IDNO, ask=T)
 f$FILELOCATION <- 'https://github.com/ivanhanigan/HutchinsonDroughtIndex' 
 f$NOTES <- 'Test of github'
 

 # d <- add_datadscr(data_frame = d, fileid = 2, ask=T)
 # loop thru a series of filenames all with same structure 
 # j=3
 # for(town in c('sydney','newcastle')){
 # # town <- 'perth'
 # j <- j +1
 # f <- rbind(f, gsub('wollongong', town, f[1,]))
 # f$FILEID[j-1] <- j
 # d2 <- d
 # d2$FILEID <- j
 # d <- rbind(d, d2) 
 
 # }
 f
 write.table(f,'metadata/filedscr.csv',sep=',',row.names=F, col.names=F, append=T)
# write.table(d,'metadata/datadscr.csv',sep=',',row.names=F)
 
 
###############################################################################
# newnode include OTHRSTDYMAT
	

 dir.create('references')
 file.copy(dir('~/My Dropbox/references', pattern = 'Hutchinson', full.names = T), file.path('references',dir('~/My Dropbox/references', pattern = 'Hutchinson')))
  f <- add_filedscr(fileid = 3, idno = s$IDNO, ask=T)
 f$FILELOCATION <- 'https://github.com/ivanhanigan/HutchinsonDroughtIndex' 
 f$NOTES <- 'Smith, D. I, Hutchinson, M. F, & McArthur, R. J. (1992) Climatic and
 Agricultural Drought: Payments and Policy. (Centre for Resource and Environmental
 Studies, Australian National University, Canberra, Australia).'
 f
 write.table(f,'metadata/filedscr.csv',sep=',',row.names=F, col.names=F, append=T)
 
 
###############################################################################
# newnode include data
	
 
 # newnode abs data
 f <- add_filedscr(fileid = 4, idno = s$IDNO, ask=T)
 f$FILELOCATION <- 'https://github.com/ivanhanigan/HutchinsonDroughtIndex/data/abs_sd' 
 f
 write.table(f,'metadata/filedscr.csv',sep=',',row.names=F, col.names=F, append=T)
 # can do this after run project
 load('data/abs_sd/nswsd.Rdata')
 ls()
 d <- add_datadscr(data_frame = sd2@data, fileid = 4, ask=T)
 write.table(d,'metadata/datadscr.csv',sep=',',row.names=F)

 f <- add_filedscr(fileid = 5, idno = s$IDNO, ask=T)
 f$FILELOCATION <- 'https://github.com/ivanhanigan/HutchinsonDroughtIndex/data/bom_HQ_monthly_prcp' 
 f
 write.table(f,'metadata/filedscr.csv',sep=',',row.names=F, col.names=F, append=T)
 # newnode bom data
 filname <- dir('data/bom_HQ_monthly_prcp', pattern = 'HQ_monthly_prcp_stations.csv', full.names=T)
 df <-  read.csv(filname)
 d <- add_datadscr(data_frame = df, fileid = 5, ask=T)
 write.table(d,'metadata/datadscr.csv',sep=',',row.names=F, col.names=F, append=T)

 
###############################################################################
# newnode add metadata for files to oracle
	
 
 f<-read.table('metadata/filedscr.csv',as.is=T,sep=',',header=T)
 f2 <- as.data.frame(matrix(nrow = 0, ncol=ncol(f)))
 for(i in 1:nrow(f)){
 f2 <- rbind(f2,as.data.frame(t(unlist(ifelse(is.na(f[i,]),'',f[i,])  ))))
 }
 names(f2) <- names(f)
 
 replaceDDI <- F
 if(replaceDDI == T) { dbSendUpdate(oracle, sprintf("delete from filedscr where idno = '%s'",idno))}
 extant <- dbGetQuery(oracle, sprintf("select * from filedscr where idno = '%s'",idno))
 
 if(nrow(extant) == 0){
  dbWriteTable(oracle, 'NUFILES', f2)
  dbSendUpdate(oracle,
  'insert into ivan.filedscr (IDNO, FILENAME, FILETYPE, PROCSTAT, SPECPERMFILE, DATEARCHIVED, DATEDESTROY, FILEDSCR, NOTES, REQID, PUBLISHDDI, BACKUPVALID, DATEBACKUPVALID, CHECKED, BACKUPLOCATION, FILEID, FILELOCATION)
  select IDNO, FILENAME, FILETYPE, PROCSTAT, SPECPERMFILE, to_date(DATEARCHIVED), DATEDESTROY, FILEDSCR, NOTES, REQID, PUBLISHDDI, BACKUPVALID, to_date(DATEBACKUPVALID), CHECKED, BACKUPLOCATION, FILEID, FILELOCATION from nufiles
  ')
  dbSendUpdate(oracle,'
  drop table nufiles
  ')

  } else {
 
  for(i in 1:nrow(f2)){
   #i <- 1
   print(f2$FILENAME[i])
   if(length(grep(f2$FILENAME[i], extant$FILENAME)) != 0) {next}
   dbWriteTable(oracle, 'NUFILES', f2[i,])
   dbSendUpdate(oracle,
   'insert into ivan.filedscr (IDNO, FILENAME, FILETYPE, PROCSTAT, SPECPERMFILE, DATEARCHIVED, DATEDESTROY, FILEDSCR, NOTES, REQID, PUBLISHDDI, BACKUPVALID, DATEBACKUPVALID, CHECKED, BACKUPLOCATION, FILEID, FILELOCATION)
   select IDNO, FILENAME, FILETYPE, PROCSTAT, SPECPERMFILE, to_date(DATEARCHIVED), DATEDESTROY, FILEDSCR, NOTES, REQID, PUBLISHDDI, BACKUPVALID, to_date(DATEBACKUPVALID), CHECKED, BACKUPLOCATION, FILEID, FILELOCATION from nufiles
   ')
   dbSendUpdate(oracle,'
   drop table nufiles
   ')
   }
  }
  
 
###############################################################################
# newnode add metadata for data to oracle
	
 
 
 
 # NOW NEED TO IDENTIFY ID NUMBERS
 dbGetQuery(oracle,paste(
  "
  SELECT IDNO, min(FILEID), max(FILEID) FROM FILEDSCR 
  WHERE IDNO = '",idno,"'
  group by idno
  ", sep='')
  )

 # FILEIDS ARE 
 minfileid <- 3102
 maxfileid <- 3103
 fileids <- seq(minfileid,maxfileid)
 
 datarows <- read.csv('metadata/datadscr.csv')
 names(table(datarows$FILEID))
 for(i in 1:length(names(table(datarows$FILEID)))){
  rows <- names(table(datarows$FILEID))[i]
  fid<-fileids[i]
  cat(paste('insert into ivan.datadscr (',
  paste(names(read.csv(dir('metadata',full.names=T)[grep('datadscr.csv',dir('metadata',full.names=T))])),sep='',collapse=', '),
  ')
  
  select ',
  gsub('FILEID',fid,paste(names(read.csv(dir('metadata',full.names=T)[grep('datadscr.csv',dir('metadata',full.names=T))])),sep='',collapse=', ')),
  ' from nudata
  WHERE FILEID = ',rows,';
  ',
  sep='')
  )
  }


 
 # upload the data table
 nudata <- read.csv('metadata/datadscr.csv')
 nudata
 dbWriteTable(oracle,'NUDATA', nudata)
 
 dbSendUpdate(oracle,
 'insert into ivan.datadscr (LABL, NOTES, SPECPERMVAR, FILEID)
 select LABL, NOTES, SPECPERMVAR, 3102 from nudata
 WHERE FILEID = 4
 ')
 
 dbSendUpdate(oracle,
 'insert into ivan.datadscr (LABL, NOTES, SPECPERMVAR, FILEID)
 select LABL, NOTES, SPECPERMVAR, 3103 from nudata
 WHERE FILEID = 5
 ')

 
 dbSendUpdate(oracle,
 'drop table nudata
 ')
 
 
###############################################################################
# newnode oracle2xml-makeTex
	
 
 setwd('I:/My Dropbox/projects/0.3 Catalogue/')
 
 # run I:/My Dropbox/projects/0.3 Catalogue/oracle2xml-makeTex.r 
 setwd(wd)

 
###############################################################################
# newnode create catalogue and ddi xmls
	
 oldwd <- getwd()
 setwd('I:/My Dropbox/projects/0.3 Catalogue/')
 setwd(oldwd)
 
###############################################################################
# newnode synchronise local metadata
	
 
 s <- dbGetQuery(oracle, paste("select * from stdydscr where idno = '",idno,"'", sep = ''))
 matrix(s)
 f <- dbGetQuery(oracle, paste("select * from filedscr where idno = '",idno,"'", sep = ''))
 f

 d <- dbGetQuery(oracle, paste("select * from datadscr where fileid in (",paste(f$FILEID, collapse = ','),")", sep = ''))
 d

 # now overwrite the local copies
 dir('metadata')
 write.csv(s, 'metadata/stdydscr.csv', row.names=F)
 write.csv(f, 'metadata/filedscr.csv', row.names=F)
 write.csv(d, 'metadata/datadscr.csv', row.names=F)


 doclist <- dir(file.path('I:/My Dropbox/projects/0.3 Catalogue/publishddi',idno), pattern = tolower(idno))
 doclist
 
 for(doc in doclist){
 file.copy(file.path('I:/My Dropbox/projects/0.3 Catalogue/publishddi',idno,doc), file.path('metadata',doc), overwrite = T)
 }
 
 doc <- dir(file.path('I:/My Dropbox/projects/0.3 Catalogue/publishddi',idno,'reports'), pattern = 'pdf')
 file.copy(file.path('I:/My Dropbox/projects/0.3 Catalogue/publishddi',idno,'reports',doc), file.path('metadata',gsub('_doc','_metadata',doc)), overwrite = T)
 
 

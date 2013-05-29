x <- 1:100
y<-((rank(x)-1)/(length(x)-1)) 
# rescale between -4 and +4 to replicate palmer index
z<-8*(y-.5)
df <- as.data.frame(cbind(x,y,z))
head(df)
df$j <- round(df$z,1)
head(df)
df
for(i in 1:nrow(df)){df$j[i] <- strsplit(as.character(df$j),split="\\.")[[i]][[1]]}
boxplot(split(df$y,df$j))

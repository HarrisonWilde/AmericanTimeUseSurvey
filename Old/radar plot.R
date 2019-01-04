#export the clean and tidy csv file
write.csv(Data_minus_rows,'Dataset.csv')

library(rio)
Dataset <- import('Dataset.csv')
plot04 <- aggregate( tu04 ~ TUYEAR , Dataset , mean )
plot04 <- as.data.frame(plot04)

library(fmsb)
data=as.data.frame(matrix( plot04$tu04 , ncol=15)) 
colnames(data)=c('2003','2004','2005','2006','2007','2008','2009',
                 '2010','2011','2012','2013','2014','2015','2016','2017')
## Changes lower limit to 0
data=rbind(rep(10,1) , rep(0,1) , data)
radarchart(data)
## Reverse max and min (how points will be plotted)
data2 = data
data2[1:2,] = data2[2:1,]
radarchart( data2, axistype=1 ,
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , #custom polygon
            cglcol="grey", cglty=1, axislabcol="grey", #custom the grid
            caxislabels=seq(10,0,-2), cglwd=0.8, # Reverse axis labeling
            vlcex=0.8) #custom labels






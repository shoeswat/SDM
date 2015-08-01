library(shiny)
require(rgeos)
library(dismo)
require(plyr)
require(ggplot2)
require(raster)
require(maptools)
require(grid)

############# MASK FOR CALIFORNIA FROM US ###################### 

fileFullPath <- list.files('/Volumes/Data/SDM/originalClimateLayers/Present/obs',pattern="*M.*asc",full.names=TRUE)
fileNames <- list.files('/Volumes/Data/SDM/originalClimateLayers/Present/obs',pattern="*M.*asc")
mask <- raster('/Volumes/Data/SDM/SDM_web/California/lgm/_P01_ob.asc')

for(idx in 1:length(fileNames)){
	fullExtent <- raster(fileFullPath[idx])
	temp <- intersect(fullExtent,mask)
	final <- mask(temp,mask)
	writeRaster(final,fileNames[idx])
}


############## CREATE SEASONAL LAYERS ######

strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
strReverse(c("abc", "Statistics"))


files <- list.files()
seasons <- c('DJF','MAM','JJA','SON')

for(idx in 1:12){
	temp <- stack(files[idx*12],files[idx*12-11],files[idx*12-10])
	temp2 <- stackApply(temp,indices=c(1,1,1),fun=mean)
	writeRaster(temp2,paste0(strReverse(substring(strReverse(files[idx*12]),8)),seasons[1],'.asc'))

	temp <- stack(files[idx*12-9],files[idx*12-8],files[idx*12-7])
	temp2 <- stackApply(temp,indices=c(1,1,1),fun=mean)
	writeRaster(temp2,paste0(strReverse(substring(strReverse(files[idx*12]),8)),seasons[2],'.asc'))

	temp <- stack(files[idx*12-6],files[idx*12-5],files[idx*12-4])
	temp2 <- stackApply(temp,indices=c(1,1,1),fun=mean)
	writeRaster(temp2,paste0(strReverse(substring(strReverse(files[idx*12]),8)),seasons[3],'.asc'))

	temp <- stack(files[idx*12-1],files[idx*12-2],files[idx*12-3])
	temp2 <- stackApply(temp,indices=c(1,1,1),fun=mean)
	writeRaster(temp2,paste0(strReverse(substring(strReverse(files[idx*12]),8)),seasons[4],'.asc'))

}


######### PLOT AND SAVE CLIMATE REFS##########


#postscript("/Volumes/Data/SDM/SDM_web/deployedData/climateRefs/precip.eps", width = 480, height = 480)
postscript("/Volumes/Data/SDM/SDM_web/deployedData/climateRefs/tas.eps", width = 480, height = 480)
#postscript("/Volumes/Data/SDM/SDM_web/deployedData/climateRefs/precip.eps", width = 480, height = 480)
#postscript("/Volumes/Data/SDM/SDM_web/deployedData/climateRefs/precip.eps", width = 480, height = 480)

#files <- list.files("/Volumes/Data/SDM/SDM_web/allData/seasonal",pattern = paste0("precip*"), full.names = TRUE)
files <- list.files("/Volumes/Data/SDM/SDM_web/allData/seasonal",pattern = paste0("tas*"), full.names = TRUE)
#files <- list.files("/Volumes/Data/SDM/SDM_web/allData/seasonal",pattern = paste0("tmin*"), full.names = TRUE)
#files <- list.files("/Volumes/Data/SDM/SDM_web/allData/seasonal",pattern = paste0("tmax*"), full.names = TRUE)


titles <- c('Modern Fall (SON)','Modern Spring (MAM)','Modern Winter (JJA)', 'Modern Summer (JJA)','mid-Holocene Fall (SON)','mid-Holocene Spring (MAM)','mid-Holocene Winter (JJA)', 'mid-Holocene Summer (JJA)', 'LGM Fall (SON)','LGM Spring (MAM)','LGM Winter (JJA)', 'LGM Summer (JJA)')
idx <- c(3,7,11,2,6,10,4,8,12,1,5,9)
op <- par(mfcol = c(4,3), oma = c(2,2,0,0), mar = c(0,0,3,1))
for(i in 1:12){
	if (i == 10){
		#plot(raster(files[idx[i]]), xlab = "Longtitude", ylab = "Latitude", breaks=seq(0,900,length.out=100), legend = FALSE)
		plot(raster(files[idx[i]]), xlab = "Longtitude", ylab = "Latitude", col=rev(rainbow(200)[1:65]), breaks=seq(-20,40,length.out=100), legend = FALSE)
		title(titles[idx[i]], line = .5)
	} else {
		#plot(raster(files[idx[i]]), axes = FALSE, breaks=seq(0,900,length.out=400), legend = FALSE)
		plot(raster(files[idx[i]]), axes = FALSE, col = rev(rainbow(200)[1:65]), breaks=seq(-20,40,length.out=100), legend = FALSE)
		axis(side = 1, labels = FALSE)
		axis(side = 2, labels = FALSE)
		title(titles[idx[i]], line = .5)
	}
}
par(op)
dev.off()

#precip lims 0 to 900
#tas lims
#tmin lims
#tmax lims

# http://stackoverflow.com/questions/13239986/avoid-wasting-space-when-placing-multiple-aligned-plots-onto-one-page
# http://www.r-bloggers.com/high-resolution-figures-in-r/


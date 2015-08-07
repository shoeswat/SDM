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

library(RColorBrewer)
library(raster)

#files <- list.files("/home/yoshi/SDM_web/allData/seasonal", pattern = paste0("precip*"), full.names = TRUE)
#files <- list.files("/home/yoshi/SDM_web/allData/seasonal",pattern = paste0("tas*"), full.names = TRUE)
#files <- list.files("/home/yoshi/SDM_web/allData/seasonal",pattern = paste0("tmin*"), full.names = TRUE)
files <- list.files("/home/yoshi/SDM_web/allData/seasonal",pattern = paste0("tmax*"), full.names = TRUE)

#PRECIP
#rng = c(0,700)
#anomRng = c(-250,250)
#clrs <- colorRampPalette(rev(brewer.pal(11,'YlGnBu')))

#TEMPERATURE
rng = c(-20,50)
anomRng = c(-15,15)
clrs <- colorRampPalette(brewer.pal(11,'Spectral'))

titles <- c('Modern Fall (SON)','Modern Spring (MAM)','Modern Summer (JJA)', 'Modern Winter (DJF)','midH Anomalies Fall (SON)','midH Anomalies Spring (MAM)','midH Anomalies Summer (JJA)', 'midH Anomalies Winter (DJF)', 'LGM Anomalies Fall (SON)','LGM Anomalies Spring (MAM)','LGM Anomalies Summer (JJA)', 'LGM Anomalies Winter (DJF)')
titles <- rev(titles)


op <- par(mfcol = c(3,4), oma = c(3,2,0,3), mar = c(0,1,4,1))

for(i in c(7,6,8,5,11,10,12,9,3,2,4,1)){
	message(files[i])
	if (i == 10 | i == 12 | i == 11){
		plot(raster(files[i]), col=rev(clrs(100)), zlim = rng, legend = FALSE)
		title(titles[i], line = .5)
	} else if (i == 9){
		plot(raster(files[i]), col=rev(clrs(100)), zlim = rng)
		title(titles[i], line = .5)
	} else if (i == 5 | i == 1){
		clrsAnom <- colorRampPalette(brewer.pal(11,'RdBu'))
		d = raster(files[i]) - raster(files[9])
		plot(d, col=rev(clrsAnom(100)), zlim = anomRng)
		title(titles[i], line = .5)
	} else {
		clrsAnom <- colorRampPalette(brewer.pal(11,'RdBu'))
		if (i %% 4 == 10 %% 4) {
			d = raster(files[i]) - raster(files[10])
		} else if (i %% 4 == 11 %% 4) {
			d = raster(files[i]) - raster(files[11])
		} else if (i %% 4 == 12 %% 4) {
			d = raster(files[i]) - raster(files[12])
		}
		plot(d, col=rev(clrsAnom(100)), zlim = anomRng, legend = FALSE)
		title(titles[i], line = .5)
	}
}
par(op)
#dev.off()

# margins!
#http://research.stowers-institute.org/mcm/efg/R/Graphics/Basics/mar-oma/index.htm

# http://stackoverflow.com/questions/13239986/avoid-wasting-space-when-placing-multiple-aligned-plots-onto-one-page
# http://www.r-bloggers.com/high-resolution-figures-in-r/


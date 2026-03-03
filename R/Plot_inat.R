library(rinat)
library(jpeg)
library(RCurl)
Search_Inat <- get_inat_obs(taxon_name = "Colomys", photo_license = "any", maxresults = 100)#search rinat
#CC.Images <- Search_Inat[grep("CC-",Search_Inat$license),]#filter by license we are already doing above, so may be able to review
ImageCheck <- CC.Images[!CC.Images$image_url=="",]
img_url <- ImageCheck$image_url[2]#get an image url by index
picture_raw <- readJPEG(getURLContent(img_url), native = TRUE)#read in in the jpeg
res = dim(picture_raw)[2:1] # get the resolution, [x, y]
#generate plot
plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
rasterImage(picture_raw,1,1,res[1],res[2])#add raster

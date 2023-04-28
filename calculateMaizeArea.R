rm(list = ls(all=TRUE))
library(raster)
library(exactextractr)
path <- "D:/RCMRD/Data/Maps/Zambia/"
# files <- list.files(path, full.names = T, recursive = T)
# head(basename(files))
# r <- list()
# for(i in 1:length(files)){
#   r[[i]] <- rast(files[i])
# }
# # Mosaic the rasters
# m <- merge(r)
r <- raster(paste0(path, "zambia_s1_s2_crop_type_rf_2022_87k_noshrub.tif"))
#plot(r, col=rainbow(7))
v <- shapefile("D:/Adm data/Zambia/2010 Districts/district_74_dd.shp")



a=aa[aa==1]
aa <- exact_extract(r, v, "count")
# count the number of cells in each class with freq
freq(r)

#To compute the area of each class:https://stackoverflow.com/questions/72945527/calculate-polygon-area-for-each-land-cover-type-using-spatraster-object
  


rm(list=ls(all=TRUE))
start.time <- Sys.time()
setwd("/home/servir/vic_outputs/")
path <- "/home/servir/vic_outputs/mwi/"
outpath <- "/home/servir/vic_outputs/"
country <- "mwi_tamsat"
library(terra)
pols <- vect('/home/servir/RHEAS/data/malawi/shp/gadm40_MWI_1.shp')
years <- 2012:2021
# ============================================================================
# 2.0 Spatial aggregation of Metrics
# ============================================================================

variable <- c("tamsat","evap", "net_long", "surf_temp", "tmax", "net_short", "tmin", "soil_moist", "soil_temp", "rootmoist", "transp_veg")
dff <- data.frame(matrix(nrow= length(pols$county), ncol = length(variable)))
colnames(dff) <- variable
dff$District <- pols$county
out <- list()
k <- 1
for(i in variable){
	temp <- list.files(path=path, pattern=glob2rx(sprintf("*_%s_*", i)), recursive=T)
	dates <-  format(as.Date(substr(temp, nchar(temp)-14, nchar(temp)-5), format="%Y-%m-%d"), "%B")
	#Define Season and extract relevant files
	season <- c("October","November","December","January","February","March","April","May","June")
	files <- temp[dates %in% season]
	  
	li <- list()
	count <- 1
	for(j in 1:length(files)){
		r <- rast(paste0(path,files[j]))
		metric <- extract(r, pols, fun=mean, na.rm=TRUE)
		dff[i] <- metric[[2]]
		dff$date <- as.Date(substr(files[j], nchar(files[j])-14, nchar(files[j])-5), format="%Y-%m-%d")
		li[[count]] <- dff
		count <- count + 1
		#dff <- rbind(dff, dff)
	}
	out[[k]] <- do.call(rbind, li)
	k <- k + 1
}

df <- do.call(rbind, out)

saveRDS(df, paste0(outpath, country,"_spatial_aggregates_v1.rds"))
# ============================================================================
# 3.0 Temporal aggregation of Metrics
# ============================================================================
# Temporally aggregate the spatial metrics per year to obtain spatial-temporal vic metrics.
seasonMean <- function(years, df, seasons=1:2) {
  res <- list()
  for (year in years) {
    season <- ifelse(seasons==1, "long", "short")
    if (season =="long") {
      sdate <- paste0(year, "-03-01")
      edate <- paste0(year, "-09-30")
      season <- "LRLD"
    } else if (season =="short") {
      sdate <- paste0(year-1, "-10-01")
      edate <- paste0(year, "-06-30")
      season <- "SRSD"
    } else {
      stop("Define season")
    }
    ydf <- df[df$date >= sdate & df$date <= edate, ]
    #ym <- aggregate(ydf[,3], ydf[,1, drop=FALSE], mean, na.rm=T)
    ydf <- subset(ydf, select=-date)
    ym <- aggregate(.~District, data=ydf, mean, na.rm=T)
    ym$year <- year
    ym$season <- season
    res[[i]] <- ym    
  }  
  do.call(rbind, res)  
}

#Now compute the temporal averages using the function.

temp <- lapply(years, seasonMean, df, seasons=2)

vic <- do.call(rbind, temp)

saveRDS(vic, paste0(outpath, country,"_spatial-temporal_aggregates_v1.rds"))
end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("Duration", time.taken))


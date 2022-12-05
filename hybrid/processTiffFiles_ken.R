rm(list=ls(all=TRUE))
start.time <- Sys.time()
setwd("/home/servir/vic_outputs/")
path <- "/home/servir/vic_outputs/ken/"
outpath <- "/home/servir/vic_outputs/"
country <- "ken_tamsat"
library(terra)
pols <- vect('/home/servir/RHEAS/data/kenya/shp/Kenya_maize_counties_dd.shp')
years <- 2010:2018
# ============================================================================
# 2.0 Spatial aggregation of Metrics
# ============================================================================

variable <- c("tamsat", "evap", "net_long", "surf_temp", "tmax", "net_short", "tmin", "soil_moist", "soil_temp", "rootmoist", "transp_veg")
dff <- data.frame(matrix(nrow= length(pols$county), ncol = length(variable)))
colnames(dff) <- variable
dff$District <- pols$county
out <- list()
k <- 1
for(i in variable){
	files <- list.files(path=path, pattern=glob2rx(sprintf("*_%s_*", i)), recursive=T)
	#dates <-  format(as.Date(substr(temp, nchar(temp)-14, nchar(temp)-5), format="%Y-%m-%d"), "%B")
	#Define Season and extract relevant files
	#season <- c("October","November","December","January","February","March","April","May","June")
	#files <- [dates %in% season]
	  
	li <- list()
	count <- 1
	for(j in 1:length(files)){
		r <- rast(paste0(path, files[j]))
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

saveRDS(df, paste0(outpath, country,"_spatial_aggregates.rds"))

# ============================================================================
# 3.0 Temporal aggregation of Metrics
# ============================================================================
# Temporally aggregate the spatial metrics per year to obtain spatial-temporal vic metrics.

df$year <- format(as.Date(df$date, format = "%Y_%m_%d"), "%Y")
db <- subset(df, select = -date)
vic <- aggregate(.~District+year, data=db, mean, na.rm=T)

saveRDS(vic, paste0(outpath, country,"_spatial-temporal_aggregates_v1.rds"))
end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("Duration", time.taken))


rm(list = ls(all=TRUE))
unlink(".RData")
library(dplyr)
library(reshape2)
root <- "D:/RCMRD/Data/Yields/"
metrics <- paste0(root, "metrics/")
ref <- paste0(root, "Reference/Zambia/MOA/")

#==============================================================================
#Load 2010-2022 MOA observed yields data
#==============================================================================
ref <- read.csv(paste0(ref, "Zambia_District _Maize_Forecasting_2011_2022.csv"), stringsAsFactors =  FALSE)
ref$District <- toupper(ref$District)
ref$Province <- trimws(toupper(ref$Province), which = c("both"))
sort(unique(ref$Province))
ref$District <- trimws(toupper(ref$District), which = c("both"))
names(ref)[7] <- "year"
ref$District[ref$District=="ITEZHI-TEZHI"] <- "ITEZHI TEZHI"

#png("figs/figure1.png", units="in", width=12, height=12, res=300, pointsize=24)
#setEPS()
#postscript(paste0(root, "Results/ZMB/MoA_observed_yields.pdf"), width=12, height=12, pointsize=24)

tiff(paste0(root, "Results/ZMB/National_MoA_observed_yields.tif"), units="px", width=2250, height=2250, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)
boxplot(yield_MT_ha~year, data=ref, col=rainbow(length(unique(ref$Year))), 
        xlab="Year", ylab = "Yield (MT/ha)", main="Zambia MoA Annual Forecasts.")

dev.off()

#Visualize the crop forecasting data from MoA per Province.

agg <- aggregate(ref[, "yield_MT_ha", drop=FALSE], ref[, c("Province", "year")], mean, na.rm=TRUE)
agg <- reshape(agg, direction="wide", idvar="Province", timevar="year")
colnames(agg) <- gsub("yield_MT_ha.", "", colnames(agg))
agg <- agg[,-2]
rownames(agg) <- agg[,1]
agg <- as.matrix(agg[,-1])#as.matrix(sapply(agg,as.numeric))
tiff(paste0(root, "Results/ZMB/Provincial_MoA_observed_yields.tif"), units="px", width=3500, height=2250, res=300, pointsize=16)
par(mar=c(4.5,4.0,2,2)) #c(bottom, left, top, right)

#barplot(agg, legend =  rownames(agg), las=2, args.leg=list(cex=1), 
        #xlab="Provinces", ylab = "Yield (MT/ha)", main="Zambia MoA Provinces Forecasts.", cex.axis=0.8) 
temp <- ref
library(stringr)
temp$Province <- str_to_title(temp$Province)
temp$Province[temp$Province=="North-Western"] <- "NW"
boxplot(yield_MT_ha~Province, data=temp, col=rainbow(length(unique(temp$Province))), xlab="Provinces", 
        ylab = "Yield (MT/ha)", main="Zambia MoA Provinces Forecasts.", cex.axis=0.8)
dev.off()

#==============================================================================
## VIC Spatial-Temporal metrics
#==============================================================================
v_t <- readRDS(paste0(root, "metrics/zmb_tamsat_vic_metrics_v1.rds"))
v_c <- readRDS(paste0(root, "metrics/zmb_chirps_vic_metrics_v1.rds"))

#==============================================================================
## MODIS/RS Spatial-temporal metrics
#==============================================================================
wide2long <- function(df, variable){
  melt(df[, -c(1,45)], variable.name = "date", 
       value.name = variable, id.vars = "county")
}
#Indices list; https://www.l3harrisgeospatial.com/docs/canopywatercontent.html#:~:text=Moisture%20Stress%20Index%20(MSI),absorption%20around%201599%20nm%20increases.
#1.0 EVI
metric <- paste0(metrics,"zmb/")
temp <- list.files(metric, pattern=glob2rx("*_evi_*"))
files <- lapply(paste0(metric, temp), read.csv, stringsAsFactors =  FALSE)
evi <- lapply(files, wide2long, "evi")
evi[[1]][1,]

#2.0 FPAR
temp <- list.files(metric, pattern=glob2rx("*fpar*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
fpar <- lapply(files, wide2long, "fpar")
fpar[[1]][1,]

#3.0 GLI
temp <- list.files(metric, pattern=glob2rx("*_gli_*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
gli <- lapply(files, wide2long, "gli")
gli[[1]][1,]

#4.0 GNDVI
temp <- list.files(metric, pattern=glob2rx("*gndvi*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
gndvi <- lapply(files, wide2long, "gndvi")
gndvi[[1]][1,]

#5.0 GPP
temp <- list.files(metric, pattern=glob2rx("*gpp*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
gpp <- lapply(files, wide2long, "gpp")
gpp[[1]][1,]

#6.0 LAI
temp <- list.files(metric, pattern=glob2rx("*lai*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
lai <- lapply(files, wide2long, "lai")
lai[[1]][1,]

#7.0 MSI
temp <- list.files(metric, pattern=glob2rx("*_msi_*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
msi <- lapply(files, wide2long, "msi")
msi[[1]][1,]

#8.0 NDMI (Normalized Difference Moisture Index)
temp <- list.files(metric, pattern=glob2rx("*ndmi*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
ndmi <- lapply(files, wide2long, "ndmi")
ndmi[[1]][1,]

#9.0 NDVI
temp <- list.files(metric, pattern=glob2rx("*_ndvi_*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
ndvi <- lapply(files, wide2long, "ndvi")
ndvi[[1]][1,]

#10.0 NPCRI
temp <- list.files(metric, pattern=glob2rx("*_npcri_*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
npcri <- lapply(files, wide2long, "npcri")
npcri[[1]][1,]

#11.0 SIPI
temp <- list.files(metric, pattern=glob2rx("*_sipi_*"))
files <-lapply(paste0(metric,temp), read.csv, stringsAsFactors =  FALSE)
sipi <- lapply(files, wide2long, "sipi")
sipi[[1]][1,]

# Format the date variable by removing unnecessary characters.

formatDate <- function(m){
  m$date <- gsub("X","", m$date)
  m$date <- gsub("_nd","", m$date)
  m$date <- as.Date(as.character(m$date), format = "%Y_%m_%d")
  m[1,]
  colnames(m)[1]<- "District"
  return(m)
}
evi <- lapply(evi, formatDate)
evi[[1]][1,]
fpar <- lapply(fpar, formatDate)
fpar[[1]][1,]
gli <- lapply(gli, formatDate)
gli[[1]][1,]
gndvi <- lapply(gndvi, formatDate)
gndvi[[1]][1,]
gpp <- lapply(gpp, formatDate)
gpp[[1]][1,]
lai <- lapply(lai, formatDate)
lai[[1]][1,]
msi <- lapply(msi, formatDate)
msi[[1]][1,]
ndmi <- lapply(ndmi, formatDate)
ndmi[[1]][1,]
ndvi <- lapply(ndvi, formatDate)
ndvi[[1]][1,]
npcri <- lapply(npcri, formatDate)
npcri[[1]][1,]
sipi  <- lapply(sipi, formatDate)
sipi[[1]][1,]


# Now combine the list into one dataframe.

evi <- do.call(rbind.data.frame, evi)
fpar <- do.call(rbind.data.frame, fpar)
gli  <- do.call(rbind.data.frame, gli)
gndvi  <- do.call(rbind.data.frame, gndvi)
gpp <- do.call(rbind.data.frame, gpp)
lai <- do.call(rbind.data.frame, lai)
msi <- do.call(rbind.data.frame, msi)
ndmi <- do.call(rbind.data.frame, ndmi)
ndvi <- do.call(rbind.data.frame, ndvi)
npcri <- do.call(rbind.data.frame, npcri)
sipi <- do.call(rbind.data.frame, sipi)

# Now merge all the remote sensing metrics to one data frame.

evi$evi <- as.numeric(evi$evi)
n <- do.call("cbind", list(evi, gli=as.numeric(gli[,-c(1:2)]), gndvi=as.numeric(gndvi[,-c(1:2)]), gpp=as.numeric(gpp[,-c(1:2)]), msi=as.numeric(msi[,-c(1:2)]), ndmi=as.numeric(ndmi[,-c(1:2)]), ndvi=as.numeric(ndvi[,-c(1:2)]), npcri=as.numeric(npcri[,-c(1:2)]), sipi=as.numeric(sipi[,-c(1:2)])))
temp  <- cbind(lai, fpar=fpar[,-c(1:2)])
temp[, c("lai","fpar")] <- lapply(c("lai","fpar"), function(x) as.numeric(temp[[x]]))
modis <- merge(n,temp, by=c("District", "date"))
modis$District[modis$District=="CHIENGI"] <- "CHIENGE"
modis$District[modis$District=="SHANGOMBO"] <- "SHANG'OMBO"

### RS Temporal Index Aggregation 

seasonMean <- function(year, df, seasons=1:2) {
  res <- list()
  for (i in seasons) {
    season <- ifelse(i==1, "long", "short")
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

years <- 2012:2022
temp <- lapply(years, seasonMean, modis, seasons=2)
rs <- do.call(rbind, temp)
rs <- rs[!rs$District=="Counties",]

#x11()
#boxplot(evi~year, data=rs)

#==============================================================================
## DSSAT Spatial-temporal metrics
#==============================================================================


```{r dssat1, echo=FALSE, warning=FALSE, message=FALSE}
path <- "/home/servir/RHEAS/yields/"
tt <- read.csv(paste0(path, "zambia_chirps_dssatTable_1990_2022.csv"), stringsAsFactors =  FALSE)
tt$harvest <- as.Date(tt$harvest)
tt$planting <- as.Date(tt$planting)
tt$date <- format(tt$harvest, format = "%Y")
names(tt)[3] <- "District"
tt$District[tt$District=="CHIENGI"] <- "CHIENGE"
tt$District[tt$District=="SHANGOMBO"] <- "SHANG'OMBO"
```


### DSSAT Spatial-Temporal metrics

Aggregate DSSAT production forecasts and metrics with respect to Districts and maize growing calendar. 

The maize growing season in Zambia starts from October to end of June. So we will aggregate the metrics and forecast with this condition using the function `RH_metrics`.

```{r rh2, echo=FALSE, warning=FALSE, message=FALSE}
RH_metrics <- function(rh, sStart, sEnd){
  rh <- subset(rh, format(as.Date(rh$planting), "%m") >= sStart & format(as.Date(rh$harvest), "%m") <= sEnd)
  rh <- aggregate(rh[,c("wsgd","lai","gwad")], rh[,c("District","date")], mean, na.rm=T)
  return(rh)
}

rh <- RH_metrics(tt, sStart ="10", sEnd = "06")
#rh <- subset(rh, select = - gwad)
names(rh)[2] <- "year"

```

Check and format District names to be consistent in all datasets.

```{r naming, echo=FALSE, warning=FALSE, message=FALSE}
c <- sort(unique(ref$District))
c[!c %in% sort(unique(rs$District))]
c[!c %in% sort(unique(rh$District))]
```

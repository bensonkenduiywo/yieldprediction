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
v_t$District <- toupper(v_t$District)
v_c <- readRDS(paste0(root, "metrics/zmb_chirps_vic_metrics_v1.rds"))
v_c$District <- toupper(v_c$District)
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

years <- 2011:2022
temp <- lapply(years, seasonMean, modis, seasons=2)
rs <- do.call(rbind, temp)
rs <- rs[!rs$District=="Counties",]
rs$District <- toupper(rs$District)

#x11()
#boxplot(evi~year, data=rs)

#==============================================================================
## DSSAT Spatial-temporal metrics
#==============================================================================
path <- paste0(root, "RHEAS/")
tt <- read.csv(paste0(path, "zambia_tamsat_25km_districts_dssatTable_2012_2022_100kg_v2.csv"), stringsAsFactors =  FALSE)
tt$harvest <- as.Date(tt$harvest)
tt$planting <- as.Date(tt$planting)
tt$date <- format(tt$harvest, format = "%Y")
names(tt)[3] <- "District"

### DSSAT Spatial-Temporal metrics

season <- c("October","November","December","January","February","March","April","May","June")
RH_metrics <- function(rh, season){
  rh <- subset(rh, format(as.Date(rh$planting), "%B") %in% season & format(as.Date(rh$harvest), "%B") %in% season)
  #rh$Season <- season
  rh <- aggregate(rh[,c("wsgd","lai","gwad"), drop=FALSE], rh[,c("District","date"), drop=FALSE], mean, na.rm=T)
  return(rh)
}

rh <- RH_metrics(tt, season)
names(rh)[2] <- "year"
rh$District <- toupper(rh$District)

c <- sort(unique(ref$District))
c[!c %in% sort(unique(rs$District))]
c[!c %in% sort(unique(rh$District))]
rheas <- rh
rheas$gwad <- rheas$gwad/1000
rheas <- merge(rheas, ref, by=c("District", "year"))
#==============================================================================
## Feature Engineering
#==============================================================================

minMax <- function(x){
  return((x-min(x, na.rm=T))/(max(x, na.rm = T) - min(x, na.rm=T)))
}

rs <- subset(rs, select=-season)
rs[,-c(1,13)] <- apply(rs[,-c(1,13)], 2, minMax)
vc <- v_t
vc <- subset(vc, select=-season)
vc[,-c(1,13)] <- apply(vc[,-c(1,13)], 2, minMax)
names(rh)[4] <- "DSSAT_lai" 
rh[,-c(1,2)] <- apply(rh[,-c(1,2)], 2, minMax)

df_list <- list(rs[rs$year > 2010,], rh[rh$year > 2010,], vc[vc$year > 2010, ], ref[,c("District", "yield_MT_ha","year")])
data <- Reduce(function(x, y) merge(x, y, by=c("District","year")), df_list)
df_list2 <- list(rs[rs$year > 2010,], ref[,c("District", "yield_MT_ha","year")])
data <- subset(data, select=-gwad)
#data <- na.omit(data)
vi <- Reduce(function(x, y) merge(x, y, by=c("District","year")), df_list2)

library(randomForest)
library(ggplot2)
library(ggthemes)
library(ggeasy)
library(dplyr)
rf = randomForest(yield_MT_ha~., data=subset(data, select = -c(District,year)), importance=TRUE, ntree = 500)
importance <- importance(rf)
importance
rf
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'%IncMSE'],2))

#Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

#Use ggplot2 to visualize the relative importance of variables
tiff(paste0(root, "Results/ZMB/ZMB_Feature_importance.tif"), units="px", width=2250, height=2250, res=300, pointsize=16)
par(mar=c(4.5,4.0,2,2)) #c(bottom, left, top, right) A4 paper size in pixels 3508 x 2480

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_hline(yintercept = 10, color = "blue")+
  #geom_text(aes(x = Variables, y = 0.5, label = Rank),
            #hjust=0, vjust=0.55, size = 4, colour = 'green') +
  scico::scale_fill_scico(palette = "lajolla")+
  labs(x = 'EO Metrics') +
  coord_flip() +
  ggtitle("Zambia")+
  theme_few(base_size = 20)+
  ggeasy::easy_center_title()
dev.off()
## Select features that have an impact of 10% MSE on prediction
Notselected <- rankImportance$Variables[rankImportance$Importance < 10]
Notselected
data <- subset(data, select=-c(ndvi, wsgd, DSSAT_lai))

#==============================================================================
## Validation
#==============================================================================
rmse <- function(error){
  sqrt(mean(error^2, na.rm=T))
}

ubrmse <- function(pred, obs){
  x <- obs-mean(obs, na.rm=T)
  y <- pred - mean(pred, na.rm=T)
  error <- y-x
  return(sqrt(mean(error^2, na.rm=T)))
}

#“Mean Bias Error” is the tendency of a measurement process to overestimate or underestimate the value of a parameter.
MBE <- function(obs, pred){
  error <- obs - pred
  return (mean(error, na.rm=T))#( mean(sum(error, na.rm=T)/length(obs)) )
}

MAPE <- function (obs, pred){
  abs_error <- (abs(obs - pred))/obs
  MAPE <- sum(abs_error, na.rm=T)/length(obs)
  return(MAPE*100)
}

R_square <- function(obs, pred) {
  #val <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
  #val
  return(cor(obs, pred)^2)
} 
#Excellent when RRMSE < 10%, Good when RRMSE is between 10% and 20%, Fair when RRMSE is between 20% and 30% and Poor when RRMSE > 30%
rrmse <- function(obs, pred){
  num <- sqrt(mean((obs - pred)^2, na.rm=T))
  den <- mean(pred, na.rm=T)
  rrmse_loss <- num/den
  return(rrmse_loss * 100)
}

library(dismo)
library(e1071)
path <-'D:\\RCMRD\\Data\\Yields\\Results\\ZMB\\'
models <- function(vi, years, accName){
  temp <- na.omit(vi)
  y <- years
  d_svm <- data.frame()
  d_rf <- data.frame()
  d_lm <- data.frame()
  for(i in 1:length(y)){
    observed_y <- 0
    svm_y <- rf_y <- 0
    print(paste0('The year ', y[i], " left out for validation.\n"))
    # Train model using all data and evaluate at district level
    train <- subset(temp, year != y[i], select=-c(year,District))
    valid <- subset(temp, year == y[i])
    val_p <- subset(valid, select=-c(year,District))
    observed_y <- valid$yield_MT_ha
    #1.0 SVM
    tuneResult <- tune.svm(yield_MT_ha~.,  data = train, epsilon = seq(0,1,0.1), cost = seq(0.5,8,.5), kernel="radial" )#tuneResult <- tune(method="svm", yield_MT_ha~.,  data = train, ranges = list(epsilon = seq(0,1,0.1), cost = (seq(0.5,8,.5))), kernel="radial" )
    svm_y <- predict(tuneResult$best.model, val_p)
    temp1 <- rbind(data.frame(District=valid$District, year=valid$year, yield=svm_y))
    d_svm <- rbind(d_svm, temp1)
    #2.0 RF
    tuneRF <- tune.randomForest(yield_MT_ha~.,  data = train, ntree=seq(100,500,50))#tuneRF <- tune(method="randomForest", yield_MT_ha~.,  data = train, ranges = list(ntree = c(100, 500))) 
    rf_y <- predict(tuneRF$best.model, val_p)
    temp2 <- rbind(data.frame(District=valid$District, year=valid$year, yield=rf_y))
    d_rf <- rbind(d_rf, temp2)
    
    #3.0 LM
    lm1 <- lm(yield_MT_ha~., data = train)
    lm_y <- predict(lm1, val_p)
    temp3 <- rbind(data.frame(District=valid$District, year=valid$year, yield=lm_y))
    d_lm <- rbind(d_lm, temp3)
  }
  saveRDS(d_svm, paste0(path, accName, "_SVM_accuracy_Districts.rds"))
  saveRDS(d_rf, paste0(path, accName, "_RF_accuracy_Districts.rds"))
  saveRDS(d_lm, paste0(path, accName, "_LM_accuracy_Districts.rds"))
  #val <- aggregate(.~District, val[, c("RMSE", "RRMSE", "MAPE", "District")] , mean, na.rm=T)
}


#VI only
models(vi, years = 2011:2022, accName = "ZMB_EO_only")
a_svm <- na.omit(readRDS("ZMB_EO_only_SVM_accuracy_Districts.rds"))
a_svm <- merge(a_svm, ref, by=c("District", "year"))
a_rf <- na.omit(readRDS("ZMB_EO_only_RF_accuracy_Districts.rds"))
a_rf <- merge(a_rf, ref, by=c("District", "year"))
a_lm <- na.omit(readRDS("ZMB_EO_only_LM_accuracy_Districts.rds"))
a_lm <- merge(a_lm, ref, by=c("District", "year"))
#VI+RHEAS
models(subset(data, select=-c(DSSAT_lai, wsgd)), years = 2011:2022, accName = "ZMB_EO_RHEAS")
b_svm <- na.omit(readRDS("ZMB_EO_RHEAS_SVM_accuracy_Districts.rds"))
b_svm <- merge(b_svm, ref, by=c("District", "year"))
b_rf <- na.omit(readRDS("ZMB_EO_RHEAS_RF_accuracy_Districts.rds"))
b_rf <- merge(b_rf, ref, by=c("District", "year"))
b_lm <- na.omit(readRDS("ZMB_EO_RHEAS_LM_accuracy_Districts.rds"))
b_lm <- merge(b_lm, ref, by=c("District", "year"))

###Compute RMSE, MAPE and R2 for RHEAS
error <- function(df, method){
  dists <- sort(unique(df$District))
  dff <- data.frame(matrix(nrow= length(dists), ncol = 7))
  colnames(dff) <- c("District", "Method","RMSE", "MAPE", "RRMSE", "MBE", 'ubRMSE')
  dff$Method <- method
  dff$District <- dists
  for(i in 1:length(unique(df$District))){
    temp <- df[df$District==dists[i], ]
    dff$RMSE[dff$District==dists[i]]  <-  rmse(temp$yield_MT_ha-temp$yield)
    dff$MAPE[dff$District==dists[i]]  <-  MAPE(temp$yield_MT_ha, temp$yield)
    dff$RRMSE[dff$District==dists[i]] <-  rrmse(temp$yield_MT_ha, temp$yield)
    dff$MBE[dff$District==dists[i]]   <-  MBE(temp$yield_MT_ha, temp$yield)
    dff$ubRMSE[dff$District==dists[i]]   <-  ubrmse(temp$yield_MT_ha, temp$yield)
  }
  return(dff)
  
} #a_rh <- readRDS("ZMB_RHEAS_accuracy_Districts.rds")
names(rheas)[5] <- "yield"
e_rh <- error(rheas, "RHEAS")
saveRDS(e_rh, paste0(path,"RHEAS_accuracy_Districts.rds"))

e_svm <- error(a_svm, "SVM-VI")
e_rf <- error(a_rf, "RF-VI")
e_lm <- error(a_lm, "LM-VI")
e2_svm <- error(b_svm, "SVM-H")
e2_rf <- error(b_rf, "RF-H")
e2_lm <- error(b_lm, "LM-H")

er <- Reduce(function(x, y) merge(x, y, all=TRUE), list(e_rh, e_svm, e_rf, e_lm, e2_svm, e2_rf, e2_lm))
write.csv(er,"ZMB_District_accuracy_metrics.csv")

png(paste0(root, "Results/ZMB/ZMB_Model_bias.png"), units="px", width=2350, height=2350, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)
boxplot(MBE~Method, data=er, ylab='Model bias (MT/ha)', main="Zambia")
abline(h=0, col="red")
dev.off()

png(paste0(root, "Results/ZMB/ZMB_RRMSE.png"), units="px", width=2350, height=2350, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)
boxplot(RRMSE~Method, data=er, ylab='RRMSE (%)', main="Zambia")
abline(h=0, col="red")
dev.off()

png(paste0(root, "Results/ZMB/ZMB_RMSE.png"), units="px", width=2350, height=2350, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)
boxplot(RMSE~Method, data=er, ylab='RMSE (MT/ha)', main="Zambia")
abline(h=0, col="red")
dev.off()

tb <- aggregate(.~Method, data=er[,-1], mean)

write.csv(tb, paste0(root, "Results/ZMB/ZMB_accuracy.csv"))
#=======================================================================
## Spatial Visualization
#=======================================================================
library(raster)
filename <- "D:/Adm data/Zambia/2010 Districts/district_74.shp"
zmb <- shapefile(filename)
names(zmb)[3] <- "District"
zmb$District <- toupper(zmb$District)
zmb$PROVINCE <- toupper(zmb$PROVINCE)
library(tmap)
library(mapview)

zmb <-  merge(zmb[,"District"], e_rh, by = "District") 
tmap_mode("plot")
map <- tm_shape(zmb, name="RMSE") +
  tm_fill("RMSE", title="RMSE", textNA = "No data") +
  tm_text("District", size = 0.4, remove.overlap = TRUE)+
  tm_layout(panel.label.size=6, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('left', 'top'))#+
map
tmap_save(map, scale =1.6, dpi= 600, filename=paste0(root, "Results/ZMB/ZMB_RHEAS_RMSE_Spatial_Distribution.png"))

tmap_mode("plot")
map <- tm_shape(zmb, name="RRMSE") +
  tm_fill("RRMSE", title="RRMSE", textNA = "No data") +
  tm_text("District", size = 0.4, remove.overlap = TRUE)+
  tm_layout(panel.label.size=6, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('left', 'top'))#+
map
tmap_save(map, scale =1.6, dpi= 600, filename=paste0(root, "Results/ZMB/ZMB_RHEAS_RRMSE_Spatial_Distribution.png"))

tmap_mode("plot")
map <- tm_shape(zmb, name="MBE") +
  tm_fill("MBE", title="MBE (MT/ha)", palette = "YlOrBr", textNA = "No data", midpoint = 0) +
  tm_text("District", size = 0.4, remove.overlap = TRUE)+
  tm_layout(panel.label.size=6, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('left', 'top'))#+
map
tmap_save(map, scale =1.6, dpi= 600, filename=paste0(root, "Results/ZMB/ZMB_RHEAS_Mean_Bias_Error_Spatial_Distribution.png"))

##ML Model

zmb <-  merge(zmb[,c("District", "PROVINCE")], e2_rf, by = "District") # duplicateGeoms = TRUE

tmap_mode("plot")
map <- tm_shape(zmb, name="RRMSE") +
  tm_fill("RRMSE", title="RRMSE", breaks = seq(10, 70, 10), textNA = "No data") +
  tm_text("District", size = 0.4, remove.overlap = TRUE)+
  tm_layout(panel.label.size=6, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('left', 'top'))#+
map

tmap_save(map, scale =1.6, dpi= 600, filename=paste0(root, "Results/ZMB/ZMB_RRMSE_Spatial_Distribution.png"))

tmap_mode("plot")
map <- tm_shape(zmb, name="MBE") +
  tm_fill("MBE", palette = "YlOrBr", title="MBE (MT/ha)", breaks = c(-0.3, -0.25, -0.2, -0.15, 0,  0.1,  0.2), textNA = "No data", midpoint=0) +
  tm_text("District", size = 0.4, remove.overlap = TRUE)+
  tm_layout(panel.label.size=6, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('left', 'top'))#+
map

tmap_save(map, scale =1.6, dpi= 600, filename=paste0(root, "Results/ZMB/ZMB_Bias_Spatial_Distribution.png"))


tmap_mode("plot")
map <- tm_shape(zmb, name="RMSE") +
  tm_fill("RMSE", palette = "YlOrBr", title="RMSE (MT/ha)", breaks = c(0.2,0.6, 0.7, 0.8, 2.0, 2.2), textNA = "No data") +
  tm_text("District", size = 0.4, remove.overlap = TRUE)+
  tm_layout(panel.label.size=6, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('left', 'top'))#+
map


tmap_save(map, scale =1.6, dpi= 600, filename=paste0(root, "Results/ZMB/ZMB_RMSE_Spatial_Distribution.png"))

zmb <-  merge(zmb[,c("District", "PROVINCE")], b_svm[b_svm$year==2022,], by = "District") # duplicateGeoms = TRUE

tmap_mode("plot")
map <- tm_shape(zmb, name="Yield") +
  tm_fill("yield", palette = "YlOrBr", title="2022 Yield (MT/ha)", breaks = seq(0,4, 1), textNA = "No data") +
  tm_text("District", size = 0.4, remove.overlap = TRUE)+
  tm_layout(panel.label.size=6, legend.position = c("right", "bottom"), title= 'Zambia', title.position = c('left', 'top'))#+
map

tmap_save(map, scale =1.6, dpi= 600, filename=paste0(root, "Results/ZMB/ZMB_SVM_Yield_Spatial_Distribution.png"))

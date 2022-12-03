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
tt <- read.csv(paste0(path, "zambia_tamsat_25km_districts_dssatTable_2010_2022_100kg.csv"), stringsAsFactors =  FALSE)
tt$harvest <- as.Date(tt$harvest)
tt$planting <- as.Date(tt$planting)
tt$date <- format(tt$harvest, format = "%Y")
names(tt)[3] <- "District"

### DSSAT Spatial-Temporal metrics

RH_metrics <- function(rh, sStart, sEnd){
  rh <- subset(rh, format(as.Date(rh$planting), "%m") >= sStart & format(as.Date(rh$harvest), "%m") <= sEnd)
  rh <- aggregate(rh[,c("wsgd","lai","gwad")], rh[,c("District","date")], mean, na.rm=T)
  return(rh)
}

rh <- RH_metrics(tt, sStart ="10", sEnd = "06")
#rh <- subset(rh, select = - gwad)
names(rh)[2] <- "year"
rh$District <- toupper(rh$District)

c <- sort(unique(ref$District))
c[!c %in% sort(unique(rs$District))]
c[!c %in% sort(unique(rh$District))]
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
vi <- Reduce(function(x, y) merge(x, y, by=c("District","year")), df_list2)

library(randomForest)
library(ggplot2)
library(ggthemes)
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

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'EO Metrics') +
  coord_flip() + 
  theme_few(base_size = 14)

## Select features that have an impact of 10% MSE on prediction
#selected <- rankImportance$Variables[rankImportance$Importance>=12]
#data <- subset(data, select=selected)

#==============================================================================
## Validation
#==============================================================================
rmse <- function(error){
  sqrt(mean(error^2, na.rm=T))
}

MAPE <- function (y_pred, y_true){
  MAPE <- mean(abs((y_true - y_pred)/y_true))
  return(MAPE*100)
}

R_square <- function(actual, predicted) {
  val <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
  val
} 

rrmse <- function(predicted, observed){
  error <- observed - predicted
  re <- sqrt(mean(error^2))/mean(observed)
  return(re*100)
}

library(dismo)
library(e1071)

models <- function(vi, years, accName){
  npredictors <- dim(vi)[2]
  svm_a <- c()
  svm_b <- c()
  rf_a <- c()
  rf_b <- c()
  r_svm <- c()
  r_rf <- c()
  cnn_r <- c()
  cnn_rmse <- c()
  cnn_mape <- c()
  df <- na.omit(subset(vi, select=-District))
  #df$District <- as.factor(df$District)
  y <- years
  for(i in 1:length(y)){
    observed_y <- 0
    svm_y <- 0
    print(paste0('The year ', y[i], " left out for validation.\n"))
    train <- subset(df, year != y[i], select=-year)
    valid <- subset(df, year == y[i], select=-year)
    observed_y <- valid$yield_MT_ha
    #SVM
    tuneResult <- tune(method="svm", yield_MT_ha~.,  data = train, ranges = list(epsilon = seq(0,1,0.1), cost = (seq(0.5,8,.5))), kernel="radial" )
    #svm <- svm(Yield_MT_HA~., data=data[, c("Yield_MT_HA","gndvi", "ndvi","ndmi", "gpp", "fpar", "Region")], kernel="radial" , cross=5)
    svm_y <- predict(tuneResult$best.model, valid)
    #svm_y <- predict(svm, valid)
    svm_a[i] <- rmse(observed_y-svm_y)
    svm_b[i] <- MAPE(observed_y, svm_y)
    cat("SVM Coefficient of determination R^2\n")
    r_svm[i] <- R_square(observed_y, svm_y)
    print(r_svm)
    #RF
    tuneRF <- tune(method="randomForest", yield_MT_ha~.,  data = train, ranges = list(ntree = c(100, 500), mtry = seq(1,npredictors,1))) #
    print(tuneRF$best.model)
    #rf = randomForest(Yield_MT_HA~., data=train, importance=TRUE, ntree = 500)
    
    rf_y <- predict(tuneRF$best.model, valid)
    rf_a[i] <- rmse(observed_y-rf_y)
    rf_b[i] <- MAPE(observed_y, rf_y)
    cat("RF Coefficient of determination R^2\n")
    r_rf[i] <- R_square(observed_y, rf_y)
    print(r_rf)
    
    # #CNN
    # y <-  as.matrix(train[ , "yield_MT_ha"])
    # x <- array(unlist(subset(train, select=-yield_MT_ha)), dim = c(nrow(train), ncol(train), 1))
    # xtest <- array(unlist(valid), dim = c(nrow(valid), ncol(valid), 1))
    # in_dim <- c(dim(x)[2:3])
    # cnns <- cnn(x, y, in_dim)
    # cnn_y <- predict(cnns, xtest)
    # cnn_rmse[i] <- rmse(observed_y-cnn_y)
    # cnn_mape[i] <- rrmse(observed_y, cnn_y)
    # cat("CNN Coefficient of determination R^2\n")
    # cnn_r[i] <- R_square(observed_y, cnn_y)
    # print(cnn_r)
    
  }
  
  cat("SVM model RMSE is ", mean(svm_a), "\n")
  cat("SVM model MAPE is ", mean(svm_b), "\n")
  cat("SVM model R2 is ", mean(r_svm), "\n")
  cat("RF model RMSE is ", mean(rf_a), "\n")
  cat("RF model R2 is ", mean(r_rf), "\n")
  cat("RF model MAPE is ", mean(rf_b), "\n")
  #cat("CNN model RMSE is ", mean(cnn_rmse), "\n")
  #cat("CNN model R2 is ", mean(cnn_r), "\n")
  #cat("CNN model MAPE is ", mean(cnn_mape), "\n")
  
  temp <- rbind(data.frame(RMSE=rf_a, Method="RF", Year=y), data.frame(RMSE=svm_a, Method="SVM", Year=y))
  #temp <- rbind(temp, data.frame(RMSE=cnn_rmse, Method="CNN", Year=y))
  
  par(mfrow=c(2,2), mar=c(4.5,4.5,1,1))
  boxplot(RMSE ~ Method, data =temp, col=c("#999999", "#E69F00"), ylab="RMSE (tons/ha)", xlab="")
  acc <- temp
  
  temp <- rbind(data.frame(MAPE=rf_b, Method="RF", Year=y), data.frame(MAPE=svm_b, Method="SVM", Year=y))
  #temp <- rbind(temp, data.frame(MAPE=cnn_mape, Method="CNN", Year=y))
  
  boxplot(MAPE ~ Method, data =temp, col=c("#999999", "#E69F00"), ylab="MAPE (%)", xlab="", ylim= c(0,100))
  acc <- merge(acc, temp, by=c("Method", "Year"))
  
  temp <- rbind(data.frame(R2=r_rf, Method="RF", Year=y), data.frame(R2=r_svm, Method="SVM", Year=y))
  #temp <- rbind(temp, data.frame(R2=cnn_r, Method="CNN", Year=y))
  
  boxplot(R2 ~ Method, data =temp, col=c("#999999", "#E69F00"), ylab=expression(R^2), xlab="", ylim= c(0,1))
  
  acc <- merge(acc,temp, by=c("Method", "Year"))
  fileName <- paste0(accName,".rds")
  saveRDS(acc, fileName)
}

models(vi, years = 2011:2022, accName = "EO_only")
#Now include RHEAS metrics and see how accuracy behaves.

models(data, years = 2011:2022, accName = "EO_RHEAS")

a <- readRDS("EO_only.rds")

b <- readRDS("EO_RHEAS.rds")
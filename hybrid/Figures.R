rm(list = ls(all=TRUE))
# zmb <- read.csv("D:/RCMRD/Data/Yields/Results/ZMB/ZMB_accuracy.csv")
# mwi <- read.csv("D:/RCMRD/Data/Yields/Results/MWI/MWI_accuracy.csv")
# ken <- read.csv("D:/RCMRD/Data/Yields/Results/KEN/KEN_accuracy.csv")

zmb <- read.csv("D:/RCMRD/Code/yieldprediction/ZMB_District_accuracy_metrics.csv")
mwi <- read.csv("D:/RCMRD/Code/yieldprediction/MWI_District_accuracy_metrics.csv")
ken <- read.csv("D:/RCMRD/Code/yieldprediction/KEN_County_accuracy_metrics.csv")

#Aggreagate
zmb <- aggregate(.~Method, mean, data = zmb[-c(1,2)], na.rm=T)
mwi <- aggregate(.~Method, mean, data = mwi[-c(1,2)], na.rm=T)
ken <- aggregate(.~Method, mean, data = ken[-c(1,2)], na.rm=T)


#Mean Bias Error
mbe <- data.frame(ZMB=zmb[, "MBE"], MWI=mwi[,"MBE"], KEN=ken[,"MBE"])
mbe <- as.matrix(mbe)
rownames(mbe) <- zmb$Method
root <- "D:/RCMRD/Data/Yields/"
png(paste0(root, "Results/MBE.png"), units="px", width=1900, height=1900, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)

barplot(t(mbe), beside=T, ylab = "Bias (MT/ha)", las=2, col=terrain.colors(3), legend =  rownames(t(mbe)),  args.leg=list(cex=1))
dev.off()
rm(mbe)

#RMSE
rmse <- data.frame(ZMB=zmb[, "RMSE"], MWI=mwi[,"RMSE"], KEN=ken[,"RMSE"])
rmse <- as.matrix(rmse)
rownames(rmse) <- zmb$Method
root <- "D:/RCMRD/Data/Yields/"
png(paste0(root, "Results/RMSE.png"), units="px", width=1900, height=1900, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)

barplot(t(rmse), beside=T, ylab = "RMSE (MT/ha)", las=2, col=terrain.colors(3), legend =  rownames(t(rmse)),  args.leg=list(cex=1))
dev.off()
rm(rmse)

#RRMSE
rrmse <- data.frame(ZMB=zmb[, "RRMSE"], MWI=mwi[,"RRMSE"], KEN=ken[,"RRMSE"])
rownames(rrmse) <- zmb$Method

png(paste0(root, "Results/RRMSE.png"), units="px", width=1900, height=1900, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)
barplot(t(rrmse), beside=T, ylab = "RRMSE (%)", las=2, col=terrain.colors(3), legend =  rownames(t(rrmse)),  args.leg=list(cex=1))
abline(h=30, lty=5, col='red')
dev.off()
rm(rrmse)

#Unbiased RMSE
ubrmse <- data.frame(ZMB=zmb[, "ubRMSE"], MWI=mwi[,"ubRMSE"], KEN=ken[,"ubRMSE"])
rownames(ubrmse) <- zmb$Method

png(paste0(root, "Results/ubRMSE.png"), units="px", width=1900, height=1900, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)

barplot(t(ubrmse), beside=T, ylab = "ubRMSE (MT/ha)", las=2, col=terrain.colors(3),   args.leg=list(cex=1))
legend('topleft', legend =  rownames(t(ubrmse)))
dev.off()
rm(ubrmse)

df <- data.frame()
###NETX CHALENGE ONE MODEL in ALL COUNTRIES.
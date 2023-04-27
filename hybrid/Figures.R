zmb <- read.csv("D:/RCMRD/Data/Yields/Results/ZMB/ZMB_accuracy.csv")
mwi <- read.csv("D:/RCMRD/Data/Yields/Results/MWI/MWI_accuracy.csv")
ken <- read.csv("D:/RCMRD/Data/Yields/Results/KEN/KEN_accuracy.csv")
rmse <- data.frame(ZMB=zmb[, "RMSE"], MWI=mwi[,"RMSE"], KEN=ken[,"RMSE"])
rmse <- as.matrix(rmse)
rownames(rmse) <- zmb$Method
root <- "D:/RCMRD/Data/Yields/"
png(paste0(root, "Results/RMSE.png"), units="px", width=1900, height=1900, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)

barplot(t(rmse), beside=T, ylab = "RMSE (MT/ha)", las=2, col=terrain.colors(3), legend =  rownames(t(rmse)),  args.leg=list(cex=1))
dev.off()

rrmse <- data.frame(ZMB=zmb[, "RRMSE"], MWI=mwi[,"RRMSE"], KEN=ken[,"RRMSE"])
rownames(rrmse) <- zmb$Method

png(paste0(root, "Results/RRMSE.png"), units="px", width=1900, height=1900, res=300, pointsize=16)
par(mar=c(4.5,4.5,2,2)) #c(bottom, left, top, right)

barplot(t(rrmse), beside=T, ylab = "RRMSE (%)", las=2, col=terrain.colors(3), legend =  rownames(t(rrmse)),  args.leg=list(cex=1))
abline(h=30, lty=5, col='red')
dev.off()

###NETX CHALENGE ONE MODEL in ALL COUNTRIES.
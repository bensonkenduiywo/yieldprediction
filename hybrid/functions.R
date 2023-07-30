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
#Bias in “Mean Bias Error” is the tendency of a measurement process to overestimate or underestimate the value of a parameter. Bias has only one direction, which can be either positive or negative. A positive bias means the error from the data is overestimated and a negative bias means the error is underestimated.
#Mean Bias Error (MBE) is the mean of the difference between the predicted values and the actual values.
#This evaluation metric quantifies the overall bias and captures the average bias in the prediction. It is
#almost similar to MAE, the only difference being the absolute value is not taken here. This evaluation
#metric should be handled carefully as the positive and negative errors can cancel each other out.
MBE <- function(obs, pred){
  error <- pred - obs
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
  
} 
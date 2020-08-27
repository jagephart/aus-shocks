# Functions
shock.id <- function(dat, thresh=0.35){
  # dat is your time series data and threshold is the threshold you want for Cook's D (defaulted to 0.35)
  outt <- array(dim=c(length(dat), 3))
  x <- 1:length(dat)
  ll <- lowess(x, dat) # Fits lowess curve (can specify other options for how the curve is estimated and can change the span)
  rr <- as.numeric(dat[order(x)]-ll$y) #residuals off lowess
  rrp1 <- rr[2:length(rr)] # Residuals at time t
  rrm1 <- rr[1:(length(rr)-1)] # Residuals at time t-1
  
  ll2 <- lm(rrp1~rrm1) # Linear fit of the residuals
  cd <- cooks.distance(ll2) # Calculate the Cook's D
  
  outt[2:length(rr),1] <- as.numeric(cd) # Output the Cook's D
  outt[,2] <- rr # Output the residuals
  outt[2:length(rr),3] <- ifelse(as.numeric(cd) >= thresh,1,0) # Logical of whether point is a shock
  
  outt <- as.data.frame(outt)
  colnames(outt) <- c("cooks.d", "residual", "shock.event")
  return(outt)
}

shock.analysis <- function(dat, ts.col.name, ave.window = 5){
  # Rename column with the time series data we are running the shock analysis on
  colnames(dat)[colnames(dat) == ts.col.name] <- "ts.col"
  
  # Initialize columns
  dat$move.ave <- NA
  dat$recovery.time <- NA
  
  # Calculate the moving average for the specified window width
  for(i in ave.window:nrow(dat)){
    dat$move.ave[i] <- mean(dat$ts.col[(i-4):i]) 
  }
  
  # Identify the shocks using the shock.id function
  shocks <- shock.id(dat$ts.col)
  
  dat <- cbind(dat, shocks)
  
  # Calculate the shock magnitude
  dat <- dat %>%
    mutate(shock.magnitude = ifelse(shock.event == 1, ts.col - move.ave, NA))
  
  # Calculate the recovery time (only relevant for negative shocks)
  
  for(i in 2:nrow(dat)){
    if(dat$shock.event[i] == 1 & dat$residual[i] < 0){
      dat$recovery.time[i] <- min(which(dat$move.ave[i:nrow(dat)] > dat$move.ave[i]))-1
    }else{
      dat$recovery.time[i] <- NA
    }
  }

  return(dat)
}

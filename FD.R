pre_processing <-  function(Data)
{
  
  #converting pression into hg level
  m <- -1.9615
  c <- 32.22
  inHG <-  Data$vac
  
  #removing outliers
  l_bound <- 0
  u_bound <- 13
  
  inHG <- inHG[inHG > l_bound]
  inHG <- inHG[inHG <= u_bound]
  
  return(inHG)
}


feature_design <- function(raw_data){
  sk = skewness(raw_data)
  ku = kurtosis(raw_data)
  mod_x = mode_x(raw_data)
  avg = mean(raw_data)
  Quantiles = quantile(raw_data,c(0.02,0.05,0.2,0.8,0.95,0.98))
  mdn <- median(raw_data)
  Var = var(raw_data)
  #mix_mod_f <- mix_mod(raw_data)
  Min =min(raw_data)
  Max = max(raw_data)
  Range = Max - Min
  
  #features = as.data.frame(t(as.vector(c(Min, Max,avg,mdn,mod_x,Var,sk,ku, Quantiles))))
  features = as.data.frame(t(as.vector(c(Min, Max,avg,mdn,mod_x,Var,sk,ku, Quantiles, Range))))
  names(features) <- c("Min","Max","Mean","Median","Mode","Variance",
                       "Skewness","Kurtosis","perc_2","perc_5",
                       "perc_20","perc_80","perc_95","perc_98","Range")
  
  
  return(features)
  
  
}



mode_x <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

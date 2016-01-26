rm(list = ls() )

library( dplyr )

source("link analysis functions integration.R")

i = 1

ldf = read.csv("time_data.csv", header = T, sep=",", stringsAsFactors = F )

names(ldf) = c('datetime')  #....... time column should be named as datetime. Other columns may be named anything.

a = unlist( lapply( ldf$datetime, function(x) strsplit(x,"T") ) )  #...... Extracting the dates

b = seq(from = 1, to = length(a), by = 2)   #.......... Creating odd indexes

c = a[b]     #......... Extracting the dates

date_match_indicator = sum( c != c[1] )  #...... All dates of a batch same means date_match_indicator = 0

if( date_match_indicator == 0 ){
  
  ldf$datetime = unlist( lapply( ldf$datetime, function(x) gsub("+", "-", x, fixed = T) ) )   #..... Replacing + sign with -
  
  t =  unlist( lapply( ldf$datetime, function(x) gsub("^.*[T]|[-].*$", "", x) ) )  #..... formatting the datetime column
  
  ldf = mutate( ldf, formatted_time = t )
  
  v = unlist( lapply( ldf$formatted_time, function(x) convert_to_seconds(x) ) )
  
  ldf = mutate( ldf, time = v )
  
  j = 2  ;  diff = NULL
  
  for( j in 2:length(v) ){
    
    diff = c( diff, abs( v[j] - v[j-1] ) )
  }
  
  ldf = mutate( ldf, difference = c(NA, diff) )
  
  df = ldf[-1,]     #....... Remove the 1st row with NA difference
  
  diff_more_than_1 = which( df$difference > 1 )      #......... Time points when OFF state is detected
  
  if( length(diff_more_than_1) == 0 ){               #......... means no link failure occurs
    
    cat("MTBF and MTTR values are not available. Reason : No link failure occurred")
    
  }else{
    
          mtbf_values = mtbf( df, diff_more_than_1 )/60   #......... MTBF values in mins ....... ideally should be greater than 2 hrs.
      
          mean_more_than_1 = mean( df[ diff_more_than_1, ]$difference )/60  #.... MTTR values in mins .... ideally should be less than 1 min
      
  }
  
}else{
  
  cat( "MTBF and MTTR values are not available. Reason : Batch contains different date" )
}
 





feature_creation = function(x){
  
  basenames = gsub("\\.csv$","", x )
  
  ldf = read.csv( x, header = F )
  
  #....... Extracting pressure value from file names .......
  
  pressure = as.numeric( gsub("^.*[p]|[f].*$", "", basenames) )
  
  #....... Extracting filter state from file names .......
  
  filter_state = as.numeric( gsub("^.*[f]|[a].*$", "", basenames) )
  
  filter_state = ifelse( filter_state == 0, 0, 1 )
  
  #....... Extracting vac from file names .......
  
  vac = as.numeric( gsub("^.*[a]|[b].*$", "", basenames) )
  
  #....... Extracting break from file names .......
  
  brk = as.numeric( gsub("^.*[b]|[a].*$", "", basenames) )
  
  #............... Filtering out bad data ............
  
  #........... Oulier removal ...........
  
  l_bound = 0
  u_bound = 13
  
  outlier_free_rows = which( ( ldf[,2] > l_bound )&( ldf[,2] <= u_bound ) )
  
  ldf = ldf[ outlier_free_rows, ]  ;  ldf = na.omit( ldf )  ;  t = ldf[,2]
  
  #........ Detecting vac and brk data ...........
  
  cutoff = ( quantile(t, 0.95) + quantile(t, 0.05) )/2
  
  indicator = ifelse( t <= cutoff, 0, 1 ) ; ldf = data.frame( t, indicator )  ; names(ldf) = c( 'values', 'vb_indicator' )
  
  run_length_vb = rle( ldf$vb_indicator )$lengths    #..... Run lengths for vac break
  
  if( length( run_length_vb )%%2 == 1 ) {
    
    run_length_vb = run_length_vb[ -length( run_length_vb ) ]   #....... If odd number of runs then delete the last run
    ldf = ldf[1:sum( run_length_vb ), ]   #......... If odd number of runs then deleting the portion of data frame containing last run
  }
  
  o = seq( 1, length( run_length_vb ), by = 2)   #...... Number of cycles in the batch
  
  cycle_store = NULL
  t = 1
  
  for( j in o ){        #........ Loop for creating classes in ith batch
    
    cycle_store = c( cycle_store, rep( t, run_length_vb[j] + run_length_vb[j+1] ) )
    t = t + 1
  }
  
  ldf = mutate( ldf, cycle = cycle_store )
  
  eligible_vb_ratios = c( 10/50, 20/40, 50/10, 1 )
  
  #....... Detecting irregular cycles  ..........
  
  distinct_cycle = unique( ldf$cycle )     #......... Extracting distinct cycle from the batch
  
  regular_cycle_number = NULL
  
  for( j in distinct_cycle ){      #...... Loop for distinct cycle
    
    data_cycle = filter( ldf, cycle == j )   #........ Data from distinct cycle from a batch
    
    vb_ratio_cycle = sum( data_cycle$vb_indicator == 1 ) / sum( data_cycle$vb_indicator == 0 )  #....... Computes vb ratio for each cycle
    
    f = sum( ( vb_ratio_cycle > ( eligible_vb_ratios - 0.02 ) ) & ( vb_ratio_cycle < ( eligible_vb_ratios + 0.02 ) ) )
    
    if( f > 0 ){
      regular_cycle_number = c( regular_cycle_number, j )
    }
  
   }
  
  data_cycle = list()
  
  for( j in regular_cycle_number ){
    
    data_cycle[[j]] = filter( ldf, cycle == j )
  }
  
  data_from_all_cycles = data.frame( bind_rows( data_cycle ) )
  
  y = na.omit( data_from_all_cycles[,1] )
  
  sk = skewness(y)
  ku = kurtosis(y)
  mod_x = mode_x(y)
  avg = mean(y)
  Quantiles = quantile(y,c(0.02,0.05,0.2,0.8,0.95,0.98))
  mdn = median(y)
  Var = var(y)
  Min = min(y)
  Max = max(y)
  Range = Max - Min

    z = c( filter_state, pressure, Min, Max, avg, mdn, mod_x, Var, sk, ku, Quantiles, Range )
  
  return( z )
  
}

mode_x <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

#.......... Function to merge multiple data sets kept in a single folder

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  datalist = do.call("rbind", datalist) }

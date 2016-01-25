outlier_data_preparation = function(x){       #..... Takes a filepath eg. filepath of a batch
  
  vacuum_data = read.csv( x, header = F )
  #print(vacuum_data)
  
  #.................... Filter based on vac data ....................
  
  #... Filter  - Oulier data removal based on pre-defined bounds ...........
  
  l_bound = 0
  u_bound = 13
  
  outlier_free_rows = which( ( vacuum_data > l_bound )&( vacuum_data <= u_bound ) )
  
  vacuum_data = vacuum_data[ outlier_free_rows, ]  ;  vacuum_data = na.omit( vacuum_data )  ;  t = vacuum_data
  
  #........ Detecting vac and brk data ...........
  
  cutoff = ( quantile(t, 0.95) + quantile(t, 0.05) )/2
  
  indicator = ifelse( t <= cutoff, 0, 1 ) ; vacuum_data = data.frame( t, indicator )  ; names(vacuum_data) = c( 'values', 'vb_indicator' )
  
  run_length_vb = rle( vacuum_data$vb_indicator )$lengths    #..... Run lengths for vac break
  
  if( length( run_length_vb )%%2 == 1 ) {
    
    run_length_vb = run_length_vb[ -length( run_length_vb ) ]   #....... If odd number of runs then delete the last run
    vacuum_data = vacuum_data[1:sum( run_length_vb ), ]   #......... If odd number of runs then deleting the portion of data frame containing last run
  }
  
  o = seq( 1, length( run_length_vb ), by = 2)   #...... Number of cycles in the batch
  
  cycle_store = NULL
  t = 1
  
  for( j in o ){        #........ Loop for creating classes in batch
    
    cycle_store = c( cycle_store, rep( t, run_length_vb[j] + run_length_vb[j+1] ) )
    t = t + 1
  }
  
  library( dplyr )
  
  vacuum_data = mutate( vacuum_data, cycle = cycle_store )
  
  #....... Detecting irregular cycles  ..........
  
  distinct_cycle = unique( vacuum_data$cycle )     #......... Extracting distinct cycle from the batch
  
  vac_lengths = NULL ; brk_lengths = NULL
  
  for( j in distinct_cycle ){      #...... Loop for distinct cycle
    
    data_cycle = filter( vacuum_data, cycle == j )   #........ Data from distinct cycle from a batch
    
    vac_lengths = c( vac_lengths, sum( data_cycle$vb_indicator == 1 ) )
    
    brk_lengths = c( brk_lengths, sum( data_cycle$vb_indicator == 0 ) )
    
  }
  
  z1 = data.frame( vac_lengths, brk_lengths, cycle = distinct_cycle )
  
  many_outlier_flag = random_batch_detector( z1 )
  
  if( many_outlier_flag == 0 ){            #........... 0 means good batch
    
    outlier_cycles = outlier_cycle_detector( z1 )
    
    d = NULL
    
    for( k in outlier_cycles ){
      
      d = c( d, which(vacuum_data$cycle == k) )   #........ stores rows corresponding to outlier cycles
    }
    
    d1 = as.vector( vacuum_data[ -d, 1] )
    
    return(d1)

  } else{
    
    return(NULL)     #.......... returns NULL for random batches ( i.e. batches with too many outliers )
  }
  
}

#.......... Detects batch with lots of outliers and where outliers forms clusters

random_batch_detector = function(x){           #..... Takes a data frame with vac_lengths etc. as column
  
  criteria = apply( x, 2, function(t) mean( quantile(t, c(0.98,0.95,0.9,0.85,0.8)) - quantile(t, c(0.02,0.05,0.1,0.15,0.2)) ) )
  
  vac_flag = ifelse( floor( criteria[1] ) <= 23 , 0, 1 )    #...... 0 == 'Good batch - non random'
  
  brk_flag = ifelse( floor( criteria[2] ) <= 23 , 0, 1 )    #...... 0 == 'Good batch - non random'
  
  return( vac_flag + brk_flag )    #...... For good batch sum must be 0
}



outlier_cycle_detector = function(x){     #..... Takes a data frame with vac_lengths etc. as column
  
  #.......... Filter 1 : Detecting outlier cycles for vac_lengths
  
  vac_lengths_cutoff = quantile( x$vac_lengths, c( 0.05, 0.95 ) )
  
  filter_1_outlier_cycles = which( ( x$vac_lengths < vac_lengths_cutoff[1] )|( x$vac_lengths > vac_lengths_cutoff[2] ) )
  
  
  #................ Filter 2 : Detecting outlier cycles for brk_lengths
  
  brk_lengths_cutoff = quantile( x$brk_lengths, c( 0.05, 0.95 ) )
  
  filter_2_outlier_cycles = which( ( x$brk_lengths < brk_lengths_cutoff[1] )|( x$brk_lengths > brk_lengths_cutoff[2] ) )
  
  
  outlier_cycles = unique( c( filter_1_outlier_cycles, filter_2_outlier_cycles ) )
  
  return( outlier_cycles )
  
}


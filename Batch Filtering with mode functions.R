outlier_data_preparation = function(x){       #..... Takes a batch containing vac values only
  
  vacuum_data = x
   
  #.................... Filter based on vac data ....................
  
  #... Filter  - Oulier data removal based on pre-defined bounds ...........
  
  vacuum_data = na.omit( vacuum_data )  ;  t = vacuum_data
  
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
    
    return(z1)
    
  } else{
    
    return(NULL)     #.......... returns NULL for random batches ( i.e. batches with too many outliers )
  }   

      
}


#.......... Detects batch with lots of outliers and where outliers forms clusters

random_batch_detector = function(x){           #..... Takes a data frame with vac_lengths etc. as column
  
  library( dplyr )
  
  n = nrow(x)
  
  criteria = apply( x, 2, function(t) Mode(t) )
  
  vac_outlier_cycles = union( which( x$vac_lengths < criteria[1] - 10 ), which( x$vac_lengths > criteria[1] + 10 ) )
  
  brk_outlier_cycles = union( which( x$brk_lengths < criteria[2] - 10 ), which( x$brk_lengths > criteria[2] + 10 ) )
  
  vac_brk_combined_outlier_cycles = union( vac_outlier_cycles, brk_outlier_cycles )
  
  flag = ifelse( length(vac_brk_combined_outlier_cycles)/n < 0.3, 0, 1 )
  
  return( flag )    #...... For good batch flag must be 0
}


#.......... Function to fid Mode ...............

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


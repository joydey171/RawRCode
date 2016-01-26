rm(list = ls())

source("Batch Filtering with mode functions.R")


batch_vac_raw_data = read.csv('vac_raw_data_test.csv', header = F, sep=",")

algo_name = 'belt_tension'    #........ Input from Java

vac_data = batch_vac_raw_data[,1]

#.................... Rejection Criteria 1 and 2 ....................

c1 = which( vac_data > 13 )  ;  c2 = which( vac_data > 10 )

if( length(c1) > 0 ){       #......... Reject batch for all algo
  
  cat('Random Batch - Prediction not available')
  
}else{
  
  if( ( length(c2) > 0 )&( ( algo_name == 'oil_level' )|( algo_name == 'oil_state' )|( algo_name == 'belt_tension' )  ) ){
    
    cat('Random Batch - Prediction not available') #......... Reject batch for oil_level, oil_state and belt_tension only
  
  }else{
    
    filtered_vac_data = outlier_data_preparation( batch_vac_raw_data[,1] )
    
    if( length( filtered_vac_data ) == 0 ){
      
      cat('Random Batch - Prediction not available')
    } else{
      
      filtered_vac_data     #......... Batch is returned
    }
  }
  
}

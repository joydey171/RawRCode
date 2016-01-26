convert_to_seconds = function(u){
  
  sapply( strsplit(u,":"),
          
          function(x) {
            x <- as.numeric(x)
            3600*x[1] + 60*x[2] + x[3]
          }
  )
}

mtbf = function(x, y){
  
  i = 0 ; a = NULL ; y = c(1, y)
  
  for( i in 1:( length(y) - 1 ) ){
    
      a[i] = sum( x[ ( y[i]+1 ):( y[i+1] - 1 ), ]$difference )    #...... Sum of difference for off1 + 1 to OFF2 - 1
  }
  
  return( mean(a) )
}

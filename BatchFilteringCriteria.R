args <- commandArgs(TRUE)
#train <- read.csv(args[1])
#test <- read.csv(args[2])
source(args[1])

batch_vac_raw_data = read.csv(args[2], header=TRUE, sep=",")
#print(batch_vac_raw_data)
print(args[2])
filtered_vac_data = outlier_data_preparation( args[2] )

if( length( filtered_vac_data ) == 0 ){
  
  cat('Random Batch - Prediction not available')
} else{
  filtered_vac_data
  print("B:")
print(filtered_vac_data)
print("E:")
 #write.csv()
}
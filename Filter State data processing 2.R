rm( list = ls() )

library( dplyr ) ; library( e1071 )

filenames = list.files( 'G:/Zreyas Technology Project/Filter/10 hp/Data/30st Dec trial 1', pattern="*.csv", full.names = T )

source( 'G:/Zreyas Technology Project/Filter/10 hp/Scripts/data processing 1.R' )

df = list()

i = 1

for( i in 1:length( filenames ) ) {
  
  df[[i]] = as.data.frame( t( feature_creation( filenames[i] ) ) )
  
}

final = bind_rows( df )

names( final ) = c( 'Filter_state', 'Pressure', 'Min', 'Max', 'Mean', 'Median', 'Mode', 'Variance', 'Skewness', 'Kurtosis', 'perc_2', 'perc_5', 'perc_20', 'perc_80', 'perc_95', 'perc_98', 'Range' )

final = arrange( final, Filter_state )

write.csv( final, file = 'G:/Zreyas Technology Project/Filter/10 hp/Data/Training datasets/30st_Dec_trial_1_10hp.csv', row.names = F )


#............ Merging data sets to create training data ...........

combined_train = multmerge('G:/Zreyas Technology Project/Filter/10 hp/Data/Training datasets')

write.csv( combined_train, file = 'G:/Zreyas Technology Project/Filter/10 hp/Data/combined_train_10hp.csv', row.names = F )


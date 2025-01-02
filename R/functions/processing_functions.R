save_data = function(data, df_name, filename){

  df_name = data

  save(df_name, file=paste(filename, '.Rda', sep=''))
  write.csv(data, file=paste(filename, '.csv', sep=''), row.names=F)
}

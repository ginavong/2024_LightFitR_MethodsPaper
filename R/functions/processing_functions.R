save_data = function(df, filename){
  save(df, file=paste(filename, '.Rda', sep=''))
  write.csv(df, file=paste(filename, '.csv', sep=''), row.names=F)
}
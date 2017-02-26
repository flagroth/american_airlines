getData <- function(){
  for(i in 1:12)
  {
    download.file(paste0("https://transtats.bts.gov//PREZIP/On_Time_On_Time_Performance_2016_",i,".zip"),paste0("data/Performance_2016_",i,".zip"))
    unzip(paste0("../data/Performance_2016_",i,".zip"),overwrite=T, exdir = "data")
  }
}

mergeFiles <- function(){
  perf <- c()
  for(i in 1:12)
  {
    aux <- fread(paste0("../data/On_Time_On_Time_Performance_2016_",i,".csv"), data.table = F, stringsAsFactors = T)
    perf <- rbind(perf,aux)
  }
  write.csv(perf,"../data/On_Time_On_Time_Performance_2016_full.csv", row.names = F, col.names = T)
}

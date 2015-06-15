pollutantmean <- function(directory, pollutant, id = 1:332) 
{ 
  data_total_sum <- 0
  data_total_length <- 0
  dir <- paste("C:/Users/akshay.kher/Desktop/",directory, sep ="")
  setwd(dir)
  for (i in seq_along(id))
  {
    if (id[i] <= 99)
    {
      id_new = sprintf("%03d", id[i]) ## eg : convert 1 to "001" and 89 to "089"
    }
    else
    {
      id_new = id[i] ## eg : 112 remains 112, 301 remains 301
    }
    #options(max.print=1000000)
    data_path = paste(id_new,".csv",sep="") ## eg : 001.csv, sep="" for no space
    data <- read.csv(data_path)
    sulphur_or_nitrate_row = which(colnames(data) == pollutant) ## find index of column
    data <- data[,sulphur_or_nitrate_row] ## store data of pollutant
    data_remove_na <- data[!is.na(data)] ## remove NA values and store data
    data_total_length = data_total_length + length(data_remove_na) ## total rows/ length of the pollutant
    data_total_sum = data_total_sum + sum(data_remove_na) ## total sum of pollutant values
  }
  ((data_total_sum) / (data_total_length)) ## mean calculation
}

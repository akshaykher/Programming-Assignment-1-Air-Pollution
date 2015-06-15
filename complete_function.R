complete <- function(directory, id = 1:332) 
{ 
  data_matrix = matrix(0, ncol = 2, nrow = length(id)) ## initialize a matrix with 2 columns and 'length of id' rows
  colnames(data_matrix) = c("id","nobs")
  data_dataframe = data.frame(data_matrix) ## convert to dataframe
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
    data_complete_cases <- data[complete.cases(data),] 
    data_complete_cases_number <- nrow(data_complete_cases) # count number of complete cases
    id_new <- as.numeric(id_new) ## eg : "001" to 1, "091" to 91
    data_dataframe[i,1] <- id_new ## columnOne contains id's
    data_dataframe[i,2] <- data_complete_cases_number ## columnTwo contains number of complete cases
  }
  data_dataframe
  
}
  


corr <- function(directory, threshold = 0) 
{
  data <- complete("specdata", id = 1:332) # show nobs for all files
  
  if(threshold > max(data[,2]))
  {
    print ("threshold value above max nobs and return value is 0")
    vec = vector(mode="numeric", length=0)
    return(vec)
  }
  
  data_logic_vector <- data[,2] > threshold # make logic vector where nobs > 150
  data_all_above_threshold <- data[data_logic_vector,]       # store all data where nobs >150
  data_all_above_threshold_id <- data_all_above_threshold [,1] # corresponding id's
  number_of_nobs_above_threshold = length(data_all_above_threshold_id) # total nobs above threshold
  for(j in 1:number_of_nobs_above_threshold)
  {
      if(j==1)
        {
         matrix_data = matrix(0,  ncol = 1, nrow = number_of_nobs_above_threshold ) ## initialize a matrix with 1 row equal to total nobs above threshold 
         dataframe_data = data.frame(matrix_data) ## convert to dataframe
        }
      
    data_id = data_all_above_threshold_id[j] ## gives corresponding id number of nobs > threshold
    
    if (data_id <= 99)
    {
      id_new = sprintf("%03d", data_id) ## eg : convert 1 to "001" and 89 to "089"
    }
    else
    {
      id_new = data_id ## eg : 112 remains 112, 301 remains 301
    }
    data_path = paste(id_new,".csv",sep="") ## eg : 001.csv, sep="" for no space
    data_of_data_path = read.csv(data_path) ## read the first file having nobs > threshold 
    data_path_sulfate = data_of_data_path[,2] ## sulfate column
    data_path_nitrate = data_of_data_path [,3] ## nitrate column   
    data_cor = cor(data_path_sulfate, data_path_nitrate, use="pairwise.complete.obs")
    dataframe_data[j,1] = data_cor
  }
  dataframe_data[,1] ##[,1] used to display data in a single row and not a column
}

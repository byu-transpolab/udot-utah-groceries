#get the original data from the csv file provided

create_data <- function(data_path){
  y<- read.csv(data_path)
  y
}
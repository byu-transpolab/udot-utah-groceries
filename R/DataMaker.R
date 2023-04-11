#get the original data from the csv file provided

create_data <- function(file){
  x<- read.csv(file)
  x
}

existing_data <- function(file1, file2, file3){
  x <- read.csv(file1)
  y <- read.csv(file2)
  z <- read.csv(file3)
  
  a <- rbind(x, y, z)
  a <- subset(a, select = -c(X))
  a
}

impute_data <- function(data, acquired_data){
  #thought process
  
  ###PROBLEM
  #at some point in the original project, the object id of each store is changed,
  #how was this new id assigned?
  #was it done manually?
  #do we need to match the geometry in order to reassign the original object id so
  #that we can use the data set of all of the store locations?
  
  #combine the data data.frame with te acquired data frame, and make columns to 
  #include the data from the acquired data.frame tp create one large data frame 
  #with missign values for unknown store attributes
  
  #segment the data into three sub frames that correspond with counties that are
  #similar to salt lake, utah, and san juan
  
  ###PROBLEM
  #we do not have the square footage data for the other utah stores, so another 
  #method of comaprison needs to be used. We could use a method that uses the nearest 
  #store that matches that name, or we could use an average
  
}
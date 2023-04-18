#get the original data from the csv file provided

create_data <- function(grocery_file, county_file){
  #reads in geojson with all of the grocery data
  groceries <- st_read(grocery_file)
  groceries$county <- ""
  
  #reads in the shape file with all of the county boundary information
  boundaries <- st_read(county_file)
  
  #finds the county associated with each store
  g_geom <- groceries %>% select(geometry) %>% st_as_sf()
  b_geom <- boundaries %>% select(geometry) %>% st_as_sf()
  
  for(x in 1:length(b_geom$geometry)){
    for(y in 1:length(g_geom$geometry)){
      if(st_intersects(g_geom[y,1], b_geom[x,1], sparse = FALSE)==TRUE){
        groceries$county[y] <- boundaries$NAME[x]
      }
    }
  }
  groceries
}


existing_data <- function(file1, file2, file3){
  x <- read.csv(file1)
  y <- read.csv(file2)
  z <- read.csv(file3)
  
  a <- rbind(x, y, z)
  a <- subset(a, select = -c(X))
  a
}

combine_data <- function(data, exist_data){
  #start with prepping the grocery stores data
  data <- as.data.frame(data)
  for(x in 1:length(data$CITY)){
    if(grepl("WEST VALLEY CITY", data$CITY[x], fixed = TRUE)== TRUE){
      data <- data[-x,]
    }
    if(grepl("SAN JUAN", data$county[x], fixed = TRUE)== TRUE){
      data <- data[-x,]
    }
    if(grepl("UTAH", data$county[x], fixed = TRUE)== TRUE){
      data <- data[-x,]
    }
  }
  data$registers <- NA
  data$selfchecko <- NA
  data$total_registers <- NA
  data$availability <- NA
  data$cost <- NA
  data$market <- NA
  data$brand <- NA
  data$Name <- data$NAME
  data$type <- data$TYPE
  
  data <- data %>% select(Name, type, county, registers, selfchecko, total_registers,
                          availability, cost, market, brand)
  
  #now for the data that already exists
  exist_data$county <- ""
  for(x in 1:length(exist_data$id)){
    if(grepl("UT", exist_data$id[x], fixed = TRUE) == TRUE){
      exist_data$county[x] <- "UTAH"
    } else if(grepl("SJ", exist_data$id[x], fixed = TRUE) == TRUE){
      exist_data$county[x] <- "SAN JUAN"
    } else if(grepl("SL", exist_data$id[x], fixed = TRUE) == TRUE){
    exist_data$county[x] <- "SALT LAKE"
    }
  }
  exist_data <- exist_data %>% select(Name, type, county, registers, selfchecko, total_registers,
                                      availability, cost, market, brand)
  
  combined_data <- rbind(data, exist_data)
  combined_data
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
  
  #segment the data into three sub data sets that correspond with counties that are
  #similar to salt lake, utah, and san juan
  
  ###PROBLEM
  #we do not have the square footage data for the other utah stores, so another 
  #method of comaprison needs to be used. We could use a method that uses the nearest 
  #store that matches that name, or we could use an average
  
}
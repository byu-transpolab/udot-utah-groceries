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
  
  groceries <- as.data.frame(groceries) %>% mutate(Latitude = unlist(map(groceries$geometry,2)),
                                                    Longitude = unlist(map(groceries$geometry,1)))
  
  groceries
}


existing_data <- function(file1, file2, file3){
  #reads in files
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
  
  data <- data %>% select(Name, type, county, 
                          #registers, selfchecko, 
                          total_registers,
                          availability, cost, market, brand, Latitude, Longitude)
  
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
  exist_data <- exist_data %>% select(Name, type, county, 
                                      #registers, selfchecko, 
                                      total_registers,
                                      availability, cost, market, brand, Latitude, Longitude)
  
  #binds the existinf data with the overall grocery stores data
  combined_data <- rbind(data, exist_data)
  
  #creates and assigns the data for the large brand stores
  for(x in 1:length(combined_data$brand)){
    if(grepl("mace", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$brand[x] <- "Macey's"
    } else if(grepl("harmon", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$brand[x] <- "Harmons"
    } else if(grepl("family", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$brand[x] <- "Family Dollar"
    } else if(grepl("smit", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$brand[x] <- "Smith's"
    } else if(grepl("targ", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$brand[x] <- "Target"
    } else if(grepl("tree", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$brand[x] <- "Dollar Tree"
    } else if(grepl("wal", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      if(grepl("mart", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
        combined_data$brand[x] <- "Walmart"
      } else if(grepl("reen", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
        combined_data$brand[x] <- "Walgreens"
      }
    } else if(grepl("winco", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$brand[x] <- "Winco"
    } else if(grepl("dollar general", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$brand[x] <- "Dollar General"
    }else {
      combined_data$brand[x] <- "Other"
    }
  }
  
  #creates and assigns the data for the type of stores
  for(x in 1:length(combined_data$type)){
    if(grepl("dollar", tolower(combined_data$Name[x]), fixed = TRUE) == TRUE){
      combined_data$type[x] <- "Dollar Store"
    } else if(grepl("grocery", tolower(combined_data$type[x]), fixed = TRUE) == TRUE){
      combined_data$type[x] <- "Grocery Store"
    } else{
      combined_data$type[x] <- "Other"
    }
    
    if(grepl("walmart", tolower(combined_data$brand[x]), fixed = TRUE) == TRUE){
      combined_data$type[x] <- "Grocery Store"
    }
  }
  
  combined_data$county_type <- NA
  
  for(x in 1:length(combined_data$county)){
    if(grepl("utah", tolower(combined_data$county[x]), fixed = TRUE) == TRUE){
      combined_data$county_type[x] <- 1
    } else if(grepl("wasatch", tolower(combined_data$county[x]), fixed = TRUE) == TRUE){
      combined_data$county_type[x] <- 1
    } else if(grepl("salt", tolower(combined_data$county[x]), fixed = TRUE) == TRUE){
      combined_data$county_type[x] <- 2
    } else{
      combined_data$county_type[x] <- 3
    }
  }
  
  combined_data
}

impute_data <- function(data){
  data <- data %>% as_tibble() %>% select(Name, type,county, total_registers,
                                          availability, cost, market, brand,
                                          county_type, income, black, asian,
                                          white, population)
  
  imp <- mice(data, method = "mean", m = 10, maxit = 25)
  
  imputed_data <- complete(imp)
  imputed_data
  
  #potentially create three county variables and use mean that way
  #pull in the ethnic data
}


get_acs_data <- function(combined_data, county_file){
  counties <- st_read(county_file) %>% select(NAME)
  
  acs_tibble <- get_acs_income_data("UT", counties$NAME[1])
  for(x in 2:length(counties$NAME)){
    acs_tibble <- rbind(acs_tibble, get_acs_income_data("UT", counties$Name[x]))
  }
  acs_tibble_2 <- acs_tibble %>% group_by(geoid) %>% slice(1)
  
  DT_sf = st_as_sf(combined_data, coords = c("Longitude", "Latitude"), crs = 4326)
  DT_sf$income <- NA
  DT_sf$black <- NA
  DT_sf$asian <- NA
  DT_sf$white <- NA
  DT_sf$population <- NA
 
  acs_geom <- acs_tibble_2 %>% select(geometry) %>% st_as_sf() %>% st_transform(4326)
  grocery_geom <- DT_sf %>% select(geometry) %>% st_as_sf()
  
  for(x in 1:length(acs_geom$geometry)){
    for(y in 1:length(grocery_geom$geometry)){
      if(st_intersects(grocery_geom$geometry[y], acs_geom$geometry[x], sparse = FALSE)==TRUE){
        DT_sf$income[y] <- acs_tibble_2$income[x]
        DT_sf$black[y] <- acs_tibble_2$black[x]
        DT_sf$asian[y] <- acs_tibble_2$asian[x]
        DT_sf$white[y] <- acs_tibble_2$white[x]
        DT_sf$population[y] <- acs_tibble_2$population[x]
      }
    }
  }
  DT_sf
  #acs_tibble_2
}

get_acs_income_data <- function(stat, county){
  variables <- c("population" = "B02001_001",
                 "income" = "B19013_001",
                 "white" = "B03002_003",
                 "black" = "B03002_004",
                 "asian" = "B03002_006")
  
  #income_table <- get_acs(geography = "tract", variables = variables,
   #       state = stat, county = county, geometry = TRUE) %>%
    #as_tibble()
  
  
  tibble <- get_acs(geography = "tract", variables = variables,
          state = stat, county = county, geometry = TRUE) %>%
    select(-moe) %>%
    spread(variable, estimate) %>%
    transmute(
      geoid = GEOID,
      group = 1,
      population,
      income,
      # many of the variables come in raw counts, but we want to consider
      # them as shares of a relevant denominator.
      black        = 100 * black / population,
      asian        = 100 * asian / population,
      white        = 100 * white / population
    ) %>%
    filter(population > 0) %>%
    as_tibble()
  
  
}



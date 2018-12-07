setwd("~/Box/P722 - MRRIC AM Support/Working Docs/P722 - HydroViz")
rm(list=ls())
cat("\014")
while (dev.cur()>1) dev.off()

if(!"openxlsx" %in% rownames(installed.packages())) {
  install.packages("openxlsx")
}

if(!"svDialogs" %in% rownames(installed.packages())) {
  install.packages("svDialogs")
}

if(!"lubridate" %in% rownames(installed.packages())) {
  install.packages("lubridate")
}

library("lubridate")

library("openxlsx")
library("dplyr")
library("svDialogs")


# Choose file and get path
file_path <- file.choose(getwd())

split_string <- strsplit(toString(file_path), "/", fixed = TRUE)[[1]]
filename <- split_string[length(split_string)]

filename_no_extension <- strsplit(toString(filename), ".", fixed = TRUE)[[1]][1]
file_path_no_extension <-strsplit(toString(file_path), ".", fixed = TRUE)[[1]][1]
  
# READ DATA
rawData <- read.xlsx(file_path, sheet = 1, colNames  = FALSE)

# Create EMPTY dataframe
df <- data.frame()

# How many rows of metadata?
# Make this more robust
meta_rows = 7 # CHECK THE ASSUMPTION THAT THE DATA WILL BE IN THE SAME FORMAT EVERY TIME!
# source = "RIVER" # Use RIVER for RAS data and RESERVOIR for RES data 

# Reshape data
for (i in 1:length(names(rawData)))
{
  # Ignore the first two columns (i = 1, 2)
  
  if(i >= 3)
  {
    # Get metadata
    message(c("Processing Data Column ", i-2, " of ", length(names(rawData))-2))
    metadata <- rawData[1:meta_rows,i]
    
    # Get values
    tempValues <- data.frame(rawData[8:nrow(rawData), c(1,2,i)])
    colnames(tempValues) <- c("id", "date", "value")
    
    # Transpose metadata
    meta_transposed <- data.frame(t(metadata))
    meta_transposed$source <- NA   # CAN I DROP STEPS LIKE THIS TO SAVE COMPUTATIONAL EFFORT??
    meta_names <- c("river", "location", "type", "EMPTY", "alternative", "units", "measure", "source")
    colnames(meta_transposed) <- meta_names
    
    # Find out if it is RES or RAS data
    # Get the first three letters of the alternative name. If RES then set source=RESERVOIR, if RAS then source=RIVER
    
    dataset_type <- substr(meta_transposed$alternative, 1, 3)
    
    if (dataset_type == "RAS") {
      source = "RIVER"
      message("* RAS Data *")
    } else if (dataset_type == "RES") {
      source = "RESERVOIR"
      message("* RES Data *")
    } else {
      source = "UNKNOWN"
      message("*** Unknown data source - not sure if it is RES or RAS ***")
    }
  
    # Add empty columns
    tempValues[,meta_names] <- NA
    
    # Add transposed metadata to each row of values
    tempValues[,4:length(tempValues)] <- meta_transposed
    tempValues[,"source"] <- source
    
    # Add alternative to the dataframe
    df <- rbind(df, tempValues)
  }
}

# Update the rownames
rownames(df) <- seq(length=nrow(df)) 

# Reorder columns
df_reordered <-df[c("id","alternative","type","source","river","location","date","value","units","measure","EMPTY")]

# Fix date format
df_reordered[,"date"] <- as.Date(df_reordered[,"date"] , origin="1899-12-30") 

## REMOVE LEAP DAYS AND 1930 (INCOMPLETE YEAR)
df_no1930 <- df_reordered[lubridate::year(df_reordered$date) != 1930,]
df_final <- df_no1930[!(lubridate::month(df_no1930$date)==2 & lubridate::day(df_no1930$date)==29),]


# Export
# temp_filename <- strsplit(toString(filename), ".", fixed = TRUE)[[1]][1]
# new_filename <- paste(temp_filename,".csv", sep="")

# save_path <- rchoose.dir(getwd(), caption = "Where would you like to save the .CSV file?")

# save_filename <-paste(save_path, paste(filename_no_extension, ".csv", sep=""), sep="")


res <- dlg_message("Would you like to save the key-value data file (.csv) to disk?", "yesno")$res

if (res == "yes") {
  save_filename <- paste(file_path_no_extension, ".csv", sep="")
  
  msg_box(c("Writing to ", save_filename))
  write.csv(df_final, file=save_filename, row.names=FALSE)
  msg_box(c("Data file written to: ", save_filename))
}
  

##  CREATE SEPARATE TABLES

## Build ALTERNATIVES table
alternatives <- data.frame(dplyr::distinct(df_final, alternative))
alternatives <- cbind(id=rownames(alternatives), alternatives)

## Build SOURCES table
sources <- data.frame(dplyr::distinct(df_final, source))
sources <- cbind(id=rownames(sources), sources)

# # Add 'Reservoir' to sources for now
# sources <- rbind(sources, data.frame(id="2", source="RESERVOIR"))

## Build TYPES table
types <- data.frame(dplyr::distinct(df_final, type, units, measure))
types <- cbind(id=rownames(types), types)

## Build RIVERS table
rivers_temp <- data.frame(dplyr::distinct(df_final, source, river))
rivers_temp <- cbind(id=rownames(rivers_temp), rivers_temp)

# # Add another river for testing purposes
# rivers_temp <- rbind(rivers_temp, data.frame(id="3", source="RESERVOIR", river="TEST"))

index <- match(rivers_temp$source, sources$source, nomatch = NA_integer_, incomparables = NULL)
source_ids <- sources$id[index]

rivers <- rivers_temp
rivers$source <- source_ids
rivers <- dplyr::rename(rivers, source_id = source)

## Build LOCATIONS table
locations <- data.frame(dplyr::distinct(df_final, river, location))
locations <- cbind(id=rownames(locations), locations)

# Find river_ids, swap out river names in 'locations' for river$id
index <- match(locations$river, rivers$river, nomatch = NA_integer_, incomparables = NULL)
locations$river <- rivers$id[index]
locations <- dplyr::rename(locations, river_id = river)


## Build DATA_BRIDGE table
bridge_table_temp <- df_final

# Alternative ids
index <- match(bridge_table_temp$alternative, alternatives$alternative, nomatch = NA_integer_, incomparables = NULL)
bridge_table_temp$alternative <- alternatives$id[index]
bridge_table_temp <- dplyr::rename(bridge_table_temp, alt_id = alternative)

# Type ids
index <- match(bridge_table_temp$type, types$type, nomatch = NA_integer_, incomparables = NULL)
bridge_table_temp$type <- types$id[index]
bridge_table_temp <- dplyr::rename(bridge_table_temp, type_id = type)

# Source ids
index <- match(bridge_table_temp$source, sources$source, nomatch = NA_integer_, incomparables = NULL)
bridge_table_temp$source <- sources$id[index]
bridge_table_temp <- dplyr::rename(bridge_table_temp, source_id = source)

# River ids
index <- match(bridge_table_temp$river, rivers$river, nomatch = NA_integer_, incomparables = NULL)
bridge_table_temp$river <- rivers$id[index]
bridge_table_temp <- dplyr::rename(bridge_table_temp, river_id = river)

# Location ids
index <- match(bridge_table_temp$location, locations$location, nomatch = NA_integer_, incomparables = NULL)
bridge_table_temp$location <- locations$id[index]
bridge_table_temp <- dplyr::rename(bridge_table_temp, location_id = location)

index <-match(c("alt_id", "type_id", "source_id", "river_id", "location_id"),colnames(bridge_table_temp), nomatch = NA_integer_, incomparables = NULL)
bridge_table <- bridge_table_temp[,index]

bridge_table <- unique(bridge_table)
rownames(bridge_table) <- seq(length=nrow(bridge_table)) 
bridge_table <- cbind(id=rownames(bridge_table), bridge_table)

## Build DATA table
df <- bridge_table
bridge_table$code <- paste(df$alt_id, df$type_id, df$source_id, df$river_id, df$location_id)

data_temp <- bridge_table_temp
data_temp$code <- paste(data_temp$alt_id, data_temp$type_id, data_temp$source_id, data_temp$river_id, data_temp$location_id)

data <- dplyr::select(data_temp, id, date, value, code)

data$code <- match(data$code, bridge_table$code, nomatch = NA_integer_, incomparables = NULL)
data <- dplyr::rename(data, data_bridge_id = code)

# Add year, month, day to data table
data$year <- lubridate::year(data$date)
data$month <- lubridate::month(data$date)
data$day <- lubridate::day(data$date)

# Reorder columns
data <-data[c("id","data_bridge_id","dates_id","date","value","year","month","day")]


# Convert values to numeric
options(digits=9)
data$value <- as.numeric(data$value)


## Build DATES table
one_year <- df_final[year(df_final$date)==1931,]$date
dates <- data.frame(id=1:365)
dates$month_name <- months(unique(one_year))
dates$month <- as.integer(format(unique(one_year), "%m"))
dates$day <- lubridate::day(unique(one_year))

data$dates_id <- dates[dates$month==data$month && dates$day==data$day, "id"]



## CREATE STATS_TABLE
# bridge_ids <- unique(data$data_bridge_id)

stats_data_temp <- tidyr::spread(data[,c("data_bridge_id", "dates_id", "year", "value")], year, value)
stats_data_temp <- cbind(id=1:nrow(stats_data_temp), stats_data_temp)

stats <- data.frame(
  minimum=numeric(),
  tenth=numeric(),
  fiftieth=numeric(),
  avg=numeric(),
  ninetieth=numeric(),
  maximum=numeric()
)


start_time <- Sys.time()

for (i in 1:nrow(stats_data_temp)) {
  message(c("Processing row ", i, " of ", nrow(stats_data_temp)))
  stats[i,c("tenth","fiftieth","ninetieth")] <- quantile(stats_data_temp[i,3:ncol(stats_data_temp)],probs=c(0.1, 0.5, 0.9))
  stats[i,"minimum"] <- min(stats_data_temp[i,3:ncol(stats_data_temp)])
  stats[i,"avg"] <- mean(unlist(stats_data_temp[i,3:ncol(stats_data_temp)]))
  stats[i,"maximum"] <- max(stats_data_temp[i,3:ncol(stats_data_temp)])
}

end_time <- Sys.time()

elapsed <- end_time - start_time
message(c("It took ", elapsed, " minutes to calculate stats for  ", nrow(stats_data_temp), " rows of data"))


stats_data <- stats_data_temp[,1:3]
stats_data <- cbind(stats_data,stats)

stats_data_all <- cbind(stats_data_temp, stats)


## NEXT STEPS
# Drop columns that aren't needed in SQL from the tables (e.g., year, month, day in data table)
# Continue to write SQL queries to add data to tables



## WRITE CSVs TO DISK
res <- dlg_message("Would you like to save each table as a .csv?", "yesno")$res

if (res == "yes") {

  save_filename <- paste(file_path_no_extension, "_ALTERNATIVES", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(alternatives, file=save_filename, row.names=FALSE)
  
  save_filename <- paste(file_path_no_extension, "_SOURCES", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(sources, file=save_filename, row.names=FALSE)
  
  save_filename <- paste(file_path_no_extension, "_TYPES", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(types, file=save_filename, row.names=FALSE)
  
  save_filename <- paste(file_path_no_extension, "_RIVERS", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(rivers, file=save_filename, row.names=FALSE)
  
  save_filename <- paste(file_path_no_extension, "_LOCATIONS", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(locations, file=save_filename, row.names=FALSE)
  
  save_filename <- paste(file_path_no_extension, "_BRIDGE_TABLE", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(bridge_table, file=save_filename, row.names=FALSE)
  
  save_filename <- paste(file_path_no_extension, "_DATA_TABLE", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(data, file=save_filename, row.names=FALSE)

  save_filename <- paste(file_path_no_extension, "_DATES", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(dates, file=save_filename, row.names=FALSE)

  save_filename <- paste(file_path_no_extension, "_STATS", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(stats_data, file=save_filename, row.names=FALSE)
  
  save_filename <- paste(file_path_no_extension, "_STATS_ALL", ".csv", sep="")
  message(c("Writing to ", save_filename))
  write.csv(stats_data_all, file=save_filename, row.names=FALSE)
}


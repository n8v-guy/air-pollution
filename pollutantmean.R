pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  table <- TRUE
  for(i in id) {
    file <- sprintf("./%s/%03d.csv", directory, i)
    fileData <- read.csv(file)
    if(typeof(table) == "logical") {
      table <- fileData
    } else {
      table <- rbind(table, fileData)      
    }
  }
  tableColumn <- table[pollutant]
  tableColumnValid <- !is.na(tableColumn)
  cleanTableColumn <- tableColumn[tableColumnValid];
  meanValue <- mean(cleanTableColumn)
  round(meanValue,3)
}

print(pollutantmean("specdata", "sulfate", 1:10))
print(pollutantmean("specdata", "nitrate", 70:72))
print(pollutantmean("specdata", "nitrate", 23))

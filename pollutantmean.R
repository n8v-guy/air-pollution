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
  
  resultVector <- vector(mode="numeric")
  pos <- 1
  for(i in id) {
    file <- sprintf("./%s/%03d.csv", directory, i)
    table <- read.csv(file)
    tableColumn <- table[pollutant]
    tableColumnValid <- !is.na(table["sulfate"]) & !is.na(table["nitrate"])
    cleanTableColumn <- tableColumn[tableColumnValid];
    meanValue <- mean(cleanTableColumn)
    resultVector[pos] <- meanValue
    pos <- pos + 1
  }
  resultMean <- mean(resultVector)
  round(resultMean,3)
}

print(pollutantmean("specdata", "sulfate", 1:10))
print(pollutantmean("specdata", "nitrate", 70:72))
print(pollutantmean("specdata", "nitrate", 23))

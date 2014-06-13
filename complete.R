complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  df <- data.frame()
  for(i in id) {
    file <- sprintf("./%s/%03d.csv", directory, i)
    table <- read.csv(file)
    test = !is.na(table["sulfate"]) & !is.na(table["nitrate"])
    df <- rbind(df, c(i, sum(test)))
  }
  if(nrow(df) > 0) {
    names(df) <- c("id", "nobs")
  }
  df
}

#print(complete("specdata", 1))
#print(complete("specdata", c(2, 4, 8, 10, 12)))
#print(complete("specdata", 30:25))
#print(complete("specdata", 3))

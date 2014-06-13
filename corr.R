source("complete.R")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ret <- vector()
  pos <- 1
  #observCount <- complete("specdata", 1:332)
  thresholdArr = observCount["id"][observCount["nobs"] >= threshold]
  for(csv in thresholdArr) {
    file <- sprintf("./%s/%03d.csv", directory, csv)
    table <- read.csv(file)
    valid = !is.na(table["sulfate"]) & !is.na(table["nitrate"])
    sulf = table["sulfate"][valid]
    nitr = table["nitrate"][valid]
    ret[pos] <- cor(sulf, nitr)
    pos <- pos + 1
  }
  ret
}

cr <- corr("specdata", 150)
print(head(cr))
print(summary(cr))

cr <- corr("specdata", 400)
print(head(cr))
print(summary(cr))

cr <- corr("specdata", 5000)
print(summary(cr))
print(length(cr))

cr <- corr("specdata")
print(summary(cr))
print(length(cr))

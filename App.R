library(plumber)
#archivo plumber
r <- plumb("/Users/davidlechugahuerta/Desktop/amazonScrapingR/miPlumber.R")
# indicamos el puerto
r$run(port=8000)

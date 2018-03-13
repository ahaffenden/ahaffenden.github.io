# https://api.kraken.com/0/public/Trades?pair=XethZEUR&since=0
library(jsonlite)
detach("package:krakenTools", unload = TRUE)
library(krakenTools)

pair <- "ETHEUR"
date <- 0 # today

root_dir <- "/home/austin/Dropbox/kraken_data"

# check to see if the root directory exists
last_date <- get_last_date(pair, root_dir)
# check to see if the pair data exists

# if it does load it and get last date

# if it doesn't consider it a first run

pair_url <- return_url(pair, last_date)



raw_data_l <- fromJSON(pair_url)
  #
  # #raw_data_l$result$last

#   return(raw_data_l)
# }
#
# # set directory for csv file
# dat_dir <- "/home/austin/Dropbox/kraken_data"
#
# # first date will always be date = 0
# # download data
# get_kraken("ETHEUR")
#
#
#
# # compare the last date (raw_data_l$result$last) to the existing data
# # if the data doesn't exist assume that this is the first run
#
# # if it is earlier get more data and add to the end of raw_data_l
#
# # do the comparison again until the last date is less than the existing data date
#
# # then need to match the dates and discard the overlap ...

---
layout: post
title: "E Chan Quant Trading Ex. 3.1"
date: 2017-07-16
tags: Quant Exercises, Scrape
---

Scrape web pags for financial data
----------------------------------

This is the first in a series of posts that will convert the MATLAB scripts from E Chan's book 'Quantative Trading: How to Build Your Own Algorithmic Trading Business' into R code.

The following code will be used to retrieve a stock's historical price information from Yahoo! Finance

``` r
rm(list = ls()) # clear the workspace
library(httr)
library(quantmod)
library(XML)
```

``` r
symbol <- "IBM" # the stock of interest
in_url <- paste0('http://finance.yahoo.com/q/hp?s=',  symbol)

# retrieving the webpage data
histPriceFile_page <-GET(in_url)

# convert to a list
histPriceFile <- readHTMLTable(rawToChar(histPriceFile_page$content), 
                               stringsAsFactors = F)

# extract what we want from the list - the second element
histPriceFile_df <- histPriceFile[[2]]

# view the contents
head(histPriceFile_df)
```

    ##           Date   Open   High    Low Close* Adj Close**    Volume
    ## 1 Jul 14, 2017 154.01 154.62 153.40 154.24      154.24 3,214,300
    ## 2 Jul 13, 2017 153.70 154.19 153.19 153.63      153.63 2,476,100
    ## 3 Jul 12, 2017 153.48 154.24 153.05 153.70      153.70 3,097,900
    ## 4 Jul 11, 2017 153.26 153.65 152.05 153.19      153.19 3,447,500
    ## 5 Jul 10, 2017 152.91 153.89 152.63 153.42      153.42 3,206,200
    ## 6 Jul 07, 2017 152.62 153.49 152.14 152.94      152.94 2,460,100

The example from the book actually allocates the data to individual variables and so just for completeness

``` r
# standardise the column names
colnames(histPriceFile_df) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volume")
head(histPriceFile_df)
```

    ##           Date   Open   High    Low  Close Adj_Close    Volume
    ## 1 Jul 14, 2017 154.01 154.62 153.40 154.24    154.24 3,214,300
    ## 2 Jul 13, 2017 153.70 154.19 153.19 153.63    153.63 2,476,100
    ## 3 Jul 12, 2017 153.48 154.24 153.05 153.70    153.70 3,097,900
    ## 4 Jul 11, 2017 153.26 153.65 152.05 153.19    153.19 3,447,500
    ## 5 Jul 10, 2017 152.91 153.89 152.63 153.42    153.42 3,206,200
    ## 6 Jul 07, 2017 152.62 153.49 152.14 152.94    152.94 2,460,100

``` r
# allocate to individual variables
op <- histPriceFile_df$Open
hi <- histPriceFile_df$High
lo <- histPriceFile_df$Low
cl <- histPriceFile_df$Close
vol <- histPriceFile_df$Volume
adjCl <- histPriceFile_df$Adj_Close
```

And that is basically it for the example. More needs to be done before we can use the data (preprocessing) but that will be another post as I want to keep the e chan examples as stand alone items.

To finish we will save our dataframe for use another time. This can be done in a variety of ways dependent on what you want to do with it. A csv file is useful for sharing and if you want to look at your data outside of R; an rds file allows you to read you data back in as an R variable.

``` r
write.csv(histPriceFile_df, file = "histPriceFile_df.csv")
saveRDS(histPriceFile_df, "histPriceFile_df.rds")

# histPriceFile_df <- read.csv("histPriceFile_df.csv", header = TRUE)
# histPriceFile_df <- readRDS("histPriceFile_df.rds")
```

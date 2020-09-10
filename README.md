Vegas Lines
================

OLD REPO -- SCRAPER NO LONGER WORKS. USE THE [bettoR](https://papagorgio23.github.io/bettoR/) PACKAGE

This function is used to scrape the historical Vegas Lines for NFL and NBA games
--------------------------------------------------------------------------------

``` r
# Load libraries
library(lubridate)
library(tidyverse)
library(rvest)
library(magrittr)
library(knitr)
```

The magic
---------

``` r
# this is the function
GetLines <- function(sport = "NBA", year, type = "both") {
  
  options(stringsAsFactors = FALSE)
  sport <- tolower(sport)
  
  # Get team urls and names
  url <- paste0("http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/", sport, "/teams/teams.html")
  html <- suppressWarnings(readLines(url))
  links <- html[grep(paste0("/data/", sport, "/teams/team[0-9]"), html)]
  links <- unique(gsub(paste0('.*/data/', sport,'/teams/([^\"]*)\"[ ]*>([^<]*).*'), '\\1,\\2', links))
  links <- strsplit(links, ',')
  
  # Create URL stems for getting team records
  url.base <- paste(gsub("SPORT", sport, "http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/SPORT/teams/pastresults/"), year - 1, '-', year, '/', sep = '')
  full.lines <- data.frame()
  
  # Put all team records in one data frame
  for (i in 1:length(links)) {
    
    url <- paste(url.base, links[[i]][1], sep = "")
    tables <- read_html(url) %>% html_table(header = TRUE, fill = TRUE) 
    
    # Get appropriate data: regular season, playoffs, or both
    if (type == 'regular season') {
      
      if (length(tables) == 0) {  # If the team didn't exist at the time, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else {
        if (sport == 'nfl') {
          temp <- tables[[length(tables) - 1]]  # Regular season is always the second to last table
        } else if (sport == 'nba') {
          temp <- tables[[length(tables)]]      # Regular season is always the last table
        }
      }
      
    } else if (type == 'playoffs') {
      
      if (length(tables) == 1) {  # If the team didn't make the playoffs, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else {
        temp <- tables[[1]]
      }
      
    } else {
      
      if (length(tables) == 0) {  # If the team didn't exist at the time, make an empty df
        temp <- data.frame(matrix(nrow = 1, ncol = 6, data = 0))
        temp <- temp[-1, ]
      } else {
        if (sport == 'nfl') {
          if (length(tables) == 2) {
            temp <- tables[[1]]
          } else {
            temp <- rbind(tables[[1]], tables[[2]])
          }
        } else if (sport == 'nba') {
          if (length(tables) == 1) {
            temp <- tables[[1]]
          } else {
            temp <- rbind(tables[[1]], tables[[2]])
          }
        }
      }
      
    }
    
    colnames(temp) <- c("date", "away.team", "score", "type", "home.line", "over.under")
    
    if (nrow(temp) > 0) {   # Add home team if the df has any info (we didn't ask for playoffs from a team that didn't make it)
      temp$home.team <- links[[i]][2]
    }
    
    try(full.lines <- rbind(full.lines, temp), silent = FALSE)
  }
  
  # Extra formatting for NFL df
  if (sport == 'nfl') {
    full.lines <- full.lines %>% filter(score != "BYE") 
    full.lines$date <- gsub("[A-Za-z ]*", "", full.lines$date)
  }
  
  # Format date
  full.lines$date <- mdy(full.lines$date)
  
  # Remove away games (the odds exist for home team already)
  full.lines <- full.lines[-grep('@', full.lines$away.team), ]
  
  # Create home score and away score columns
  full.lines$home.score <- as.numeric(gsub('[^0-9]*([0-9]+)-([0-9]+)[^0-9]*', '\\1', full.lines$score))
  full.lines$away.score <- as.numeric(gsub('[^0-9]*([0-9]+)-([0-9]+)[^0-9]*', '\\2', full.lines$score))
  
  # Fix line and over/under
  full.lines$home.line <- as.numeric(gsub('[^0-9\\.-]', '', full.lines$home.line))
  full.lines$over.under <- as.numeric(gsub('[^0-9\\.-]', '', full.lines$over.under))
  
  # Reorder columns, sort by date, and return
  full.lines <- full.lines[, c('date', 'type', 'home.team', 'away.team', 'home.score', 'away.score', 'home.line', 'over.under')]
  full.lines <- full.lines[order(full.lines$date), ]
  return(full.lines)
}
```

This is how to use the function
-------------------------------

#### Parameters:

-   **sport:** As of right now, only NFL and NBA have been fully tested... I will be adding NHL, NCAAF, NCAAB, and MLB eventually.
    -   "NBA"
    -   "NFL"
-   **year:** season. (2012 would return the data from the 2011-2012 season)
-   **type:**
    -   "regular season"
    -   "playoffs"
    -   "both"

``` r
# example
NFL.2017 <- GetLines(sport = "NFL", year = 2017, type = "both")
```

Jackpot!!
---------

``` r
kable(NFL.2017)
```

|     | date       | type       | home.team     | away.team     |  home.score|  away.score|  home.line|  over.under|
|-----|:-----------|:-----------|:--------------|:--------------|-----------:|-----------:|----------:|-----------:|
| 217 | 2016-09-08 | Week 1     | Denver        | Carolina      |          21|          20|        3.0|        40.5|
| 68  | 2016-09-11 | Week 1     | N.Y. Jets     | Cincinnati    |          22|          23|       -1.0|        42.0|
| 84  | 2016-09-11 | Week 1     | Baltimore     | Buffalo       |          13|           7|       -3.0|        44.5|
| 153 | 2016-09-11 | Week 1     | Houston       | Chicago       |          23|          14|       -5.5|        43.0|
| 169 | 2016-09-11 | Week 1     | Indianapolis  | Detroit       |          35|          39|       -2.5|        51.0|
| 185 | 2016-09-11 | Week 1     | Jacksonville  | Green Bay     |          23|          27|        3.5|        47.0|
| 201 | 2016-09-11 | Week 1     | Tennessee     | Minnesota     |          16|          25|        2.5|        40.0|
| 234 | 2016-09-11 | Week 1     | Kansas City   | L.A. Chargers |          33|          27|       -6.5|        45.5|
| 284 | 2016-09-11 | Week 1     | Dallas        | N.Y. Giants   |          19|          20|       -1.0|        47.5|
| 317 | 2016-09-11 | Week 1     | Philadelphia  | Cleveland     |          29|          10|       -4.0|        41.5|
| 420 | 2016-09-11 | Week 1     | Atlanta       | Tampa Bay     |          24|          31|       -2.5|        46.5|
| 452 | 2016-09-11 | Week 1     | New Orleans   | Oakland       |          34|          35|       -2.5|        50.0|
| 484 | 2016-09-11 | Week 1     | Arizona       | New England   |          21|          23|       -9.0|        44.5|
| 534 | 2016-09-11 | Week 1     | Seattle       | Miami         |          12|          10|      -10.5|        43.5|
| 333 | 2016-09-12 | Week 1     | Washington    | Pittsburgh    |          16|          38|        2.5|        49.0|
| 516 | 2016-09-12 | Week 1     | San Francisco | L.A. Rams     |          28|           0|        2.5|        43.0|
| 15  | 2016-09-15 | Week 2     | Buffalo       | N.Y. Jets     |          31|          37|       -1.0|        40.5|
| 51  | 2016-09-18 | Week 2     | New England   | Miami         |          31|          24|       -5.5|        42.0|
| 115 | 2016-09-18 | Week 2     | Cleveland     | Baltimore     |          20|          25|        4.0|        42.0|
| 134 | 2016-09-18 | Week 2     | Pittsburgh    | Cincinnati    |          24|          16|       -3.0|        48.0|
| 152 | 2016-09-18 | Week 2     | Houston       | Kansas City   |          19|          12|        1.0|        41.5|
| 216 | 2016-09-18 | Week 2     | Denver        | Indianapolis  |          34|          20|       -6.0|        47.0|
| 249 | 2016-09-18 | Week 2     | L.A. Chargers | Jacksonville  |          38|          14|       -3.0|        47.5|
| 266 | 2016-09-18 | Week 2     | Oakland       | Atlanta       |          28|          35|       -4.0|        47.5|
| 300 | 2016-09-18 | Week 2     | N.Y. Giants   | New Orleans   |          16|          13|       -3.5|        54.0|
| 332 | 2016-09-18 | Week 2     | Washington    | Dallas        |          23|          27|       -3.5|        47.0|
| 365 | 2016-09-18 | Week 2     | Detroit       | Tennessee     |          15|          16|       -6.0|        48.0|
| 400 | 2016-09-18 | Week 2     | Minnesota     | Green Bay     |          17|          14|        1.5|        43.0|
| 435 | 2016-09-18 | Week 2     | Carolina      | San Francisco |          46|          27|      -12.0|        44.5|
| 483 | 2016-09-18 | Week 2     | Arizona       | Tampa Bay     |          40|           7|       -7.5|        49.5|
| 499 | 2016-09-18 | Week 2     | L.A. Rams     | Seattle       |           9|           3|        5.5|        38.0|
| 348 | 2016-09-19 | Week 2     | Chicago       | Philadelphia  |          14|          29|       -3.0|        42.5|
| 50  | 2016-09-22 | Week 3     | New England   | Houston       |          27|           0|         NA|        38.5|
| 14  | 2016-09-25 | Week 3     | Buffalo       | Arizona       |          33|          18|        5.0|        48.0|
| 31  | 2016-09-25 | Week 3     | Miami         | Cleveland     |          30|          24|      -10.0|        42.0|
| 98  | 2016-09-25 | Week 3     | Cincinnati    | Denver        |          17|          29|       -3.5|        42.0|
| 167 | 2016-09-25 | Week 3     | Indianapolis  | L.A. Chargers |          26|          22|       -1.5|        51.5|
| 183 | 2016-09-25 | Week 3     | Jacksonville  | Baltimore     |          17|          19|        2.5|        45.0|
| 199 | 2016-09-25 | Week 3     | Tennessee     | Oakland       |          10|          17|        1.5|        47.0|
| 232 | 2016-09-25 | Week 3     | Kansas City   | N.Y. Jets     |          24|           3|       -3.0|        44.5|
| 282 | 2016-09-25 | Week 3     | Dallas        | Chicago       |          31|          17|       -6.5|        44.5|
| 299 | 2016-09-25 | Week 3     | N.Y. Giants   | Washington    |          27|          29|       -3.5|        47.0|
| 315 | 2016-09-25 | Week 3     | Philadelphia  | Pittsburgh    |          34|           3|        3.5|        46.5|
| 383 | 2016-09-25 | Week 3     | Green Bay     | Detroit       |          34|          27|       -6.0|        47.5|
| 434 | 2016-09-25 | Week 3     | Carolina      | Minnesota     |          10|          22|       -6.0|        43.0|
| 466 | 2016-09-25 | Week 3     | Tampa Bay     | L.A. Rams     |          32|          37|       -3.5|        40.5|
| 532 | 2016-09-25 | Week 3     | Seattle       | San Francisco |          37|          18|      -10.5|        42.0|
| 450 | 2016-09-26 | Week 3     | New Orleans   | Atlanta       |          32|          45|       -2.5|        54.0|
| 97  | 2016-09-29 | Week 4     | Cincinnati    | Miami         |          22|           7|       -7.5|        46.0|
| 49  | 2016-10-02 | Week 4     | New England   | Buffalo       |           0|          16|       -3.5|        41.0|
| 65  | 2016-10-02 | Week 4     | N.Y. Jets     | Seattle       |          17|          27|        1.0|        40.0|
| 81  | 2016-10-02 | Week 4     | Baltimore     | Oakland       |          27|          28|       -3.5|        45.0|
| 132 | 2016-10-02 | Week 4     | Pittsburgh    | Kansas City   |          43|          14|       -3.0|        48.0|
| 150 | 2016-10-02 | Week 4     | Houston       | Tennessee     |          27|          20|       -4.0|        40.0|
| 182 | 2016-10-02 | Week 4     | Jacksonville  | Indianapolis  |          30|          27|        1.0|        48.0|
| 247 | 2016-10-02 | Week 4     | L.A. Chargers | New Orleans   |          34|          35|       -3.5|        54.0|
| 330 | 2016-10-02 | Week 4     | Washington    | Cleveland     |          31|          20|       -7.5|        48.0|
| 346 | 2016-10-02 | Week 4     | Chicago       | Detroit       |          17|          14|        3.0|        48.0|
| 417 | 2016-10-02 | Week 4     | Atlanta       | Carolina      |          48|          33|        2.5|        48.5|
| 465 | 2016-10-02 | Week 4     | Tampa Bay     | Denver        |           7|          27|        3.5|        43.0|
| 481 | 2016-10-02 | Week 4     | Arizona       | L.A. Rams     |          13|          17|      -10.0|        43.5|
| 513 | 2016-10-02 | Week 4     | San Francisco | Dallas        |          17|          24|        1.0|        44.5|
| 398 | 2016-10-03 | Week 4     | Minnesota     | N.Y. Giants   |          24|          10|       -3.5|        42.5|
| 512 | 2016-10-06 | Week 5     | San Francisco | Arizona       |          21|          33|        3.5|        42.5|
| 29  | 2016-10-09 | Week 5     | Miami         | Tennessee     |          17|          30|       -2.5|        44.5|
| 80  | 2016-10-09 | Week 5     | Baltimore     | Washington    |          10|          16|       -4.0|        44.5|
| 112 | 2016-10-09 | Week 5     | Cleveland     | New England   |          13|          33|       10.0|        47.5|
| 131 | 2016-10-09 | Week 5     | Pittsburgh    | N.Y. Jets     |          31|          13|      -10.0|        50.0|
| 165 | 2016-10-09 | Week 5     | Indianapolis  | Chicago       |          29|          23|       -4.0|        47.5|
| 213 | 2016-10-09 | Week 5     | Denver        | Atlanta       |          16|          23|       -3.5|        44.5|
| 263 | 2016-10-09 | Week 5     | Oakland       | L.A. Chargers |          34|          31|       -3.5|        50.5|
| 280 | 2016-10-09 | Week 5     | Dallas        | Cincinnati    |          28|          14|        2.5|        46.0|
| 362 | 2016-10-09 | Week 5     | Detroit       | Philadelphia  |          24|          23|        3.5|        46.0|
| 382 | 2016-10-09 | Week 5     | Green Bay     | N.Y. Giants   |          23|          16|       -7.0|        49.0|
| 397 | 2016-10-09 | Week 5     | Minnesota     | Houston       |          31|          13|       -6.0|        39.0|
| 496 | 2016-10-09 | Week 5     | L.A. Rams     | Buffalo       |          19|          30|        2.5|        41.5|
| 432 | 2016-10-10 | Week 5     | Carolina      | Tampa Bay     |          14|          17|       -6.0|        46.5|
| 245 | 2016-10-13 | Week 6     | L.A. Chargers | Denver        |          21|          13|        3.0|        44.0|
| 11  | 2016-10-16 | Week 6     | Buffalo       | San Francisco |          45|          16|       -7.5|        44.0|
| 28  | 2016-10-16 | Week 6     | Miami         | Pittsburgh    |          30|          15|        7.5|        49.5|
| 47  | 2016-10-16 | Week 6     | New England   | Cincinnati    |          35|          17|       -7.5|        48.0|
| 148 | 2016-10-16 | Week 6     | Houston       | Indianapolis  |          26|          23|       -3.0|        47.5|
| 196 | 2016-10-16 | Week 6     | Tennessee     | Cleveland     |          28|          26|       -7.5|        44.5|
| 262 | 2016-10-16 | Week 6     | Oakland       | Kansas City   |          10|          26|        1.0|        46.5|
| 296 | 2016-10-16 | Week 6     | N.Y. Giants   | Baltimore     |          27|          23|       -3.5|        42.5|
| 328 | 2016-10-16 | Week 6     | Washington    | Philadelphia  |          27|          20|        3.0|        45.0|
| 344 | 2016-10-16 | Week 6     | Chicago       | Jacksonville  |          16|          17|       -2.5|        46.0|
| 361 | 2016-10-16 | Week 6     | Detroit       | L.A. Rams     |          31|          28|       -2.5|        44.0|
| 381 | 2016-10-16 | Week 6     | Green Bay     | Dallas        |          16|          30|       -5.0|        47.0|
| 448 | 2016-10-16 | Week 6     | New Orleans   | Carolina      |          41|          38|        2.5|        53.5|
| 530 | 2016-10-16 | Week 6     | Seattle       | Atlanta       |          26|          24|       -7.0|        45.5|
| 479 | 2016-10-17 | Week 6     | Arizona       | N.Y. Jets     |          28|           3|       -7.0|        45.5|
| 380 | 2016-10-20 | Week 7     | Green Bay     | Chicago       |          26|          10|       -7.5|        46.5|
| 27  | 2016-10-23 | Week 7     | Miami         | Buffalo       |          28|          25|        2.5|        46.0|
| 62  | 2016-10-23 | Week 7     | N.Y. Jets     | Baltimore     |          24|          16|       -2.5|        40.0|
| 94  | 2016-10-23 | Week 7     | Cincinnati    | Cleveland     |          31|          17|      -11.0|        46.5|
| 129 | 2016-10-23 | Week 7     | Pittsburgh    | New England   |          16|          27|        7.5|        49.0|
| 180 | 2016-10-23 | Week 7     | Jacksonville  | Oakland       |          16|          33|       -2.0|        47.5|
| 195 | 2016-10-23 | Week 7     | Tennessee     | Indianapolis  |          26|          34|       -4.0|        48.0|
| 229 | 2016-10-23 | Week 7     | Kansas City   | New Orleans   |          27|          21|       -7.0|        51.5|
| 312 | 2016-10-23 | Week 7     | Philadelphia  | Minnesota     |          21|          10|        3.0|        39.0|
| 360 | 2016-10-23 | Week 7     | Detroit       | Washington    |          20|          17|        1.0|        50.0|
| 414 | 2016-10-23 | Week 7     | Atlanta       | L.A. Chargers |          30|          33|       -4.5|        52.5|
| 478 | 2016-10-23 | Week 7     | Arizona       | Seattle       |           6|           6|       -2.5|        43.0|
| 494 | 2016-10-23 | Week 7     | L.A. Rams     | N.Y. Giants   |          10|          17|        2.5|        44.5|
| 510 | 2016-10-23 | Week 7     | San Francisco | Tampa Bay     |          17|          34|       -1.0|        45.0|
| 211 | 2016-10-24 | Week 7     | Denver        | Houston       |          27|           9|       -8.5|        40.0|
| 194 | 2016-10-27 | Week 8     | Tennessee     | Jacksonville  |          36|          22|       -3.0|        43.5|
| 9   | 2016-10-30 | Week 8     | Buffalo       | New England   |          25|          41|        5.5|        48.0|
| 93  | 2016-10-30 | Week 8     | Cincinnati    | Washington    |          27|          27|       -3.0|        49.0|
| 109 | 2016-10-30 | Week 8     | Cleveland     | N.Y. Jets     |          28|          31|        2.5|        45.5|
| 146 | 2016-10-30 | Week 8     | Houston       | Detroit       |          20|          13|       -1.0|        46.5|
| 162 | 2016-10-30 | Week 8     | Indianapolis  | Kansas City   |          14|          30|        3.0|        50.5|
| 210 | 2016-10-30 | Week 8     | Denver        | L.A. Chargers |          27|          19|       -3.5|        43.0|
| 278 | 2016-10-30 | Week 8     | Dallas        | Philadelphia  |          29|          23|       -5.0|        44.0|
| 413 | 2016-10-30 | Week 8     | Atlanta       | Green Bay     |          33|          32|       -3.0|        51.0|
| 430 | 2016-10-30 | Week 8     | Carolina      | Arizona       |          30|          20|       -2.5|        45.5|
| 446 | 2016-10-30 | Week 8     | New Orleans   | Seattle       |          25|          20|        1.0|        50.0|
| 462 | 2016-10-30 | Week 8     | Tampa Bay     | Oakland       |          24|          30|       -1.0|        48.0|
| 342 | 2016-10-31 | Week 8     | Chicago       | Minnesota     |          20|          10|        4.5|        39.5|
| 461 | 2016-11-03 | Week 9     | Tampa Bay     | Atlanta       |          28|          43|        4.5|        49.0|
| 26  | 2016-11-06 | Week 9     | Miami         | N.Y. Jets     |          27|          23|       -3.5|        45.0|
| 77  | 2016-11-06 | Week 9     | Baltimore     | Pittsburgh    |          21|          14|        3.5|        46.0|
| 108 | 2016-11-06 | Week 9     | Cleveland     | Dallas        |          10|          35|        7.0|        49.0|
| 227 | 2016-11-06 | Week 9     | Kansas City   | Jacksonville  |          19|          14|       -7.0|        42.5|
| 242 | 2016-11-06 | Week 9     | L.A. Chargers | Tennessee     |          43|          35|       -3.5|        47.5|
| 259 | 2016-11-06 | Week 9     | Oakland       | Denver        |          30|          20|       -1.0|        44.5|
| 294 | 2016-11-06 | Week 9     | N.Y. Giants   | Philadelphia  |          28|          23|       -3.0|        42.5|
| 378 | 2016-11-06 | Week 9     | Green Bay     | Indianapolis  |          26|          31|       -7.5|        52.0|
| 394 | 2016-11-06 | Week 9     | Minnesota     | Detroit       |          16|          22|       -4.5|        42.5|
| 493 | 2016-11-06 | Week 9     | L.A. Rams     | Carolina      |          10|          13|        3.0|        44.5|
| 509 | 2016-11-06 | Week 9     | San Francisco | New Orleans   |          23|          41|        5.0|        53.0|
| 527 | 2016-11-07 | Week 9     | Seattle       | Buffalo       |          31|          25|       -5.5|        43.5|
| 76  | 2016-11-10 | Week 10    | Baltimore     | Cleveland     |          28|           7|       -7.5|        44.0|
| 44  | 2016-11-13 | Week 10    | New England   | Seattle       |          24|          31|       -7.5|        49.5|
| 59  | 2016-11-13 | Week 10    | N.Y. Jets     | L.A. Rams     |           6|           9|        1.0|        39.0|
| 127 | 2016-11-13 | Week 10    | Pittsburgh    | Dallas        |          30|          35|       -3.0|        50.5|
| 177 | 2016-11-13 | Week 10    | Jacksonville  | Houston       |          21|          24|       -3.0|        42.0|
| 192 | 2016-11-13 | Week 10    | Tennessee     | Green Bay     |          47|          25|        3.0|        48.5|
| 241 | 2016-11-13 | Week 10    | L.A. Chargers | Miami         |          24|          31|       -4.0|        48.5|
| 309 | 2016-11-13 | Week 10    | Philadelphia  | Atlanta       |          24|          15|       -2.0|        48.5|
| 325 | 2016-11-13 | Week 10    | Washington    | Minnesota     |          26|          20|       -2.5|        42.0|
| 428 | 2016-11-13 | Week 10    | Carolina      | Kansas City   |          17|          20|       -3.0|        44.0|
| 444 | 2016-11-13 | Week 10    | New Orleans   | Denver        |          23|          25|       -3.0|        50.0|
| 460 | 2016-11-13 | Week 10    | Tampa Bay     | Chicago       |          36|          10|        2.5|        45.0|
| 476 | 2016-11-13 | Week 10    | Arizona       | San Francisco |          23|          20|      -13.5|        46.5|
| 293 | 2016-11-14 | Week 10    | N.Y. Giants   | Cincinnati    |          21|          20|        1.0|        49.5|
| 427 | 2016-11-17 | Week 11    | Carolina      | New Orleans   |          23|          20|       -3.5|        52.5|
| 91  | 2016-11-20 | Week 11    | Cincinnati    | Buffalo       |          12|          16|       -2.5|        48.0|
| 106 | 2016-11-20 | Week 11    | Cleveland     | Pittsburgh    |           9|          24|        8.0|        45.5|
| 160 | 2016-11-20 | Week 11    | Indianapolis  | Tennessee     |          24|          17|       -3.0|        53.5|
| 225 | 2016-11-20 | Week 11    | Kansas City   | Tampa Bay     |          17|          19|       -7.0|        45.0|
| 275 | 2016-11-20 | Week 11    | Dallas        | Baltimore     |          27|          17|       -7.0|        44.5|
| 292 | 2016-11-20 | Week 11    | N.Y. Giants   | Chicago       |          22|          16|       -7.0|        41.5|
| 324 | 2016-11-20 | Week 11    | Washington    | Green Bay     |          42|          24|       -3.0|        48.0|
| 357 | 2016-11-20 | Week 11    | Detroit       | Jacksonville  |          26|          19|       -5.5|        47.0|
| 392 | 2016-11-20 | Week 11    | Minnesota     | Arizona       |          30|          24|       -2.0|        39.5|
| 491 | 2016-11-20 | Week 11    | L.A. Rams     | Miami         |          10|          14|       -1.0|        39.0|
| 507 | 2016-11-20 | Week 11    | San Francisco | New England   |          17|          30|       10.5|        51.5|
| 525 | 2016-11-20 | Week 11    | Seattle       | Philadelphia  |          26|          15|       -6.5|        42.5|
| 258 | 2016-11-21 | Week 11    | Oakland       | Houston       |          27|          20|       -6.5|        45.5|
| 159 | 2016-11-24 | Week 12    | Indianapolis  | Pittsburgh    |           7|          28|        8.0|        50.0|
| 274 | 2016-11-24 | Week 12    | Dallas        | Washington    |          31|          26|       -5.5|        53.0|
| 356 | 2016-11-24 | Week 12    | Detroit       | Minnesota     |          16|          13|       -1.5|        42.0|
| 6   | 2016-11-27 | Week 12    | Buffalo       | Jacksonville  |          28|          21|       -8.5|        43.5|
| 23  | 2016-11-27 | Week 12    | Miami         | San Francisco |          31|          24|       -7.5|        44.5|
| 58  | 2016-11-27 | Week 12    | N.Y. Jets     | New England   |          17|          22|        8.5|        48.0|
| 74  | 2016-11-27 | Week 12    | Baltimore     | Cincinnati    |          19|          14|       -3.5|        41.5|
| 105 | 2016-11-27 | Week 12    | Cleveland     | N.Y. Giants   |          13|          27|        6.5|        46.5|
| 143 | 2016-11-27 | Week 12    | Houston       | L.A. Chargers |          13|          21|        2.5|        45.5|
| 207 | 2016-11-27 | Week 12    | Denver        | Kansas City   |          27|          30|       -3.5|        40.0|
| 257 | 2016-11-27 | Week 12    | Oakland       | Carolina      |          35|          32|       -3.5|        48.5|
| 339 | 2016-11-27 | Week 12    | Chicago       | Tennessee     |          21|          27|        6.5|        41.5|
| 410 | 2016-11-27 | Week 12    | Atlanta       | Arizona       |          38|          19|       -4.0|        49.0|
| 442 | 2016-11-27 | Week 12    | New Orleans   | L.A. Rams     |          49|          21|       -8.0|        45.0|
| 458 | 2016-11-27 | Week 12    | Tampa Bay     | Seattle       |          14|           5|        5.0|        46.0|
| 307 | 2016-11-28 | Week 12    | Philadelphia  | Green Bay     |          13|          27|       -4.0|        47.0|
| 390 | 2016-12-01 | Week 13    | Minnesota     | Dallas        |          15|          17|        3.0|        43.5|
| 41  | 2016-12-04 | Week 13    | New England   | L.A. Rams     |          26|          10|      -13.0|        44.5|
| 73  | 2016-12-04 | Week 13    | Baltimore     | Miami         |          38|           6|       -3.5|        41.5|
| 89  | 2016-12-04 | Week 13    | Cincinnati    | Philadelphia  |          32|          14|       -2.0|        42.0|
| 124 | 2016-12-04 | Week 13    | Pittsburgh    | N.Y. Giants   |          24|          14|       -6.5|        49.5|
| 174 | 2016-12-04 | Week 13    | Jacksonville  | Denver        |          10|          20|        3.5|        38.5|
| 239 | 2016-12-04 | Week 13    | L.A. Chargers | Tampa Bay     |          21|          28|       -3.5|        49.0|
| 256 | 2016-12-04 | Week 13    | Oakland       | Buffalo       |          38|          24|       -3.0|        48.5|
| 338 | 2016-12-04 | Week 13    | Chicago       | San Francisco |          26|           6|         NA|        44.0|
| 374 | 2016-12-04 | Week 13    | Green Bay     | Houston       |          21|          13|       -6.5|        44.5|
| 409 | 2016-12-04 | Week 13    | Atlanta       | Kansas City   |          28|          29|       -5.0|        50.0|
| 441 | 2016-12-04 | Week 13    | New Orleans   | Detroit       |          13|          28|       -6.5|        53.0|
| 473 | 2016-12-04 | Week 13    | Arizona       | Washington    |          31|          23|       -2.5|        48.5|
| 523 | 2016-12-04 | Week 13    | Seattle       | Carolina      |          40|           7|       -8.0|        43.5|
| 57  | 2016-12-05 | Week 13    | N.Y. Jets     | Indianapolis  |          10|          41|       -1.0|        48.5|
| 222 | 2016-12-08 | Week 14    | Kansas City   | Oakland       |          21|          13|       -3.5|        46.0|
| 4   | 2016-12-11 | Week 14    | Buffalo       | Pittsburgh    |          20|          27|        1.0|        45.5|
| 21  | 2016-12-11 | Week 14    | Miami         | Arizona       |          26|          23|        2.0|        44.0|
| 104 | 2016-12-11 | Week 14    | Cleveland     | Cincinnati    |          10|          23|        4.5|        42.0|
| 157 | 2016-12-11 | Week 14    | Indianapolis  | Houston       |          17|          22|       -6.5|        47.5|
| 173 | 2016-12-11 | Week 14    | Jacksonville  | Minnesota     |          16|          25|        3.0|        38.0|
| 189 | 2016-12-11 | Week 14    | Tennessee     | Denver        |          13|          10|       -2.0|        44.0|
| 289 | 2016-12-11 | Week 14    | N.Y. Giants   | Dallas        |          10|           7|        3.5|        47.0|
| 305 | 2016-12-11 | Week 14    | Philadelphia  | Washington    |          22|          27|        2.0|        48.5|
| 354 | 2016-12-11 | Week 14    | Detroit       | Chicago       |          20|          17|       -7.5|        42.0|
| 373 | 2016-12-11 | Week 14    | Green Bay     | Seattle       |          38|          10|        3.0|        47.0|
| 424 | 2016-12-11 | Week 14    | Carolina      | L.A. Chargers |          28|          16|       -1.0|        48.5|
| 456 | 2016-12-11 | Week 14    | Tampa Bay     | New Orleans   |          16|          11|       -2.0|        52.0|
| 488 | 2016-12-11 | Week 14    | L.A. Rams     | Atlanta       |          14|          42|        4.5|        44.0|
| 504 | 2016-12-11 | Week 14    | San Francisco | N.Y. Jets     |          17|          23|       -3.0|        42.5|
| 40  | 2016-12-12 | Week 14    | New England   | Baltimore     |          30|          23|       -6.0|        45.0|
| 521 | 2016-12-15 | Week 15    | Seattle       | L.A. Rams     |          24|           3|      -15.0|        39.5|
| 55  | 2016-12-17 | Week 15    | N.Y. Jets     | Miami         |          13|          34|        2.5|        40.0|
| 3   | 2016-12-18 | Week 15    | Buffalo       | Cleveland     |          33|          13|      -10.5|        43.0|
| 71  | 2016-12-18 | Week 15    | Baltimore     | Philadelphia  |          27|          26|       -5.0|        41.0|
| 87  | 2016-12-18 | Week 15    | Cincinnati    | Pittsburgh    |          20|          24|        3.0|        45.5|
| 140 | 2016-12-18 | Week 15    | Houston       | Jacksonville  |          21|          20|       -3.5|        39.5|
| 204 | 2016-12-18 | Week 15    | Denver        | New England   |           3|          16|        3.0|        43.0|
| 221 | 2016-12-18 | Week 15    | Kansas City   | Tennessee     |          17|          19|       -6.0|        43.0|
| 237 | 2016-12-18 | Week 15    | L.A. Chargers | Oakland       |          16|          19|        2.5|        49.5|
| 271 | 2016-12-18 | Week 15    | Dallas        | Tampa Bay     |          26|          20|       -7.0|        47.5|
| 288 | 2016-12-18 | Week 15    | N.Y. Giants   | Detroit       |          17|           6|       -4.0|        42.5|
| 336 | 2016-12-18 | Week 15    | Chicago       | Green Bay     |          27|          30|        4.5|        40.0|
| 388 | 2016-12-18 | Week 15    | Minnesota     | Indianapolis  |           6|          34|       -5.0|        44.5|
| 407 | 2016-12-18 | Week 15    | Atlanta       | San Francisco |          41|          13|      -13.5|        51.5|
| 471 | 2016-12-18 | Week 15    | Arizona       | New Orleans   |          41|          48|       -3.0|        48.5|
| 320 | 2016-12-19 | Week 15    | Washington    | Carolina      |          15|          26|       -7.0|        50.5|
| 303 | 2016-12-22 | Week 16    | Philadelphia  | N.Y. Giants   |          24|          19|       -1.5|        42.5|
| 2   | 2016-12-24 | Week 16    | Buffalo       | Miami         |          31|          34|       -4.5|        44.5|
| 38  | 2016-12-24 | Week 16    | New England   | N.Y. Jets     |          41|           3|      -17.0|        45.0|
| 102 | 2016-12-24 | Week 16    | Cleveland     | L.A. Chargers |          20|          17|        4.5|        45.0|
| 139 | 2016-12-24 | Week 16    | Houston       | Cincinnati    |          12|          10|       -3.0|        41.5|
| 171 | 2016-12-24 | Week 16    | Jacksonville  | Tennessee     |          38|          17|        4.0|        44.0|
| 253 | 2016-12-24 | Week 16    | Oakland       | Indianapolis  |          33|          25|       -3.5|        52.0|
| 335 | 2016-12-24 | Week 16    | Chicago       | Washington    |          21|          41|        3.0|        49.0|
| 371 | 2016-12-24 | Week 16    | Green Bay     | Minnesota     |          38|          25|       -6.0|        44.5|
| 422 | 2016-12-24 | Week 16    | Carolina      | Atlanta       |          16|          33|        3.0|        49.0|
| 438 | 2016-12-24 | Week 16    | New Orleans   | Tampa Bay     |          31|          24|       -3.0|        53.0|
| 486 | 2016-12-24 | Week 16    | L.A. Rams     | San Francisco |          21|          22|       -6.0|        39.5|
| 520 | 2016-12-24 | Week 16    | Seattle       | Arizona       |          31|          34|       -9.0|        43.5|
| 121 | 2016-12-25 | Week 16    | Pittsburgh    | Baltimore     |          31|          27|       -5.5|        46.5|
| 220 | 2016-12-25 | Week 16    | Kansas City   | Denver        |          33|          10|       -3.5|        38.0|
| 270 | 2016-12-26 | Week 16    | Dallas        | Detroit       |          42|          21|       -6.5|        46.5|
| 18  | 2017-01-01 | Week 17    | Miami         | New England   |          14|          35|        7.5|        47.0|
| 53  | 2017-01-01 | Week 17    | N.Y. Jets     | Buffalo       |          30|          10|        3.5|        43.0|
| 85  | 2017-01-01 | Week 17    | Cincinnati    | Baltimore     |          27|          10|        2.5|        40.5|
| 120 | 2017-01-01 | Week 17    | Pittsburgh    | Cleveland     |          27|          24|       -3.0|        41.5|
| 154 | 2017-01-01 | Week 17    | Indianapolis  | Jacksonville  |          24|          20|       -5.5|        48.5|
| 186 | 2017-01-01 | Week 17    | Tennessee     | Houston       |          24|          17|       -3.0|        41.5|
| 202 | 2017-01-01 | Week 17    | Denver        | Oakland       |          24|           6|       -1.0|        40.0|
| 235 | 2017-01-01 | Week 17    | L.A. Chargers | Kansas City   |          27|          37|        5.5|        45.0|
| 302 | 2017-01-01 | Week 17    | Philadelphia  | Dallas        |          27|          13|       -6.5|        44.0|
| 318 | 2017-01-01 | Week 17    | Washington    | N.Y. Giants   |          10|          19|       -9.0|        47.5|
| 351 | 2017-01-01 | Week 17    | Detroit       | Green Bay     |          24|          31|        3.5|        50.0|
| 386 | 2017-01-01 | Week 17    | Minnesota     | Chicago       |          38|          10|       -6.0|        44.0|
| 405 | 2017-01-01 | Week 17    | Atlanta       | New Orleans   |          38|          32|       -7.5|        58.5|
| 453 | 2017-01-01 | Week 17    | Tampa Bay     | Carolina      |          17|          16|       -3.0|        44.5|
| 485 | 2017-01-01 | Week 17    | L.A. Rams     | Arizona       |           6|          44|        7.0|        39.0|
| 501 | 2017-01-01 | Week 17    | San Francisco | Seattle       |          23|          25|       11.5|        45.0|
| 137 | 2017-01-07 | Wildcard   | Houston       | Oakland       |          27|          14|       -4.0|        38.0|
| 518 | 2017-01-07 | Wildcard   | Seattle       | Detroit       |          26|           6|       -8.0|        45.5|
| 119 | 2017-01-08 | Wildcard   | Pittsburgh    | Miami         |          30|          12|      -11.0|        47.5|
| 369 | 2017-01-08 | Wildcard   | Green Bay     | N.Y. Giants   |          38|          13|       -5.0|        46.5|
| 36  | 2017-01-14 | Divisional | New England   | Houston       |          34|          16|      -16.0|        44.0|
| 404 | 2017-01-14 | Divisional | Atlanta       | Seattle       |          36|          20|       -6.5|        51.0|
| 218 | 2017-01-15 | Divisional | Kansas City   | Pittsburgh    |          16|          18|       -2.5|        45.5|
| 268 | 2017-01-15 | Divisional | Dallas        | Green Bay     |          31|          34|       -5.5|        52.5|
| 35  | 2017-01-22 | Conference | New England   | Pittsburgh    |          36|          17|       -5.5|        50.0|
| 403 | 2017-01-22 | Conference | Atlanta       | Green Bay     |          44|          21|       -6.5|        59.5|
| 402 | 2017-02-05 | Super Bowl | Atlanta       | New England   |          28|          34|        3.0|        57.0|

bonus function
--------------

This function will combine multiple seasons of data together.

**Please be kind when scraping websites**

``` r
GetLinesRange <- function(sport = "NBA", year.start, year.end, type = "both") {
  lines <- data.frame()
  
  for (year in year.start:year.end) {
    temp <- GetLines(sport, year, type)
    temp$season <- year
    lines <- rbind(lines, temp)
  }
  
  return(lines)
}
```

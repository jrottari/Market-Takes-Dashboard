library(tidyverse)
library(devtools)
library(blsR)
library(httr)
library(glue)
library(lubridate)
library(xlsx)
library(readxl)

## if the user wants to download this data to their own local 
setwd("C:/Users/user/Dropbox/Josiah/Economic Research/Economic Indicators Data")


## two different ways to put stuff in r_environ
## other users can use this to store their BLS api key in their local R environment

#file.edit("~/.Renviron")
#usethis::edit_r_environ()

## checking the api key is in Renviron and setting the key with "get_key"
## other users should use this section to use their own API key and one must
## register with the BLS to be able to call 20 years of data at a time
Sys.getenv()
has_key <- bls_has_key()
api_key = bls_get_key()

# Demo with the BLS API ------
## the following is done using the tutorial found here: 
## https://www.youtube.com/watch?v=118FyvU6OSc

# API URL
url = "https://api.bls.gov/publicAPI/v2/timeseries/data/"

#a function that takes in data from BLS API, formats the dates in a nice way, and
## selects just the dates and values to create a 2 column tibble
#------

BLS.cleaner = function(data){
  data$year = as.numeric(data$year)
  data$value = as.numeric(data$value)
  data = data %>% mutate(periodName = recode(periodName,
                                             "January" =1,"February"=2, "March"=3, "April"=4,
                                             "May"=5, "June"=6, "July" =7, "August"=8, 
                                             "September"=9, "October"=10, "November"=11,
                                             "December" =12
  ))
  
  data = data %>% mutate(Date = make_date(year, periodName))
  data = select(data, value, Date)
  return(invisible(data))
}
#-----

##using the BLS API to get CPI, Core CPI, PPI, New Jobs 3 month average,
## and unemployment rate
## we can only get 20 years of data at a time so we have to do a few different
## calls to the api
## this first request is the one that gets you the most current data, so the end
## year here is the one that needs to be adjusted to get the most up-to-date 
## data
# BLS Data Through API ----------------------------------------------------

payload1 = glue('{
"seriesid":["CUUR0000SA0", "CUUR0000SA0L1E", "PCUOMFG--OMFG--", "CES0000000026",
"LNS14000000"], 
"startyear":"2005",
"endyear":"2023",
"registrationkey":"{{bls_get_key()}}"
}', .open="{{", .close = "}}")

# POST request

response1 = POST(url,
                body = payload1,
                content_type("application/json"),
                encode = "json") 
##the content type thing is found in
#the API signatures section on BLS

# Convert JSON to list
x1 = content(response1, "text") %>% jsonlite::fromJSON()

payload2 = glue('{
"seriesid":["CUUR0000SA0", "CUUR0000SA0L1E", "PCUOMFG--OMFG--", "CES0000000026",
"LNS14000000"], 
"startyear":"1984",
"endyear":"2004",
"registrationkey":"{{bls_get_key()}}"
}', .open="{{", .close = "}}")

# POST request

response2 = POST(url,
                 body = payload2,
                 content_type("application/json"),
                 encode = "json") 
##the content type thing is found in
#the API signatures section on BLS

# Convert JSON to list
x2 = content(response2, "text") %>% jsonlite::fromJSON()

payload3 = glue('{
"seriesid":["CUUR0000SA0", "CUUR0000SA0L1E", "PCUOMFG--OMFG--", "CES0000000026",
"LNS14000000"], 
"startyear":"1963",
"endyear":"1983",
"registrationkey":"{{bls_get_key()}}"
}', .open="{{", .close = "}}")

# POST request

response3 = POST(url,
                 body = payload3,
                 content_type("application/json"),
                 encode = "json") 
##the content type thing is found in
#the API signatures section on BLS

# Convert JSON to list
x3 = content(response3, "text") %>% jsonlite::fromJSON()

payload4 = glue('{
"seriesid":["CUUR0000SA0", "CUUR0000SA0L1E", "PCUOMFG--OMFG--", "CES0000000026",
"LNS14000000"], 
"startyear":"1942",
"endyear":"1962",
"registrationkey":"{{bls_get_key()}}"
}', .open="{{", .close = "}}")

# POST request

response4 = POST(url,
                 body = payload4,
                 content_type("application/json"),
                 encode = "json") 
##the content type thing is found in
#the API signatures section on BLS

# Convert JSON to list
x4 = content(response4, "text") %>% jsonlite::fromJSON()

payload5 = glue('{
"seriesid":["CUUR0000SA0", "CUUR0000SA0L1E", "PCUOMFG--OMFG--", "CES0000000026",
"LNS14000000"], 
"startyear":"1921",
"endyear":"1941",
"registrationkey":"{{bls_get_key()}}"
}', .open="{{", .close = "}}")

# POST request

response5 = POST(url,
                 body = payload5,
                 content_type("application/json"),
                 encode = "json") 
##the content type thing is found in
#the API signatures section on BLS

# Convert JSON to list
x5 = content(response5, "text") %>% jsonlite::fromJSON()

# ----------------------------------------------


## combining all the cpi data for various years
cpi = rbind(x1$Results$series$data[[1]][,c(1,2,3,5,6)],x2$Results$series$data[[1]], 
            x3$Results$series$data[[1]], x4$Results$series$data[[1]],
            x5$Results$series$data[[1]]) %>% as_tibble()

cpi = BLS.cleaner(cpi) 
write.csv(cpi, file = "cpi.csv")
##plot to compare to the time series found here: https://fred.stlouisfed.org/series/CPIAUCSL
## to ensure that everything looks good
ggplot(cpi) + geom_line(mapping = aes(x = Date, y = value))


ccpi = rbind(x1$Results$series$data[[2]][,c(1,2,3,5,6)],x2$Results$series$data[[2]], 
             x3$Results$series$data[[2]], x4$Results$series$data[[2]],
             x5$Results$series$data[[2]]) %>% as_tibble()
ccpi = BLS.cleaner(ccpi)
write.csv(ccpi, file = "ccpi.csv")

ppi = rbind(x1$Results$series$data[[3]][,c(1,2,3,5,6)],x2$Results$series$data[[3]], 
            x3$Results$series$data[[3]], x4$Results$series$data[[3]],
            x5$Results$series$data[[3]]) %>% as_tibble()
ppi = BLS.cleaner(ppi)
write.csv(ppi, file = "ppi.csv")

jobs = rbind(x1$Results$series$data[[4]][,c(1,2,3,5,6)],x2$Results$series$data[[4]], 
             x3$Results$series$data[[4]], x4$Results$series$data[[4]],
             x5$Results$series$data[[4]]) %>% as_tibble()
jobs = BLS.cleaner(jobs)
write.csv(jobs, file = "jobs.csv")

unemployment = rbind(x1$Results$series$data[[5]][,c(1,2,3,5,6)],x2$Results$series$data[[5]], 
                     x3$Results$series$data[[5]], x4$Results$series$data[[5]],
                     x5$Results$series$data[[5]]) %>% as_tibble()
unemployment = BLS.cleaner(unemployment)
write.csv(unemployment, file = "unemployment.csv")

##pulling data from URLS
library(readxl)
library(openxlsx)
library(httr)

##getting mortgage data and formatting it as desired
mortgage = read.xlsx("http://www.freddiemac.com/pmms/docs/historicalweeklydata.xls",
                     detectDates = TRUE)
mortgage = mortgage[seq(5, length(mortgage[,1]), by = 1),c(1,2)]
colnames(mortgage) = c("Date", "30 yr fixed-rate")
write.csv(mortgage, file = "Mortgage.rates.csv")

## getting gdp data
gdp = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=A191RL1Q225SBEA&scale=left&cosd=1947-04-01&coed=2023-07-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-11-22&revision_date=2023-11-22&nd=1947-04-01")
colnames(gdp) = c("Date", "GDP (Billions)")
write.csv(gdp, file = "GDP.csv")

##getting housing starts data
housing.starts = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=718&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=HOUST&scale=left&cosd=1959-01-01&coed=2023-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-11-22&revision_date=2023-11-22&nd=1959-01-01")
colnames(housing.starts) = c("Date", "Starts (Thousands")
write.csv(housing.starts, file = "Housing.Starts.csv")

##getting home price index data
shiller.HPI = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=718&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CSUSHPINSA&scale=left&cosd=1987-01-01&coed=2023-08-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-11-22&revision_date=2023-11-22&nd=1987-01-01")
colnames(shiller.HPI) = c("Date", "Shiller HPI")
write.csv(shiller.HPI, file = "Shiller.HPI.csv")

##getting Federal interest rates
interest = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=718&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=FEDFUNDS&scale=left&cosd=1954-07-01&coed=2023-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-11-22&revision_date=2023-11-22&nd=1954-07-01")
colnames(interest) = c("Date", "FEDFUNDS")
write.csv(interest, file = "Interest.Rates.csv")

##getting shiller PE ratio
url1 = "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
PE = read_excel(tf, 5L)
PE = PE[, c(1,13)]
PE = PE[seq(7, as.numeric(count(PE[,1])), by = 1),]
colnames(PE) = c("Date", "CAPE")
PE$CAPE = as.numeric(PE$CAPE)
PE = PE %>% drop_na()
write.csv(PE, "Shiller.PE.csv")

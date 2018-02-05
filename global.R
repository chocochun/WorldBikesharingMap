library(leaflet)
library(shiny)
library(htmltools)
library(doSNOW)
library(plyr)
library(gdata)
library(XML)
library(RCurl)
library(countrycode)
library(httr)
library(maps)
library(googlesheets)
library(DT)
library(ISOcodes)

googlesheets::gs_auth(token = "googlesheets_token.rds")
sheet_key <- "yourkeys"
ss <- googlesheets::gs_key(sheet_key)

data(ISO_4217)

options(stringsAsFactors = F)

network <- gs_read(ss,  ws = "livemap")
Sys.sleep(5)
detail_to_collect <- gs_read(ss,  ws = "detail")
#network<- read.csv("network_manual.csv")
data <- network
#data<- read.csv("world.csv")

bikeIcons <- iconList(
  operation = makeIcon("bicycle.png", iconWidth = 20, iconHeight = 20),
  planning = makeIcon("question.png", iconWidth = 20, iconHeight = 20),
  stopped = makeIcon("forbidden.png",iconWidth = 20, iconHeight = 20)
)


worldUSA <- data[data$country == "United States of America", ]
worldUSA <- worldUSA[ - which(is.na(worldUSA$X)) ,]
worldUSA <- worldUSA[ worldUSA$Status == "operation" , ]


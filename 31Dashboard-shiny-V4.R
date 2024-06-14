library(RSQLite)
library(dplyr)
library(wordcloud2)
library(readr)
library(leaflet)
library(maps)
library(shiny)
#library(rgdal)
library(RColorBrewer)
library(mapdata)
library(rnaturalearth)
library(tigris)
library(ggplot2)
library(leaflegend)
library(shinydashboard)
library(tidyr)
library(sf)
library(lubridate)
library(dygraphs)
library(xts)
library(DT)
library(tidytext)
library(scales)
library(quanteda)
library(newsmap)
library(tools)
library(shinycssloaders)
library(zoo)
library(future)
library(BMS)
library(plotly)
library(sf)


#setwd("C:/Users/jclopezr/Box/UC_Davis/PhD/Working_folder_JCL/Projects/Resilience-SupplyChain/Task3-Dashboard") #for PC
#setwd("~/Library/CloudStorage/Box-Box/UC_Davis/PhD/Working_folder_JCL/Projects/Resilience-SupplyChain/Task3-Dashboard") #for MAC


#DICTIONARIES
dictionary_topics <- yaml::read_yaml("Data/dictionary-3.yml")


#Reading data
con5 <- dbConnect(RSQLite::SQLite(), dbname = "Data/sentiment.db")

sentiment_word_base <- dbReadTable(con5, "sentiment_word")
sentiment_score_base <- dbReadTable(con5, "sentiment_score")

# Close the database connection
dbDisconnect(con5)

#---------------------

## Word_frequency

# Establish a connection to the database
con4 <- dbConnect(RSQLite::SQLite(), dbname = "Data/word_frequency.db")

unigrams_base <- dbReadTable(con4, "unigrams")
bigrams_base <- dbReadTable(con4, "bigrams")

# Close the database connection
dbDisconnect(con4)

#-----------------------

con6 <- dbConnect(RSQLite::SQLite(), dbname = "Data/locations.db")

countries_base <- dbReadTable(con6, "countries")
states_base <- dbReadTable(con6, "states")
cities_base <- dbReadTable(con6, "cities")

# Close the database connection
dbDisconnect(con6)

#---------------------

con7 <- dbConnect(RSQLite::SQLite(), dbname = "Data/topics.db")

topics_level1_base <- dbReadTable(con7, "topics_level1")
topics_level2_base <- dbReadTable(con7, "topics_level2")

# Close the database connection
dbDisconnect(con7)



#IMPORTING METRICS

#GSCPI ---------------------
gscpi <- read.csv("Data/gscpi.csv") %>%
  select(-"X") %>%
  mutate(Date = dmy(Date))

#Imports and exports US ---------------------

imports <- read.csv("Data/imports.csv") %>%
  select(-"X") %>%
  mutate(time = ym(time))

exports <- read.csv("Data/exports.csv") %>%
  select(-"X") %>%
  mutate(time = ym(time))

#Port Optimizer ---------------------
port_optimizer <- read.csv("Data/port_optimizer.csv") %>%
  select(-"X") %>%
  mutate(date = as.Date(date))

#FRED CA ---------------------
data_FRED_CA <- read.csv("Data/data_bases_FRED-CA.csv") %>%
  select(-c("X", "realtime_start", "realtime_end"))

#FRED US ---------------------
data_FRED_US <- read.csv("Data/data_FRED_values.csv") %>%
  select(-"X")

data_FRED_US_notes <- read.csv("Data/data_FRED_notes.csv") %>%
  select(-"X")
  #select(-c("X", "realtime_start", "realtime_end"))




#IMPORING THE FORECASTING MODELS

## ------------------- GLOBAL -----------------------
## Global supply chain pressure index -------------
model_gscpi <- readRDS("Data/modelgscpi-topic2.rds")
prediction_gscpi <- read_csv(("Data/PredictionGSCPI-topic2.csv")) %>%
  select(-"...1")

## -------------------- US ------------------------------
### Consumer Price Index (CPI) -----------
model_cpi <- readRDS("Data/modelcpi_cpi-topic2.rds")
prediction_cpi <- read_csv(("Data/PredictionCPI_US-topic2.csv")) %>%
  select(-"...1") %>%
  rename(cpi = CPI_US)

### Total Non-Farm Employment (PAYEMS) -----------
model_payems <- readRDS("Data/modelpayems-topic2.rds")
prediction_payems <- read_csv(("Data/PredictionPAYEMS-topic2.csv")) %>%
  select(-"...1") %>%
  rename(payems = PAYEMS_rate)

### Total Port exports (Exports) -----------
model_exports_us <- readRDS("Data/model_exports_us-topic2.rds")
prediction_exports_us <- read_csv(("Data/PredictionExports_US-topic2.csv")) %>%
  select(-"...1") %>%
  rename(exports_us = exports)

### Total Port imports (Ixports) -----------
model_imports_us <- readRDS("Data/model_imports_us-topic2.rds")
prediction_imports_us <- read_csv(("Data/PredictionImports_US-topic2.csv")) %>%
  select(-"...1") %>%
  rename(imports_us = imports)

## -------------------- CA ------------------------------
### Total Non-Farm Employment - California (CANA) -----------
model_cana <- readRDS("Data/model_cana-topic2.rds")
prediction_cana <- read_csv(("Data/PredictionCANA-topic2.csv")) %>%
  select(-"...1") %>%
  rename(cana = CANA_rate)

### Total Port exports (Exports) -----------
model_exports_ca <- readRDS("Data/model_exports_ca-topic2.rds")
prediction_exports_ca <- read_csv(("Data/PredictionExports_CA-topic2.csv")) %>%
  select(-"...1") %>%
  rename(exports_ca = exports)

### Total Port imports (Ixports) -----------
model_imports_ca <- readRDS("Data/model_imports_ca-topic2.rds")
prediction_imports_ca <- read_csv(("Data/PredictionImports_CA-topic2.csv")) %>%
  select(-"...1") %>%
  rename(imports_ca = imports)




#Pre-processing files:
publishing_dates <- sentiment_score_base %>% 
  select(c(doc_id, published_at)) %>%
  group_by(doc_id) %>%
  summarise(published_at = mean(published_at)) %>%
  mutate(published_at = as_date(as_datetime(published_at)))

#WORD FREQUENCIES

#Unigrams
unigrams <- unigrams_base %>%
  mutate(doc_id = as.integer(doc_id)) %>%
  left_join(publishing_dates, by = "doc_id") %>%  
  group_by(published_at, word) %>%
  summarise(Freq = sum(n))

#NEW:
rm(unigrams_base)

# unigrams_tf_idf <- unigrams %>%
#   bind_tf_idf(word, published_at, Freq) %>%
#   select(-c(tf, idf)) %>%
#   arrange(desc(tf_idf))

#Bigrams
bigrams <- bigrams_base %>%
  mutate(doc_id = as.integer(doc_id)) %>%
  left_join(publishing_dates, by = "doc_id") %>%  
  group_by(published_at, bigram) %>%
  summarise(Freq = sum(n))

rm(bigrams_base)

# bigrams_tf_idf <- bigrams %>%
#   bind_tf_idf(bigram, published_at, Freq) %>%
#   select(-c(tf, idf)) %>%
#   arrange(desc(tf_idf))


#SENTIMENT PER TOPIC
sentiment_topic <- topics_level1_base %>% 
  merge(select(sentiment_word_base, -c(published_at)), by = "doc_id") %>% 
  mutate(published_at = as_date(as_datetime(published_at))) %>% 
  group_by(published_at, Topic, Polarity) %>%
  summarise(Words = round(weighted.mean(word_ratio*100,Share),1))

sentiment_topic_score <- topics_level1_base %>%
  merge(select(sentiment_score_base, -c(published_at)), by = "doc_id") %>% 
  #filter(Polarity == "positive") %>%
  mutate(published_at = as_date(as_datetime(published_at))) %>% 
  group_by(published_at, Topic) %>% 
  summarise(Sentiment = weighted.mean(Sentiment, w = Share))

#SENTIMENTS

sentiment_word <- sentiment_word_base %>%
  mutate(published_at = as_date(as_datetime(published_at))) %>% 
  group_by(published_at, Polarity) %>%
  summarise(Words = mean(word_ratio)*100) #%>% #Keep an eye on mean
#pivot_wider(names_from = Polarity, values_from = Words) %>%
#rename(positive_words = positive, negative_words = negative)

#NEW:
rm(sentiment_word_base)

sentiment_score <- sentiment_score_base %>% 
  mutate(published_at = as_date(as_datetime(published_at))) %>% 
  #filter(Polarity == "positive") %>%
  group_by(published_at) %>%
  summarise(Sentiment = mean(Sentiment)) %>%
  mutate(Sentiment = scale(Sentiment)) %>%
  rename(sentiment_score = Sentiment)

rm(sentiment_score_base)

#TOPICS

topics_level1 <- topics_level1_base %>%
  mutate(published_at = as_date(as_datetime(published_at))) %>% 
  group_by(published_at, Topic) %>%
  summarise(Share = mean(Share))

#NEW:
rm(topics_level1_base)

topics_level2 <- topics_level2_base %>%
  mutate(published_at = as_date(as_datetime(published_at))) %>% 
  group_by(published_at, Topic) %>%
  summarise(Share = mean(Share))

#NEW:
rm(topics_level2_base)

#News per date

news_dates <- publishing_dates %>%
  group_by(published_at) %>%     
  summarize(total_news= n())

#CA INDEX
ca_index <- states_base %>% 
  filter(location == "CA") %>%
  mutate(published_at = as_date(published_at))%>%
  distinct(published_at, .keep_all = TRUE)%>%
  left_join(news_dates, by = "published_at") %>%
  mutate(ca_index = Freq/total_news)



wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}




#Setting the maps ------------------
world.data <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(c(admin, iso_a2, iso_a3, continent, geometry)) %>%
  mutate(iso_a2 = case_when(
    admin == "France" ~ "FR",
    admin == "Norway" ~ "NO",
    admin == "Taiwan" ~ "TW",
    admin == "Kosovo" ~ "KV",
    TRUE ~ iso_a2))

states.data <- st_read("Data/states_data.shp") %>%
  select(c(STUSPS, NAME, geometry))

cities <- as.data.frame(read.csv("Data/cal_cities_lat_long.csv")) %>% mutate (Name = tolower(Name)) #Reference data

countries <- countries_base %>%
  left_join(world.data %>%
              st_drop_geometry() %>%
              select(c(iso_a2, admin)), by = c("location" = "iso_a2")) %>%
  mutate(location = admin) %>%
  select(-admin)

states <- states_base %>%
  left_join(states.data %>%
              st_drop_geometry() %>%
              select(c(STUSPS, NAME)), by = c("location" = "STUSPS")) %>%
  mutate(location = NAME) %>%
  select(-NAME)

#Mapping function -------------------------
mapping.function <- function(data_map, region){
  
  # World map ----------------------------------
  if (region == "countries"){
    world_map <- world.data %>% 
      left_join(data_map, by = c("iso_a2" = "location"))
    
    # Create a color palette with handmade bins.
    colourpalette_w <- colorNumeric( palette="YlOrBr", domain=world_map$Freq, na.color = "transparent")
    
    # Prepare the text for tooltips:
    popup_w <- paste(
      "Country: ", world_map$admin,"<br/>", 
      "No. of news: ", world_map$Freq, 
      sep="") %>%
      lapply(htmltools::HTML)
    
    #Creating the World's map
    map_dashboard <- leaflet(world_map) %>% 
      addTiles()  %>% 
      setView( lat=0, lng=0 , zoom=1) %>%
      addPolygons(     
        fillColor = ~colourpalette_w(Freq),
        stroke = TRUE,
        fillOpacity = 0.9,
        color="white",
        weight=0.3,
        label=popup_w,    
        labelOptions=labelOptions(
          style= list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      leaflet::addLegend(pal = colourpalette_w,
                         values=~Freq,            
                         opacity = 0.9,
                         title="Frequency of news",
                         position = "bottomleft")
    
    # State map ----------------------------------  
  }  else if (region == "states") {
    states_map <- states.data %>%
      left_join(data_map, by = c("STUSPS" = "location")) %>%
      filter(!is.na(Freq))
    
    # Creating a color palette based on the number range in the total column
    colourpalette_s <- colorNumeric("Greens", domain=states_map$Freq)
    
    popup_s <- paste(
      "State: ", states_map$NAME,"<br/>", 
      "No. of news: ", states_map$Freq,
      sep ="") %>%
      lapply(htmltools::HTML)
    
    #Creating the U.S. map
    map_dashboard <- leaflet(states_map) %>% 
      addTiles()  %>% 
      #addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 3) %>% 
      addPolygons(fillColor = ~colourpalette_s(Freq), 
                  stroke = TRUE,
                  fillOpacity = 0.9,
                  color="white",
                  weight = 0.3, 
                  label = popup_s,
                  labelOptions=labelOptions(
                    style= list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  )
      ) %>%
      leaflet::addLegend(pal = colourpalette_s, 
                         values = states_map$Freq, 
                         position = "bottomright", 
                         title = "Frequency of news")
    
    
    # CA map ----------------------------------  
  } else if (region=="cities"){
    ca_map_data <- left_join(data_map , cities, by = c("location"="Name")) #Joining CA data with freq of words
    
    markers <- makeSymbolsSize( #Creating the markers
      values = ca_map_data$Freq,
      shape = 'circle',
      color = 'red',
      fillColor = 'red',
      opacity = .5,
      baseSize =1
    )
    
    popup_c <- paste( #Pop up for the map
      "City: ", toTitleCase(ca_map_data$location),"<br/>", 
      "No. of news: ", ca_map_data$Freq,
      sep ="") %>%
      lapply(htmltools::HTML)
    
    map_dashboard <- leaflet() %>% #Map
      addTiles()  %>% 
      #addProviderTiles("CartoDB.Positron") %>%
      setView(-120.855028, 36.910233, zoom = 5) %>%
      addMarkers(
        data=ca_map_data,
        icon = markers,
        lat=~Latitude, lng = ~Longitude,
        label = popup_c,
        labelOptions=labelOptions(
          style= list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegendSize(
        values = ca_map_data$Freq,
        color = 'red',
        fillColor = 'red',
        opacity = 0.5,
        title = "No. of News",
        shape = "circle",
        orientation = "vertical",
        breaks = 5,
        baseSize =1,
        position = "bottomleft"
      )
  }
}




# -------- PAGE 1 DASHBOARD -------

#Number of news published every day
news_dates.xts <- xts(news_dates$total_news, order.by = news_dates$published_at) 

names(news_dates.xts) <- "No of News"

topics_dates <- news_dates %>%
  merge(topics_level1, by = "published_at") %>%
  mutate(News_by_topic = round(total_news*Share,0))



#Creating the topics time series objects 

#TOPICS SHARE - Level 1 ------------------------------
xts_topics <- topics_level1 %>%
  left_join(news_dates, by = "published_at") %>%
  mutate(News = total_news*Share) %>%
  select(published_at, News, Topic) %>%  
  mutate(News = round(News,1)) %>%
  pivot_wider(names_from = Topic, values_from = News, values_fill = NULL) %>%
  ungroup() %>%
  xts(order.by = .$published_at[, drop=FALSE])

storage.mode(xts_topics) <- "numeric"

xts_topics <- xts_topics[, -which(colnames(xts_topics) == "published_at")]

#TOPICS SHARE - Level 2 (subtopics) ----------------

xts_subtopics <- topics_level2 %>%
  left_join(news_dates, by = "published_at") %>%
  mutate(News = total_news*Share) %>%
  select(published_at, News, Topic) %>%  
  mutate(News = round(News,1)) %>%
  pivot_wider(names_from = Topic, values_from = News, values_fill = NULL) %>%
  xts(order.by = .$published_at[, drop=FALSE])

storage.mode(xts_subtopics) <- "numeric"

xts_subtopics <- xts_subtopics[, -which(colnames(xts_subtopics) == "published_at")]



#Time series object for CA_index
ca_index.xts <- xts(ca_index$ca_index*100, order.by = as_date(ca_index$published_at))

names(ca_index.xts) <- "California Index"




# ------------ DASHBOARD PAGE 2 -------------

#News sentiment
sentiment_positive <- sentiment_word %>%
  filter(Polarity == "positive") %>%
  select(published_at, Words)

xts_sentiment_positive <- xts(sentiment_positive$Words, order.by = sentiment_positive$published_at)

names(xts_sentiment_positive) <- "Positive"


sentiment_negative <- sentiment_word %>%
  filter(Polarity == "negative") %>%
  select(published_at, Words)

xts_sentiment_negative <- xts(sentiment_negative$Words, order.by = sentiment_negative$published_at)

names(xts_sentiment_negative) <- "Negative"

xts_sentiment_news <- cbind(xts_sentiment_negative, xts_sentiment_positive)



# ---------------- PAGE 3 DASHBOARD ------------

#Processing metrics

##FRED US ---------------------------------------------------
metrics_FRED_US <- data_FRED_US %>%
  left_join(data_FRED_US_notes, by = "id") %>%
  mutate(date = as_date(date))

#NEW:
rm(data_FRED_US)
rm(data_FRED_US_notes)
  
#select("name", "value", "date", "seasonal_adj",  "notes", "units") %>% #"topics",
  

##FRED CA ---------------------------------------------------
metrics_FRED_CA <- data_FRED_CA %>%
  select("name", "value", "date", "seasonal_adj", "topics", "notes", "units") %>%
  mutate(date = as_date(date))

#NEW
rm(data_FRED_CA)

## GSCPI --------------------------------------------------
gscpi.xts <- xts(gscpi$gscpi, order.by = gscpi$Date)
names(gscpi.xts) <- "GSCPI"

#NEW:
rm(gscpi)

#Exports US ------------------------------------------------
exports_USA <- exports %>%
  filter(PORT_NAME == "TOTAL FOR ALL PORTS") %>%
  select(-c(MONTH, PORT)) 

mean_2018_WGT <- exports_USA %>%
  filter(year(time) == 2018) %>%
  summarize(mean(CNT_WGT_MO)) %>%
  pull()

mean_2018_VAL <- exports_USA %>%
  filter(year(time) == 2018) %>%
  summarize(mean(CNT_VAL_MO)) %>%
  pull()

exports_USA <- exports_USA %>%
  mutate(CNT_WGT_MO = (CNT_WGT_MO/mean_2018_WGT)*100) %>%
  mutate(CNT_VAL_MO = (CNT_VAL_MO/mean_2018_VAL)*100)

exports_USA.xts <- xts(exports_USA$CNT_VAL_MO, order.by = exports_USA$time)
names(exports_USA.xts) <- "Exports U.S."

#NEW:
rm(exports_USA)

#Imports US ------------------------------------------------
imports_USA <- imports %>%
  filter(PORT_NAME == "TOTAL FOR ALL PORTS") %>%
  select(-c(MONTH, PORT)) 

mean_2018_WGT <- imports_USA %>%
  filter(year(time) == 2018) %>%
  summarize(mean(CNT_WGT_MO)) %>%
  pull()

mean_2018_VAL <- imports_USA %>%
  filter(year(time) == 2018) %>%
  summarize(mean(CNT_VAL_MO)) %>%
  pull()

imports_USA <- imports_USA %>%
  mutate(CNT_WGT_MO = (CNT_WGT_MO/mean_2018_WGT)*100) %>%
  mutate(CNT_VAL_MO = (CNT_VAL_MO/mean_2018_VAL)*100)

imports_USA.xts <- xts(imports_USA$CNT_VAL_MO, order.by = imports_USA$time)
names(imports_USA.xts) <- "Imports U.S."

#NEW:
rm(imports_USA)

# Exports CA -------------------------------
exports_CA <- exports %>%
  filter(grepl(", CA", PORT_NAME)) %>%
  group_by(time) %>%
  summarize(CNT_WGT_MO = sum(CNT_WGT_MO), 
            CNT_VAL_MO = sum(CNT_VAL_MO))

mean_2018_WGT <- exports_CA %>%
  filter(year(time) == 2018) %>%
  summarize(mean(CNT_WGT_MO)) %>%
  pull()

mean_2018_VAL <- exports_CA %>%
  filter(year(time) == 2018) %>%
  summarize(mean(CNT_VAL_MO)) %>%
  pull()

exports_CA <- exports_CA %>%
  mutate(CNT_WGT_MO = (CNT_WGT_MO/mean_2018_WGT)*100) %>%
  mutate(CNT_VAL_MO = (CNT_VAL_MO/mean_2018_VAL)*100)

exports_CA.xts <- xts(exports_CA$CNT_VAL_MO, order.by = exports_CA$time)
names(exports_CA.xts) <- "Exports CA"

#NEW:
rm(exports_CA)


# Imports CA ------------------------------
imports_CA <- imports %>%
  filter(grepl(", CA", PORT_NAME)) %>%
  group_by(time) %>%
  summarize(CNT_WGT_MO = sum(CNT_WGT_MO), 
            CNT_VAL_MO = sum(CNT_VAL_MO))

mean_2018_WGT <- imports_CA %>%
  filter(year(time) == 2018) %>%
  summarize(mean(CNT_WGT_MO)) %>%
  pull()

mean_2018_VAL <- imports_CA %>%
  filter(year(time) == 2018) %>%
  summarize(mean(CNT_VAL_MO)) %>%
  pull()

imports_CA <- imports_CA %>%
  mutate(CNT_WGT_MO = (CNT_WGT_MO/mean_2018_WGT)*100) %>%
  mutate(CNT_VAL_MO = (CNT_VAL_MO/mean_2018_VAL)*100)

imports_CA.xts <- xts(imports_CA$CNT_VAL_MO, order.by = imports_CA$time)
names(imports_CA.xts) <- "Imports CA"

#NEW:
rm(imports_CA)

# ---------------- PAGE 5 DASHBOARD ------------

port_optimizer.xts <- port_optimizer %>%
  xts(order.by = .$date[, drop=FALSE])

storage.mode(port_optimizer.xts) <- "numeric"

port_optimizer.xts <- port_optimizer.xts[, -which(colnames(port_optimizer.xts) == "date")]

#NEW:
rm(port_optimizer)



# ----------------- DASHBOARD ---------------

header <- dashboardHeader(title = "Resilience Dashboard")

#Preliminar data (default) ----------
dates_news <- publishing_dates$published_at %>% 
  na.omit()

metrics_names <- unique(metrics_FRED_US$name)

# Sidebarsetting -----------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(menuItem("Summary of News", tabName = "timeseries"),
              menuItem("News Metrics", tabName = "charts"),
              menuItem("Macroeconomic and Supply Chain", tabName = "metrics"),
              menuItem("Predictive Modeling", tabName = "forecasting"),
              menuItem("California Indicators", tabName = "california"))
)


#Body setting ----------------------------
body <- dashboardBody(
  tabItems(
    
    #Tab 1 -----------------------
    tabItem(tabName = "timeseries",
            fluidRow(
              box(
                title = "Number of news",
                width = 6,
                valueBoxOutput("BoxNews_today"),
                valueBoxOutput("BoxNews_week"),
                valueBoxOutput("BoxNews_month"),
                height = "12em"
              ),
              column(width = 6,
                     box(
                       title = "Customize",
                       collapsible = TRUE,
                       status = "warning",
                       width = NULL,
                       sliderInput(
                         inputId = "smooth",
                         label = "Smooth for 'n' days",
                         min = 0,
                         max = 30,
                         value = 7
                       ),
                       height = "12em"
                     )
              )
            ),
            fluidRow(
              column(width = 6,
                     box(
                       title = "Time series of news",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       dygraphOutput("timeseries", height = "30em"),
                       height = "35em"
                     ),
                     box(
                       title = "Share of news",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       selectInput(
                         inputId = "topic_choice_news",
                         label = NULL,
                         choices = c(
                           "All news" = "news",
                           "Political" = "political",
                           "Environmental" = "environmental",
                           "Infrastructure" = "infrastructure",
                           "Financial" = "financial",
                           "Supply and Demand" = "supply_demand",
                           "Logistic" = "logistic",
                           "System" = "system",
                           "Sector" = "sector"
                         )
                       ),
                       dygraphOutput("topic_share", height = "27em"),
                       height = "35em"
                     )
              ),
              column(width = 6,
                     box(
                       title = "Sentiment of news",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       dygraphOutput("sentiment_score", height = "30em"),
                       height = "35em"
                     ),
                     box(
                       title = "California index",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       dygraphOutput("ca_index", height = "30em"),
                       height = "35em"
                     )
              )
            )
    ),
    
    #Tab 2 ---------------------------------
    tabItem(tabName = "charts",
            fluidRow(
              column(width = 8,
                     box(
                       title = "Heatmaps of news",
                       width = NULL,
                       solidHeader = TRUE,
                       status= "primary",
                       leafletOutput("plot_map", height = "35em") %>%
                         withSpinner(color = "#0dc5c1"),
                       height = "35em"
                     ),
                     
                     fluidRow(
                       box(
                         title = "Wordcloud",
                         width = 4,
                         solidHeader = TRUE,
                         status= "primary",
                         selectInput(
                           inputId = "wordcloud_type",
                           label = "Wordcloud level: ",
                           choices = c(
                             "Unigrams",
                             "Bigrams")),
                         wordcloud2Output("word_cloud", height = "20em") %>%
                           withSpinner(color = "#0dc5c1"),
                         height = "30em"
                       ),
                       
                       box(
                         title = "Sentiment of words on topics",
                         width = 8,
                         solidHeader = TRUE,
                         status = "primary",
                         dygraphOutput("sentimentwords_topics", height = "26em") %>%
                           withSpinner(color = "#0dc5c1"),
                         height = "30em"
                       )
                     )
              ),
              
              column(width = 4,
                     box(
                       title = "Geographical region: ",
                       collapsible = TRUE,
                       status= "warning",
                       width = NULL,
                       radioButtons("geo_choice", 
                                    label = NULL,
                                    choices = c(
                                      "World" = "countries",
                                      "U.S." = "states",
                                      "California" = "cities"
                                    ),
                                    selected = "countries"
                       )
                     ),
                     
                     box(
                       title = "Select Date Range: ",
                       collapsible = TRUE,
                       status= "warning",
                       width = NULL,
                       sliderInput(
                         inputId = "dates",
                         label = NULL,
                         min = min(dates_news),
                         max = max(dates_news),
                         value = c(min(dates_news), max(dates_news)),
                         timeFormat = "%Y-%m-%d"
                       )
                     ),
                     
                     box(
                       title = "Select Topic",
                       collapsible = TRUE,
                       status = "warning",
                       width = NULL,
                       selectInput(
                         inputId = "topic_choice",
                         label = NULL,
                         choices = c(
                           "All news" = "news",
                           "Political" = "political",
                           "Environmental" = "environmental",
                           "Infrastructure" = "infrastructure",
                           "Financial" = "financial",
                           "Supply and Demand" = "supply_demand",
                           "Logistic" = "logistic",
                           "System" = "system",
                           "Sector" = "sector"
                         )
                       )
                     ),
                     
                     box(
                       title = "Top 10 locations",
                       solidHeader = TRUE,
                       status= "primary",
                       width = NULL,
                       selectInput(
                         input = "top_news_choice",
                         label = NULL,
                         choices = c(
                           "Last 48 hours" = "day",
                           "Last 7 days" = "week",
                           "Last 30 days" = "month"
                         )
                       ),
                       
                       tableOutput("top_news")
                     )
              )
            )
    ),
    
    # Tab 3 -----------------------------------
    tabItem(tabName = "metrics",
            fluidRow(
              column(width = 8,
                     box(
                       title = "Metrics on supply chain",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       dygraphOutput("data_FRED", height = "30em"),
                       height = "35em"
                     ),
                     fluidRow(
                       box(
                         title = "Global supply chain pressure Index",
                         width = 6,
                         solidHeader = TRUE,
                         status = "primary",
                         dygraphOutput("GSCPI", height = "25em"),
                         height = "30em"
                       ),
                       box(
                         title = "Imports - Exports",
                         width = 6,
                         solidHeader = TRUE,
                         status = "primary",
                         selectInput(
                           input = "imports_exports",
                           label = NULL,
                           choices = c(
                             "Imports",
                             "Exports"
                           )                         
                         ),
                         dygraphOutput("import_export", height = "22em"),
                         height = "30em"
                       )
                     )
              ),
              
              column(width = 4,
                     box(
                       title = "Selection Menu",
                       collapsible = TRUE,
                       status = "warning",
                       width = NULL,
                       radioButtons(
                         inputId = "geo_choice_FRED", 
                         label = NULL,
                         choices = c(
                           "U.S." = "national",
                           "California" = "state"
                         ),
                         selected = "national"
                       ),
                       selectizeInput(
                         inputId= "metric_choice",
                         label = "Select Metric Name",
                         choices = metrics_names,
                         #choices = NULL#,
                         selected = NULL,
                         multiple = FALSE,
                         
                         options = list(create=TRUE)
                       ),
                       
                       radioButtons(
                         inputId = "seasonal_adj", 
                         label = "Seasonal adjustment: ",
                         choices = c(
                           "Seasonally adjusted" = "SA",
                           "Not seasonally adjusted" = "NSA",
                           "Seasonally Adjusted Annual Rate" = "SAAR"
                         ),
                         #choices = NULL
                         selected = "NSA"
                       ),
                       
                       div(
                         style = "height: 230px; overflow-y: scroll; border: 1px solid #ccc; padding: 5px;",
                         p("Metric description: "),
                         
                         p(textOutput(
                           outputId = "metric_description"))
                       )
                       
                     )
              )
            )
    ),
    
    # Tab 4 -----------------------------------
    tabItem(tabName = "forecasting",
            fluidRow(
              column(width = 8,
                     box(
                       title = "Prediction chart",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       dygraphOutput("forecasted_model")
                     )
              ),
              
              column(width = 4,
                     box(
                       title = "Forecasting Model",
                       status = "warning",
                       width = NULL,
                       selectInput(
                         input = "forecasting_model",
                         label = NULL,
                         choices = c(
                           "Global Supply Chain Pressure Index" = "gscpi",
                           "Consumer Price Index" = "cpi",
                           "Total Non-Farm Employment - U.S. level" = "payems",
                           "Seaports exports - U.S. level" = "exports_us",
                           "Seaports imports - U.S. level" = "imports_us",
                           "Total Non-Farm Employment - California level" = "cana",
                           "Seaports exports - California" = "exports_ca",
                           "Seaports imports - California" = "imports_ca"
                         )
                       )
                     ),
                     box(
                       title = "Model statistics",
                       solidHeader = TRUE,
                       status = "primary",
                       width = NULL,
                       tableOutput("model_stats")
                     )
              )
            ),
            
            fluidRow(
              column(
                width = 6,
                box(
                  title = "Model Inclusion",
                  width = NULL,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("BMA")
                )
              ),
              
              column(
                width = 6,
                box(
                  title = "Marginal Effects",
                  width = NULL,
                  solidHeader = TRUE,
                  status = "primary",
                  plotlyOutput("marginal_effect")
                )
              )
            )
    ),
    
    #Tab 5 ---------------------------------
    tabItem(tabName = "california",
            fluidRow(
              column(width = 6,
                     box(
                       title = "Avg. time at Berth - Port of LA",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       dygraphOutput("time_berth", height = "30em"),
                       height = "35em"
                     )
              ),
              
              column(width = 6,
                     box(
                       title = "Avg. Truck Times",
                       width = NULL,
                       solidHeader = TRUE,
                       status = "primary",
                       fluidRow(
                         column(width = 6,
                                selectInput(
                                  input = "port",
                                  label = "Select port",
                                  choices = c(
                                    "Los Angeles" = "LA",
                                    "Long Beach" = "LB"
                                  )
                                )
                         ),
                         column(width = 6,
                                selectInput(
                                  input = "port_variable",
                                  label = "Select variable",
                                  choices = c(
                                    "Avg. Queue time" = "queue",
                                    "Avg. Terminal time" = "terminal",
                                    "Avg. Total turn time" = "turn"
                                  )
                                )
                         )
                       ),
                       dygraphOutput("data_ports", height = "25em"),
                       height = "35em"
                     )
              )
            )
    )
  )
)

# Setting
ui <- dashboardPage(header, sidebar, body)

#Outputs ----------------------------------
server <- function(input, output, session) {
  
  # ---------------- REACTIVE OBJECTS -------------------
  
  ##TAB 1 - TIME SERIES ---------------------------
  
  ##TAB 2 - CHARTS -------------------------------
  selected_region <- reactive({
    input$geo_choice
  })
  
  min_date <- reactive({
    input$dates[1]
  })
  max_date <- reactive({
    input$dates[2]
  })
  
  ## TAB 3 - METRICS ----------------------------
  
  ## TAB 4 - FORECASTING ----------------------------
  models <- list(
    model_cana = model_cana,
    model_cpi = model_cpi,
    model_exports_ca = model_exports_ca,
    model_exports_us = model_exports_us,
    model_gscpi = model_gscpi,
    model_imports_ca = model_imports_ca,
    model_imports_us = model_imports_us,
    model_payems = model_payems
  )
  
  predictions <- list(
    prediction_cana = prediction_cana,
    prediction_cpi = prediction_cpi,
    prediction_exports_ca = prediction_exports_ca,
    prediction_exports_us = prediction_exports_us,
    prediction_gscpi = prediction_gscpi,
    prediction_imports_ca = prediction_imports_ca,
    prediction_imports_us = prediction_imports_us,
    prediction_payems = prediction_payems
  )
  
  # ---------------------- REACTIVE DATA ---------------
  
  ##TAB 1 - TIMESERIES ----------------------------
  data_timeseries_news <- reactive ({
    data_timeseries_news <- news_dates.xts
    return(data_timeseries_news)
  })
  
  sentiment_timeseries <- reactive ({
    sentiment_timeseries <- xts(sentiment_score$sentiment_score, order.by = sentiment_score$published_at)
    names(sentiment_timeseries) <- "Sentiment"
    return(sentiment_timeseries)
  })
  
  data_topic <- reactive({
    if(input$topic_choice_news == "news"){
      data_topic <- xts_topics
    } else {
      data_topic <- xts_subtopics[, names(dictionary_topics[[input$topic_choice_news]])]
    }
    return(data_topic)
  })
  
  ##TAB 2 - CHARTS -----------------------------------
  #Map
  data_map <- reactive({
    if (selected_region() == "countries") {
      entities_data <- countries_base
    } else if (selected_region() == "states") {
      entities_data <- states_base
    } else if (selected_region() == "cities") {
      entities_data <- cities_base
    }
    data_map <- entities_data %>%
      dplyr::filter(published_at>= min_date(), published_at <= max_date()) %>%
      group_by(location) %>%
      summarize(Freq = sum(Freq))
    
    return(data_map)
  }) %>%
    bindCache(min_date(), max_date(), selected_region())
  
  #Wordcloud
  wordcloud_data <- reactive({
    if (input$wordcloud_type == "Unigrams"){
      unigrams_data <- unigrams %>%
        dplyr::filter(published_at>= min_date(), published_at <= max_date()) %>%
        ungroup() %>%
        group_by(word) %>%
        summarise(Freq = sum(Freq))
      if (input$topic_choice == "news"){
        wordcloud_data <- unigrams_data %>%
          arrange(desc(Freq)) %>%  # Arrange by frequency in descending order
          slice(1:2000)
      } else {
        wordcloud_data <- unigrams_data %>%
          filter(word %in% unlist(dictionary_topics[[input$topic_choice]])) %>%
          arrange(desc(Freq)) %>%  # Arrange by frequency in descending order
          slice(1:2000)
      }
      
    } else if (input$wordcloud_type == "Bigrams") {
      bigrams_data <- bigrams %>%
        dplyr::filter(published_at>= min_date(), published_at <= max_date()) %>%
        ungroup() %>%
        group_by(bigram) %>%
        summarise(Freq = sum(Freq)) 
      if (input$topic_choice == "news"){
        wordcloud_data <- bigrams_data %>%
          arrange(desc(Freq)) %>%  # Arrange by frequency in descending order
          slice(1:2000)
      } else {
        wordcloud_data <- bigrams_data %>%
          filter(sapply(strsplit(bigram, " "), function(words) any(words %in% unlist(dictionary_topics[[input$topic_choice]])))) %>%
          arrange(desc(Freq)) %>%  # Arrange by frequency in descending order
          slice(1:2000)
      }
    }
    return(wordcloud_data)
  }) %>%
    bindCache(min_date(), max_date(), input$wordcloud_type, input$topic_choice)
  
  #Sentiment words
  sentiment_words_data <- reactive({
    if (input$topic_choice == "news"){
      sentiment_words_data <- xts_sentiment_news
    } else {
      topic_sentiment_negative <- sentiment_topic %>%
        filter(Polarity == "negative") %>%
        filter(Topic == input$topic_choice) %>%
        ungroup()%>%
        select(published_at, Words)
      
      xts_sentiment_negative_dyn <- xts(topic_sentiment_negative$Words, order.by = topic_sentiment_negative$published_at)
      names(xts_sentiment_negative_dyn) <- "negative"
      
      topic_sentiment_positive <- sentiment_topic %>%
        filter(Polarity == "positive") %>%
        filter(Topic == input$topic_choice) %>%
        ungroup()%>%
        select(published_at, Words)
      
      xts_sentiment_positive_dyn <- xts(topic_sentiment_positive$Words, order.by = topic_sentiment_positive$published_at)
      names(xts_sentiment_positive_dyn) <- "positive"
      
      # Combine the xts objects
      sentiment_words_data <- cbind(xts_sentiment_negative_dyn, xts_sentiment_positive_dyn)
    }
    return(sentiment_words_data)
  }) %>% 
    bindCache(input$topic_choice)
  
  ## Table top 10 locations
  
  data_top_news <- reactive({
    if (selected_region() == "countries") {
      table_data <- countries
    } else if (selected_region() == "states") {
      table_data <- states
    } else if (selected_region() == "cities") {
      table_data <- cities_base
    }
    
    if(input$top_news_choice == "day") {
      data_top_news <- table_data %>%
        filter(published_at >= Sys.Date()-2)
    } else if (input$top_news_choice == "week") {
      data_top_news <- table_data %>%
        filter(published_at >= Sys.Date()-7)
    } else if (input$top_news_choice == "month") {
      data_top_news <- table_data %>%
        filter(published_at >= Sys.Date()-30)
    }
    return(data_top_news)
  })
  
  ## TAB 3 - METRICS ----------------------------
  
  ### Metrics FRED
  metrics_FRED <- reactive({
    if (input$geo_choice_FRED == "national"){
      metrics_FRED <- metrics_FRED_US
    } else if (input$geo_choice_FRED == "state") {
      metrics_FRED <- metrics_FRED_CA
    }
    return (metrics_FRED)
  })
  
  observe({
    req(metrics_FRED())
    metrics_names <- metrics_FRED() %>%
      distinct(name) %>%
      pull(name)
    
    updateSelectizeInput(
      session,
      inputId = "metric_choice",
      label = "Select Metric Name",
      choices = metrics_names,
      selected = NULL,
      #multiple = FALSE,
      options = list(create=TRUE)
    )
  })
  
  observe({
    req(input$metric_choice, metrics_FRED())
    if (!is.null(input$metric_choice) && input$metric_choice != "") {
      seasonal_adjustment <- metrics_FRED() %>%
        filter(name == input$metric_choice) %>%
        distinct(seasonal_adj) %>%
        pull(seasonal_adj)
      
      if(length(seasonal_adjustment) > 0) {
        updateRadioButtons(
          session,
          inputId = "seasonal_adj", 
          label = "Seasonal adjustment: ",
          choices = seasonal_adjustment
        )
      } 
    }
  })
  
  data_metrics <- reactive({
    req(input$metric_choice, input$seasonal_adj, metrics_FRED())
    if (!is.null(input$metric_choice) && input$metric_choice != "") {
      data_metrics <- metrics_FRED() %>%
        filter(name == input$metric_choice, seasonal_adj == input$seasonal_adj) %>%#, date >= min_date_FRED(), date <= max_date_FRED()) %>%
        select("date", "value", "notes", "units")
      
      if (nrow(data_metrics) > 0) {
        return(data_metrics)
      } else {
        return(NULL)
      }
    } else{
      return (NULL)
    }
  })
  
  data_metrics.xts <- reactive({
    req(data_metrics())
    if (!is.null(input$metric_choice) && input$metric_choice != "") {
      metrics_filtered.xts <- data_metrics() %>%
        select("date", "value")
      metrics.xts <- xts(metrics_filtered.xts$value, order.by = metrics_filtered.xts$date)
      names(metrics.xts) <- input$metric_choice
      return(metrics.xts)
    } else{
      return (NULL)
    }
  })
  
  ### Imports - Exports
  data_imp_exp <- reactive({
    if(input$imports_exports == "Imports" && input$geo_choice_FRED == "national"){
      data_imp_exp <- imports_USA.xts
    } else if (input$imports_exports == "Exports" && input$geo_choice_FRED == "national") {
      data_imp_exp <- exports_USA.xts
    } else if (input$imports_exports == "Imports" && input$geo_choice_FRED == "state") {
      data_imp_exp <-imports_CA.xts
    } else if (input$imports_exports == "Exports" && input$geo_choice_FRED == "state") {
      data_imp_exp <- exports_CA.xts
    }
    
    return(data_imp_exp)
  })
  
  ## TAB 4 - FORECASTING ----------------------------
  
  #Forecasting plot
  forecasting_data <- reactive({
    forecasting_name <- paste0("prediction_", input$forecasting_model)
    predictions[[forecasting_name]]
  })
  
  forecasting.xts <- reactive({
    forecasting.xts <- forecasting_data() %>%
      distinct(date, .keep_all = TRUE) %>%
      xts(order.by = .$date[, drop=FALSE])
    
    forecasting.xts <- forecasting.xts[, -which(colnames(forecasting.xts) %in% c("date", "residuals"))]
    storage.mode(forecasting.xts) <- "numeric"
    return (forecasting.xts)
  })
  
  #Model plot
  
  bma_model <- reactive({
    model_name <- paste0("model_", input$forecasting_model)
    models[[model_name]]
  })
  
  #Marginal effect
  effect_data <- reactive({
    coef_data <- coef(bma_model(), include.pip = TRUE, include.postmean = TRUE, include.postsd = TRUE)
    
    effect_data <- data.frame(
      variable = rownames(coef_data),
      post_mean = coef_data[, "Post Mean"],
      post_sd = coef_data[, "Post SD"]
    )
    
    return (effect_data)
  })
  
  #Model stats
  model_stats <- reactive ({
    fullmodel <- fullmodel.ssq(bma_model())
    
    model_stats <- data.frame(
      R2 = fullmodel$R2,
      ESS = fullmodel$ymy,
      RSS = fullmodel$ypy,
      TSS = fullmodel$yty,
      Fstat = fullmodel$Fstat
    )
    
    return (model_stats)
  })
  
  ## TAB 5 - CALIFORNIA ----------------------------
  data_ports <- reactive({
    if(input$port == "LA" && input$port_variable == "queue"){
      data_ports <- port_optimizer.xts %>% subset(., select = "queue_pola")
    } else if(input$port == "LA" && input$port_variable == "terminal"){
      data_ports <- port_optimizer.xts %>% subset(., select = "terminal_pola")
    } else if(input$port == "LA" && input$port_variable == "turn"){
      data_ports <- port_optimizer.xts %>% subset(., select = "turn_pola")
    } else if(input$port == "LB" && input$port_variable == "queue"){
      data_ports <- port_optimizer.xts %>% subset(., select = "queue_polb")
    } else if(input$port == "LB" && input$port_variable == "terminal"){
      data_ports <- port_optimizer.xts %>% subset(., select = "terminal_polb")
    } else if(input$port == "LB" && input$port_variable == "turn"){
      data_ports <- port_optimizer.xts %>% subset(., select = "turn_polb")
    }
    
    return(data_ports)
  })
  
  
  
  
  # --------------------- RENDERING GRAPHS ---------------------------
  
  ## TAB 1 - TIME SERIES -------------------------
  
  ### Info boxes
  output$BoxNews_today <- renderValueBox({
    valueBox(sum(data_timeseries_news()[paste(Sys.Date()-0:2)]), "Last 48 hours")#, icon = icon("fa-newspaper"))
  })
  
  output$BoxNews_week <- renderValueBox({
    valueBox(sum(data_timeseries_news()[paste(Sys.Date()-0:7)]), "Last 7 days")#, icon = icon("fa-solid fa-newspaper"))
  })
  
  output$BoxNews_month <- renderValueBox({
    valueBox(sum(data_timeseries_news()[paste(Sys.Date()-0:30)]), "Last 30 days")#, icon = icon("fa-solid fa-newspaper"))
  })
  
  ### Time series of news
  output$timeseries <- renderDygraph({
    dygraph(data_timeseries_news(), group="news") %>%
      dyAxis("y", label = "News/day") %>%
      dyRangeSelector(dateWindow = c(min(dates_news), max(dates_news))) %>%
      dyLegend(width = 400) %>% 
      dyRoller(rollPeriod = input$smooth)
  })
  
  ### Sentiment score
  output$sentiment_score <- renderDygraph({
    mn = mean(sentiment_timeseries(), na.rm = TRUE)
    # std = sd(sentiment_timeseries(), na.rm = TRUE)
    
    # if(input$topic_choice_news == "news") {
    dygraph(sentiment_timeseries(), group="news") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyAxis("y", label = "Sentiment score") %>% #, valueRange = c(-1, 1)) %>%
      dyLimit(mn, color = "red") %>%
      dyRangeSelector(dateWindow = c(min(dates_news), max(dates_news))) %>%
      dyLegend(width = 400) %>% 
      dyRoller(rollPeriod = input$smooth)
  })
  
  ### Topic share
  output$topic_share <- renderDygraph({
    dygraph(data_topic(), group="news") %>%
      dyStackedRibbonGroup(names(data_topic())) %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
      dyLegend(show = "always") %>%
      dyAxis("y", label = "News by topic") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = input$smooth)
  })
  
  ### California Index
  output$ca_index <- renderDygraph({
    dygraph(ca_index.xts, group="news") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyAxis("y", label = "California index") %>%
      dyRangeSelector(dateWindow = c(min(dates_news), max(dates_news))) %>%
      dyLegend(width = 400) %>% 
      dyRoller(rollPeriod = input$smooth)
  })
  
  ##TAB 2 - CHARTS --------------------------------------
  
  ### Render the map based on the selected region
  output$plot_map <- renderLeaflet({
    mapping.function(data_map(), selected_region())
  })
  
  ### Render the word cloud based on the selected region
  output$word_cloud <- renderWordcloud2({
    wordcloud2a(wordcloud_data(), color = "random-dark", size = 1.5)
  })
  
  ###Render the time series of sentiment words
  output$sentimentwords_topics <- renderDygraph({
    dygraph(sentiment_words_data()) %>%
      dyAxis("y", label = "Share of words per document [%]") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2) %>%
      dyRangeSelector(dateWindow = c(min_date(), max_date()))%>%
      dyLegend(width = 400) %>%
      dyRoller(rollPeriod = 7)
  })
  
  ### Render the table of top locations
  output$top_news <- renderTable({
    data_top_news() %>%
      group_by(location) %>%
      summarise(Freq = sum(Freq)) %>%
      arrange(desc(Freq)) %>%
      select(location) %>%
      rename(Location = location) %>%
      mutate(Location = toTitleCase(Location)) %>%
      head(10)
  })
  
  
  ##TAB 3 - METRICS ---------------------------------
  ##Render the timeseries of the metrics
  output$data_FRED <- renderDygraph({
    if (!is.null(input$metric_choice) && input$metric_choice != "") {
      dygraph(data_metrics.xts(), group = "metrics") %>%
        dyAxis("y", label = paste0(unique(data_metrics()$units)[1])) %>%
        dyRangeSelector(dateWindow = c((min(zoo::index(data_imp_exp()))), max(zoo::index(data_imp_exp())))) %>%
        dyLegend(width = 400)
    } 
    else {
      div("Please choose a metric to display")
    }
  })
  
  ##Render the metrics description
  output$metric_description <- renderText({
    if (!is.null(input$metric_choice) && input$metric_choice != ""){
      if(!is.null(data_metrics()) && nrow(data_metrics()) > 0) {
        text <- data_metrics() %>% 
          # select(notes) %>%
          distinct(notes) %>%
          pull(notes)
        
        paste0(text, ".") 
      }
    } else {
      print("Please choose a metric")
    }
  })
  
  ## Render GSCPI
  output$GSCPI <- renderDygraph(
    dygraph(gscpi.xts, group = "metrics") %>%
      dyAxis("y", label = "GSCPI") %>%
      dyRangeSelector(dateWindow = c((min(zoo::index(data_imp_exp()))), max(zoo::index(data_imp_exp())))) %>%
      dyLegend(width = 400)
  )
  
  output$import_export <- renderDygraph(
    dygraph(data_imp_exp(), group = "metrics") %>%
      dyAxis("y", label = paste0("Value of ", input$imports_exports, " (2018 = 100)")) %>%
      dyRangeSelector(dateWindow = c((min(zoo::index(data_imp_exp()))), max(zoo::index(data_imp_exp())))) %>%
      dyLegend(width = 400)
  )
  
  ##TAB 4 - FORECASTING ---------------------------------
  
  output$forecasted_model <- renderDygraph(
    dygraph(forecasting.xts()) %>%
      #dyAxis("y", label = input$forecasting_model) %>%
      dySeries(input$forecasting_model, label = "Actual") %>%
      dySeries(c("predicted_lower", "fitted_values", "predicted_upper"), label = "Predicted") %>%
      dyRangeSelector() %>%
      dyLegend(width = 400)
  )
  
  output$BMA <- renderPlot(
    image(bma_model())
  )
  
  output$marginal_effect <- renderPlotly({
    plot_marg_eff <- ggplot(effect_data(), aes(x = reorder(variable, -post_mean), y = post_mean, 
                                               text = paste(toTitleCase(variable), ": ", sprintf("%.2f", post_mean), "<br>",
                                                            "Lower Limit :", sprintf("%.2f", post_mean - post_sd), "<br>",
                                                            "Upper Limit :", sprintf("%.2f", post_mean + post_sd))
    )) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_errorbar(aes(ymin = post_mean - post_sd, ymax = post_mean + post_sd), width = 0.01*(max(effect_data()$post_mean) - min(effect_data()$post_mean))/mean(effect_data()$post_mean)) +
      labs(x = "Variables",
           y = "Posterior Mean of Coefficients") +
      theme_minimal() +
      coord_flip()
    
    ggplotly(plot_marg_eff, tooltip = "text") %>% 
      layout(hoverlabel = list(bgcolor = "steelblue", font = list(size = 12))) %>%
      config(displayModeBar = FALSE)
  })
  
  output$model_stats <- renderTable(
    model_stats()
  )
  
  ##TAB 5 - CALIFORNIA ---------------------------------
  output$time_berth <- renderDygraph(
    dygraph(port_optimizer.xts %>% subset(., select = "time_berth"), group = "port") %>%
      dyAxis("y", label = "Days") %>%
      #dySeries(label = "Days") %>%
      dyRangeSelector() %>%
      dyLegend(width = 400)
  )
  
  output$data_ports <- renderDygraph(
    dygraph(data_ports(), group = "port") %>%
      dyAxis("y", label = "Minutes") %>%
      #dySeries(label = "Minutes") %>%
      dyRangeSelector() %>%
      dyLegend(width = 400)
  )
  
}

shinyApp(ui, server)


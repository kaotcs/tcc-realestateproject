###### R - Webscrapping

pacotes <- c("XML","RSelenium","stringr","rvest", 'xml2', 'tidyverse', 'stringr') #,"textreadr"

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

######
# FIRST EXAMPLES
#####
simple <- read_html("https://dataquestio.github.io/web-scraping-pages/simple.html")

simple

forecasts <- read_html("https://forecast.weather.gov/MapClick.php?lat=37.7771&lon=-122.4196#.Xl0j6BNKhTY") %>% 
  html_nodes('.temp') %>%  html_text()

forecasts

parse_number(forecasts)

###### https://www.scrapingbee.com/blog/web-scraping-r/

scrape_url <- "https://www.scrapingbee.com/"

flat_html <- readLines(con = scrape_url)
flat_html

# SCRAPPING WIKIPEDIA
wiki_url <- "https://en.wikipedia.org/wiki/Leonardo_da_Vinci"

wiki_read <- readLines(wiki_url, encoding = "UTF-8")

parsed_wiki <- htmlParse(wiki_read, encoding = "UTF-8")

wiki_intro_text <- parsed_wiki["//p"]

wiki_intro_text[[4]]

getHTMLLinks(wiki_read)

length(getHTMLLinks(wiki_read))

wiki_url1 <- "https://en.wikipedia.org/wiki/Help:Table"

wiki_read1 <- readLines(wiki_url1, encoding = "UTF-8")

length((readHTMLTable(wiki_read1)))

names(readHTMLTable(wiki_read1))

readHTMLTable(wiki_read1)$"The table's caption\n"

# CLIMATE HISTORICAL DAILY

html_form_page <- 'http://www.weather.gov.sg/climate-historical-daily' %>% read_html()

weatherstation_identity <- html_form_page %>% html_nodes('button#cityname + ul a') %>% 
  html_attr('onclick') %>%  sub(".*'(.*)'.*", '\\1', .)

weatherdf <- expand.grid(weatherstation_identity, month = sprintf('%02d', 1:12), year = 2016:2020)
str(weatherdf)

urlPages <- paste0('http://www.weather.gov.sg/files/dailydata/DAILYDATA_', 
            weatherdf$Var1, '_', weatherdf$year, weatherdf$month, '.csv')

#lapply(urlPages, function(url){download.file(url, basename(url), method = 'curl')})

# IMDB

library(rvest)

sharknado <- read_html("https://www.imdb.com/title/tt8031422/")

sharknado %>% html_nodes("table") %>% html_table()

######
# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
###### 

url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
webpage <- read_html(url)

# STEP 1 - RANKING
#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

# STEP 2 - TITLE
#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

# STEP 3 - Description, Runtime, Genre, Rating, Metascore, Votes, Gross_Earning_in_Mil , Director and Actor data.
# DESCRIPTION
#Using CSS selectors to scrape the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Let's have a look at the description data
head(description_data)

#Data-Preprocessing: removing '\n'
description_data<-gsub("\n","",description_data)

#Let's have another look at the description data 
head(description_data)

# RUNTIME
#Using CSS selectors to scrape the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

#Let's have a look at the runtime
head(runtime_data)

#Data-Preprocessing: removing mins and converting it to numerical

runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Let's have another look at the runtime data
head(runtime_data)

# GENRE
#Using CSS selectors to scrape the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Let's have a look at the runtime
head(genre_data)

#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces
genre_data<-gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Let's have another look at the genre data
head(genre_data)

# IMDB RATING
#Using CSS selectors to scrape the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Let's have a look at the ratings
head(rating_data)

#Data-Preprocessing: converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Let's have another look at the ratings data
head(rating_data)

# IMDB VOTES
#Using CSS selectors to scrape the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Let's have a look at the votes data
head(votes_data)

#Data-Preprocessing: removing commas
votes_data<-gsub(",","",votes_data)

#Data-Preprocessing: converting votes to numerical
votes_data<-as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)

# IMDB DIRECTORS
#Using CSS selectors to scrape the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Let's have a look at the directors data
head(directors_data)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

# IMDB ACTORS
#Using CSS selectors to scrape the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#Let's have a look at the actors data
head(actors_data)

#Data-Preprocessing: converting actors data into factors
actors_data<-as.factor(actors_data)

# IMDB METASCORE
#Using CSS selectors to scrape the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore data 
head(metascore_data)

#Data-Preprocessing: removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)

#Lets check the length of metascore data
length(metascore_data)

for (i in c(60, 69, 95)){
  a<-metascore_data[1:(i-1)]
  b<-metascore_data[i:length(metascore_data)]
  metascore_data<-append(a,list("NA"))
  metascore_data<-append(metascore_data,b)
}

#Data-Preprocessing: converting metascore to numerical
metascore_data<-as.numeric(metascore_data)

#Let's have another look at length of the metascore data
length(metascore_data)

summary(metascore_data)

# GROSS REVENUE
#Using CSS selectors to scrape the gross revenue section
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Let's have a look at the votes data
head(gross_data)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)

gross_data<-substring(gross_data,2,6)

#Let's check the length of gross data
length(gross_data)

#Filling missing entries with NA
for (i in c(57, 60, 66, 71, 80, 86, 95)){
  a<-gross_data[1:(i-1)]
  b<-gross_data[i:length(gross_data)]
  gross_data<-append(a,list("NA"))
  gross_data<-append(gross_data,b)
}

#Data-Preprocessing: converting gross to numerical
gross_data<-as.numeric(gross_data)

#Let's have another look at the length of gross data
length(gross_data)

summary(gross_data)

#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      Description = description_data, Runtime = runtime_data,
                      Genre = genre_data, Rating = rating_data,
                      Metascore = metascore_data, Votes = votes_data,
                      Gross_Earning_in_Mil = gross_data,
                      Director = directors_data, 
                      Actor = actors_data)

#Structure of the data frame
str(movies_df)

library('ggplot2')

qplot(data = movies_df,Runtime,fill = Genre,bins = 30)

ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))

ggplot(movies_df,aes(x=Runtime,y=Gross_Earning_in_Mil))+
  geom_point(aes(size=Rating,col=Genre))

######
# SELENIUM http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
######

# SETA O NAVEGADOR
rD <- rsDriver(port = 4813L,
               ##Define a versão do Chrome que o Webdriver deve utilizar     
               chromever = '96.0.4664.45',
               ##Remove as informações do console
               verbose = F)

# CRIA O NAVEGADOR
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4813L,
  browserName = "chrome"
)

#Abre o servidor
remDr$open()

# ACESSA AO WEBPAGE
remDr$navigate("https://www.fcc.gov/media/engineering/dtvmaps")

zip <- "30308"
# Dentro do browser encontrar o item com id=startpoint / enviar ao elemento a variável zip
remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))
# other possible ("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text")

# Encontrar o elemento id=btnsub
remDr$findElements("id", "btnSub")[[1]]$clickElement()

Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

signals <- read_html(html) %>% # parse HTML
  html_nodes("table.tbl_mapReception") %>% # extract table nodes with class = "tbl_mapReception"
  .[3] %>% # keep the third of these tables
  .[[1]] %>% # keep the first element of this list
  html_table(fill=T) # have rvest turn it into a dataframe
View(signals)

names(signals) <- c("rm", "callsign", "network", "ch_num", "band", "rm2") # rename columns

signals <- signals %>%
  slice(2:n()) %>% # drop unnecessary first row
  filter(callsign != "") %>% # drop blank rows
  select(callsign:band) # drop unnecessary columns

head(signals)

read_html(html) %>% 
  html_nodes(".callsign") %>% 
  html_attr("onclick")

read_html(html) %>% 
  html_nodes(".callsign") %>% 
  html_attr("onclick") %>% 
  str_extract("(?<=RX Strength: )\\s*\\-*[0-9.]+")

strength <- read_html(html) %>% 
  html_nodes(".callsign") %>% 
  html_attr("onclick") %>% 
  str_extract("(?<=RX Strength: )\\s*\\-*[0-9.]+")

signals <- cbind(signals, strength)
signals

#####
# https://thatdatatho.com/tutorial-web-scraping-rselenium/
#####

# SETA O NAVEGADOR
rD <- rsDriver(port = 4813L,
               ##Define a versão do Chrome que o Webdriver deve utilizar     
               chromever = '96.0.4664.45',
               ##Remove as informações do console
               verbose = F)

# CRIA O NAVEGADOR
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4813L,
  browserName = "chrome"
)

#Abre o servidor
remDr$open()

# ACESSA AO WEBPAGE
remDr$navigate("https://www.latlong.net/convert-address-to-lat-long.html")

address_element <- remDr$findElement(using = 'class', value = 'width70')

address_element$sendKeysToElement(list("Lombard Street, San Francisco"))

button_element <- remDr$findElement(using = 'class', value = "button")

button_element$clickElement()

out <- remDr$findElement(using = "class", value="coordinatetxt")
lat_long <- out$getElementText()

street_names <- c("Lombard Street, San Francisco", 
                  "Santa Monica Boulevard", 
                  "Bourbon Street, New Orleans", 
                  "Fifth Avenue, New York", 
                  "Richards Street, Vancouver")

get_lat_lon <- function(street_names) {
  remDr$navigate("https://www.latlong.net/convert-address-to-lat-long.html")
  final <- c()
  for(i in 1:length(street_names)) {
    
    remDr$refresh()
    Sys.sleep(1)
    
    address_element <- remDr$findElement(using = 'class', value = 'width70')
    
    address_element$sendKeysToElement(list(street_names[i]))
    button_element <- remDr$findElement(using = 'class', value = "button")
    
    button_element$clickElement()
    Sys.sleep(3)
    
    out <- remDr$findElement(using = "class", value = "coordinatetxt")
    output <- out$getElementText()
    final <- c(final, output)
    
  }
  
  return(final)
}


vector_out <- get_lat_lon(street_names)

data.frame(street_names, purrr::flatten_chr(vector_out)) %>%
  dplyr::mutate(., vector_out = stringr::str_remove_all(vector_out, "\\(|\\)")) %>%
  tidyr::separate(., vector_out, into = c("latitude", "longitude"), sep = ",")

url <- "https://www.canadapost.ca/cpo/mc/personal/postalcode/fpc.jsf"
remDr$navigate(url)

address_element <- remDr$findElement(using = 'id', value = 'addressComplete')
address_element$sendKeysToElement(list("413 Seymour Street Vancouver"))

button_element <- remDr$findElement(using = 'id', value = 'searchFpc')
button_element$clickElement()


output <- remDr$findElement(using = "id", value="HeaderAddressLabel")
output <- output$getElementText()
output

unlist(output) %>%
  stringr::str_sub(., start = -7, end = -1)

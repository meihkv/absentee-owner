library(rvest)
library(tidyr)

pages = sprintf("https://www.zillow.com/litchfield-park-az/sold/%d_p/",1:20)

page <- read_html("https://www.zillow.com/litchfield-park-az/sold/house_type/?searchQueryState=%7B%22pagination%22%3A%7B%7D%2C%22usersSearchTerm%22%3A%22Litchfield%20Park%2C%20AZ%22%2C%22mapBounds%22%3A%7B%22west%22%3A-112.58214517041014%2C%22east%22%3A-112.29032082958983%2C%22south%22%3A33.42040618291239%2C%22north%22%3A33.60789660940369%7D%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A32525%2C%22regionType%22%3A6%7D%5D%2C%22isMapVisible%22%3Atrue%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22auc%22%3A%7B%22value%22%3Afalse%7D%2C%22pmf%22%3A%7B%22value%22%3Afalse%7D%2C%22pf%22%3A%7B%22value%22%3Afalse%7D%2C%22ah%22%3A%7B%22value%22%3Atrue%7D%2C%22con%22%3A%7B%22value%22%3Afalse%7D%2C%22mf%22%3A%7B%22value%22%3Afalse%7D%2C%22manu%22%3A%7B%22value%22%3Afalse%7D%2C%22land%22%3A%7B%22value%22%3Afalse%7D%2C%22tow%22%3A%7B%22value%22%3Afalse%7D%2C%22apa%22%3A%7B%22value%22%3Afalse%7D%2C%22apco%22%3A%7B%22value%22%3Afalse%7D%2C%22fsbo%22%3A%7B%22value%22%3Afalse%7D%2C%22cmsn%22%3A%7B%22value%22%3Afalse%7D%2C%22fore%22%3A%7B%22value%22%3Afalse%7D%2C%22rs%22%3A%7B%22value%22%3Atrue%7D%2C%22fsba%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22mapZoom%22%3A12%7D")

houses <- page %>%
  html_elements(".photo-cards")

z_id <- houses %>% html_attr("id")

address <- houses %>%
  html_elements(".list-card-addr") %>%
  html_text()

price <- houses %>%
  html_elements(".list-card-price") %>%
  html_text() %>%
  readr::parse_number()

params <- houses %>%
  html_elements(".list-card-info") %>%
  html_text() %>%
  strsplit("\u00b7")

beds <- params %>% purrr::map_chr(1) %>% readr::parse_number()
baths <- params %>% purrr::map_chr(2) %>% readr::parse_number()
house_area <- params %>% purrr::map_chr(3) %>% readr::parse_number()

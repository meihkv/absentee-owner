get_parcel_type = function(parcel_number){
  library(tidyverse)
  library(rvest)
  library(stringr)
  library(async)
  library(reshape)
  
  async_get <- async(function(url) {
    result = http_get(url)$
      then(function(x) { rawToChar(x$content)})$
      then(function(x) { read_html(x, user_agent = user_agent_str) } )$
      then(function(x) { html_elements(x, '.h5')})$
      then(function(x) { html_text2(x) })
    print(url)
    return(result)
  })
  
  url = sprintf('https://preview.mcassessor.maricopa.gov/mcs/?q=%s', str_replace_all(unique(parcel_number), '-', ''))
  user_agent_str = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:73.0) Gecko/20100101 Firefox/73.0"
  
  bodies = synchronise(async_map(url,async_get))
  parcel_list = as.data.frame(do.call(rbind, bodies))
  names(parcel_list) = c('Parcel.Number', 'Parcel.Type')
  
  return(parcel_list)
}

# get_parcel_type(c("102-17-421", "102-82-048", "102-85-600", "102-85-682"))

get_owner_names = function(Property.Street.Number, 
                           Property.Street.Direction,
                           Property.Street.Name,
                           Property.Street.Type,
                           Property.City,
                           Property.Zip.Code){
  library(tidyverse)
  library(rvest)
  library(stringr)
  library(async)
  library(reshape)

  url = sprintf("https://www.whitepages.com/address/%s-%s-%s-%s/%s-%s/",
          Property.Street.Number,
          Property.Street.Direction,
          str_replace_all(trimws(Property.Street.Name), '\\s+', '-'),
          Property.Street.Type,
          Property.City,
          Property.Zip.Code)

  user_agent_str = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:73.0) Gecko/20100101 Firefox/73.0"
  async_get <- async(function(url) {
    result = http_get(url)$
      then(function(x) { rawToChar(x$content) } )$
      then(function(x) { read_html(x, user_agent = user_agent_str) } )$
      then(function(x) { html_elements(x, '.resident-card') } )$
      then(function(x) { html_elements(x, '.d-flex') } )$
      then(function(x) { html_element(x, 'div') } )$
      then(function(x) { html_text2(x) } )
    print(url)
    return(result)
  })
  bodies = synchronise(async_map(url,async_get))
}

# data[1:3,] %>% mutate(ok = get_owner_names(Property.Street.Number,
#                                            Property.Street.Direction,
#                                            Property.Street.Name,
#                                            Property.Street.Type,
#                                            Property.City,
#                                            Property.Zip.Code))

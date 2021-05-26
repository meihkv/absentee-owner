library(tidyverse)
library(parallel)
library(rvest)
library(stringr)
library(async)
library(reshape)
url = sprintf('https://preview.mcassessor.maricopa.gov/mcs/?q=%s', str_replace_all(unique(data$Parcel.Number), '-', ''))
url = "https://preview.mcassessor.maricopa.gov/mcs/?q=10290796"
get_parcels = function(url){
  parcel = read_html(url) %>% 
    html_elements('.h5') %>%
    html_text2()
  return(parcel[2])
}

cl = makeCluster(detectCores())
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(rvest))
clusterExport(cl, varlist = c('url'))

parcels = parSapply(cl, url[1:4], get_parcels)



async_get <- async(function(url) {
  result = http_get(url)$
    then(function(x) { rawToChar(x$content)})$
    then(function(x) { read_html(x) } )$
    then(function(x) { html_elements(x, '.h5')})$
    then(function(x) { html_text2(x) })
  print(url)
  return(result)
})

bodies = synchronise(async_map(url,async_get))
parcel_list = as.data.frame(do.call(rbind, bodies))
names(parcel_list) = c('Parcel.Number', 'Parcel.Type')

write.csv(parcel_list, 'parcel_list.csv', row.names = FALSE)

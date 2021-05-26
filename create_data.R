library(tidyverse)
library(reshape)
library(stringr)
library(parallel)
library(RecordLinkage)

source('utils.r')

data = read.csv('./data/Parcels.csv')

data = data %>% 
  filter(!is.na(Property.Address) & trimws(Property.Address) != '') %>% 
  filter(Property.Address != 'N.A.')

owners = data %>% mutate(owners = get_owner_names(Property.Street.Number,
                                           Property.Street.Direction,
                                           Property.Street.Name,
                                           Property.Street.Type,
                                           Property.City,
                                           Property.Zip.Code)) %>% 
  pull(owners)

# owners[sapply(owners, length) == 0] = NULL

owners_melt = sapply(owners, function (x) {paste0(unlist(x), collapse = ' | ')})

owners_melt = as.data.frame(owners_melt) %>% 
  mutate(id = row_number()) %>% 
  separate_rows(owners_melt, sep = '( \\| )') %>% 
  separate(owners_melt, c('name', 'age'), sep = '\\n') %>% 
  mutate(age = as.numeric(str_replace_all(age , pattern = '\\D',replacement = '')),
         Property.Address = data$Property.Address[id])

# owners_melt = melt(owners) %>%
#   separate(value, c('name', 'age'),sep = '\\n') %>%
#   mutate(age = as.numeric(str_replace_all(age , pattern = '\\D',replacement = '')),
#          Property.Address = data$Property.Address[L1]) %>% 
#   select(-L1)

write.csv(owners_melt, sprintf('data/owners_%s.csv', str_replace_all(Sys.time(), '[\\s\\:]', '-')), row.names = FALSE)

data = data %>%
  inner_join(owners_melt, by = 'Property.Address')

write.csv(data, sprintf('data/data_%s.csv', str_replace_all(Sys.time(), '[\\s\\:]', '-')), row.names = FALSE)

parcel_list = get_parcel_type(unique(data$Parcel.Number))

write.csv(parcel_list, sprintf('parcel_list%s.csv', 
          str_replace_all(Sys.time(), '[\\s\\:]', '-')), 
          row.names = FALSE)

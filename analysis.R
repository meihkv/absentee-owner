library(tidyverse)
library(reshape)
library(RecordLinkage)
library(stringr)
library(stats)
library(scales)
library(plotly)
library(ggplot2)

data = read.csv(file.choose()) %>% 
  mutate(mailing_compare = 
           trimws(paste(str_replace_all(Mailing.Address1, regex('\\s+UNIT', ignore_case = TRUE), ''),
                        Mailing.Address.City,
                        str_replace(Mailing.Address.Zip.Code, '-(.*)', '')
                        ))
         ) %>% 
  mutate(address_probs = levenshteinSim(mailing_compare, Property.Address))

parcel_list = read.csv(file.choose())

split = melt(str_split(trimws(data$Owner.Name),
                       pattern = '[\\s+\\\\/]'))

split2 = melt(str_split(trimws(data$name),
                        pattern = '[\\s+\\\\/]'))

join_split = split %>% 
  inner_join(split2, by = 'L1') %>% 
  dplyr::rename(Owner.Name.parsed = value.x) %>%
  dplyr::rename(name.parsed = value.y) %>% 
  mutate(Owner.Name = data$Owner.Name[L1],
         name = data$name[L1],
         Property.Address = data$Property.Address[L1],
         Mailing.Address = data$Mailing.Address[L1],
         Parcel.Number = data$Parcel.Number[L1]) %>% 
  mutate(name_probs = levenshteinSim(Owner.Name.parsed, toupper(name.parsed))) %>% 
  group_by(Parcel.Number) %>%
  mutate(rank = rank(name_probs, ties.method = 'first')) %>% 
  group_by(Parcel.Number) %>%
  filter(rank >= max(rank)-1) %>%
  arrange(Parcel.Number, -rank )%>% 
  summarise(name_probs = mean(name_probs)) %>% 
  select(Parcel.Number, name_probs)

join_data = data %>% inner_join(join_split, by = 'Parcel.Number') %>%
  inner_join(parcel_list, by = 'Parcel.Number') %>% 
  select(-name) %>% 
  mutate(scaled_address_probs = rescale(address_probs),
         scaled_name_probs = rescale(name_probs)) %>% 
  mutate(name_address_probs = rescale(name_probs*address_probs)) %>% 
  distinct()

run_model = function(clusters){
  set.seed(4)
  model = kmeans(as.matrix(join_data[c('scaled_address_probs', 'scaled_name_probs')]),
         centers = clusters)
  model$tot.withinss
}

k_means_results = unlist(lapply(1:10, run_model))

ggplot(data.frame(k_means_results,k = 1:10)) + geom_line(aes(x= k, y= k_means_results), color = 'orange', size = 1.5) +
  geom_point(aes(x= k, y= k_means_results), color = 'orange2', size = 4) +
  labs(x = 'Number of clusters', y = 'Sum of Squared Distance') +
  theme_minimal() +
  scale_x_continuous(breaks = 1:10)

set.seed(4)
cluster_data = kmeans(as.matrix(join_data[c('scaled_address_probs', 'scaled_name_probs')]),
       centers = 4)

clusters = as.factor(cluster_data$cluster)

test = join_data %>% 
  mutate(clusters = clusters) %>% 
  mutate(owner_occup = if_else(clusters == 1 | Parcel.Type == 'Rental Parcel', 
                               FALSE, 
                               TRUE))

to_plot = ggplot(test, aes(label = Owner.Name,
                           address = Mailing.Address, 
                           address1 = Property.Address,
                           type = Parcel.Type,
                           parcel = Parcel.Number)) + 
  geom_point(aes(x = address_probs, y = name_probs, color = clusters))

ggplotly(to_plot)

write.csv(test, 'data/absentee-owner.csv',row.names = FALSE)

write.csv(test %>%
            select(owner_occup,
                   Owner.Name,
                   In.Care.Of,
                   Parcel.Number,
                  Parcel.Type,
                  Property.Address, 
                  Mailing.Address,
                  Floor,
                  Shape_Area,
                  Deed.Date,
                  Deed.Number,
                  Current.Full.Cash.Value,
                  address_probs,
                  name_probs), 
          'data/absentee-owner_maps.csv', row.names = FALSE)

#create test df
rimini_geocoded <- rimini

address <- base::sample(x = rimini_geocoded$indirizzo_2019, 
                               size = 1, 
                               replace = TRUE)

#register google key
ggmap::register_google(key = key)

#extract geocoded addresses with ggmap (test)
ggmap::geocode(location = address)

geocoded <- purrr::map_df(.x = rimini_geocoded$indirizzo_2019, .f = ggmap::geocode)

#add longitude and latitude data to df
rimini_geocoded$lng <- geocoded$lon
rimini_geocoded$lat <- geocoded$lat

write.csv(rimini_geocoded, "rimini_geocoded.csv", row.names = F)

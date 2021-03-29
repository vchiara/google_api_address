google_address_rn <- function(x, location){

  #search businesses by name
  search <- google_places(search_string = x, 
                          location = location,
                          radius = 30291,
                          key = key,
                          language = "it",
                          simplify =  TRUE)
  
  results <- search$results
  
  if(is.list(results) & length(results) == 0){
    final <- data.frame("nome_struttura_2020" = NA,
                        "longitudine_2020" = NA,
                        "latitudine_2020" = NA,
                        "indirizzo_2020" = NA, 
                        "business_status" = NA,
                        "types" = NA, 
                        "rating" = NA, 
                        "place_id" = NA, 
                        "user_ratings_total" = NA,
                        "Pref._2020" = NA, 
                        "Telefono_2020" = NA, 
                        "sito_2020" = NA)
    return(final)
  } else{ 
    json <- toJSON(results)
    
    flat_df <- fromJSON(json, flatten = T)
    
    if("business_status" %in% colnames(flat_df)){
      address <- flat_df %>% 
        mutate(types = sapply(types, toString)) %>%
        select(name, business_status, formatted_address, 
               geometry.location.lat, geometry.location.lng,
               rating, types, place_id, user_ratings_total)
    } else{
      address <- flat_df %>% 
        mutate(types = sapply(types, toString)) %>%
        select(name, formatted_address, 
               geometry.location.lat, geometry.location.lng,
               types, place_id)
      address <- address %>%
        add_column(business_status = NA, .after = "name") %>%
        add_column(rating = NA) %>%
        add_column(user_ratings_total = NA)
    }
    
    #select results in the province of Rimini
    str <- "RN, Italia"
    
    address <- address %>% 
      filter(str_detect(formatted_address, str))
    
    if(nrow(address) == 0){
      address <- address %>% 
        add_row()
    } else {
      if(nrow(address) == 1){
        address <- address
      } else {
        if(address$formatted_address[1] == address$formatted_address[2]){
          address <- address[1:2,] %>% 
            slice(which.max(user_ratings_total))
        } else {
          address <- address[1,]
        }
      }
    }
    
    #select variables from search results
    address <- address[c("name", "geometry.location.lng", "geometry.location.lat",
                         "formatted_address", "business_status", "types", "rating", 
                         "place_id", "user_ratings_total")]
    
    address <- address %>% 
      rename(nome_struttura_2020 = name,
             longitudine_2020 = geometry.location.lng,
             latitudine_2020 = geometry.location.lat,
             indirizzo_2020 = formatted_address)
    
    #deal with missing values
    if(is.na(address[1]) == FALSE){
      details <- google_place_details(address$place_id, 
                                      key = key,
                                      simplify = TRUE)
      details <- details$result
    } else {
      details <- data.frame(Pref._2020 = NA, Telefono_2020 = NA, sito_2020 = NA)
      final <- cbind(address, details)
    }
    
    if("formatted_phone_number" %in% names(details) == T && "website" %in% names(details) == T){
      details <- details[c("formatted_phone_number", "website")]
      details <- data.frame(details, stringsAsFactors = FALSE)
      details <- details %>% separate(col = 1, into = c("Pref._2020", "Telefono_2020"), extra = "merge", fill = "left")
      details <- details %>% rename(sito_2020 = website)
      final <- cbind(address, details)
    } else {
      if("formatted_phone_number" %in% names(details) == F && "website" %in% names(details) == F){
        details_temp <- data.frame(Pref._2020 = NA, Telefono_2020 = NA, sito_2020 = NA)
        final <- cbind(address, details_temp)
      } else {
        if("formatted_phone_number" %in% names(details) == F) {
          website <- details$website
          details_temp <- data.frame(Pref._2020 = NA, Telefono_2020 = NA, sito_2020 = website, stringsAsFactors = FALSE)
          final <- cbind(address, details_temp)
        } else {
          if("website" %in% names(details) == F) {
            formatted_phone_number <- details$formatted_phone_number
            details_temp <- data.frame(formatted_phone_number = formatted_phone_number, sito_2020 = NA, stringsAsFactors = FALSE)
            details_temp <- details_temp %>% separate(col = 1, into = c("Pref._2020", "Telefono_2020"), extra = "merge", fill = "left")
            final <- cbind(address, details_temp)
          }
        }
      }
    }
    
    return(final)
  }
}
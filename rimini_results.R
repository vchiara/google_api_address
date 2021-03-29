location_rn <- c(43.922539, 12.422072)

#search business info with google api
rn_temp <- map_dfr(rimini$nome_struttura[1:25],
                   google_address_rn,
                   location = location_rn)

rn_temp1 <- map_dfr(rimini$nome_struttura[26:55],
                   google_address_rn,
                   location = location_rn)

rn_temp1 <- add_column(rn_temp1, n = 26:55, .before = 1)
  
rn_temp2 <- map_dfr(rimini$nome_struttura[56:75],
                    google_address_ra,
                    location = location_rn)

rn_temp2 <- add_column(rn_temp2, n = 56:75, .before = 1)

rn_temp3 <- map_dfr(rimini$nome_struttura[76:100],
                    google_address_ra,
                    location = location_rn)

rn_temp4 <- map_dfr(rimini$nome_struttura[101:150],
                    google_address_ra,
                    location = location_rn)

rn_temp5 <- map_dfr(rimini$nome_struttura[151:175],
                    google_address_ra,
                    location = location_rn)

rn_temp6 <- map_dfr(rimini$nome_struttura[176:198],
                    google_address_rn,
                    location = location_rn)

rn_results <- rbind(rn_temp, rn_temp1, rn_temp2, rn_temp3, rn_temp4,
                    rn_temp5, rn_temp6)

rn_results <- rn_results %>% 
  add_column(idDB = rimini$idDB, .before = 1)

rn_results <- rn_results %>% 
  add_column(nome_temp = str_to_upper(rn_results$nome_struttura_2020), 
             .before = 3)

#check if the business name is the same in the google api results and the original dataset
match_name <- map2_lgl(rn_results$nome_temp,
                       rimini$nome_struttura, is_match)

rn_results <- rn_results %>% 
  add_column(match_name = match_name, .before = 2)

#check if the business address is the same in the google api results and the original dataset
match_address <- map2_lgl(rn_results$indirizzo_2020,
                          rimini$indirizzo_2019, is_match)

rn_results <- rn_results %>% 
  add_column(match_address = match_address, .before = 8)

#extract postal code from full address
cap_temp <- rn_results$indirizzo_2020 %>% 
  word(-2, sep = fixed(',')) %>% 
  str_trim()

cap <- cap_temp %>% 
  word(1)

#extract municipality name from full address
comune <- cap_temp %>%
  str_remove_all("\\d") %>% 
  word(sep = "RN") %>%
  str_trim()

rn_results <- rn_results %>% 
  add_column(comune = comune, .after = "match_address") %>%
  add_column(cap = cap, .after = "comune")

rn_results$nome_temp <- NULL

write.csv(rn_results, "rn_results.csv", row.names = F)
setwd("~/R/apt")
library(tidyverse)
library(googleway)
library(ggmap)
library(jsonlite)
library(stringr)
library(purrr)
library(stringdist)

#read original dataset
apt_data <- read_csv("apt_data.csv")

apt_data <- apt_data %>% 
  rename(nome_struttura = `Nome Struttura`,
  indirizzo_2019 = `Indirizzo 2019`)

#google api key
key <- Sys.getenv("GOOGLE_KEY")

#split df
unique(apt_data$Provincia_2019)

apt_data$Provincia_2019[apt_data$Provincia_2019=="Rn"]  <- "RN"

ferrara <- apt_data %>% 
  filter(Provincia_2019 == "FE")

forli_cesena <- apt_data %>% 
  filter(Provincia_2019 == "FC")

ravenna <- apt_data %>% 
  filter(Provincia_2019 == "RA")

rimini <- apt_data %>% 
  filter(Provincia_2019 == "RN")
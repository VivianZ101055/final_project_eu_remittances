library(shiny)
library(shinythemes)
library(plotly)
library(readxl)
library(janitor)
library(rvest)
library(magrittr)
library(base)
library(reshape2)
library(leaflet)
library(maps)
library(tidyverse)

#---------------------------------------------------------------------
#----------------------EU Facts Scraped from Online-------------------

eumemberinfo <- read_html('https://en.wikipedia.org/wiki/Member_state_of_the_European_Union', skip = 0) %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[5]') %>%
  html_table()

eumemberinfo <- eumemberinfo[[1]]

# Removing Wikipedia annotations

eumemberinfo$Name <- str_remove_all(eumemberinfo$Name, "\\[.*\\]")

# Select only the first four numbers to get the accession year

eumemberinfo$Accession <- substr(eumemberinfo$Accession, 0, 4)

# Changing the names because Eastern European countries are labeled
# differently on the two datasets

eumemberinfo$Name[eumemberinfo$Name == "Czechia"] <- "Czech Republic"

eumemberinfo$Name[eumemberinfo$Name == "Slovakia"] <- "Slovak Republic"

name <- eumemberinfo %>%
  select(Name)

names(name) <- c("sender")

#-------------------------------------------------------------------
#----------------------Remittances in USD---------------------------

# Load remittances in USD in a given country, in alphabetical order

usd <- read_excel("shiny/data/remittances_usd.xls", skip = 2)

# Trying to get rid of the x in front of the data
# Filter to only show between 1980 to present
# Repeatedly getting the error that argument "pattern" is missing, with no
# default
# Convert USD to Euros - maybe later

usd <- usd %>%
  pivot_longer(
    cols = `1980`:`2018`,
    names_to = "year",
    names_prefix = "yr"
  )

usd <- usd[,-c(5:25)]

# join this with the eumemberinfo to only look at member states

full_data <- usd %>%
  inner_join(eumemberinfo, by = c("Country Name" = "Name")) %>%
  rename(remittances_in_usd = value) %>%
  clean_names %>%
  filter(country_name %in% name$sender)

# Load remittances as percentage of GDP for a given country

percent_gdp <- read_excel("shiny/data/remittance_percentgdp.xls", skip = 2)

percent_gdp <- percent_gdp %>%
  pivot_longer(
    cols = `1980`:`2018`,
    names_to = "year",
    names_prefix = "yr") %>%
  rename(remittances_percent_gdp = value) %>%
  clean_names %>%
  filter(country_name %in% name$sender)

percent_gdp <- percent_gdp[,-c(5:25)]

# Join this with the previous data and select only the relevant columns

full_data <- full_data %>%
  inner_join(percent_gdp, by = c("country_name"="country_name", "year"="year"), suffix = c("_usd", "_gdp")) %>%
  select(country_name, country_code_usd, accession, year, remittances_in_usd, remittances_percent_gdp)

# Setup for ggplot of Remittances in USD

mydata <- full_data %>%
  na.omit %>%
  group_by(year) %>%
  summarize(sum_remittance_usd = sum(remittances_in_usd)) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year)))


#global.R

#paketid
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(zoo)
library(plotly)
library(shinycssloaders)


#andmed
load("andmed/andmed_filter.RData")
load("andmed/andmed_emta.RData") 

andmed_emta <- andmed_emta %>%
  filter(registrikood %in% andmed_filter$registrikood)

maakonnad <- unique(andmed_filter$maakond) %>% sort()
ettevotted <- andmed_filter %>% filter(aasta == 2020) %>% arrange(-kaive) %>% pull(nimi)
  
#Kümne aste sõnaga
kymne_aste = function(tunnus){
  max_vaartus = max(abs(tunnus[abs(tunnus)<Inf]), na.rm = TRUE)
  if (max_vaartus > 1e+09) {
    div = 1e+09
    label = "(miljardites)"
  } else { 
    if (max_vaartus > 1e+06) {
      div = 1e+06
      label = "(miljonites)"
    } else {
      if (max_vaartus > 1000) {
        div = 1000
        label = "(tuhandetes)"
      } else {
        
        div = 1
        label = ""
      }
    }}
  return(list(div = div, label = label)) 
}
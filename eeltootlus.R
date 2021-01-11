#Mina vs sarnased ettevõtted
#Annegrete Molloka
#21.12.2020

#paketid 
library(tidyverse)
library(zoo)

#Andmed sisse
load("andmed/andmed_emta.RData")
load("andmed/andmed_esa.RData")
load("andmed/emtaknames.Rdata")

missingtozero <- function(x) ifelse(is.na(x), 0, x)
#Võtame sidumise aluseks EMTA andmed
andmed_emta_sidumiseks <- andmed_emta %>% 
  #Äriühingud
  filter(liik == "Äriühing") %>% 
  #puuduvad nulliks
  mutate_at(vars(rmaksud, toomaksud, kaive, tootajad), missingtozero) %>% 
  mutate(maakond = stringr::word(maakond, sep = fixed(" ")))

#Kõige vanema ja uusima kvartali leidmine ettevõtte kohta
abi_emta <- andmed_emta_sidumiseks %>% 
  group_by(registrikood) %>% 
  summarise(min_aastakv = min(aeg),
            max_aastakv = max(aeg))

#Kõikidele ettevõtetele kõik kvartalid ja min-max järgi filtereerimine ja andmete täitmine
andmed_emta_sidumiseks <- andmed_emta_sidumiseks %>% 
  complete(registrikood, aeg, fill = list(rmaksud = 0, toomaksud = 0, kaive = 0, tootajad = 0)) %>% 
  left_join(abi_emta) %>% 
  filter(aeg >= min_aastakv, aeg <= max_aastakv) %>% 
  select(-min_aastakv, -max_aastakv) %>% 
  group_by(registrikood) %>% 
  fill(nimi, liik, KMK, maakond, EMTAK) %>% 
  ungroup()

andmed_esa <- andmed_esa %>% 
  select(registrikood, emtak4, emtak_taht) %>% 
  mutate(emtak2 = substr(emtak4, 1, 2)) %>% 
  left_join(emtaknames %>% mutate(emtaktahttekst = as.factor(nrtekst)) %>% select(emtak, emtaktahttekst), 
            by = c("emtak_taht" = "emtak")) %>% 
  left_join(emtaknames %>% mutate(emtak2tekst = as.factor(nrtekst)) %>% select(emtak, emtak2tekst), 
            by = c("emtak2" = "emtak")) 

#Seome juurde ESA andmetest EMTAKi, see on viimane seis
andmed_seotud <- andmed_emta_sidumiseks %>% left_join(andmed_esa, by = "registrikood")
  

andmed_uus <- andmed_seotud %>% 
  group_by(registrikood, aasta = year(aeg)) %>% 
  summarise(kaive = sum(kaive),
            tootajad  = mean(tootajad)) %>% 
  ungroup()

andmed_uus <- andmed_seotud %>% 
  select(registrikood, nimi, maakond, EMTAK, emtaktahttekst, emtak2tekst) %>% 
  group_by(registrikood) %>% 
  slice(1) %>% 
  ungroup() %>% 
  right_join(andmed_uus, by = "registrikood")

andmed_filter <- andmed_uus %>% 
  mutate(emtaktahttekst = ifelse(!is.na(emtaktahttekst), as.character(emtaktahttekst), 
                              case_when(EMTAK == "KINNISVARAALANE TEGEVUS" ~ "L - Kinnisvaraalane tegevus",
                                        EMTAK == "HULGI- JA JAEKAUBANDUS; MOOTORSõIDUKITE JA MOOTORRATASTE REMONT" ~ "G - Hulgi- ja jaekaubandus; mootorsõidukite ja mootorrataste remont",
                                        EMTAK == "INFO JA SIDE" ~ "J - Info ja side",
                                        EMTAK == "PÕLLUMAJANDUS, METSAMAJANDUS JA KALAPÜÜK" ~ "A - Põllumajandus, metsamajandus ja kalapüük",
                                        EMTAK == "TÖÖTLEV TÖÖSTUS" ~ "C - Töötlev tööstus",
                                        EMTAK == "KUTSE-, TEADUS- JA TEHNIKAALANE TEGEVUS" ~ "M - Kutse-, teadus- ja tehnikaalane tegevus",
                                        EMTAK == "EHITUS" ~ "F - Ehitus",
                                        EMTAK == "HALDUS- JA ABITEGEVUSED" ~ "N - Haldus- ja abitegevused",
                                        EMTAK == "VEONDUS JA LAONDUS" ~ "H - Veondus ja laondus",
                                        EMTAK == "VEEVARUSTUS; KANALISATSIOON; JÄÄTME- JA SAASTEKÄITLUS" ~ "E - Veevarustus; kanalisatsioon, jäätme- ja saastekäitlu",
                                        EMTAK == "MAJUTUS JA TOITLUSTUS" ~ "I - Majutus ja toitlustus",
                                        EMTAK == "TERVISHOID JA SOTSIAALHOOLEKANNE" ~ "Q - Tervishoid ja sotsiaalhoolekanne",
                                        EMTAK == "FINANTS- JA KINDLUSTUSTEGEVUS" ~ "K - Finants- ja kindlustustegevus",
                                        EMTAK == "KUNST, MEELELAHUTUS JA VABA AEG" ~ "R - Kunst, meelelahutus ja vaba aeg",
                                        EMTAK == "MUUD TEENINDAVAD TEGEVUSED" ~ "S - Muud teenindavad tegevused",
                                        EMTAK == "MÄETÖÖSTUS" ~ "B - Mäetööstus",
                                        EMTAK == "HARIDUS" ~ "P - Haridus",
                                        EMTAK == "ELEKTRIENERGIA, GAASI, AURU JA KONDITSIONEERITUD ÕHUGA VARUSTAMINE" ~ "D - Elektrienergia, gaasi, auru ja konditsioneeritud õhuga varustamine",
                                        EMTAK == "AVALIK HALDUS JA RIIGIKAITSE; KOHUSTUSLIK SOTSIAALKINDLUSTU" ~ "O - Avalik haldus ja riigikaitse; kohustuslik sotsiaalkindlustu"
                                        )),
         maakond = ifelse(maakond == "", NA, maakond)) %>% 
  select(-EMTAK)

save(andmed_filter, file = "andmed/andmed_filter.RData")

# #Andmed_emta uued tunnused
# andmed_emta <- andmed_emta %>% 
#   mutate(
#     aeg = as.yearqtr(paste(aasta, kvartal, sep = "-")),
#     rmaksud_kaive = round(rmaksud/kaive*100, 1),
#     tmaksud_kaive = round(toomaksud/kaive*100, 1),
#     kaive_tootaja = round(kaive/tootajad, 1),
#     tmaksud_tootaja = round(toomaksud/tootajad, 1),
#     aadress = maakond, 
#     maakond = stringr::word(maakond, sep = fixed(" "))
#   ) %>% 
#   arrange(desc(aeg), desc(kaive)) %>% 
#   select(-aasta, -kvartal)
# 
# save(andmed_emta, file = "andmed/andmed_emta.RData")  

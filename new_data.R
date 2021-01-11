##Data Processing
#Annegrete Peek

# Paketid ----------------------------------------------------------------------
library(dplyr)
library(readxl)
library(zoo)
library(lubridate)
library(httr)
library(stringr)

# Parameetrid ------------------------------------------------------------------
kuu <- month(Sys.Date())
#kuu <- 10
kvartal <- case_when(kuu == 1 ~ "iv",
                     kuu == 4 ~ "i",
                     kuu == 7 ~ "ii",
                     kuu == 10 ~ "iii"
)
kvartal2 <- case_when(kuu == 1 ~ 4,
                     kuu == 4 ~ 1,
                     kuu == 7 ~ 2,
                     kuu == 10 ~ 3
)
aasta <- year(Sys.Date())
#aasta <- 2020
aasta <- ifelse(kuu == 1, aasta - 1, aasta)
url1 <- paste("https://www.emta.ee/sites/default/files/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud/tasutud_maksud",
             aasta, kvartal, "kvartal.xlsx", 
             sep = "_")

missingtozero <- function(x) ifelse(is.na(x), 0, x)
# Andmete sisselugemine --------------------------------------------------------

GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
dt1 <- read_excel(tf)


# Andmete töötlemine -----------------------------------------------------------
load("C:/Users/annegrete.peek/Documents/EMTA/MISA/andmed/andmed_emta.RData")  

names(dt1) <- c("registrikood", "nimi", "liik", "KMK", "EMTAK", "aadress", "rmaksud", "toomaksud", "kaive", "tootajad")

dt1 <- dt1 %>%
  mutate(
    aeg = as.yearqtr(paste(aasta, kvartal2, sep = "-")),
    rmaksud_kaive = round(rmaksud/kaive*100, 1),
    tmaksud_kaive = round(toomaksud/kaive*100, 1),
    kaive_tootaja = round(kaive/tootajad, 1),
    tmaksud_tootaja = round(toomaksud/tootajad, 1),
    maakond = stringr::word(aadress, sep = fixed(" "))
  ) %>%
  mutate_at(vars(rmaksud, toomaksud, kaive, tootajad), missingtozero) %>% 
  arrange(desc(aeg), desc(kaive)) %>%
  select(names(andmed_emta)) 

andmed_emta <- dt1 %>% 
  rbind(andmed_emta)

save(andmed_emta, file = "C:/Users/annegrete.peek/Documents/EMTA/MISA/andmed/andmed_emta.RData")  

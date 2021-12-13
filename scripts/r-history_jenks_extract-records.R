library(pdftools)
library(purrr)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(lubridate)
library(ggplot2)
library(writexl)

#setwd("~/r-history_nov21") # Denne linje ændrer hvor R leder efter filer (ændre med CTRL+SHIFT+H)

# Indlæs enkeltfil (antager at fil ligger i mappe "data")
pdf_load <- pdf_text("./data/jenks_lca_iii_1.pdf")

## pdf_load er nu en vector med et indeks for hver side. Nedenstående printer side 10:
pdf_load[10]

# Indlæs flere filer
data_files <- list.files("./data") # Finder filer i mappen "data" (antager at denne mappe findes)
data_files <- file.path(".", "data", data_files) # Komplette filstier for hver fil i mappen "data"

pdfs_load <- map(data_files, pdf_text) # Indlæser alle pdf filer

## TEXT MINING EKSEMPEL
### I nedenstående laves et simpelt text mining eksempel med introteksten
# Undersøg intro-tekst
intro_pages <- pdf_load[7:48] # Introsiderne er fra side 7-48

intro_text <- paste(intro_pages, collapse = "\n") # Sætter introsiderne sammen til en tekst/string
nchar(intro_text) # Antal karakterer i introtekst

edward_search <- str_count(intro_text, regex("Edward IV", ignore_case = TRUE)) # Optælling af hvor mange gange "Edward IV nævnes"

### ORDFREKVENSER MED TIDY TEKST
pdf_df <- data.frame(text = intro_pages) # Dataframe ud fra introsider (en række per side)
tokens_df <- unnest_tokens(pdf_df, word, text) # Tidy text: En række per gang ord optræder

stop_list <- c("it", "is") # Liste af ord, som  skal frasorteres (stopord) - kan evt. udvides
stop_words_df <- data.frame(word = stop_list) # Stopord som data frame

tokens_df <- anti_join(tokens_df, stop_words_df, by = "word") # Stopord frasorteres med anti-join

# Word counts
## Nedenstående optæller ord ved at gruppere på ord og optælle antal gange ordet opræder
counts_df <- tokens_df %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) # arrangeres efter højeste antal først

## Visualisering af top 20 ord som søjlediagram
counts_df %>% 
  slice_max(count, n = 20) %>%
  ggplot(aes(x = fct_reorder(word, count), y = count)) + 
  geom_col()


## MINING CUSTOMS INFO
### I det følgende forsøges at skabe et struktureret datasæt ud fra "Petty Customs" sektionen
petty_customs <- pdf_load[49:102] # Petty customs er på side 49-102 i denne pdf
petty_customs <- paste(petty_customs, collapse = "\n") # Sættes sammen til én tekst/string
nchar(petty_customs) # Antal karakterer

## Optælling ud fra rå tekst
str_count(petty_customs, regex("\\ballecis\\b", ignore_case = TRUE)) # Optæl ål (\\b markerer ordafgrænsning)


## Records info som data frame
### I det følgende udledes enkelte oplysninger med forskellige regular expressions

## Records
### Først gøres records til dataframe - en række per record
record_pattern <- "\\[\\d{1,3}\\].*?(?=\\[\\d{1,3}\\])" # Regex mønster for record
records <- str_extract_all(petty_customs, regex(record_pattern,  # Find records i tekst
                                                multiline = TRUE, 
                                                dotall = TRUE))
records <- unlist(records) # Gør til vector (frem for liste)

records_df <- data.frame(records = records) %>% # Omdan til data frame
  mutate(record_no = seq_along(1:nrow(.))) # Danner kolonne for record nummer


## Udled datoer
date_pattern <- "\\[\\d{1,2} \\w{3,4}\\.? \\d{4}\\]" # Regex mønster

records_df <- records_df %>%
  mutate(date = str_extract(str_squish(records), date_pattern), # Tilføj kolonne for dato
         date = str_replace_all(date, "\\[|\\]" ,""), # Fjern kantede paranteser fra dato
         date = dmy(date)) # Konverter til datoformat


## Kaptajner
captain_pattern <- "(?<=(navi )|(caraka )|(galea ))[A-Z]\\w+( \\b\\w+?){0,2} \\b[A-Z]\\w+\\b" # Regex mønster

records_df <- records_df %>%
  mutate(captain = str_extract(str_squish(records), captain_pattern)) # Tilføj kolonne for kaptajn

records_df %>% # Opfælling af kaptajner (top 20)
  group_by(captain) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)


## Customs/fortoldninger info som dataframe
### I nedenstående dannes data frame over fortoldninger ud fra records dataframe
customs_pattern <- "(?<=\\n)De.*?Precii.*?\\r\\n" # Regex mønster

customs_df <- records_df %>%
  mutate(customs = str_extract_all(records, regex(customs_pattern, # Kolonne for fortoldninger
                                                  multiline = TRUE, # Fortæller at der skal søges over flere linjer
                                                  dotall= TRUE))) %>% # Fortæller at . skal matche nye linjer o.l.
  unnest(customs) # Dan række per cusom


## Fortoldningsværdi
value_pattern <- "(?<=\\.{5}).+(?=\\r\\n)" # Regex mønster for værdi
customs_df <- customs_df %>%
  mutate(value = str_extract(customs, regex(value_pattern)), # Kolonne for værdi
         value = str_replace_all(value, "\\.", "")) %>% # Fjern punktummer
  mutate(pounds = str_extract(value, "(?<=£)\\d{1,5}"), # Kolonne for pund
         pounds = as.numeric(pounds), # Gøres numerisk
         shilling = str_extract(value, "\\d{1,2}(?=s)"), # Kolonne for shilling
         shilling = as.numeric(shilling), # Gøres numerisk
         pence = str_extract(value, "\\d{1,2}(?=d)"), # Kolonne for pence
         pence = as.numeric(pence)) %>% # Gøres numerisk
  replace_na(list(pounds = 0, # Missingværdier i pounds, shilling, pence erstattes med 0
                  shilling = 0,
                  pence = 0)) %>%
  mutate(total = pounds + shilling/96 + (pence/12)/96) # Kolonne for total (i pund)

## Varer
### Nedenstående danner dataframe hvor hver vare er splittet ud på række (kræver mere finpudsning)
## Customs split
goods_df <- customs_df %>%
  mutate(goods = str_split(customs, ",")) %>%
  unnest(goods) %>%
  mutate(goods = str_replace(goods, 
                             "Precii.*", 
                             ""))

## UDFORSKNING AF DATA  

# Højeste fortoldninger (top 20)
top_customs <- customs_df %>%
  arrange(desc(total)) %>%
  select(record_no, date, captain, customs, value, total) %>%
  head(20)


# Fitted linje - total fortoldningsværdi over tid (giver måske ikke så meget mening)
ggplot(data = customs_df, aes(x = date, y = total)) +
  geom_smooth()

# Grupperet datasæt over tid
## Nedenstående summerer fortoldningværdi per dato
customs_grouped_date <- customs_df %>%
  group_by(date) %>%
  summarize(totalpounds = round(sum(total, na.rm = TRUE), 
                                digits = 2))

# Linjeplot over fortoldning over tid
ggplot(data = customs_grouped_date, aes(x = date, y = totalpounds)) +
  geom_line() + 
  scale_x_date(date_breaks = "5 day") + # Ændrer x-akse mærker
  theme(axis.text.x = element_text(angle = 90)) # Roterer x-akse mærker med 90 grader

# Samme som ovenstående, bare filteret:
customs_df %>%
  group_by(date) %>%
  summarize(totalpounds = round(sum(total, na.rm = TRUE), 
                                digits = 2)) %>%
  filter(totalpounds < 2500,
         totalpounds > 10) %>%
  ggplot(aes(x = date, y = totalpounds)) +
  geom_line() + 
  scale_x_date(date_breaks = "5 day") +
  theme(axis.text.x = element_text(angle = 90))


# Grupperet datasæt på records
## Nedenstående summer fortoldningsværdi per dato per record og laver det som punktplot
## Værdilabels sættes på de records, hvor fortoldning overstiger 100 pund
customs_df %>%
  group_by(record_no, date) %>%
  summarize(totalpounds = round(sum(total, na.rm = TRUE), 
                                digits = 2)) %>%
  filter(totalpounds > 10) %>%
  ggplot(aes(x = date, y = totalpounds, label = record_no)) +
  geom_point(aes(colour = record_no)) +
  geom_text(aes(label = ifelse(totalpounds > 100, as.character(record_no), "")), nudge_y = 100)


# Grupperet datasæt på datoer - antal skibe per dag
records_datecount <- records_df %>%
  group_by(date) %>%
  summarize(count = n())

## Barplot over antal skibe per dag  
ggplot(data = records_datecount, aes(x = date, y = count)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_date(date_breaks = "5 day")

## Zoom ind på 1461 
records_datecount %>%
  filter(date < "1462-01-01") %>%
  ggplot(aes(x = date, y = count)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_date(date_breaks = "5 day") + 
  geom_text(aes(label = count), nudge_y = 0.2)


## UDFORSKNING AF VARER
### Nedenstående søger på specifikke varer

## Eels
eels_df <- goods_df %>%
  filter(str_detect(str_to_lower(goods), "anguil"))

## Herring
herring_df <- goods_df %>%
  filter(str_detect(str_squish(str_to_lower(goods)), "allecis albi"))

### Sild over tid
herring_df %>%
  group_by(date) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = date, y = count)) + 
  geom_line()

## Goods summary (kræver mere finpudsning)
goods_df  %>%
  mutate(goods = str_squish(str_extract(goods, "(?<=\\d ).*")),
         goods = str_replace_all(goods, "\\.", "")) %>%
  group_by(goods) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(25)


## EKSPORT AF DATA
### Nedenstående eksporterer "customs_df" (fortoldninger) som excel fil (med pakken writexl)
## Lidt oprydning
customs_df_exp <- customs_df %>%
  mutate(customs = str_replace(customs, "Precii.*", ""),
         date = as.character(date))
write_xlsx(customs_df_exp, "./output/jenks-III_customs.xlsx") # Placeres i mappe "output" (skal ligge i arbejdssti)

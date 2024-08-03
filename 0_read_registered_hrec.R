# 0_read_registered_hrec.R
# read in the NHMRC-registered HRECs with additional data searched from the web by Adrian
# May 2024
library(tidyr)
library(dplyr)
library(janitor)
library(readxl)
library(stringr)
library(XML)

# downloaded latest PDF file from here: https://www.nhmrc.gov.au/research-policy/ethics/human-research-ethics-committees, converted to Excel
hrecs = read_excel('data/HREC-Human-Research-Ethics-Committees-registered-with-NHMRC-LIST_Feb_2024.xlsx') %>%
  clean_names() %>%
  unite('organisation', c('organisation', 'x3'), na.rm=TRUE) %>% # combine columns
  unite('url_address', c('url_address', 'x4'), na.rm=TRUE) %>% # combine columns
  unite('code', c('code', 'x7'), na.rm=TRUE) %>% # combine columns
  unite('status', c('status', 'x9'), na.rm=TRUE) %>% # combine columns
  mutate(rownum = 1:n() + 2, # row number for potential later merging, plus 2 for headers
         registered = str_detect(status, 'Registered'), # simplify status
         certified = str_detect(status, 'Certified'),
         email = str_squish(email),
         generic_email = str_squish(generic_email),
         chair = str_squish(chair)) %>%
  select(-starts_with('x'))
  
# now remove empty lines
hrecs = filter(hrecs, name != 'Name', !is.na(code), code!='', status!='CLOSED') %>% # remove header rows and two closed committees
  select(-status, -registered) # no longer needed, all are registered
  
# quick checks
nrow(hrecs)
str(sample_n(hrecs,1))
table(nchar(hrecs$code)) # should all be same length
table(table(hrecs$code)) # should all be 1 (unique)

## check for duplicate Chairs
doubles = unique(hrecs$chair[duplicated(hrecs$chair)]) # 1 by name
doubles = unique(hrecs$email[duplicated(hrecs$email)]) # 2 by email
doubles = doubles[!is.na(doubles)] # remove missing
duplicated_chairs = filter(hrecs, email %in% doubles) %>%
  select(code, chair, name, email) %>%
  arrange(chair, code)
duplicated_chairs

# checks of missing contact information
library(rmarkdown)
render(input = "0_check_contact.Rmd",
       output_format = "word_document",
       output_file = "0_check_contact.docx") 

# flag chairs who are doubles
doubles = group_by(hrecs, chair) %>% slice(2) %>% filter(!is.na(chair)) %>% pull(chair)
hrecs = mutate(hrecs, 
               double = chair %in% doubles,
               double = ifelse(is.na(double), FALSE, double)) # replace NA

## add co-chairs
hrecs = mutate(hrecs, 
               co_chair = str_detect(tolower(notes), pattern='co.?chair'),
               co_chair = ifelse(is.na(co_chair), FALSE, co_chair)) # replace missing with false
filter(hrecs,co_chair) %>% pull(chair) # should all have ;

# order rows by code
hrecs = arrange(hrecs, code)

# remove hashtag in name
hrecs = mutate(hrecs, 
               email = str_squish(email),
               generic_email = str_squish(generic_email),
               name = str_remove(name, '^\\# '))

# save
save(hrecs, file = 'data/0_hrecs.RData')

# export numbers for Qualtrics
qualtrics = function(){
for_qualtrics = select(hrecs, code) # keep code for linking
write.csv(for_qualtrics, file = 'emails/for_qualtrics.csv', quote=FALSE, row.names = FALSE)
}
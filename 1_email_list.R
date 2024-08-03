# 1_email_list.R
# used by 1_send_emails.R and 1b_post_letter
# June 2024
library(dplyr)
library(janitor)
library(stringr)
library(readxl)

## get the personal links from Qualtrics
this_file = 'emails/export-EMD_8D7SdGOUfKtNjqI-2024-06-18T08-53-20-636Z.csv'
links = read.csv(file = this_file) %>%
  clean_names() %>%
  select(external_data_reference, link) %>%
  rename('code' = 'external_data_reference')

## add emails to links
load('data/0_hrecs.RData')
links = left_join(hrecs, links, by='code')

## remove responders (file from Qualtrics, Personal links, download history, do just before letter creation date)
to_load = dir('emails', pattern='export-EMD')
setwd('emails')
responders = NULL
for (response_file in to_load){
  this_responders = read.csv(file = response_file) %>%
    clean_names() %>%
    filter(status == 'Survey Finished') %>%
    select(external_data_reference) %>%
    rename('code' = 'external_data_reference')
  responders = bind_rows(responders, this_responders)
}
responders = unique(responders) # safety net for duplicates
setwd('..') # move back to project directory
links = anti_join(links, responders, by='code')

## exclude opted out, linked by code; no need to exclude dead as I tried to find new emails
excluded = read_excel('emails/dead_emails_hrecs.xlsx', sheet = 'opt out') %>%
  mutate(code = str_squish(code))
#
N_before = nrow(links)
links = filter(links, !code%in%excluded$code)
N_after = nrow(links)
if(N_before - N_after != nrow(excluded)){cat('Error actually excluded ', N_before - N_after, ', should have been ', nrow(excluded), sep='')}



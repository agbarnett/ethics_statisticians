# 1b_post_letter.R
# create PDF letter and labels to send to non-responders
# June 2024
library(janitor)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(rmarkdown)
library(readxl)
library(httr) # next two for short url
library(jsonlite)
source('99_functions.R')

# get non-responders and exclude non-contacts
source('1_email_list.R')

# get the updated address data
address = read_excel('data/HREC-Human-Research-Ethics-Committees-registered-with-NHMRC-LIST_Feb_2024.xlsx') %>%
  clean_names() %>%
  select(code, status, name, organisation, postal_address)

# merge with those who have already answered
links = select(links, code, link)
those_to_mail = left_join(links, address, by='code') %>%
  arrange(code) %>%
  filter(status != 'CLOSED', !is.na(status))
  

# make letters
today = as.Date(Sys.Date())
not_sent = NULL
for (k in 1:nrow(those_to_mail)){
  outfile = paste(those_to_mail$code[k], '.pdf', sep = '')
  short_url = ShortURL(those_to_mail$link[k])$shorturl
  if(is.na(those_to_mail$postal_address[k])){ # do not send if there's no address
    not_sent = bind_rows(not_sent, those_to_mail[k,])
    next
  }
  rmarkdown::render(input = "1_make_letter.Rmd",
                    output_format = "pdf_document",
                    output_file = outfile,
                    output_dir = "letters")
}
# should just be 2 in not sent

# save the number sent
n_letters = nrow(those_to_mail) - nrow(not_sent)
save(n_letters, file='data/1_letters.RData')

# export addresses to text file to make sticky labels
label.file = 'letters/mail_labels.txt'
ofile = file(label.file , 'w')
for (k in 1:nrow(those_to_mail)){
  cat(k, '\n', file=ofile)
  name = str_remove_all(those_to_mail$name[k], '\\#|\\n')
  name = str_squish(name)
  organisation = str_remove_all(those_to_mail$organisation[k], '\\#|\\n')
  postal_address = str_remove_all(those_to_mail$postal_address[k], '\\#|\\n')
  postal_address = str_replace_all(postal_address, ';', '\n')
  cat(name,'\n', file=ofile)
  cat(organisation,'\n', file=ofile)
  cat(postal_address,'\n', file=ofile)
  cat('\n\n', file=ofile)
}
close(ofile)


# 1_send_emails.R
# send emails to potential participants
# use links generated in Qualtrics
# May 2024
library(janitor)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(RDCOMClient) # for sending emails direct from R (installed from github)

## Section 1: get the sample ##
## get the personal links from Qualtrics, links made in 0_read_registered_hrec.R, links generated on 11-May-2024
source('1_email_list.R')
N_sample = nrow(links)

### Section 2: make to and cc fields; chose co chair
TeachingDemos::char2seed('wrexham')
links1 = filter(links, co_chair == TRUE) %>%
  mutate(random = as.numeric(runif(n=n())<0.5)+1) %>% # random number for selecting chair
  separate(col = email, into=c('email1','email2'), sep=';') %>% # split chairs and emails
  separate(col = chair, into=c('chair1','chair2'), sep=';') %>%
  mutate(chair = ifelse(random==1, chair1, chair2), # replace email and chair
         email = ifelse(random==1, email1, email2))
links2 = filter(links, co_chair == FALSE)
links = bind_rows(links1, links2) %>%
  arrange(code) %>%
  mutate(chair = str_squish(chair),
         email = str_squish(email))
links = mutate(links,
               to = case_when(
                 !is.na(chair) & !is.na(email) ~ email, # send to chair if they have a name and an email ...
                 !is.na(chair) & is.na(email) ~ generic_email, # send to generic but use chair's name ...
                 is.na(chair) ~ generic_email #... or generic if we don't know chair
               ),
               cc = case_when(
                 !is.na(email) & !is.na(generic_email) ~ generic_email, # cc generic if known email
                 is.na(email) & !is.na(generic_email) ~ '' # do not double cc
               )
)
links = arrange(links, code) # re-sort by code

## Section 3: create emails ##
# read in the email text as one variable
approach = 2 # which approach number, this controls the email content (1 = first email, 2 = reminder)
email = read.table(paste('emails/email',approach,'.txt',sep=''), sep='!', quote='')
email = email$V1
# extract the subject
subject = str_remove(email[1], '^Subject: ')
email = email[-1] # remove first row (subject)
n_email = length(email)

# make individual emails for each HREC
email.body = list()
double_sentence = 'We believe that you are also chair of another committee and hence you will receive two emails from us.'
generic_chair = 'HREC Chair'
for (k in 1:N_sample){ # loop through participants
  email.body[[k]] = ''
  for (j in 1:n_email){ # loop through email lines
    text = email[j]
    if(links$double[k] == TRUE){text = str_replace(text, '\\[double\\]', double_sentence)} # add double text ...
    if(links$double[k] == FALSE){text = str_remove(text, '\\[double\\]')} # ... or remove place-holder
    if(is.na(links$chair[k]) == FALSE){text = str_replace(text, '\\[name\\]', links$chair[k])} # add name of chair if known ...
    if(is.na(links$chair[k]) == TRUE){text = str_replace(text, '\\[name\\]', generic_chair)} # ... or generic chair name
    text = str_replace(text, '\\[hrec\\]', links$name[k]) # add HREC name
    text = str_replace(text, '\\[code\\]', links$code[k]) # add HREC code
    text = str_replace(text, '\\[this link\\]', paste('<a href="', links$link[k], '">this link</a>', sep='')) # add link 
    email.body[[k]] = paste(email.body[[k]], text, '<br>', sep='')
  }
}

#### Section 4: send emails ###
excluded = NULL
min.send = 21 # used to run in batches
#max.send = 20
max.send = nrow(links)
for (k in min.send:max.send){ 
  if(is.na(links$to[k]) & is.na(links$cc[k])){ # if nothing to send
    excluded = bind_rows(excluded, links[k,])
    next
  }
  # start the app
  OutApp <- COMCreate("Outlook.Application")
  # create an email 
  outMail = OutApp$CreateItem(0)
  outMail[["To"]] = links$to[k] # comment out for testing ...
#  outMail[["To"]] = 'a.barnett@qut.edu.au' # ... for testing
  if(!is.na(links$cc[k]) & links$cc[k]!=''){outMail[["cc"]] = links$cc[k]} # comment out for testing ...
#  outMail[["cc"]] = 'a.barnett@qut.edu.au' # ... for testing
  outMail[["subject"]] = subject
  outMail[["HTMLbody"]] = email.body[[k]]
  # send it                     
  outMail$Send()
  Sys.sleep(2) # short break
}

# save number sent
n_sent = nrow(links) - nrow(excluded)
save(n_sent, excluded, file='data/1_number_sent_first_reminder.RData')

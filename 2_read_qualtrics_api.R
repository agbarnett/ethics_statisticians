# 2_read_qualtrics_api.R
# read the latest survey data from Qualtrics using the API
# July 2024
library(tidyr)
library(qualtRics)
library(dplyr)
library(stringr)
library(janitor)
library(readxl)
#source('99_functions.R')

### Part 1: labels ###
# get the variable names (from downloading Excel file and removing all data)
names = read_excel('data/qualtrics_labels.xlsx', col_names = FALSE, n_max = 1) %>% clean_names()
names = janitor::make_clean_names(names)  
# ... then get the labels
in_labels = read_excel('data/qualtrics_labels.xlsx', col_names = FALSE, skip=1, n_max = 1)
labs = as.character(matrix(in_labels))
# make a frame of labels
labels = bind_cols(names=names, labels=labs) %>%
  filter(!str_detect(names, 'start_date'), # do not need these variables
         !str_detect(names, 'end_date'),
         !str_detect(names, 'response_id'),
         !str_detect(names, 'user_language'),
         !str_detect(names, 'recipient'),
         !str_detect(names, 'policy'),
         !str_detect(names, 'sentiment')) 


### Part 2: data ###
## set up API access
source('0_my_key.R') # do not share

## list all available surveys
surveys = all_surveys() %>%
  filter(str_detect(name, pattern='^Statisticians')) # just the statisticians surveys - all distributions

## now get HREC survey
sdata = fetch_survey(surveyID = surveys$id, 
                          label = TRUE, # use text for surveys
                          add_column_map = FALSE,
                     force_request = TRUE, # must have this
                          verbose = TRUE)
sdata = clean_names(sdata) %>%
  select(-q_data_policy_violations) %>%
  mutate()
# filter just those who completed using the link (exclude any tests)
sdata = filter(sdata, distribution_channel == 'gl') %>% select(-distribution_channel)

## drop random ordering variables (not needed)
sdata = select(sdata, -contains('_do_'))

## removals ##
# remove people with zero progress ... 
cat('There were ', nrow(filter(sdata, progress==0)),' respondents with a zero progress.\n', sep='')
raw = filter(sdata, progress > 0)

# ... non-consenters
cat('There were ', nrow(filter(sdata, q1_1!= 'I consent to participate')),' respondents that did not consent.\n', sep='')
raw = filter(sdata, q1_1 == 'I consent to participate') %>%
  select(-q1_1)

# ... and who did not answer any questions
comment_questions = c('q3_5_text','q10_9_text', 'q11_9_text', 'q12_4_text', 'q14_6_text', 'q15_5_text', 'q17', 'q19', 'q24', 'q25') # 
selected_questions = select(sdata, 'response_id', starts_with('q')) %>% # 
  select(-q1_1, -all_of(comment_questions)) %>% # remove comments
  mutate_all(as.character) %>%
  pivot_longer(cols = -response_id) %>%
  filter(!str_detect(name, pattern='_do')) %>% # do not need random ordering questions
  mutate(missing = is.na(value)) 
# overall missing
all_missing = group_by(selected_questions, response_id) %>%
  summarise(n=n(), miss = sum(missing)) %>%
  filter(miss == n)
cat('There were ', nrow(all_missing),' respondents who completed nothing\n', sep='')
sdata = sdata[!sdata$response_id %in% all_missing$response_id,]

## data edits

# Convert survey duration to minutes; edit hours spent on review
sdata = mutate(sdata, 
              duration_mins = duration_in_seconds/ 60, # use duration in minutes rather than seconds
              progress_cat = cut(progress, c(-0.001,5,50,75,100)), # progress percent as categories
              recorded_date_time = recorded_date,
              recorded_date = as.Date(recorded_date)) %>% # simplify
  mutate_at(everything(comment_questions), .funs = ~ str_squish(.)) %>%
  select(-user_language,  
        -start_date, -end_date, # just use recorded_date
        -duration_in_seconds) # use minutes instead

## few more edits and drops
sdata = select(sdata, -status, -response_id) %>% # no longer needed
  arrange(recorded_date_time) %>% # sort from old to new
  mutate(id = 1:n(), # make ID variable
         recorded_date = as.Date(recorded_date)) %>% 
  select(id, everything())

# move `secretariat` from other to "Coordinator or Officer"
index = str_detect(tolower(sdata$q3_5_text), 'secretariat|secretary|executive Officer')
index[is.na(index)] = FALSE
sdata$q3_5[index] = NA
sdata$q3_5_text[index] = NA
sdata$q3_3[index] = "Coordinator or Officer"

## impute mean if only range given
sdata = mutate(sdata,
               q8_1_1 = ifelse(is.na(q8_1_1) & !is.na(q8_1_2) & !is.na(q8_1_3), (q8_1_2 + q8_1_3)/2, q8_1_1))

# what to do if q8_1_1 and q8_1_2 and q8_1_3 are missing.

# exclude respondent who "was chair until one year ago"
excluded = filter(sdata, q3_5_text == "was chair until one year ago")
sdata = filter(sdata, id != excluded$id)

### save ###
data = sdata # rename
save(data, labels, excluded, file='data/2_HREC_Responses.RData')
cat('Number of data rows = ', nrow(data), '.\n', sep='')


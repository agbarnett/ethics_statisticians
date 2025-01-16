# 2_read_qualtrics_api.R
# read the latest survey data from Qualtrics using the API
# July 2024
library(forcats) # for renaming level
library(tidyr)
library(qualtRics)
library(dplyr)
library(stringr)
library(janitor)
library(readxl)
source('99_functions.R')

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
         !str_detect(names, 'sentiment')) %>% # next shorten some labels
  mutate(labels = ifelse(str_detect(labels, pattern='^Option 1'),
                         'Option 1: How many applications did the committee consider in 2023 that included quantitative data and/or analysis?',
                         labels),
         labels = ifelse(str_detect(labels, pattern='^What are the statistician'),
                         'What are the statistician’s qualifications in statistics?',
                         labels),
         labels = ifelse(str_detect(labels, pattern='have a statistician on the committee'),
                         "Why don't you have a statistician on the committee?",
                         labels),
         labels = ifelse(str_detect(labels, pattern='^The latest'),
                         "Does this change in wording alter your opinion about having a qualified statistician member of the committee?",
                         labels),
         labels = str_remove_all(labels, pattern = '- Selected Choice'),
         labels = str_remove_all(labels, pattern = '\\(tick all that apply\\)'))


### Part 2: data ###
## set up API access
source('0_my_key.R') # do not share on github

## list all available surveys
surveys = all_surveys() %>%
  filter(str_detect(name, pattern='^Statisticians')) # just the statisticians surveys - all distributions

## now get HREC survey
sdata = fetch_survey(surveyID = surveys$id, 
                          label = TRUE, # use text for surveys
                          add_column_map = FALSE,
                     import_id = FALSE, # not helpful
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
sdata$q3_5[index] = NA # Blank Other
sdata$q3_5_text[index] = NA # Blank Other
sdata$q3_3[index] = "Coordinator or Officer" # add to variable
# move `acting chair` to 'chair'
index = str_detect(tolower(sdata$q3_5_text), 'acting chair')
index[is.na(index)] = FALSE
sdata$q3_5[index] = NA # Blank Other
sdata$q3_5_text[index] = NA # Blank Other
sdata$q3_1[index] = "Chair" # add to variable

## impute mean if only range given
sdata = mutate(sdata,
               q8_1_1 = ifelse(is.na(q8_1_1) & !is.na(q8_1_2) & !is.na(q8_1_3), (q8_1_2 + q8_1_3)/2, q8_1_1))
# what to do if q8_1_1 and q8_1_2 and q8_1_3 are missing.


## add variable that looks at whether committees have adequate statistical oversight
# main outcome from protocol: 'percentage that have a statistician as a full member and provide the statistician's qualifications'
# text from other that is okay to use as stats qualifications:
other_text_good = c('BSc in statistics',"Master of Medical Statistics","The AIHW is the second largest national")
other_text_good = paste(other_text_good, collapse='|')
# note that order in case_when below is important
sdata = mutate(sdata,
               adequate1 = case_when(
                 is.na(q9_1) ~ 'Unknown',
                 q9_1 == 'No' ~ 'No',
                 q9_1 == 'Yes' & q10_1!='' ~ 'No', # no formal
                 q9_1 == 'Yes' & q10_8!='' ~ 'No', # Don't know, counting this as no qualifications given
                 q9_1 == 'Yes' & q10_2!='' ~ 'Yes', # BSc
                 q9_1 == 'Yes' & q10_3!='' ~ 'Yes', # Diploma 
                 q9_1 == 'Yes' & q10_4!='' ~ 'Yes', # MSc
                 q9_1 == 'Yes' & q10_5!='' ~ 'Yes', # MBiostat
                 q9_1 == 'Yes' & q10_6!='' ~ 'Yes', # PhD
                 q9_1 == 'Yes' & q10_7!='' ~ 'Yes', # Accredited 
                 q9_1 == 'Yes' & str_detect(q10_9_text, other_text_good) ~ 'Yes',
                 q9_1 == 'Yes' & is.na(q10_1) & is.na(q10_2) & is.na(q10_3) & is.na(q10_4) & is.na(q10_5) & is.na(q10_6) & is.na(q10_7) & is.na(q10_8) ~ 'No'  # assume 'no' if no qualifications given
               ),
               adequate2 = case_when(
                 is.na(q9_2) ~ 'Unknown',
                 q9_2 == 'No' ~ 'No',
                 q9_2 == 'Yes' & q10_1!='' ~ 'No', # no formal
                 q9_2 == 'Yes' & q10_8!='' ~ 'No', # Don't know, counting this as no qualifications given
                 q9_2 == 'Yes' & q10_2!='' ~ 'Yes', # BSc
                 q9_2 == 'Yes' & q10_3!='' ~ 'Yes', # Diploma 
                 q9_2 == 'Yes' & q10_4!='' ~ 'Yes', # MSc
                 q9_2 == 'Yes' & q10_5!='' ~ 'Yes', # MBiostat
                 q9_2 == 'Yes' & q10_6!='' ~ 'Yes', # PhD
                 q9_2 == 'Yes' & q10_7!='' ~ 'Yes', # Accredited 
                 q9_2 == 'Yes' & str_detect(q10_9_text, other_text_good) ~ 'Yes',
                 q9_2 == 'Yes' & is.na(q10_1) & is.na(q10_2) & is.na(q10_3) & is.na(q10_4) & is.na(q10_5) & is.na(q10_6) & is.na(q10_7) & is.na(q10_8) ~ 'No'  # assume 'unknown' if no qualifications given
               ),
               
               any_adequate = case_when( # combination of some adequate cover
                 adequate1 == 'Yes' | adequate2 == 'Yes' ~ 'Yes', # order matters, so any yes first
                 adequate1 == 'No' | adequate2 == 'No' ~ 'No',
                 adequate1 == 'Unknown' & adequate2 == 'Unknown' ~ 'Unknown'
               )
               )

# useful code for checking:
#View(filter(sdata, q9_1 == 'Yes', adequate1 !='Yes') %>% select(id, q9_1, starts_with('q10_')))
#View(filter(sdata, q9_2 == 'Yes', adequate2 !='Yes') %>% select(id, q9_2, starts_with('q10_')))
#View(filter(sdata, str_detect(q10_9_text, other_text_good)) %>% select(q10_9_text))
sel = select(sdata, id, q9_1, adequate1, q9_2, adequate2, starts_with('q10')) %>% arrange(q9_1, q9_2); write.table(sel, sep='\t', file='data/check_main_questions.txt', quote=FALSE, row.names=FALSE) # for detailed check

# rename one level to remove 'please specify' text
sdata = mutate(sdata, 
               q11 = fct_recode(q11, "Other" = "Other, please specify"),
               q14 = fct_recode(q14, "Other" = "Other, please specify"))

# exclude respondent who "was chair until one year ago"
excluded = filter(sdata, q3_5_text == "was chair until one year ago")
sdata = filter(sdata, id != excluded$id)

### save ###
data = sdata # rename
save(data, labels, excluded, file='data/2_HREC_Responses.RData')
cat('Number of data rows = ', nrow(data), '.\n', sep='')


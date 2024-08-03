# 99_functions.R
# handy functions
# May 2024

# make table for tick all
make_tab_tick_all = function(indata, var_start){
  make_tab = select(indata, id, starts_with(var_start)) %>%
    select(-ends_with('_text')) %>%
    pivot_longer(cols = starts_with(var_start)) %>%
    mutate(answer = !is.na(value))
  freqs = group_by(make_tab, name) %>% # answers to questions
    summarise(n=n(), r = sum(answer)) %>%
    ungroup() %>%
    arrange(desc(r)) # order by frequency
  no_answer = group_by(make_tab, id) %>% # people who ticked nothing
    summarise(n=n(), r= sum(answer)) %>%
    filter(r == 0)%>%
    ungroup()
  no_answer_frame = data.frame(name = 'No answer', n = nrow(data), r = nrow(no_answer))
  # subtract no answer from frequencies
  freqs = mutate(freqs,
                 n = n - nrow(no_answer))
  tab = bind_rows(freqs, no_answer_frame) %>%
    mutate(percent = round(100*r/n),
           cell = paste(r, ' (', percent, '%)', sep='')) %>%
    select(name, cell) %>%
    rename('n (%)' = 'cell')
  return(tab)
}


# rename for tables
nice_rename = function(x){
  y = case_when(
    x == 'q3_1' ~ 'Chair', # note, not in same order as Qualtrics as new categories were added
    x == 'q3_6' ~ 'Co-chair',
    x == 'q3_7' ~ 'Deputy chair or Associate chair',
    x == 'q3_2' ~ 'Full member',
    x == 'q3_3' ~ 'Coordinator or Officer',
    x == 'q3_4' ~ 'Statistician',
    x == 'q3_8' ~ 'Lawyer',
    x == 'q3_9' ~ 'Lay person',
    x == 'q3_5' ~ 'Other',
    x == 'q6_1_1' ~ 'Mean applications with data and/or analysis',
    x == 'q6_1_2' ~ 'Lower applications with data and/or analysis',
    x == 'q6_1_3' ~ 'Upper applications with data and/or analysis',
    x == 'q8_1_1' ~ 'Mean percent with data and/or analysis',
    x == 'q8_1_2' ~ 'Lower percent with data and/or analysis',
    x == 'q8_1_3' ~ 'Upper percent with data and/or analysis',
    x == 'q9_1' ~ 'As a full member of the committee?',
    x == 'q9_2' ~ 'As a non-member but who can be consulted on statistical issues?',
    x == 'q10_1' ~ 'No formal qualifications',
    x == 'q10_2' ~ 'BSc Statistics',
    x == 'q10_3' ~ 'Diploma Statistics',
    x == 'q10_4' ~ 'MSc Statistics',
    x == 'q10_5' ~ 'MBiostat',
    x == 'q10_6' ~ 'Phd Statistics',
    x == 'q10_7' ~ 'Accredited statistician',
    x == 'q10_8' ~ 'Don`t know',
    x == 'q10_9' ~ 'Other',
    x == 'q15_1' ~ 'Cannot recruit one',
    x == 'q15_2' ~ 'Statisticians` expertise is not necessary for the decision-making process',
    x == 'q15_3' ~ 'Only need occasional statistical expertise',
    x == 'q15_4' ~ 'Other committee members are able to cover statistical questions',
    x == 'q15_6' ~ 'We have access to statistical expertise outside the committee', # number order not the same as qualtrics order as another category was added
    x == 'q15_5' ~ 'Other',
    TRUE ~ as.character(x) # safety net
  )
  return(y)  
}


## function to clean up comments
clean_comments = function(text){
  text = unique(text)
  text = text[!is.na(text)]
  text = text[!tolower(text) %in% c('nil','no')]
  text = str_replace_all(text, '\n', ' ')
  return(text)
}

# from https://www.listendata.com/2021/06/how-to-shorten-urls-with-r.html
ShortURL <- function(link, linkPreview = FALSE) {
  
  api <- if(linkPreview) {"http://v.gd/create.php?format=json"} else {"http://is.gd/create.php?format=json"}
  query <- list(url = link)
  request <- httr::GET(api, query = query)
  content <- httr::content(request, as = "text", encoding = "utf-8")
  result <- jsonlite::fromJSON(content)
  
  return(result)
  
}
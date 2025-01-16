# 99_functions.R
# handy functions
# August 2024

# functions used in calc_miss
sum.na = function(x){sum(is.na(x))}
sum.n = function(x){sum(!is.na(x))}

# calculate missing for 4_results
calc_miss = function(indata, question, condition){
  if(!is.null(condition)){
    indata = filter(indata, condition)
  }
  var = select(indata, question) %>%
    summarise(across(everything(), list(n = sum.n, missing = sum.na))) %>%
    mutate(question = question)
  names(var) = c('n','missing','question')
  return(var)
}

# function to remove very short comments
remove_short = function(x){
  x = str_remove(x, '^(n|N)o.?$|^(n|N)il.?$')
  x
}

# alternative missing replace
my_replace = function(x){replace_na(x, replace='')}

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
  no_answer_frame = data.frame(name = 'No answer', n = nrow(indata), r = nrow(no_answer))
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
    x == 'adequate1' ~ 'As a qualified full member of the committee?',
    x == 'adequate2' ~ 'As a qualified non-member but who can be consulted on statistical issues?',
    x == 'q9_either' ~ 'Either a full member or who can be consulted',
    x == 'q10_1' ~ 'No formal qualifications',
    x == 'q10_2' ~ 'BSc Statistics',
    x == 'q10_3' ~ 'Diploma Statistics',
    x == 'q10_4' ~ 'MSc Statistics',
    x == 'q10_5' ~ 'MBiostat',
    x == 'q10_6' ~ 'PhD Statistics',
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

# make bootstrap intervals for main and secondary outcome that adjust for non-response in the census
bootstrap_intervals = function(indata, 
                               non_response, # amount of non-response
                               n_boot = 1000, # number of bootstraps
                               v1 = 'q9_1', # variable names
                               v2 = 'q9_2') 
  {

    # matrix of conditional probabilities (shows correlation between two questions)
    # matrix = prop.table(with(indata, table(q9_1, q9_2, useNA = 'always')),1)
    
    # rename variables to generic names
    index = names(indata) == v1
    names(indata)[index] = 'v1'
    index = names(indata) == v2
    names(indata)[index] = 'v2'
    
    # take pairs of data to simulate the correlation
    all_tabs = NULL
    for (k in 1:n_boot){ # loop through bootstrap samples
      # make new data
      fill_miss = sample_n(indata, size = non_response, replace = TRUE) # sample with replacement
      total = bind_rows(indata, fill_miss)
      # calculate frequency statistics
      tab = make_table(total) %>%
        mutate(boot = k)
      all_tabs = bind_rows(all_tabs, tab)
    }
    # calculate the overall statistics for the proportions
    boot_stats = mutate(all_tabs, p = r / n) %>%
      group_by(name, value) %>%
      summarise(#n = n(), # check of sample number - not needed
                bootp = mean(p),
                lower = quantile(p, 0.025),
                upper = quantile(p, 0.975))
    # add bootstraps proportion statistics back to observed data
    intable = make_table(indata) %>%
      left_join(boot_stats, by=c('name','value')) %>%
      mutate(p = r/n,
             p_check = p - bootp)
    # check for differences in the proportion
    errors = filter(intable, abs(p_check)>0.005)
    #
    intable = select(intable, name, value, r, n, p, lower, upper) %>%
      mutate(p = p*100,
             lower = lower*100,
             upper = upper*100) 
    # rename back from generic to specific
    intable = mutate(intable,
                     name = case_when(name =='v1' ~ v1,
                                      name =='v2' ~ v2,
                                      TRUE ~ as.character(name)))

    return(intable)
} # end of function

# make table for bootstrap function
make_table = function(for_stats){
  # make secondary outcome (either are 'yes')
  for_tab = mutate(for_stats, q9_either = case_when(
    v1 == 'Yes' | v2 == 'Yes' ~ 'Yes', # first match counts, so ordering matters here
    v1 == 'No' | v2 == 'No' ~ 'No'
  ))
  for_tab = pivot_longer(for_tab, -id) 
  numerator = group_by(for_tab, name, value) %>%
    tally() %>%
    rename('r' = 'n')
  denominator = group_by(for_tab, name) %>%
    tally()
  tab = left_join(numerator, denominator, by='name') %>%
    ungroup()
  # 
  return(tab)
}

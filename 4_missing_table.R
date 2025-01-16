# 4_missing_table.R
# item-missing data table, called by 4_results_*.Rmd
# Sep 2024
library(tidyr)

# make question completion patterns
mdata = mutate(data, 
               ## simplify some variables for missing plot
               # any answer to q3
               q3 = !is.na(q3_1)+!is.na(q3_2)+!is.na(q3_3)+!is.na(q3_4)+!is.na(q3_5)+
                 !is.na(q3_6)+!is.na(q3_7)+!is.na(q3_8)+!is.na(q3_9) > 0,
               # any answer to q10
               q10 = !is.na(q10_1)+!is.na(q10_2)+!is.na(q10_3)+!is.na(q10_4)+
                 !is.na(q10_5)+!is.na(q10_6)+!is.na(q10_7)+!is.na(q10_8)+
                 !is.na(q10_9) > 0,
               # any answer to q15
               q15 = !is.na(q15_1)+!is.na(q15_2)+!is.na(q15_3)+!is.na(q15_4)+
                 !is.na(q15_5)+!is.na(q15_6) > 0,
               # to delete ;;; patterns based on question logic
               q10_11_21_22_complete = ifelse(q9_1 == 'Yes' | q9_2 == 'Yes', 'q10', ''),
               q12_complete = ifelse(q9_2 == 'Yes', 'q12', ''),
               q13_to_16_complete = ifelse(q9_1 == 'No', 'q13', ''),
               q18_complete = ifelse(q16 == 'No', 'q18', ''),
               q20_complete = ifelse(q16 == 'No', 'q20', '')) 

## create summaries
# a) for questions that anyone can answer
anyone = c('q3','q4','q6_1_1','q7','q9_1','q9_2')
numbers1 = select(mdata, all_of(anyone)) %>%
  mutate_all(is.na)%>%
  pivot_longer(everything())
# b) for conditional questions 1
conditional1 = c('q10','q11','q21','q22')
numbers2 = filter(mdata, q9_1 == 'Yes' | q9_2 == 'Yes') %>%
  select(all_of(conditional1)) %>%
  mutate_all(is.na)%>%
  pivot_longer(everything())
# c) for conditional questions 2
conditional2 = c('q12')
numbers3 = filter(mdata, q9_2 == 'Yes') %>%
  select(all_of(conditional2)) %>%
  mutate_all(is.na)%>%
  pivot_longer(everything())
# d) for conditional questions 3
conditional3 = c('q13','q14','q15','q16')
numbers4 = filter(mdata, q9_1 == 'No') %>%
  select(all_of(conditional3)) %>%
  mutate_all(is.na)%>%
  pivot_longer(everything())
# e) for conditional questions 4
conditional4 = c('q18')
numbers5 = filter(mdata, q16 == 'No') %>%
  select(all_of(conditional4)) %>%
  mutate_all(is.na)%>%
  pivot_longer(everything())

# counts
missing_table = bind_rows(numbers1, numbers2, numbers3, numbers4, numbers5) %>%
  group_by(name) %>%
  summarise(n = n(), missing = sum(value)) %>%
  ungroup() %>%
  left_join(labels, by=c('name' = 'names')) %>% # merge with question labels
  select(labels, n, missing) %>%
  arrange(desc(n), desc(missing)) %>% # order by size and missing
  mutate(percent = round(100*missing/n))



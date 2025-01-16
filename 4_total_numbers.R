# 4_total_numbers.R
# estimate the national number of applications with uncertainty

TeachingDemos::char2seed('everton')
n_boot = 1000 # number of bootstrap resamples
n_complete = filter(data, !is.na(q6_1_1) | (!is.na(q7) & !is.na(q8_1_1))) %>% nrow() # number who completed one of the two options
n_missing = n_sent - n_complete # number of non-responsive HRECs (assuming n_sent is total number of valid HRECs)
# 
boot_samples = boot_samples_by = NULL
for (k in 1:n_boot){
  sample1 = filter(data, !is.na(q6_1_1)) %>% # completed first option
    select(id, any_adequate, starts_with('q6_1')) %>%
    mutate(
      q6_1_2 = ifelse(is.na(q6_1_2), q6_1_1, q6_1_2), # set to mean if no uncertainty
      q6_1_3 = ifelse(is.na(q6_1_3), q6_1_1, q6_1_3), # set to mean if no uncertainty
      q6_1_2 = ifelse(q6_1_2 > q6_1_1, q6_1_1, q6_1_2), # set to mean if outside
      q6_1_3 = ifelse(q6_1_3 < q6_1_1, q6_1_1, q6_1_3), # set to mean if outside
      q6_1_2 = ifelse(q6_1_2 == q6_1_1, q6_1_1-0.1, q6_1_2), # add negligible amount of variance for triangular distribution
      q6_1_3 = ifelse(q6_1_3 == q6_1_1, q6_1_1+0.1, q6_1_3), # add negligible amount of variance for triangular distribution
      r = round(rtri(n=n(), min = q6_1_2, max = q6_1_3, mode = q6_1_1))) 
  sample2 = filter(data, is.na(q6_1_1), !is.na(q7), !is.na(q8_1_1)) %>% # completed second option
    select(id, any_adequate, q7, starts_with('q8_1')) %>%
    mutate(
      q8_1_2 = ifelse(is.na(q8_1_2), q8_1_1, q8_1_2), # set to mean if no uncertainty
      q8_1_3 = ifelse(is.na(q8_1_3), q8_1_1, q8_1_3), # set to mean if no uncertainty
      q8_1_2 = ifelse(q8_1_2 > q8_1_1, q8_1_1, q8_1_2), # set to mean if outside
      q8_1_3 = ifelse(q8_1_3 < q8_1_1, q8_1_1, q8_1_3), # set to mean if outside
      q8_1_2 = ifelse(q8_1_2 == q8_1_1, q8_1_1-0.1, q8_1_2), # add negligible amount of variance for triangular distribution
      q8_1_3 = ifelse(q8_1_3 == q8_1_1, q8_1_1+0.1, q8_1_3), # add negligible amount of variance for triangular distribution
      p = round(rtri(n=n(), min = q8_1_2, max = q8_1_3, mode = q8_1_1)),
      r = round((p/100)*q7)) # percent times application numbers
  # combine samples
  sample = bind_rows(sample1, sample2)
  # impute HRECs that did not respond
  imputed = select(sample, id, any_adequate, r) %>%
    sample_n(size = n_missing, replace = TRUE) %>% # sample with replacement, take whole row in case of correlation between `any_adequate` and numbers
    mutate(id = id + 999) # shift ID to avoid overlap
  # get overall total
  total = bind_rows(sample, imputed) %>% 
    summarise(n=n(), total = sum(r)) %>%
    mutate(k = k)
  # get overall total by access to statistician
  total_by = bind_rows(sample, imputed) %>% 
    group_by(any_adequate) %>%
    summarise(n=n(), total = sum(r)) %>%
    ungroup() %>%
    mutate(k = k) 
  #
  boot_samples = bind_rows(boot_samples, total)
  boot_samples_by = bind_rows(boot_samples_by, total_by)
}
# check
if(any(boot_samples$n != n_sent)){cat('Error in numbers\n')}

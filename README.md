# Do ethics committees include statisticians? A census of Australian health and medical committees

R code to run a national census of human research ethics committees in Australia to ascertain if they have access to a qualified statistician.

The files are in order

- 0_ reads the sampling frame

- 1_ prepares the letters to HRECs

- 2_ reads the online survey data from *Qualtrics*

- 3_ removes any personal information from the data

- 4_ creates the reports and figures

### R version

```
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
[3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.utf8    

time zone: Australia/Brisbane
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rmarkdown_2.27  XML_3.99-0.14   readxl_1.4.2    gridExtra_2.3   visdat_0.6.0   
 [6] ggplot2_3.5.1   EnvStats_2.8.1  flextable_0.9.6 janitor_2.2.0   dplyr_1.1.4    
[11] stringr_1.5.0   tidyr_1.3.1    

loaded via a namespace (and not attached):
 [1] gtable_0.3.3            xfun_0.45               vctrs_0.6.5            
 [4] tools_4.3.1             generics_0.1.3          curl_5.0.1             
 [7] tibble_3.2.1            fansi_1.0.4             pkgconfig_2.0.3        
[10] data.table_1.14.8       uuid_1.1-0              lifecycle_1.0.3        
[13] compiler_4.3.1          textshaping_0.3.6       munsell_0.5.0          
[16] snakecase_0.11.0        httpuv_1.6.11           fontquiver_0.2.1       
[19] fontLiberation_0.1.0    htmltools_0.5.5         yaml_2.3.7             
[22] later_1.3.1             pillar_1.9.0            crayon_1.5.2           
[25] gfonts_0.2.0            ellipsis_0.3.2          openssl_2.0.6          
[28] mime_0.12               fontBitstreamVera_0.1.1 TeachingDemos_2.12     
[31] tidyselect_1.2.0        zip_2.3.0               digest_0.6.31          
[34] stringi_1.7.12          purrr_1.0.2             fastmap_1.1.1          
[37] grid_4.3.1              colorspace_2.1-0        cli_3.6.1              
[40] magrittr_2.0.3          crul_1.4.0              utf8_1.2.3             
[43] withr_2.5.0             gdtools_0.3.7           scales_1.3.0           
[46] promises_1.2.0.1        lubridate_1.9.3         timechange_0.2.0       
[49] officer_0.6.5           cellranger_1.1.0        askpass_1.1            
[52] ragg_1.2.5              shiny_1.7.4             evaluate_0.21          
[55] knitr_1.47              rlang_1.1.1             Rcpp_1.0.10            
[58] xtable_1.8-4            glue_1.6.2              httpcode_0.3.0         
[61] xml2_1.3.4              rstudioapi_0.14         jsonlite_1.8.5         
[64] R6_2.5.1
```

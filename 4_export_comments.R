# 4_export_comments.R
# export the comments for analysis by Taya
# August 2024
library(dplyr)
library(openxlsx)
library(stringr)
source('99_functions.R')

# get data from 2_read_qualtrics_api.R, can give unblinded comments to Taya
load('data/2_HREC_Responses.RData')

## Fix short labels for q17 and q19
labels = mutate(labels,
                labels = ifelse(names == 'q17', 'For commitees without a statistician, (Optional) Do you have any comments in relation to the above questions?', labels),
                labels = ifelse(names == 'q19', 'National Statement wording, Please explain why or why not', labels)
                )


# list the comment questions to export to Excel
questions_to_export = c('q10_9_text', 'q11_9_text','q12_4_text','q14_6_text','q15_5_text','q17','q19','q24','q25')



## export to excel
header_style <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD", halign = "left",
                            valign = "center", wrapText = TRUE, textDecoration = "Bold", border = "TopBottomLeftRight") # header style
cell_style <- createStyle(wrapText = TRUE, borderColour = "#4F81BD") # wrap text
wb <- createWorkbook(title = 'Comments from survey of HRECs', creator = 'Adrian Barnett', subject = 'created by 4_export_comments.R')
for (sheet in questions_to_export){ # separate sheet per question
  # get label
  label = filter(labels, names == sheet) %>% pull(labels)
  # select comments
  selected = select(data, id, q3_1, all_of(sheet)) %>% # Add ID, position
    mutate(q3_1 = ifelse(is.na(q3_1), 'Missing', q3_1))
  index = complete.cases(selected)
  selected = selected[index,] # remove missing rows
  selected = mutate_if(selected, is.character, remove_short)
  index = complete.cases(selected)
  selected = selected[index,] # remove missing again
  #
  addWorksheet(wb, sheet, gridLines = TRUE)
  freezePane(wb, sheet=sheet, firstActiveRow = 3)
  writeData(wb, sheet = sheet, x= label, startRow=1)
  writeData(wb, sheet = sheet, x=selected, startRow = 2, headerStyle = header_style, rowNames=FALSE)
  addStyle(wb, sheet=sheet, style = cell_style, cols=1:3, rows=2:100, gridExpand = TRUE)
  setColWidths(wb, sheet = sheet, cols=c(1,2,3), widths=c(5,10,120))
}
saveWorkbook(wb, "comments/survey_comments.xlsx", overwrite = TRUE)
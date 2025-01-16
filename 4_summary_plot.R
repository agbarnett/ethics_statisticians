# 4_summary_plot.R
# summary plot of main results; multiple plots with links
# https://stackoverflow.com/questions/35631889/align-multiple-plots-with-varying-spacings-and-add-arrows-between-them

source('99_functions.R') # for nice rename
library(forcats) # for factor rename
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(grid) # for nullGrob
library(stringr) # for str_wrap
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
# colours
library(RColorBrewer)
#
colour_yes = 'pink'
colour_no = 'darkseagreen3'
colour_unknown = 'darkorchid2'

## data
# from 3_blind_comments_do_not_share.R
load('data/3_HREC_Responses.RData')
n_non_consent = sum(data$q1_1 != 'I consent to participate')
# only those who consented
data = filter(data, q1_1 == 'I consent to participate') %>%
  select(-q1_1)

## 1) bar plots of statistical coverage
for_plot = mutate(data, 
                  q9_1 = ifelse(is.na(q9_1), 'Missing', q9_1),
                  q9_2 = ifelse(is.na(q9_2), 'Missing', q9_2))
# labels
plabels = group_by(for_plot, q9_1) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = prop.table(n)*100,
         label = paste(round(percent),'%',sep=''),
         labely = n/2, # label y position
         labely = ifelse(n<3, n+1, labely)) # adjust small labels that don't appear in bars
# plot
plot1a = ggplot(for_plot, aes(x = q9_1, fill=q9_1))+
  geom_bar(width=0.99)+
  xlab('')+
  ylab('Frequency')+
  scale_y_continuous(expand=c(0,NA))+ # no space
  scale_x_discrete(expand=c(0.01,0.01))+
  geom_text(data = plabels, aes(x = q9_1, y = labely, label = label), color='white', size = 5)+
  scale_fill_manual(NULL, values = c(colour_unknown, colour_no, colour_yes))+
  g.theme +
  coord_flip()+
  theme(legend.position = 'none',
#        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "grey88"))+
  ggtitle('Statistician as a full committee member')
plot1a

# labels
plabels = group_by(for_plot, q9_2) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = prop.table(n)*100,
         label = paste(round(percent),'%',sep=''))
# plot
plot1b = ggplot(for_plot, aes(x = q9_2, fill=q9_2))+
  geom_bar(width=0.99)+
  xlab('')+
  ylab('Frequency')+
  scale_y_continuous(expand=c(0,NA))+ # no space
  scale_x_discrete(expand=c(0.01,0.01))+
  geom_text(data = plabels, aes(x = q9_2, y = n, label = label), color='white', size = 5, position = position_stack(vjust=0.5))+
  scale_fill_manual(NULL, values = c(colour_unknown, colour_no, colour_yes))+
  g.theme +
  coord_flip()+
  theme(legend.position = 'none',
        panel.background = element_rect(fill = "grey88"),
        plot.title.position = "plot")+ # move title to left
  ggtitle('Non-member who can be consulted on statistical issues')
plot1b


## 2a) bar plot of qualifications (if yes)
tab = filter(data, q9_1 == 'Yes' | q9_2 == 'Yes') %>% # only those who could answer question
  make_tab_tick_all( var_start = 'q10') %>%
  filter(name != 'No answer') %>% # remove `no answer`
  rename('Role' = 'name', 'stats' = 'n (%)') %>%
  separate(stats, into = c('n', 'percent'), convert = TRUE) %>% # can ignore warning
  mutate(Role = nice_rename(Role),
         label = paste(percent,'%',sep='')) %>%
  filter(n>0) %>% # remove one no answer(diploma)
  arrange(n) # needed for text colour
# colour from white to black to stand out better
text_colour = rev(brewer.pal(n = nrow(tab), 'Greys')) 
text_colour = rep('black', nrow(tab)); text_colour[nrow(tab)] = 'white'
# plot
plot2a = ggplot(tab, aes(x = fct_reorder(Role, n), y = n, fill=fct_reorder(Role,n)))+
  geom_bar(width=0.99, stat='identity')+
  xlab('')+
  ylab('Frequency')+
  scale_y_continuous(expand=c(0,NA))+ # no space
  scale_x_discrete(expand=c(0.01,0.01), labels = function(x) str_wrap(x, width = 15))+
  geom_text(data = tab, aes(x = Role, y = n, label = label), color=text_colour, size = 5, position = position_stack(vjust=0.5))+ # slightly smaller text
  scale_fill_brewer(NULL, palette = "Reds")+
  g.theme +
  coord_flip()+
  theme(legend.position = 'none',
        plot.title.position = "plot", # move title to left
        panel.background = element_rect(fill = "grey88"),
        plot.background = element_rect(fill = colour_yes))+
  ggtitle("Statistician's qualifications")
plot2a


## 2b) bar plot of employment (if yes)
to_plot = filter(data, q9_1 == 'Yes' | q9_2 == 'Yes') %>%
  mutate(q11 = fct_na_value_to_level(q11, 'Missing'))
# labels
plabels = group_by(to_plot, q11) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = prop.table(n)*100,
         label = paste(round(percent),'%',sep=''),
         labely = n/2, # label y position
         labely = ifelse(n<3, n+1, labely) # adjust small labels that don't appear in bars
  ) %>%
  arrange(n)
# colour from white to black to stand out better
text_colour = rep('black', nrow(plabels)); text_colour[nrow(plabels)] = 'white'
# plot
plot2b = ggplot(to_plot, aes(x = fct_rev(fct_infreq(q11)), fill=fct_rev(fct_infreq(q11))))+ # order by frequency
  geom_bar(width=0.99)+
  xlab('')+
  ylab('Frequency')+
  scale_y_continuous(expand=c(0,NA))+ # no space
  scale_x_discrete(expand=c(0.01,0.01), labels = function(x) str_wrap(x, width = 10))+
  geom_text(data = plabels, aes(x = q11, y = labely, label = label), color=text_colour, size = 5)+ #  
  scale_fill_brewer(NULL, type='qual', palette = "Reds")+
  g.theme +
  coord_flip()+
  theme(legend.position = 'none',
        plot.title.position = "plot", # move title to left
        panel.background = element_rect(fill = "grey88"),
        plot.background = element_rect(fill = colour_yes))+
  ggtitle("Currently employed as a statistician")
plot2b

## 3a) How are statistical aspects of studies dealt with?
#
to_plot = filter(data, q9_1 == 'No') %>% # with no statistician on committee
  mutate(q14 = fct_na_value_to_level(q14, 'Missing'))
# labels
plabels = group_by(to_plot, q14) %>%
  tally() %>%
  ungroup() %>%
  mutate(percent = prop.table(n)*100,
         label = paste(round(percent),'%',sep=''),
         labely = n/2, # label y position
         labely = ifelse(n<3, n+1, labely)) %>% # adjust small labels that don't appear in bars
  arrange(n)
# white except first against yellow
text_colour = rep('white', nrow(plabels)); text_colour[1] = 'grey22'; text_colour[nrow(plabels)] = 'grey22'
#
plot3a = ggplot(to_plot, aes(x = fct_rev(fct_infreq(q14)), fill=fct_rev(fct_infreq(q14))))+
  geom_bar(width=0.99)+
  xlab('')+
  ylab('Frequency')+
  scale_y_continuous(expand=c(0,NA)) + #, limits=c(0,12), breaks=c(0,4,8,12))+ # avoid decimals on axis 
  scale_x_discrete(expand=c(0.01,0.01), labels = function(x) str_wrap(x, width = 35))+
  geom_text(data = plabels, aes(x = q14, y = labely, label = label), color=text_colour, size = 5)+
  scale_fill_viridis_d(NULL)+
  g.theme +
  coord_flip()+
  theme(legend.position = 'none',
        plot.title.position = "plot", # move title to left
        panel.background = element_rect(fill = "grey88"),
        plot.background = element_rect(fill = colour_no))+
  ggtitle('How are statistical aspects of studies dealt with?')
plot3a

## 3b) Why don't you have a statistician on the committee? 
#
tab = filter(data, q9_1 == 'No') %>% # with no statistician on committee 
  make_tab_tick_all(var_start = 'q15') %>%
  filter(name != 'No answer') %>% # remove `no answer`
  rename('Reason' = 'name', 'stats' = 'n (%)') %>%
  separate(stats, into = c('n', 'percent'), convert = TRUE) %>% # can ignore warning
  mutate(Reason = nice_rename(Reason),
         label = paste(percent,'%',sep=''),
         labely = n/2, # label y position
         labely = ifelse(n<3, n+1, labely) # adjust small labels
         ) %>%
  arrange(n)
# white except first against yellow
text_colour = rep('white', nrow(tab)); text_colour[1] = 'grey22'
# plot
plot3b = ggplot(tab, aes(x = fct_reorder(Reason, n), y = n, fill=fct_reorder(Reason,n)))+ # re-order by frequency
  geom_bar(width=0.99, stat='identity')+
  xlab('')+
  ylab('Frequency')+
  scale_y_continuous(expand=c(0,NA))+ # no space
  scale_x_discrete(expand=c(0.01,0.01), labels = function(x) str_wrap(x, width = 30))+
  geom_text(data = tab, aes(x = Reason, y = labely, label = label), color=rev(text_colour), size = 5) + 
  scale_fill_viridis_d(NULL)+
  g.theme +
  coord_flip()+
  theme(legend.position = 'none',
        plot.title.position = "plot", # move title to left
        panel.background = element_rect(fill = "grey88"),
        plot.background = element_rect(fill = colour_no))+ # change background colour
  ggtitle("Why don't you have a statistician on the committee?")
plot3b

## make the grid
# matrix
layout_matrix = rbind(c(NA, 1, 1, NA, 2, 2, NA),
                      c(NA, NA, NA, NA, NA, NA, NA), # for gap
                      c(3, 3, 3, 5, 5, 5, 5),
                      c(4, 4, 4, 6, 6, 6, 6))
# start export
jpeg('figures/4_combined_plot.jpg', width=9.5, height=11.5, units='in', res=500, quality=100)
# make plot
grid.arrange(
  grobs = list(plot1b, plot1a, plot2a, plot2b, plot3a, plot3b),
  layout_matrix = layout_matrix, 
  heights=c(0.5, 0.02, 1, 1), # small second number for gap
  widths = c(0.1, 1, 1, 0.1, 1, 1, 0.1))
# Switch to viewport for arrows
vp = viewport(x = 0.5, y=0.5, width=1, height=1)
pushViewport(vp)

# a) first Yes arrow and label
grid.lines(x = c(0.6,0.45),
           y = c(0.95,0.80),
           gp = gpar(col=colour_yes, lwd=3.5),
           arrow=arrow(type="closed", length=unit(4,"mm")))
# bent line
grid.lines(x = c(0.03,0.10),
           y = c(0.94,0.94),
           gp = gpar(col=colour_yes, lwd=3.5))
grid.lines(x = c(0.03,0.03),
           y = c(0.94,0.80),
           gp = gpar(col=colour_yes, lwd=3.5),
           arrow=arrow(type="closed", length=unit(4,"mm")))
# b) No arrow and label from full committee
grid.lines(x = c(0.84,0.84),
           y = c(0.90,0.80),
           gp = gpar(col=colour_no, lwd=3.5),
  arrow=arrow(type="closed", length=unit(4,"mm")))
dev.off()


## part 2: export smaller plots for presentation
# make axis text bigger
plot1a = plot1a + theme(axis.text = element_text(size=16), axis.title = element_text(size=14))
plot1b = plot1b + theme(axis.text = element_text(size=16), axis.title = element_text(size=14))
#
jpeg('figures/4_plot1_for_slides.jpg', width=9.2, height=5, units='in', res=500, quality=100)
# make plot
grid.arrange(grobs = list(plot1b, plot1a), ncol = 2)
dev.off()

# make axis text bigger
plot2a = plot2a + theme(axis.text = element_text(size=16), axis.title = element_text(size=14))
plot2b = plot2b + theme(axis.text = element_text(size=16), axis.title = element_text(size=14))
#
jpeg('figures/4_plot2_for_slides.jpg', width=9.2, height=5, units='in', res=500, quality=100)
# make plot
grid.arrange(grobs = list(plot2a, plot2b), ncol = 2)
dev.off()

# make axis text bigger
plot3a = plot3a + theme(axis.text = element_text(size=11), axis.title = element_text(size=14))
plot3b = plot3b + theme(axis.text = element_text(size=11), axis.title = element_text(size=14))
#
jpeg('figures/4_plot3_for_slides.jpg', width=9.2, height=5, units='in', res=500, quality=100)
# make plot
grid.arrange(grobs = list(plot3a, plot3b), ncol = 2)
dev.off()

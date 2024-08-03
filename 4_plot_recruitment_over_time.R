# 3_plot_recruitment_over_time.R
# plot the recruitment by journal over time
# called from 3_results.Rmd
# May 2024

# use today as last date
censor.date = max(data$recorded_date)+1 # one day after last returned survey

# cumulative counts by journal
counts = group_by(data, recorded_date) %>%
  tally() %>%
  arrange(recorded_date) %>%
  mutate(csum = cumsum(n)) %>%
  ungroup()
# add zero at start
zeros = arrange(counts, recorded_date) %>%
  slice(1) %>% # earliest date
  ungroup() %>%
  mutate(csum=0)
# add final date at end
final = arrange(counts, desc(recorded_date)) %>%
  slice(1) %>%
  mutate(recorded_date = censor.date)

#
to_plot = bind_rows(zeros, counts, final) %>% 
  unique() # in case of duplicates due to adding final date

# plot
label1 = data.frame(recorded_date = as.Date('2024-05-20'), csum=30, label='First email sent')
label2 = data.frame(recorded_date = as.Date('2024-06-03'), csum=0, label='First reminder sent')
label3 = data.frame(recorded_date = as.Date('2024-06-26'), csum=0, label='76 letters sent')
gplot = ggplot(data=to_plot, aes(x=recorded_date, y=csum)) +
  geom_step(linewidth=1.05, col='darkseagreen4')+
  geom_text(data = label1, aes(x=recorded_date, y=csum, label=label), angle=90, adj=0, nudge_x=-0.5, col='grey55')+
  geom_text(data = label2, aes(x=recorded_date, y=csum, label=label), angle=90, adj=0, nudge_x=0, col='grey55')+
  geom_text(data = label3, aes(x=recorded_date, y=csum, label=label), angle=90, adj=0, nudge_x=0, col='grey55')+
  ylab('Cumulative responses')+
  xlab('Date')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position=c(0.2,0.85),
        plot.margin = margin(0, 5, 0, 0, "mm")) # trbl
#
jpeg('figures/4_recruitment.jpg', width=5, height=4, units='in', res=500, quality = 100)
print(gplot)
dev.off()



# 4_consort_flow.R
# CONSORT flow chart of participants
# July 25
library(diagram)

### to complete after survey is closed

# key numbers
nhmrc_committees_in_sampling_frame = 194
nhmrc_closed = 5 # closed in original form
our_closed = 2 # closed because we found they were closed after searching for contact data
original_links_created = nhmrc_committees_in_sampling_frame - nhmrc_closed - our_closed
regenerated_links = 11
sent_links = 201 # some multiples
linked = 154
# not sure here, paused collection
opened = 119
opened_no_answer = 25
excluded = 1 
analysed = answer - excluded

# labels
l1 = paste('Number of NHMRC-\nregistered HRECs\n(N=', nhmrc_committees_in_sampling_frame, ')', sep='') # 
l2 = paste('Closed (N=', nhmrc_closed, ')', sep='') # 
l3 = paste('Closed after our\naddress search (N=', our_closed, ')', sep='') # 
l4 = paste('Emails sent with\nsurvey link (N=', original_links_created, ')', sep='') # 
l5 = paste('Link\nregenerated (N=', regenerated_links, ')', sep='') # 
l6 = paste('Links opened (N=', linked, ')', sep='') # 
l7 = paste('Survey opened (N=', opened, ')', sep='') # 
l8 = paste('Empty response (N=', opened_no_answer, ')', sep='') # 
l9 = paste('Excluded (N=', excluded, ')', sep='') # 
l10 = paste('Analysed (N=', analysed, ')', sep='') #
null = '' # for arrow placements
labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, null, null, null, null, null)
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.25	0.94	white	square	0.45	0.165
2	0.75	0.84	white	square	0.45	0.15
3	0.75	0.72	white	square	0.40	0.175
4	0.25	0.62	white	square	0.45	0.16
5	0.75	0.53	white	square	0.40	0.16
6	0.25	0.42	white	square	0.33	0.18
7	0.75	0.32	white	square	0.35	0.17
8	0.25	0.22	white	square	0.45	0.15
9	0.75	0.12	white	square	0.38	0.15
10	0.25	0.05	white	square	0.35	0.15
11	0.25	0.84	white	square	0	0
12	0.25	0.72	white	square	0	0
13	0.25	0.53	white	square	0	0
14	0.25	0.32	white	square	0	0
15	0.25	0.12	white	square	0	0')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[4, 1] = "' '"
M[2, 11] = "' '"
M[3, 12] = "' '"
M[6, 4] = "' '"
M[8, 6] = "' '"
M[5, 13] = "' '"
M[7, 14] = "' '"
M[9, 15] = "' '"
M[10, 8] = "' '"

## make figure 
jpeg('figures/4_consort_flow.jpg', width=5, height=7, units='in', res=400, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        dtext = 0.12, # controls the position of arrow text relative to arrowhead
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = 'black')
dev.off()

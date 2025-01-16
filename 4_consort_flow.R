# 4_consort_flow.R
# CONSORT flow chart of participants
# August 2024
library(diagram)

# key numbers
nhmrc_committees_in_sampling_frame = 194
nhmrc_closed = 5 # closed in original form
our_closed = 2 # closed because we found they were closed after searching for contact data
original_links_created = nhmrc_committees_in_sampling_frame - nhmrc_closed - our_closed
regenerated_links = 11
sent_links = 201 # some multiples
governance_issues = 1 # "Apologies for not responding earlier. It is a requirement of our institution that in order to participate in research a study must be approved by a certified health HREC."
withdrew = 1
# not sure here, paused collection
test_response = 1 # my test response
opened = 155 - test_response # 'surveys started' on Qualtrics
opened_only_picf = 33 # did not get beyond page 1, so no data for Qualtrics to download
opened_no_answer = 27
excluded = 1 
answer = 94
analysed = answer - excluded

# labels
l1 = paste('Number of NHMRC-\nregistered HRECs\n(N=', nhmrc_committees_in_sampling_frame, ')', sep='') # 
l2 = paste('Closed (N=', nhmrc_closed + our_closed, ')\n- Before our search (N=', nhmrc_closed, ')\n- After our search (N=', our_closed, ')', sep='') # 
l3 = paste('Emails sent with\nsurvey link (N=', original_links_created, ')', sep='') # 
l4 = paste('Withdrew (N=', governance_issues + withdrew, ')\n- No reason (N=', withdrew, ')\n- Our ethical approval\nwas insufficient (N=', governance_issues, ')', sep='') # 
l5 = paste('Opened first\npage (N=', opened, ')', sep='') # 
l6 = paste('No progress beyond\nfirst page (N=', opened_only_picf, ')', sep='') # 
l7 = paste('Started\nquestions (N=', opened - opened_only_picf, ')', sep='') # 
l8 = paste('Empty\nresponse (N=', opened_no_answer, ')', sep='') # 
l9 = paste('Excluded (N=', excluded, ')\n- Ex-chair', sep='') # 
l10 = paste('Analysed (N=', analysed, ')', sep='') #
null = '' # for arrow placements
c_labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, null, null, null, null, null)
n.c_labels = length(c_labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.25	0.94	white	square	0.45	0.18
2	0.75	0.84	white	square	0.43	0.22
3	0.25	0.72	white	square	0.40	0.175
4	0.75	0.61	white	square	0.45	0.19
5	0.25	0.52	white	square	0.40	0.16
6	0.75	0.451	white	square	0.40	0.21
7	0.25	0.35	white	square	0.33	0.18
8	0.75	0.26	white	square	0.35	0.17
9	0.75	0.16	white	square	0.33	0.16
10	0.25	0.06	white	square	0.33	0.18
11	0.25	0.84	white	square	0	0
12	0.25	0.61	white	square	0	0
13	0.25	0.45	white	square	0	0
14	0.25	0.26	white	square	0	0
15	0.25	0.16	white	square	0	0')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = n.c_labels, ncol = n.c_labels, byrow = TRUE, data = 0)
M[3, 1] = "' '" # main arrow down the middle
M[5, 3] = "' '"
M[7, 5] = "' '"
M[10, 7] = "' '"
M[2, 11] = "' '" # side arrows
M[4, 12] = "' '"
M[6, 13] = "' '"
M[8, 14] = "' '"
M[9, 15] = "' '"

## make figure 
jpeg('figures/4_consort_flow.jpg', width=5, height=7, units='in', res=400, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = c_labels, lwd = 1, shadow.size=0, curve=0,
        dtext = 0.12, # controls the position of arrow text relative to arrowhead
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = 'black')
dev.off()
# eps version
postscript('figures/4_consort_flow.eps', width=5, height=7)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = c_labels, lwd = 1, shadow.size=0, curve=0,
        dtext = 0.12, # controls the position of arrow text relative to arrowhead
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = 'black')
dev.off()


library(cowplot)
library(Gmisc)
library(ggplot2)
library(grid)
library(gridExtra)
library(png)

grid.newpage()
# BD Flowchart
gp <- gpar(fill = alpha("lightcoral", 1), fontsize=20)
# create boxes
(pop <- boxGrob("Assume Stochastic\n Population Growth", 
 x=leftx-0.05, y=.9, box_gp = gp, txt_gp=gp))

(samp <- boxGrob("Sampling Frequency\n Information", 
 x=midx, y=.9, box_gp = gp, txt_gp=gp))
# connect boxes like this

(Tree <- boxGrob("Time-tree\n of samples", 
 x=rightx+0.05, y=.9, box_gp = gp, txt_gp=gp))

(Lhood <- boxGrob("Birth-Death\n Likelihood Function", 
 x=midx, y=.75, box_gp = gp, txt_gp=gp))

(Output <- boxGrob(expression('Output quantities:'~italic(lambda*delta*p)~'and'~italic(lambda-delta)), 
 x=midx, y=.6, box_gp = gp, txt_gp=gp))

(epi_op <- boxGrob("Rearrange for\n epidemiological parameters", 
 x=midx, y=.45, box_gp = gp, txt_gp=gp))

(T <- boxGrob(expression(italic(T==x[0]+tMRCA)), 
 x=leftx, y=.3, box_gp = gp, txt_gp=gp))
(Rnaught <- boxGrob(expression(italic(R[0]==frac(lambda,delta))), 
 x=midx-0.1, y=.2, box_gp = gp, txt_gp=gp))
(r <- boxGrob(expression(italic(r==lambda-delta)), 
 x=midx, y=.3, box_gp = gp, txt_gp=gp))
(db_time <- boxGrob(expression(italic(t[2]==frac(ln(2),r))), 
 x=midx+0.1, y=.2, box_gp = gp, txt_gp=gp))
(IT <- boxGrob(expression(italic(I(T)==e^(r*T))), 
 x=rightx, y=.3, box_gp = gp, txt_gp=gp))

connectGrob(pop, Lhood, "L")
connectGrob(samp, Lhood, "v")
connectGrob(Tree, Lhood, "L")
connectGrob(Lhood, Output, "v")
connectGrob(Output, epi_op, "v")
connectGrob(epi_op, T, "h")
connectGrob(epi_op, Rnaught, "v")
connectGrob(epi_op, r, "v")
connectGrob(epi_op, db_time, "v")
connectGrob(epi_op, IT, "h")

#dev.off()

bdFlowchart <- grid.grab()

grid.newpage()
# Coalescent flowchart

leftx <- .25
midx <- .5
rightx <- .75
width <- .2
gp <- gpar(fill = alpha("deepskyblue", 1), fontsize=20)
# create boxes
(pop <- boxGrob("Assume Deterministic\n Population Growth", 
 x=leftx-0.1, y=.9, box_gp = gp, txt_gp=gp))

(coal <- boxGrob("Coalescent Frequency\n conditioned on sampling times", 
 x=midx, y=.9, box_gp = gp, txt_gp=gp))
# connect boxes like this

(Tree <- boxGrob("Time-tree\n of samples", 
 x=rightx+0.1, y=.9, box_gp = gp, txt_gp=gp))

(Lhood <- boxGrob("Coalescent\n Likelihood Function", 
 x=midx, y=.75, box_gp = gp, txt_gp=gp))

(Output <- boxGrob(expression('Output quantities:'~italic(r)~'and'~italic(phi==frac(I(0),2*lambda))), 
 x=midx, y=.6, box_gp = gp, txt_gp=gp))

(epi_op <- boxGrob("Rearrange for\n epidemiological parameters", 
 x=midx, y=.45, box_gp = gp, txt_gp=gp))

(T <- boxGrob(expression(italic(tMRCA)), 
 x=leftx, y=.3, box_gp = gp, txt_gp=gp))
(Rnaught <- boxGrob(expression(italic(R[0]==r*D+1)), 
 x=midx-0.2, y=.1, box_gp = gp, txt_gp=gp))
(db_time <- boxGrob(expression(italic(t[2]==frac(ln(2),r))), 
 x=midx+0.2, y=.1, box_gp = gp, txt_gp=gp))
(I0 <- boxGrob(expression(italic(I(0)==e^(r*T))), 
 x=rightx, y=.3, box_gp = gp, txt_gp=gp))

connectGrob(pop, Lhood, "L")
connectGrob(samp, Lhood, "v")
connectGrob(Tree, Lhood, "L")
connectGrob(Lhood, Output, "v")
connectGrob(Output, epi_op, "v")
connectGrob(epi_op, T, "h")
connectGrob(epi_op, Rnaught, "v")
connectGrob(epi_op, db_time, "v")
connectGrob(epi_op, I0, "h")

ceFlowchart <- grid.grab()

# printing full figure
pdf(file='Flowchart.pdf', useDingbats=F, width=25, height=11)
	plot_grid(bdFlowchart, ceFlowchart, ncol=2, labels='AUTO', label_size=20)
dev.off()








library(ggplot2)
library(igraph)
library(ape)
library(cowplot)
library(ggtree)
library(rjson)
library(reshape)
library(tidytree)
library(ggraph)
library(tidyverse)
library(tidygraph)

# tree to get network from
set.seed(1234)
t <- rtree(4)
#t$root.edge=0.4

# adding full tree plot
plot.phylo(t, root.edge=T, node.pos=1, edge.width=1.2, show.tip.label=F, main='Complete Tree', cex.main=1.2)
nodelabels(LETTERS[1:3], frame='circle', bg=alpha('lightcoral', 1), cex=1.5)
tiplabels(LETTERS[4:7], frame='circle', bg=c(rep(alpha('deepskyblue', 1), times=3), alpha('lightcoral', 1)), cex=1.5)
tFull <- recordPlot()

# adding sampled tree plot
plot.phylo(drop.tip(t, 3, root.edge=1), root.edge=T, node.pos=1, edge.width=1.2, show.tip.label=F, main='Sample Tree', cex.main=1.2)
nodelabels(LETTERS[1:2], frame='circle', bg=alpha('lightcoral', 1), cex=1.5)
tiplabels(LETTERS[c(4,5,6)], frame='circle', bg=alpha('deepskyblue', 1), cex=1.5) 
tSamp <- recordPlot()

# epi netowrk, reconstructed from complete tree
edge_list <- tibble(from = c(1, 1, 2, 2, 3, 3, 4, 5, 6, 7), to = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
					label=c(rep('lambda', times=6), rep('psi', times=3), 'mu'))
node_list <- tibble(id = 1:11, label=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'D', 'E', 'F', 'G'))
colNodes <- rep('lightcoral', 11)
colNodes[c(8:10)] <- rep('deepskyblue', 4)
routes_tidy <- tbl_graph(nodes = node_list, edges = edge_list, directed = TRUE)

net <-  ggraph(routes_tidy, circular=T) + 
			geom_edge_link(aes(label=label), label_parse=T,
							angle_calc = 'along',
		                   	label_dodge = unit(3, 'mm'),
		                   	arrow = arrow(length = unit(4, 'mm')), 
		                   	end_cap = circle(6, 'mm'))  + 
			geom_node_point(shape=21, size=10, fill=colNodes) + 
			geom_node_text(aes(label = label), size=5) + 
			ggtitle('Transmission Netowork') +
	        theme(legend.position = "none",
	        	panel.background = element_blank(),
	        	plot.title=element_text(hjust = 0.5, face='bold', size=14))

# epi traj fig
traj <- fromJSON(file = 'BD.nwk.json')
sir <- as.data.frame(cbind(traj$t, traj$I, traj$I_sampled))
colnames(sir) <- c('time', "I", 'sampI')
sir <- melt(sir, id='time')

curve <- ggplot(sir) + geom_step(aes(x=time, y=value, col=variable), lwd=1.2) +
				ylab('Compartment Size') + xlab('Time (arbitrary units)') + ggtitle('Example Case Trajectory') +
				scale_colour_manual(values=c(alpha('lightcoral', 1), alpha('deepskyblue', 1)), labels=c('Total Infections','Sampled Infections')) +
				theme(legend.position='bottom',
						legend.title=element_blank(),
						axis.title.x=element_text(size=14),
						axis.title.y=element_text(size=14),
						axis.text.y=element_text(size=14),
						axis.text.x=element_text(size=14),
						legend.text=element_text(size=14),
						panel.background = element_blank(), 
						panel.grid.major.x = element_line('grey'), 
						#panel.grid.minor.x = element_blank(),
						panel.grid.major.y = element_line('grey'), 
						#panel.grid.minor.y = element_blank(),
						strip.text = element_text(size=14, face='bold'),
						plot.title=element_text(hjust = 0.5, face='bold', size=14))



# plotting 
pdf(file='epiFig.pdf', useDingbats=F, width=12, height=12)
	plot_grid(curve, net, tFull, tSamp, ncol=2, labels="AUTO")
dev.off()










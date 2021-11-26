library(ape)
library(phytools)
library(TreeSim)
library(NELSI)
library(phangorn)
library(rjson)
library(cowplot)

# function below for plotting coalescent events at node heights
get.ordinates <- function(tr){
    nodeHeights <- allnode.times(tr, reverse = T)
    nodeHeights <- nodeHeights[!names(nodeHeights) %in% 1:length(tr$tip.label)]
    ordinates <- matrix(NA, tr$Nnode, 2)
    ordinates[, 1] <- seq(from = (tr$Nnode+length(tr$tip.label)), by = -1, length.out = nrow(ordinates))
    tips <- 1:length(tr$tip.label)
    for(i in 1:nrow(ordinates)){
        descendants <- get.descending.nodes.branches(tr, ordinates[i, 1])$descending.nodes[2:3]
        areTips <- descendants %in% tips
        if(all(areTips)){
            ordinates[i, 2] <- mean(descendants)
        }else if(sum(areTips) == 1){
            ordNode <- ordinates[ordinates[, 1] == descendants[!areTips], 2]
            ordinates[i, 2] <- mean(c(ordNode, descendants[areTips]))
        }else if(any(!areTips)){
            ordinates[i, 2] <- mean(c(ordinates[ordinates[, 1] == descendants[1], 2],
                                      ordinates[ordinates[, 1] == descendants[2], 2]))
        }
    }
    ordinates <- rbind(ordinates, cbind(length(tr$tip.label):1, length(tr$tip.label):1))
    ordinates <- ordinates[nrow(ordinates):1, ]
    ordinates <- cbind(allnode.times(tr), ordinates)
    colnames(ordinates) <- c('x.coord', 'node.index', 'y.coord')
    return(ordinates)
}

# input tree and trajectory 
tr <- read.tree('BD.nwk.tre')
traj <- fromJSON(file = 'BD.nwk.json')

# total stochastic pop trajectory: BD curves
plot(x = traj$t, y = traj$I, type = 's',
	xlab = 'Time', ylab = 'Compartment Size', main='Birth-Death', cex.main=1.5, col = 'black', 
	las = 1, cex.lab = 1.5, cex.axis = 1.5)
points(x = traj$t, y = traj$I_sampled, type = 's', lty = 1, cex = 1.5, col = 'lightcoral')
legend('topleft', 
	legend = c(expression(italic('I(t)')), expression(italic(' Sampled I(t)'))),
	col = c('black', 'lightcoral'), lty = c(1,1),
	cex = 1.5, bty = "n")
bdTraj <- recordPlot()

# tree: BD with sampling events
tip_ages <- allnode.times(tr, tipsonly = T)+tr$root.edge
plot(tr, root = T, show.tip.label = F, bty = 'n', edge.width= 2.5)

for(i in 1:length(tip_ages)){
    lines(rep(tip_ages[i], 2), c(-2, i), lty = 2, lwd = 1, col = 'lightcoral')
}
axis(side = 1, at = c(tip_ages), labels = F, col.ticks = 'lightcoral', lwd.ticks = 0.5)
axis(side = 1, at = c(0,5), labels = c(0,5), col.ticks = 'black', cex.axis=1.5)
mtext('Time of sampling events', side = 1, line = 2, cex=1.5)
bdTree <- recordPlot()


# Coalescent curve
curve(exp((1)*x), from = 0, to = 5,
	ylab = 'Compartment Size', xlab = 'Time', main='Coalescent exponential', cex.main=1.5, lwd = 2, col = 'black',
	cex.lab = 1.5, cex.axis = 1.5, las = 1)
legend('topleft', 
	legend = c(expression(italic('I(t)')~exponential~model)),
	col = c('black'), lty = c(1),
	cex = 1.5, bty = "n")
ceTraj <- recordPlot()

# tree: CE with coalescent events
plot(tr, root = T, show.tip.label = F, bty = 'n', edge.width= 2.5)

ordinates <- get.ordinates(tr)

for(i in length(tr$tip.label):nrow(ordinates)){
    lines(rep(ordinates[i, 1]+tr$root.edge, 2), c(-2, ordinates[i, 3]),
      col = 'deepskyblue', lty = 2)
}

axis(side = 1, at = c(ltt_coal[,1]), labels = F, col.ticks = 'deepskyblue', lwd.ticks = 0.5)
axis(side = 1, at = c(0,5), labels = c(0,5), col.ticks = 'black', cex.axis=1.5)
mtext('Time of coalescent events', side = 1, line = 2, cex=1.5)
ceTree <- recordPlot()

# saving panel plot
pdf(file='demographicAssumptions.pdf', useDingbats=F, height=14, width=14)
	plot_grid(bdTraj, ceTraj, bdTree, ceTree, ncol=2, labels='AUTO', align='v')
dev.off()











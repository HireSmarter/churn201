library(ggplot2)
library(gridExtra)
library(scales)
library(manipulate)

# set up my defaults in a global list
def <- list()
def$max.yrs <- 3
def$max.benefit <- 0.5
def$cost.ramp <- 3.5
def$cost.scale <- 2
def$salary <- 0.5
def$shape.good <- 2.5
def$scale.good <- 1.5
def$shape.bad <- 1.66
def$scale.bad <- 0.33
def$good.bad.ratio <- 0.6
def$max.yrs <- 3
def$col.benefit <- "DarkGreen"
def$col.cost <- "DarkRed"
def$col.good <- "RoyalBlue2"
def$col.bad <- "DarkOrange2"
def$col.be <- "SteelBlue"
def$col.be.cume <- "SteelBlue"


# run the sim with manipulators
manipSim202 <- function() {
	manipulate(runSim202(max.yrs, max.benefit, 
						 cost.ramp, cost.scale, salary,
						 shape.good, scale.good, shape.bad, scale.bad, 
						 good.bad.ratio),
			   good.bad.ratio = slider(0.0, 1, initial=def$good.bad.ratio),
			   shape.good = slider(1.01, 5, initial=def$shape.good), 
			   scale.good = slider(0.01, 5, initial=def$scale.good), 
			   shape.bad = slider(1.01, 5, initial=def$shape.bad), 
			   scale.bad = slider(0.01, 5, initial=def$scale.bad), 
			   max.benefit = slider(0.25, 3, initial =def$max.benefit),
			   cost.ramp = slider(0.5, 5, initial=def$cost.ramp), 
			   cost.scale = slider(0.5, 5, initial=def$cost.scale), 
			   salary = slider(0.0, 1, initial=def$salary),
			   max.yrs = slider(1, 8, initial=def$max.yrs))
}

# run the sim, return the plots and print out cume info
runSim202 <- function(max.yrs=def$max.yrs, max.benefit=def$max.benefit, 
					  cost.ramp=def$cost.ramp, cost.scale=def$cost.scale, salary=def$salary,
					  shape.good=def$shape.good, scale.good=def$scale.good, 
					  shape.bad=def$shape.bad, scale.bad=def$scale.bad, 
					  good.bad.ratio=def$good.bad.ratio) {

	# divide our years uniformly, 100 pts a year
	dist.year <- as.data.frame( 0:(max.yrs*100)/100 )
	names(dist.year) <- "tenure"
	dist.year$benefit <- empBenefit(dist.year$tenure, max.benefit)
	dist.year$benefit.cume <- empBenefitCume(dist.year$tenure, max.benefit)
	dist.year$cost <- empCost(dist.year$tenure, cost.ramp, cost.scale, salary)
	dist.year$cost.cume <- empCostCume(dist.year$tenure, cost.ramp, cost.scale, salary)
	dist.year$prob.good <- dweibull(dist.year$tenure, shape=shape.good, scale=scale.good)
	dist.year$prob.bad <- dweibull(dist.year$tenure, shape=shape.bad, scale=scale.bad)
	dist.year$prob.good.wt <- dist.year$prob.good * good.bad.ratio
	dist.year$prob.bad.wt <- dist.year$prob.bad * (1-good.bad.ratio)

	# calc breakeven points
	be.pt.id <- which.max(dist.year$benefit - dist.year$cost>0)
	be.pt <- dist.year$tenure[be.pt.id]
	be.cume.id <- which.max(dist.year$benefit.cume - dist.year$cost.cume>0)
	be.cume <- dist.year$tenure[be.cume.id]

	fig1 <- suppressWarnings(ggplot(data=dist.year, aes(x=tenure)) + 
							 geom_vline(xintercept=be.pt, col=def$col.be, size=0.5, linetype="dashed") +
							 geom_vline(xintercept=be.cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
							 geom_ribbon(fill=def$col.cost, size=0, aes(ymax=cost,ymin=benefit,alpha=cost>benefit)) + 
							 scale_alpha_discrete(range=c(0,.25)) + 
							 theme(legend.position="none") +
							 geom_line(col=def$col.cost, size=1, aes(y=cost)) + 
							 geom_line(col=def$col.benefit, size=1, aes(y=benefit)) +
							 scale_y_continuous(labels = percent) +
							 theme_bw() +
							 theme(legend.position="none") +
							 labs(title="Monthly Benefit & Cost from One Employee", 
								  x="Tenure in Years", 
								  y="% Potential Monthly Value"))
			# TODO annotate with labels
			# TODO annotate with breakeven numbers be.pt, be.cume

	fig2 <- suppressWarnings(ggplot(data=dist.year, aes(x=tenure)) + 
							 geom_vline(xintercept=be.pt, col=def$col.be, size=0.5, linetype="dashed") +
							 geom_vline(xintercept=be.cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
							 geom_line(aes(y=prob.bad), col=def$col.bad, size=1) +
							 geom_line(aes(y=prob.good), col=def$col.good, size=1) +
							 scale_y_continuous(labels = percent) +
							 theme_bw() +
							 xlim(c(0,max.yrs)) +
							 labs(title="Probability of Employee Termination", 
								  x="Tenure in Years", 
								  y="Probability"))
			# TODO annotate with color for lines

	fig3 <- suppressWarnings(ggplot(data=dist.year, aes(x=tenure)) + 
							 geom_vline(xintercept=be.pt, col=def$col.be, size=0.5, linetype="dashed") +
							 geom_vline(xintercept=be.cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
							 geom_hline(yintercept=0, col=def$col.be.cume, size=0.5, linetype="dotted") +
							 geom_ribbon(fill=def$col.bad, size=0, alpha=0.5, ymin=0,
										 aes(ymax=(benefit.cume-cost.cume)*prob.bad.wt)) + 
							 geom_line(aes(y=(benefit.cume-cost.cume)*prob.bad.wt), col=def$col.bad, size=1) +
							 geom_ribbon(fill=def$col.good, size=0, alpha=0.5, ymin=0,
										 aes(ymax=(benefit.cume-cost.cume)*prob.good.wt)) + 
							 geom_line(aes(y=(benefit.cume-cost.cume)*prob.good.wt), col=def$col.good, size=1) +
							 scale_y_continuous(labels = percent) +
							 theme_bw() +
							 xlim(c(0,max.yrs)) +
							 labs(title="Expected Cumulative Net Benefit", 
								  x="Tenure in Years", 
								  y="Cumulative Net Benefit x Probability"))
			# TODO annotate with color for lines
			# TODO annotate with cume result

	fig.all <- arrangeGrob(fig1, fig2, fig3, ncol=1)

	runPredNetCume(max.benefit, cost.ramp, cost.scale, salary,
				  shape.good, scale.good, shape.bad, scale.bad, 
				  good.bad.ratio, verbose=TRUE)

	return(fig.all)
}

# vector-friendly benefit from employee, modeled as a sigmoid function
empBenefit <- function(tenure, max.benefit) {
	1/(1+exp(-(tenure/max.benefit*12-6)))
}

# vector friendly cumulative benefit, the integral of empBenefit
empBenefitCume <- function(tenure, max.benefit) {
	# use sapply to make integrate is vector-friendly
	sapply(tenure, function(x) { integrate(empBenefit, 0, x, 
										   max.benefit=max.benefit)$value })
}

# vector-friendly cost of employee, modeled as a gompertz function
empCost <- function(tenure, cost.ramp, cost.scale, salary) {
	exp(-exp(cost.ramp * tenure)) * cost.scale + salary
}

# vector friendly cumulative cost, the integral of empCost
empCostCume <- function(tenure, cost.ramp, cost.scale, salary) {
	# use sapply to make integrate is vector-friendly
	sapply(tenure, function(x) { integrate(empCost, 0, x, 
										   cost.ramp=cost.ramp, 
										   cost.scale=cost.scale, 
										   salary=salary)$value })
}

# one function for one tenure moment, for use by empPredNetCume and graphing
empPredNet <- function(tenure, 
					   max.yrs, max.benefit, 
					   cost.ramp, cost.scale, salary,
					   d.shape, d.scale) {

	# net = benefit - cost
	z.net <- empBenefit(tenure, max.benefit) - empCost(tenure, cost.ramp, cost.scale, salary)

	# weighted by probability of that tenure position
	z.prob <- dweibull(tenure, shape=d.shape, scale=d.scale)

	return(z.net * z.prob)
}

# the sum of all net benefits given these settings
empPredNetCume <- function(max.benefit, 
						   cost.ramp, cost.scale, salary, 
						   d.shape, d.scale) {
	integrate(empPredNet, 0, Inf,
			  max.benefit=max.benefit,
			  cost.ramp=cost.ramp, 
			  cost.scale=cost.scale, 
			  salary=salary,
			  d.shape=d.shape,
			  d.scale=d.scale
			  )$value
}

# run the sim for just the cume value, maybe print out values, return cume
runPredNetCume <- function(max.benefit = def$max.benefit, 
						  cost.ramp = def$cost.ramp, cost.scale = def$cost.scale, salary = def$salary,
						  shape.good = def$shape.good, scale.good = def$scale.good, 
						  shape.bad = def$shape.bad, scale.bad = def$scale.bad,
						  good.bad.ratio = def$good.bad.ratio,
						  verbose=FALSE) {

	if (FALSE & verbose) {
		writeLines(sprintf("Sim for max.benefit = %.2f, cost.ramp = %.2f, cost.scale = %.2f, salary = %.2f",
						   max.benefit, cost.ramp, cost.scale, salary))
		writeLines(sprintf("  shape.good = %.2f, scale.good = %.2f, shape.bad = %.2f, scale.bad = %.2f",
						   shape.good, scale.good, shape.bad, scale.bad))
		writeLines(sprintf("  good.bad.ratio = %.2f", good.bad.ratio))
	}

	cume.good <- empPredNetCume(max.benefit, cost.ramp, cost.scale, salary, 
								shape.good, scale.good) *  good.bad.ratio
	cume.bad <- empPredNetCume(max.benefit, cost.ramp, cost.scale, salary, 
							   shape.bad, scale.bad) * (1-good.bad.ratio)

	cume.total <- cume.good + cume.bad

	if (verbose) {
		writeLines(sprintf("cume.good = %.1f%%, cume.bad = %.1f%%, total = %.1f%%", 
						   cume.good * 100, cume.bad * 100, cume.total * 100))
	}

	return(cume.total)
}

# modify one variable through its range to calc sensitivity
# limited analysis - just based on the "reasonable" starting point
runSensitivityTests <- function() {
	writeLines("Running Sensitivity Tests")

	# modify good.bad.ratio from 0-1 from base of 0.3
	writeLines(sprintf("  good.bad.ratio:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$good.bad.ratio, 
					   (runPredNetCume(good.bad.ratio=def$good.bad.ratio*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0, 1, 0.05)
	g.gbr <- sensitivityPlot("Good.Bad.Ratio", def$good.bad.ratio, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(good.bad.ratio=x)}))

	# modify shape.good from 1-5 from base of 2.5 
	writeLines(sprintf("  shape.good:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$shape.good, 
					   (runPredNetCume(shape.good=def$shape.good*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(1, 5, 0.05)
	g.shg <- sensitivityPlot("Shape.Good", def$shape.good, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(shape.good=x)}))

	# modify scale.good from 0.01-5 from base of 1.5 
	writeLines(sprintf("  scale.good:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$scale.good, 
					   (runPredNetCume(scale.good=def$scale.good*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.01, 5, 0.05)
	g.scg <- sensitivityPlot("Scale.Good", def$scale.good, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(scale.good=x)}))

	# modify shape.bad from 1-5 from base of 1.66 
	writeLines(sprintf("  shape.bad:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$shape.bad, 
					   (runPredNetCume(shape.bad=def$shape.bad*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(1, 5, 0.05)
	g.shb <- sensitivityPlot("Shape.Bad", def$shape.bad, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(shape.bad=x)}))

	# modify scale.bad from 0.01-5 from base of 0.33 
	writeLines(sprintf("  scale.bad:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$scale.bad, 
					   (runPredNetCume(scale.bad=def$scale.bad*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.01, 5, 0.05)
	g.scb <- sensitivityPlot("Scale.Bad", def$scale.bad, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(scale.bad=x)}))

	# modify max.benefit from 0.25-3, initial = 0.5
	writeLines(sprintf("  max.benefit:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$max.benefit, 
					   (runPredNetCume(max.benefit=def$max.benefit*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.25, 3, 0.05)
	g.mb <- sensitivityPlot("Max.Benefit", def$max.benefit, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(max.benefit=x)}))

	# modify cost.ramp from 0.5-5 from base of 3.5 
	writeLines(sprintf("  cost.ramp:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$cost.ramp, 
					   (runPredNetCume(cost.ramp=def$cost.ramp*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.5, 5, 0.05)
	g.cr <- sensitivityPlot("Cost.Ramp", def$cost.ramp, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(cost.ramp=x)}))

	# modify cost.scale from 0.5-5 from base of 2 
	writeLines(sprintf("  cost.scale:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$cost.scale, 
					   (runPredNetCume(cost.scale=def$cost.scale*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.5, 5, 0.05)
	g.cs <- sensitivityPlot("Cost.Scale", def$cost.scale, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(cost.scale=x)}))

	# modify salary from 0-1 from base of 0.5
	writeLines(sprintf("  salary:\t1%% change from %.2f results in %.2f%% change in ENCB",
					   def$salary, 
					   (runPredNetCume(salary=def$salary*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0, 1, 0.05)
	g.sa <- sensitivityPlot("Salary", def$salary, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(salary=x)}))

	# plot all of them
	writeLines("  making plot")
	fig.all <- arrangeGrob(g.gbr, g.shg, g.scg, 
						   g.shb, g.scb, g.mb, 
						   g.cr, g.cs, g.sa, ncol=3)

	return(fig.all)
}
	
sensitivityPlot <- function(label, def.value, input, output) {
	zd <- data.frame(input=input, output=output)
	zg <- suppressWarnings(ggplot(data=zd, aes(x=input, y=output)) + 
						   geom_vline(xintercept=def.value, col="SteelBlue", linetype="dashed") +
						   geom_hline(yintercept=0, col="DarkRed", linetype="dotted") +
						   geom_line(col="DarkGreen", size=1) +
						   theme_bw() +
						   labs(x=label, y="Exp Net Cume Benefit"))
	return(zg)
}

runHistograms <- function(sample=1000,
						  good.bad.ratio = def$good.bad.ratio, 
						  shape.good = def$shape.good, 
						  scale.good = def$scale.good, 
						  shape.bad = def$shape.bad, 
						  scale.bad = def$scale.bad) {

	good.fit <- rweibull(sample * good.bad.ratio, shape=shape.good, scale=scale.good)
	bad.fit <- rweibull(sample * (1-good.bad.ratio), shape=shape.bad, scale=scale.bad)

	fig1 <- ggplot(data=data.frame(tenure=c(good.fit, bad.fit)), aes(x=tenure)) + 
				geom_histogram(binwidth=1/12, fill=def$col.benefit) + 
				xlim(c(0,3)) +
				theme_bw() +
				theme(plot.title = element_text(size = 10),
					  axis.title = element_text(size = 8)) +
				labs(title="All Employees", 
					 x="Tenure in Years", 
					 y="Count")

	fig2 <- ggplot(data=data.frame(tenure=good.fit), aes(x=tenure)) + 
				geom_histogram(binwidth=1/12, fill=def$col.good) + 
				xlim(c(0,3)) +
				theme_bw() +
				theme(plot.title = element_text(size = 10),
					  axis.title = element_text(size = 8)) +
				labs(title="'Good Fit' Employees", 
					 x="Tenure in Years", 
					 y="Count")

	fig3 <- ggplot(data=data.frame(tenure=bad.fit), aes(x=tenure)) + 
				geom_histogram(binwidth=1/12, fill=def$col.bad) + 
				xlim(c(0,3)) +
				theme_bw() +
				theme(plot.title = element_text(size = 10),
					  axis.title = element_text(size = 8)) +
				labs(title="'Bad Fit' Employees", 
					 x="Tenure in Years", 
					 y="Count")

	fig.all <- arrangeGrob(fig1, fig2, fig3, main="Employment Tenure", ncol=1)

	return(fig.all)
}

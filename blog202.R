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
def$good.bad.ratio <- 0.3
def$max.yrs <- 3

# run the sim with manipulators
manipSim202 <- function() {
	manipulate(runSim202(max.yrs, max.benefit, 
						 cost.ramp, cost.scale, salary,
						 shape.good, scale.good, shape.bad, scale.bad, 
						 good.bad.ratio),
			   good.bad.ratio = slider(0, 1, initial=def$good.bad.ratio),
			   shape.good = slider(1, 5, initial=def$shape.good), 
			   scale.good = slider(0.01, 5, initial=def$scale.good), 
			   shape.bad = slider(1, 5, initial=def$shape.bad), 
			   scale.bad = slider(0.01, 5, initial=def$scale.bad), 
			   max.benefit = slider(0.25, 3, initial =def$max.benefit),
			   cost.ramp = slider(0.5, 5, initial=def$cost.ramp), 
			   cost.scale = slider(0.5, 5, initial=def$cost.scale), 
			   salary = slider(0, 1, initial=def$salary),
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

	# calc breakeven points
	be.pt.id <- which.max(dist.year$benefit - dist.year$cost>0)
	be.pt <- dist.year$tenure[be.pt.id]
	be.cume.id <- which.max(dist.year$benefit.cume - dist.year$cost.cume>0)
	be.cume <- dist.year$tenure[be.cume.id]

	col.benefit <- "DarkGreen"
	col.cost <- "DarkRed"
	col.good <- "RoyalBlue2"
	col.bad <- "DarkOrange2"
	col.be <- "SteelBlue"
	col.be.cume <- "SteelBlue"

	fig1 <- suppressWarnings(ggplot(data=dist.year, aes(x=tenure)) + 
							 geom_vline(xintercept=be.pt, col=col.be, size=0.5, linetype="dashed") +
							 geom_vline(xintercept=be.cume, col=col.be.cume, size=0.5, linetype="dashed") +
							 geom_ribbon(fill=col.cost, size=0, aes(ymax=cost,ymin=benefit,alpha=cost>benefit)) + 
							 scale_alpha_discrete(range=c(0,.25)) + 
							 theme(legend.position="none") +
							 geom_line(col=col.cost, size=1, aes(y=cost)) + 
							 geom_line(col=col.benefit, size=1, aes(y=benefit)) +
							 scale_y_continuous(labels = percent) +
							 theme_bw() +
							 theme(legend.position="none") +
							 labs(title="Monthly Benefit & Cost from One Employee", 
								  x="Tenure in Years", 
								  y="% Potential Monthly Value"))
			# TODO annotate with labels
			# TODO annotate with breakeven numbers be.pt, be.cume

	fig2 <- suppressWarnings(ggplot(data=dist.year, aes(x=tenure)) + 
							 geom_vline(xintercept=be.pt, col=col.be, size=0.5, linetype="dashed") +
							 geom_vline(xintercept=be.cume, col=col.be.cume, size=0.5, linetype="dashed") +
							 geom_line(aes(y=prob.bad), col=col.bad, size=1) +
							 geom_line(aes(y=prob.good), col=col.good, size=1) +
							 scale_y_continuous(labels = percent) +
							 theme_bw() +
							 xlim(c(0,max.yrs)) +
							 labs(title="Probability of Employee Termination", 
								  x="Tenure in Years", 
								  y="Probability"))
			# TODO annotate with color for lines

	fig3 <- suppressWarnings(ggplot(data=dist.year, aes(x=tenure)) + 
							 geom_vline(xintercept=be.pt, col=col.be, size=0.5, linetype="dashed") +
							 geom_vline(xintercept=be.cume, col=col.be.cume, size=0.5, linetype="dashed") +
							 geom_hline(yintercept=0, col=col.be.cume, size=0.5, linetype="dotted") +
							 geom_ribbon(fill=col.bad, size=0, alpha=0.5, ymin=0,
										 aes(ymax=(benefit.cume-cost.cume)*prob.bad)) + 
							 geom_line(aes(y=(benefit.cume-cost.cume)*prob.bad), col=col.bad, size=1) +
							 geom_ribbon(fill=col.good, size=0, alpha=0.5, ymin=0,
										 aes(ymax=(benefit.cume-cost.cume)*prob.good)) + 
							 geom_line(aes(y=(benefit.cume-cost.cume)*prob.good), col=col.good, size=1) +
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

	if (verbose) {
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
	# modify good.bad.ratio from 0-1 from base of 0.3
	z.in <- seq(0, 1, 0.05)
	g.gbr <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(good.bad.ratio=x)}),
							 "Good.Bad.Ratio")

	# modify shape.good from 1-5 from base of 2.5 
	z.in <- seq(0, 5, 0.05)
	g.shg <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(shape.good=x)}),
							 "Shape.Good")

	# modify scale.good from 0.01-5 from base of 1.5 
	z.in <- seq(0, 5, 0.05)
	g.scg <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(scale.good=x)}),
							 "Scale.Good")

	# modify shape.bad from 1-5 from base of 1.66 
	z.in <- seq(0, 5, 0.05)
	g.shb <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(shape.bad=x)}),
							 "Shape.Bad")

	# modify scale.bad from 0.01-5 from base of 0.33 
	z.in <- seq(0, 5, 0.05)
	g.scb <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(scale.bad=x)}),
							 "Scale.Bad")

	# modify max.benefit from 0.25-3, initial = 0.5
	z.in <- seq(0.25, 3, 0.05)
	g.mb <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(max.benefit=x)}),
							 "Max.Benefit")

	# modify cost.ramp from 0.5-5 from base of 2 
	z.in <- seq(0.5, 5, 0.05)
	g.cr <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(cost.ramp=x)}),
							 "Cost.Ramp")
	# modify cost.scale from 0.5-5 from base of 2 
	z.in <- seq(0.5, 5, 0.05)
	g.cs <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(cost.scale=x)}),
							 "Cost.Scale")

	# modify salary from 0-1 from base of 0.5
	z.in <- seq(0, 1, 0.05)
	g.sa <- sensitivityPlot(z.in, 
							 sapply(z.in, function(x) {runPredNetCume(salary=x)}),
							 "Salary")

	# plot all of them
	fig.all <- arrangeGrob(g.gbr, ncol=1)

	return(fig.all)
}
	
sensitivityPlot <- function(input, output, label) {
	zt <- data.frame(input=input, output=output)
	zg <- suppressWarnings(ggplot(data=t.gbr, aes(x=input, y=output)) + 
						   geom_point(col="DarkGreen") +
						   geom_smooth(method="loess") +
						   theme_bw() +
						   labs(x=label))
	return(zg)
}

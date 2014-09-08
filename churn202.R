library(ggplot2)
library(gridExtra)
library(scales)
library(manipulate)
library(reshape)
library(survival)
library(GGally)

setDefaults <- function() {
	gpal <- scales::hue_pal(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction=1)(8)

	# set up my defaults in a global list
	def <- list()
	def$max.benefit <- 1.5
	def$cost.ramp <- 1.5
	def$cost.scale <- 2
	def$salary <- 0.5
	def$shape.good <- 2.5
	def$scale.good <- 2.5
	def$shape.bad <- 1.66
	def$scale.bad <- 0.63
	def$good.bad.ratio <- 0.6
	def$max.yrs <- 4
	def$col.benefit <- gpal[3]	# dark green
	def$col.cost <- gpal[1]		# salmon
	def$col.good <- gpal[5]		# blue-grey
	def$col.bad <- gpal[2]		# brown
	def$col.other1 <- gpal[7]	# purple
	def$col.other2 <- gpal[6]	# blue
	def$col.be <- "DarkGray"
	def$col.be.cume <- "DarkGray"

	return(def) 
}

manipSim202 <- function() {
	# run the sim with manipulators
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

runSim202 <- function(max.yrs=def$max.yrs, max.benefit=def$max.benefit, 
					  cost.ramp=def$cost.ramp, cost.scale=def$cost.scale, salary=def$salary,
					  shape.good=def$shape.good, scale.good=def$scale.good, 
					  shape.bad=def$shape.bad, scale.bad=def$scale.bad, 
					  good.bad.ratio=def$good.bad.ratio,
					  do.annotate=FALSE) {
	# run the sim, return the plots and print out cume info

	dist.year <- calcDist(max.yrs, max.benefit, cost.ramp, cost.scale, salary,
					  shape.good, scale.good, shape.bad, scale.bad, good.bad.ratio)

	break.even <- calcBreakeven(dist.year)

	evh <- runPredNetCume(max.benefit, cost.ramp, cost.scale, salary,
				  shape.good, scale.good, shape.bad, scale.bad, 
				  good.bad.ratio, verbose=TRUE)

	fig4 <- g.probTerm(dist.year, break.even, max.yrs, do.annotate)
	fig5 <- g.costBenefit(dist.year, break.even, max.yrs, do.annotate)
	fig6 <- g.cumeValue(dist.year, break.even, max.yrs, do.annotate)
	fig7 <- g.expCume(dist.year, break.even, evh, max.yrs, do.annotate)

	fig45 <- arrangeGrob(fig4, fig5, main="Employee Tenure, Benefit, and Costs", ncol=1)
	fig67 <- arrangeGrob(fig6, fig7, main="Employee Cumulative Net Benefit", ncol=1)
	fig4567 <- arrangeGrob(fig45, fig67, ncol=2)

	return(fig4567)

}

genTimeline <- function(emp=150) {
	days <- def$max.yrs*365
	target <- 2014.2

	d.t <- data.frame(id=1:emp)

	# fake hire date
	d.t$hire <- sample(days, emp, replace=TRUE)/365 + 2010
	# fake tenure from weibulls
	d.t$tenure.yrs <- c(rweibull(def$good.bad.ratio * emp, shape=def$shape.good, scale=def$scale.good),
						rweibull((1-def$good.bad.ratio) * emp, shape=def$shape.bad, scale=def$scale.bad))
	d.t$tenure.yrs <- sapply(d.t$tenure.yrs, function(x) { max(0.1,x)})
	# fake emp.type
	d.t$emp.type <- factor(ifelse(d.t$id <= def$good.bad.ratio * emp,"group.a","group.b"))
	# fake term date
	d.t$term <- d.t$hire + d.t$tenure.yrs
	# is.term
	d.t$is.term <- d.t$term <= target
	# term.class
	d.t$term.class <- factor(ifelse(d.t$is.term, "already.term", "still.working"))
	# randomized order 0-1
	d.t$randex <- sample(nrow(d.t),nrow(d.t)) / nrow(d.t)
	return(d.t)
}

calcDist <- function(max.yrs=def$max.yrs, max.benefit=def$max.benefit, 
					  cost.ramp=def$cost.ramp, cost.scale=def$cost.scale, salary=def$salary,
					  shape.good=def$shape.good, scale.good=def$scale.good, 
					  shape.bad=def$shape.bad, scale.bad=def$scale.bad, 
					  good.bad.ratio=def$good.bad.ratio) {
	
	### TIME
	# divide our years uniformly, 100 pts a year (from now one day = 1/100 year)
	dist.year <- data.frame( tenure=0:(max.yrs*100)/100 )

	### BENEFIT
	# daily employee benefit 
	dist.year$benefit <- empBenefit(dist.year$tenure, max.benefit)
	# cumulative emp benefit
	dist.year$benefit.cume <- empBenefitCume(dist.year$tenure, max.benefit)

	### BENEFIT
	# daily emp cost
	dist.year$cost <- empCost(dist.year$tenure, cost.ramp, cost.scale, salary)
	# cumulative emp cost
	dist.year$cost.cume <- empCostCume(dist.year$tenure, cost.ramp, cost.scale, salary)

	### COST/BENEFIT BREAKOUT
	# salary costs per "day" (/100)
	dist.year$salary <- salary
	# non-salary costs per "day"
	dist.year$non.salary <- dist.year$cost - salary

	# cost of learning - cost of being less productive than optimal benefit 1
	dist.year$learning <- 1 - dist.year$benefit

	# cumulative amount "owed" by being cost>benefit
	# need to use cume because the integration is correct, not dailies! (just by a small fraction)
	dist.year$debt <- dist.year$cost.cume - dist.year$benefit.cume
	dist.year$debt[dist.year$debt < 0] <- 0

	# the employer profit
	dist.year$profit <- dist.year$benefit - dist.year$cost
	dist.year$profit[dist.year$profit < 0] <- 0

	# debt.payment is slightly off because of integration vs. daily
	# so sum(dist.year$debt.payment) is close but not exactly = sum(dist.year$debt)
	dist.year$debt.payment <- ifelse(dist.year$debt > 0 & dist.year$profit > 0,
									 dist.year$profit, 0)
	dist.year$profit <- dist.year$profit - dist.year$debt.payment

	### PROBABILITIES
	# prob of good.fit terminating today, within good.fit sample
	dist.year$prob.good <- dweibull(dist.year$tenure, shape=shape.good, scale=scale.good)
	# prob of bad.fit terminating today, within bad.fit sample
	dist.year$prob.bad <- dweibull(dist.year$tenure, shape=shape.bad, scale=scale.bad)

	# prob of good.fit terminating today, within whole sample
	dist.year$prob.good.wt <- dist.year$prob.good * good.bad.ratio
	# prob of bad terminating today, within whole sample
	dist.year$prob.bad.wt <- dist.year$prob.bad * (1-good.bad.ratio)

	### CHANGE MIX BY 10%
	# prob of good.fit terminating today, within whole sample
	dist.year$prob.good.wt2 <- dist.year$prob.good * (good.bad.ratio + 0.1)
	# prob of bad terminating today, within whole sample
	dist.year$prob.bad.wt2 <- dist.year$prob.bad * (1-good.bad.ratio-0.1)

	### CHANGE MIX BY 10%
	# prob of good.fit terminating today, within whole sample
	dist.year$prob.good.wt2 <- dist.year$prob.good * (good.bad.ratio + 0.1)
	# prob of bad terminating today, within whole sample
	dist.year$prob.bad.wt2 <- dist.year$prob.bad * (1-good.bad.ratio-0.1)

	### CDF for survival
	dist.year$cdf.good <- cumsum(dist.year$prob.good/100)
	dist.year$cdf.bad <- cumsum(dist.year$prob.bad/100)

	return(dist.year)
}

calcBreakeven <- function(dist.year) {
	# calc breakeven points
	# TODO C: could solve for breakeven point in another way, to handle off-chart cases, 
	#         but in that case, they would be off the chart.... so who cares.

	break.even <- list()
	break.even$pt.id <- which.max(dist.year$benefit - dist.year$cost > 0)
	break.even$pt <- dist.year$tenure[break.even$pt.id]
	break.even$cume.id <- which.max(dist.year$benefit.cume - dist.year$cost.cume > 0)
	break.even$cume <- dist.year$tenure[break.even$cume.id]

	writeLines(sprintf("Daily breakeven at %.2f, cume breakeven at %.2f", 
					   break.even$pt, break.even$cume))
	return(break.even)
}

empBenefit <- function(tenure, max.benefit) {
	# vector-friendly benefit from employee, modeled as a sigmoid function
	1/(1+exp(-(tenure/max.benefit*12-6)))
}

empBenefitCume <- function(tenure, max.benefit) {
	# vector friendly cumulative benefit, the integral of empBenefit
	# use sapply to make integrate vector-friendly
	sapply(tenure, function(x) { integrate(empBenefit, 0, x, 
										   max.benefit=max.benefit)$value })
}

empCost <- function(tenure, cost.ramp, cost.scale, salary) {
	# vector-friendly cost of employee, modeled as a gompertz function
	exp(-exp(cost.ramp * tenure)) * cost.scale + salary
}

empCostCume <- function(tenure, cost.ramp, cost.scale, salary) {
	# vector friendly cumulative cost, the integral of empCost
	# use sapply to make integrate vector-friendly
	sapply(tenure, function(x) { integrate(empCost, 0, x, 
										   cost.ramp=cost.ramp, 
										   cost.scale=cost.scale, 
										   salary=salary)$value })
}

empPredNet <- function(tenure, 
					   max.yrs, max.benefit, 
					   cost.ramp, cost.scale, salary,
					   d.shape, d.scale) {
	# one function for one tenure moment, for use by empPredNetCume and graphing

	# net = benefit - cost
	z.net <- empBenefit(tenure, max.benefit) - empCost(tenure, cost.ramp, cost.scale, salary)

	# weighted by probability of that tenure position
	z.prob <- dweibull(tenure, shape=d.shape, scale=d.scale)

	return(z.net * z.prob)
}

empPredNetCume <- function(max.benefit, 
						   cost.ramp, cost.scale, salary, 
						   d.shape, d.scale) {
	# the sum of all net benefits given these settings
	# hacked the 0.01 instead of 0 here to avoid error on super-high prob low value events
	integrate(empPredNet, 0.01, Inf,
			  max.benefit=max.benefit,
			  cost.ramp=cost.ramp, 
			  cost.scale=cost.scale, 
			  salary=salary,
			  d.shape=d.shape,
			  d.scale=d.scale
			  )$value
}

runPredNetCume <- function(max.benefit = def$max.benefit, 
						  cost.ramp = def$cost.ramp, cost.scale = def$cost.scale, salary = def$salary,
						  shape.good = def$shape.good, scale.good = def$scale.good, 
						  shape.bad = def$shape.bad, scale.bad = def$scale.bad,
						  good.bad.ratio = def$good.bad.ratio,
						  verbose=FALSE) {
	# run the sim for just the cume value, maybe print out values, return cume

	# if (verbose) {
	# 	writeLines(sprintf("Sim for max.benefit = %.2f, cost.ramp = %.2f, cost.scale = %.2f, salary = %.2f",
	# 					   max.benefit, cost.ramp, cost.scale, salary))
	# 	writeLines(sprintf("  shape.good = %.2f, scale.good = %.2f, shape.bad = %.2f, scale.bad = %.2f",
	# 					   shape.good, scale.good, shape.bad, scale.bad))
	# 	writeLines(sprintf("  good.bad.ratio = %.2f", good.bad.ratio))
	# }

	cume.good <- empPredNetCume(max.benefit, cost.ramp, cost.scale, salary, shape.good, scale.good)
	cume.bad <- empPredNetCume(max.benefit, cost.ramp, cost.scale, salary, shape.bad, scale.bad)

	cume.good.wt <- cume.good * good.bad.ratio 
	cume.bad.wt <- cume.bad * (1-good.bad.ratio)

	cume.total <- cume.good.wt + cume.bad.wt

	if (verbose) {
		writeLines(sprintf("Good Fit: %.1f%% net benefit * %.0f%% weight = %.1f%% overall contribution",
						   cume.good * 100, good.bad.ratio * 100, cume.good.wt * 100))
		writeLines(sprintf("Bad Fit: %.1f%% net benefit * %.0f%% weight = %.1f%% overall contribution",
						   cume.bad * 100, (1-good.bad.ratio) * 100, cume.bad.wt * 100))
		writeLines(sprintf("Overall EVH = %.1f%%", cume.total * 100))

		# TODO C: better printout, compare to salary
	}

	return(cume.total)
}

g.probTerm <- function(dist.year, break.even, max.yrs=def$max.yrs, do.annotate=FALSE) {
	zg <- ggplot(data=dist.year, aes(x=tenure)) + 
			geom_vline(xintercept=break.even$pt, col=def$col.be, size=0.5, linetype="dashed") +
			geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
			geom_line(aes(y=prob.bad), col=def$col.bad, size=1) +
			geom_line(aes(y=prob.good), col=def$col.good, size=1) +
			scale_y_continuous(labels = percent) +
			theme_bw() +
			xlim(c(0, max.yrs)) +
			labs(title="Probability of Employee Termination", 
				 x="Tenure in Years", 
				 y="Daily Probability of Termination")

	if (do.annotate) {
		zg <- zg + 
				annotate("text", 
						x=1.75, y=0.47, hjust=0, vjust=1,
						color=def$col.good,
						label="Good Fit") +
				annotate("text", 
						 x=0.5, y=1.25, hjust=0, vjust=1,
						 color=def$col.bad,
						 label="Bad Fit")
	}
	return(zg)
}

g.costBenefit <- function(dist.year, break.even, max.yrs=def$max.yrs, 
						  do.annotate=FALSE, line.size=1, text.size=10 ) {
	zg <- ggplot(data=dist.year, aes(x=tenure)) + 
			geom_vline(xintercept=break.even$pt, col=def$col.be, size=line.size/2, linetype="dashed") +
			geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=line.size/2, linetype="dashed") +
			geom_ribbon(fill=def$col.cost, size=0, aes(ymax=cost,ymin=benefit,alpha=cost>benefit)) + 
			scale_alpha_discrete(range=c(0,.25)) + 
			theme(legend.position="none") +
			geom_line(col=def$col.cost, size=line.size, aes(y=cost)) + 
			geom_line(col=def$col.benefit, size=line.size, aes(y=benefit)) +
			scale_y_continuous(labels = percent) +
			theme_bw() +
			theme(legend.position="none", 
				  text = element_text(size = text.size*2) ) +
			labs(title="Benefit & Cost of One Employee", 
				 x="Tenure in Years", 
				 y="% Potential Value")

	if (do.annotate) {
		zg <- zg +
			 annotate("text", 
					  x=2.5, y=1.02, hjust=0, vjust=-0.2,
					  color=def$col.benefit,
					  size=text.size,
					  label="Benefit") +
			 annotate("text", 
					  x=2.5, y=0.52, hjust=0, vjust=-0.2,
					  color=def$col.cost,
					  size=text.size,
					  label="Cost") +
			 annotate("text", 
					  x=break.even$pt, y=0.1, hjust=-0.1, vjust=0,
					  color=def$col.be,
					  size=text.size,
					  label="B/E") +
			 annotate("text", 
					  x=break.even$cume, y=0.1, hjust=-0.1, vjust=0,
					  color=def$col.be,
					  size=text.size,
					  label="B/E Cume") 
	}
	return(zg)
}

g.cumeCostBenefit <- function(dist.year, break.even, max.yrs=def$max.yrs, do.annotate=FALSE) {

	zg <- ggplot(data=dist.year, aes(x=tenure)) + 
			geom_vline(xintercept=break.even$pt, col=def$col.be, size=0.5, linetype="dashed") +
			geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
			geom_hline(yintercept=0, col=def$col.be.cume, size=0.5, linetype="dotted") +

			geom_ribbon(size=0, alpha=0.5,
						aes(ymax=cost.cume, ymin=benefit.cume, 
							fill=(benefit.cume-cost.cume)>0)) + 
			geom_line(aes(y=benefit.cume), size=1, col=def$col.benefit) +
			geom_line(aes(y=cost.cume), size=1, col=def$col.cost) +

			scale_y_continuous(labels = percent) +
			scale_color_manual(values=c(def$col.cost, def$col.benefit)) +
			scale_fill_manual(values=c(def$col.cost, def$col.benefit)) +
			theme_bw() +
		   	theme(legend.position="none") +
		   	xlim(c(0,max.yrs)) +
		   	labs(title="Cumulative Cost and Benefit", 
					x="Tenure in Years", 
					y="% Potential Value")

	if (do.annotate) {
		zg <- zg +
				 annotate("text", 
						  x=2.5, y=0.9, hjust=0.5, vjust=0,
						  color=def$col.benefit,
						  label="Cumulative Benefit") +
				 annotate("text", 
						  x=0.65, y=0.9, hjust=0.6, vjust=0,
						  color=def$col.cost,
						  label="Cumulative Cost") #+
				 # annotate("text", 
						  # x=break.even$pt, y=0.90, hjust=-0.1, vjust=0,
						  # color=def$col.be,
						  # label="B/E") +
				 # annotate("text", 
						  # x=break.even$cume, y=0.90, hjust=-0.1, vjust=0,
						  # color=def$col.be,
						  # label="B/E Cume")
	}
	return(zg)
}

g.cumeValue <- function(dist.year, break.even, max.yrs=def$max.yrs, do.annotate=FALSE) {

	zg <- ggplot(data=dist.year, aes(x=tenure)) + 
			geom_vline(xintercept=break.even$pt, col=def$col.be, size=0.5, linetype="dashed") +
			geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
			geom_hline(yintercept=0, col=def$col.be.cume, size=0.5, linetype="dotted") +
			geom_ribbon(size=0, alpha=0.5, ymin=0,
						aes(ymax=(benefit.cume-cost.cume), 
							fill=(benefit.cume-cost.cume)>0)) + 
			geom_line(size=1, 
					  aes(y=(benefit.cume-cost.cume),
						  col=(benefit.cume-cost.cume)>0)) +
			scale_y_continuous(labels = percent) +
			scale_fill_manual(values=c(def$col.cost, def$col.benefit)) +
			scale_color_manual(values=c(def$col.cost, def$col.benefit)) +
			theme_bw() +
			theme(legend.position="none") +
			xlim(c(0,max.yrs)) +
			labs(title="Cumulative Net Benefit", 
				 x="Tenure in Years", 
				 y="% Potential Value")

	if (do.annotate) {
		zg <- zg +
				 annotate("text", 
						  x=3, y=-0.1, hjust=0.5, vjust=0,
						  color=def$col.benefit,
						  label="Net Benefit") +
				 annotate("text", 
						  x=1.4, y=0.1, hjust=0.5, vjust=1,
						  color=def$col.cost,
						  label="Net Cost") +
				 annotate("text", 
						  x=break.even$pt, y=0.90, hjust=-0.1, vjust=0,
						  color=def$col.be,
						  label="B/E") +
				 annotate("text", 
						  x=break.even$cume, y=0.90, hjust=-0.1, vjust=0,
						  color=def$col.be,
						  label="B/E Cume")
	}
	return(zg)
}

g.cumeValueTimeline <- function(dist.year, break.even, time.line, max.yrs=def$max.yrs, do.annotate=FALSE) {

	zg <- g.cumeValue(dist.year, break.even, max.yrs, do.annotate) +

			geom_errorbarh(data=time.line, height=0.03, size=0.4, col="steelblue",
						   aes(x=tenure.yrs, 
							   xmin=0, 
							   xmax=tenure.yrs, 
							   y=randex)) +
			labs(main="")

	return(zg)
}

g.expCume <- function(dist.year, break.even, evh, max.yrs=def$max.yrs, do.annotate=FALSE) {

	zg <- ggplot(data=dist.year, aes(x=tenure)) + 
		   geom_vline(xintercept=break.even$pt, col=def$col.be, size=0.5, linetype="dashed") +
		   geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
		   geom_hline(yintercept=0, col=def$col.be.cume, size=0.5, linetype="dotted") +

		   geom_ribbon(fill=def$col.bad, size=0, alpha=0.5, ymin=0,
					   aes(ymax=(benefit.cume-cost.cume)*prob.bad.wt)) + 
		   geom_line(aes(y=(benefit.cume-cost.cume)*prob.bad.wt), col=def$col.bad, size=1) +

		   geom_ribbon(fill=def$col.good, size=0, alpha=0.5, ymin=0,
					   aes(ymax=(benefit.cume-cost.cume)*prob.good.wt)) + 
		   geom_line(aes(y=(benefit.cume-cost.cume)*prob.good.wt), col=def$col.good, size=1) +

		   annotate("text", 
					x=3, y=-0.1, hjust=0, vjust=0,
					size=4,
					color=ifelse(evh<0,def$col.cost,def$col.benefit),
					label=sprintf("EVH = %.1f%%",evh*100)) +
		   scale_y_continuous(labels = percent) +
		   theme_bw() +
		   xlim(c(0,max.yrs)) +
		   labs(title="Expected Cumulative Net Benefit", 
				x="Tenure in Years", 
				y="% Potential Value")

	if (do.annotate) {
		zg <- zg +
				 annotate("text", 
						  x=1.9, y=0.07, hjust=0, vjust=1.5,
						  color=def$col.good,
						  label="Good Fit") +
				 annotate("text", 
						  x=0.80, y=-0.2, hjust=0, vjust=0,
						  color=def$col.bad,
						  label="Bad Fit")
	}
	return(zg)
}

g.costArea <- function(dist.year, break.even, max.yrs=def$max.yrs, 
					   alpha=0.7, main.title="Disposition of Employee Costs & Benefits",
					   multiplier=1, line.size=1, text.size=10 ) {

	zd <- dist.year[,c("tenure", "salary", "non.salary", 
					   "learning", "debt.payment", "profit")]
	zd.melt <- melt(zd, id.vars="tenure")

	# use to scale the chart by probability
	zd.melt$value <- zd.melt$value * pmax(0,multiplier)	

	zg <- 	ggplot(data=zd.melt, aes(x=tenure)) + 
			geom_vline(xintercept=break.even$pt, col=def$col.be, size=line.size/2, linetype="dashed") +
			geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=line.size/2, linetype="dashed") +
			geom_hline(yintercept=1, col=def$col.be.cume, size=line.size, linetype="dashed") +

			geom_area(aes(fill=variable, y=value), stat="identity", alpha=alpha) +

			# geom_line(aes(y=salary), col=def$col.cost, size=1) +
			# geom_line(aes(y=non.salary), col=def$col.cost, size=1, linetype="dotted") +
			# geom_line(aes(y=learning), col=def$col.cost, size=1, linetype="dashed") +

			# geom_line(aes(y=debt), col=def$col.bad, size=1, linetype="dashed") +
			# geom_line(aes(y=debt.payment), col=def$col.good, size=1) +
			# geom_line(aes(y=profit), col=def$col.benefit, size=1, linetype="dotted") +

			scale_y_continuous(labels = percent) +
			scale_fill_manual(values=c(def$col.good, 
									   def$col.other1, 
									   def$col.other2, 
									   def$col.cost, 
									   def$col.benefit )) +
			theme_bw() +
			theme(legend.position="bottom",
				  text = element_text(size = text.size*2) ) +
			# theme(text = element_text(size=8), 
			# 	  axis.title.y = element_text(size=8)) +
			# ylim(c(0,max(dist.year$cost.cume))) +
			xlim(c(0,max.yrs)) +
			labs(title=main.title, 
				 x="Tenure in Years", 
				 y="% Potential Value")
	return(zg)
}

g.costAreaTimeline <- function(dist.year, break.even, time.line, max.yrs=def$max.yrs, 
							   main.title="Multiple Tenures in Employee Cost System") {

	zg <- g.costArea(dist.year, break.even, max.yrs, alpha=0.4, main.title=main.title) +

			geom_errorbarh(data=time.line, height=0.05, size=1, #col="steelblue",
						   aes(x=tenure.yrs, 
							   xmin=0, 
							   xmax=tenure.yrs, 
							   col=tenure.yrs > break.even$cume,
							   y= 2 * randex)) +
			theme(legend.position="none") +
			labs(main=main.title)

	return(zg)
}

g.survivalCurve <- function(dist.year, break.even, max.yrs=def$max.yrs, do.annotate=FALSE) {

	z.rate <- 1 - dist.year$cdf.good[101] * def$good.bad.ratio - dist.year$cdf.bad[101] * (1 - def$good.bad.ratio)

	writeLines(sprintf("Overall attrition rate %.1f%%", (1-z.rate)*100))

	zg <- ggplot(data=dist.year, aes(x=tenure)) + 
		   geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +

		   geom_hline(yintercept=z.rate, col=def$col.benefit, size=0.5, linetype="dotted") +
		   geom_vline(xintercept=1, col=def$col.benefit, size=0.5, linetype="dotted") +

		   geom_line(aes(y=1 - 
						 	cdf.good * def$good.bad.ratio - 
							cdf.bad  * (1 - def$good.bad.ratio)), col=def$col.benefit, size=1) +

		   scale_y_continuous(labels = percent) +
		   theme_bw() +
		   xlim(c(0,max.yrs)) +
		   ylim(c(0,1)) +
		   labs(title="Survival Curve", 
				x="Tenure in Years", 
				y="Probability of Reaching Tenure")

	if (do.annotate) {
		zg <- zg +
				 annotate("text", 
						  x=1, y=z.rate, hjust=-0.1, vjust=-0.1,
						  color=def$col.benefit,
						  label=sprintf("%.0f%% Attrition", (1-z.rate)*100))
	}
	return(zg)
}

g.survivalCurveGoodBad <- function(dist.year, break.even, max.yrs=def$max.yrs, do.annotate=FALSE) {

	z.rate.good <- 1 - dist.year$cdf.good[101]
	z.rate.bad <- 1 - dist.year$cdf.bad[101]
	writeLines(sprintf("Attrition rates good %.1f%% bad %.1f%%", (1-z.rate.good)*100, (1-z.rate.bad)*100))

	zg <- ggplot(data=dist.year, aes(x=tenure)) + 
		   geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +

		   geom_hline(yintercept=z.rate.good, col=def$col.good, size=0.5, linetype="dotted") +
		   geom_hline(yintercept=z.rate.bad, col=def$col.bad, size=0.5, linetype="dotted") +
		   geom_vline(xintercept=1, col=def$col.good, size=0.5, linetype="dotted") +

		   geom_line(aes(y=1 - cdf.good), col=def$col.good, size=1) +
		   geom_line(aes(y=1 - cdf.bad), col=def$col.bad, size=1) +

		   scale_y_continuous(labels = percent) +
		   theme_bw() +
		   xlim(c(0,max.yrs)) +
		   ylim(c(0,1)) +
		   labs(title="Survival Curve", 
				x="Tenure in Years", 
				y="Probability of Reaching Tenure")

	if (do.annotate) {
		zg <- zg +
				 annotate("text", 
						  x=1, y=z.rate.good, hjust=-0.1, vjust=-0.1,
						  color=def$col.good,
						  label=sprintf("Good Fit = %.0f%% Attrition", (1-z.rate.good)*100)) +
				 annotate("text", 
						  x=1, y=z.rate.bad, hjust=-0.1, vjust=-0.1,
						  color=def$col.bad,
						  label=sprintf("Bad Fit = %.0f%% Attrition", (1-z.rate.bad)*100))
	}
	return(zg)
}

g.survivalCurveDelta <- function(dist.year, break.even, pct1=def$good.bad.ratio, pct2=def$good.bad.ratio+0.1, max.yrs=def$max.yrs, do.annotate=FALSE) {

	zd <- data.frame(tenure=dist.year$tenure)
	zd$cdf.pct1 <- dist.year$cdf.good * pct1 + dist.year$cdf.bad * (1 - pct1)
	zd$cdf.pct2 <- dist.year$cdf.good * pct2 + dist.year$cdf.bad * (1 - pct2)

	# 1 year is 101st record
	z.rate.pct1 <- 1 - zd$cdf.pct1[101]
	z.rate.pct2 <- 1 - zd$cdf.pct2[101]

	writeLines(sprintf("Attrition rates before %.1f%% after %.1f%%", (1-z.rate.pct1)*100, (1-z.rate.pct2)*100))

	zg <- ggplot(data=zd, aes(x=tenure)) + 
		   geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +

		   geom_hline(yintercept=z.rate.pct1, col=def$col.bad, size=0.5, linetype="dotted") +
		   geom_hline(yintercept=z.rate.pct2, col=def$col.good, size=0.5, linetype="dotted") +
		   geom_vline(xintercept=1, col=def$col.benefit, size=0.5, linetype="dotted") +

		   geom_line(aes(y=1 - cdf.pct1), col=def$col.bad, size=1) +
		   geom_line(aes(y=1 - cdf.pct2), col=def$col.good, size=1) +

		   scale_y_continuous(labels = percent) +
		   theme_bw() +
		   xlim(c(0,max.yrs)) +
		   ylim(c(0,1)) +
		   labs(title="Survival Curve", 
				x="Tenure in Years", 
				y="Probability of Reaching Tenure")

	if (do.annotate) {
		zg <- zg +
				 annotate("text", 
						  x=1, y=z.rate.pct1, hjust=-0.4, vjust=-0.1,
						  color=def$col.bad,
						  label=sprintf("Before = %.0f%% Attrition", (1-z.rate.pct1)*100)) +
				 annotate("text", 
						  x=1, y=z.rate.pct2, hjust=-0.4, vjust=-0.1,
						  color=def$col.good,
						  label=sprintf("After = %.0f%% Attrition", (1-z.rate.pct2)*100))
	}
	return(zg)
}
g.timeline <- function(d.timeline) {

	zg <- ggplot(d.timeline, aes(x=hire, 
								 xmin=hire, 
								 xmax=term, 
								 y=randex, 
								 col=term.class)) + 
			   geom_errorbarh(height=0.02, size=0.6) + 
			   scale_color_hue(name="term.class") +
			   geom_vline(xintercept=2014.2,col="red", size=0.65, linetype="dashed") +
			   annotate("text", 
						x= 2014.2 + 0.02, 
						y= 0, 
						size=4,
						color="Red",
						label="Today",
						hjust=0, vjust=1) +
			   scale_x_continuous(limits=c(2010,2016)) +
			   labs(x="Hire Date", y="Employees") +
			   theme_bw() + 
			   theme(legend.position="bottom",
					 axis.ticks.y=element_blank(),
					 axis.text.y=element_blank())
	return(zg)
}

g.timeline0 <- function(d.timeline, line.height=0.02, line.size=0.6, text.size=10) {

	zg <- ggplot(d.timeline, aes(x=hire, 
								 xmin=0, 
								 xmax=tenure.yrs, 
								 y=randex,
								 col=tenure.yrs>2.09)) +
				geom_errorbarh(height=line.height, size=line.size) + 
				scale_x_continuous(limits=c(0,max(d.timeline$tenure.yrs))) +
				geom_vline(xintercept=break.even$pt, col=def$col.be, size=0.5, linetype="dashed") +
				geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
				labs(x="Tenure in Years", y="Employees") +
				theme_bw() + 
				theme(legend.position="none",
					  text = element_text(size = text.size*2),
					  axis.ticks.y=element_blank(),
					  axis.text.y=element_blank())
	return(zg)
}

g.histogram <- function(d.timeline, break.even, max.yrs=def$max.yrs) {

	zg <- ggplot(data=d.timeline, aes(x=tenure.yrs)) + 
			   geom_histogram(binwidth=1/12, fill=def$col.benefit) + 

			   geom_vline(xintercept=break.even$pt, col=def$col.be, size=0.5, linetype="dashed") +
			   geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
			   theme_bw() +
			   # theme(text = element_text(size=8), 
					 # axis.title.y = element_text(size=8)) +
			   xlim(c(0, max.yrs)) +
			   labs(title="All Employees", 
					x="Tenure in Years", 
					y="Count")
	return(zg)
}

g.survival <- function(data, break.even, 
					   title="Kaplan-Meier Survival Estimator", 
					   surv.split=FALSE, 
					   max.yr = def$max.yr) {

	surv.obj <- Surv(data$tenure.yrs, data$is.term)

	if (surv.split) {
		surv.fit <- survfit(surv.obj ~ data$emp.type)
	} else {
		surv.fit <- survfit(surv.obj ~ 1)
	}

	zg <- ggsurv(surv.fit, plot.cens=FALSE) + 
				scale_y_continuous(labels = percent_format()) +
				geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +
				labs(title=title,
					 x="Tenure in Years", 
					 y="Probability of Attaining Tenure") + 
				theme_bw() +
				theme(legend.position = "none") +
				xlim(c(0, max.yr))
	return(zg)
}

g.deltaProfit <- function(dist.year, pct1=def$good.bad.ratio, pct2=def$good.bad.ratio+0.1, 
						  base.salary=20000, num.reps=200) {
	zp1 <- (sum(dist.year$profit * dist.year$prob.good * pct1) 
		   + sum(dist.year$profit * dist.year$prob.bad * (1-pct1)))/100
	zp2 <- (sum(dist.year$profit * dist.year$prob.good * pct2) 
		   + sum(dist.year$profit * dist.year$prob.bad * (1-pct2)))/100

	writeLines(sprintf("change pct good from %.0f%% to %.0f%% leads to:", 
					   pct1*100, pct2*100))
	writeLines(sprintf("  profit from %.2f%% to %.2f%%, change of %.2f%%", 
					   zp1*100, zp2*100, (zp2/zp1-1)*100))

	writeLines(sprintf("  for %i reps with %.0f salary:", 
					   num.reps, base.salary))
	writeLines(sprintf("    from %.0f /rep to %.0f /rep", 
					   zp1*base.salary, zp2*base.salary))
	writeLines(sprintf("    delta %.0f /rep or %.0f for %i reps", 
					   (zp2-zp1)*base.salary, (zp2-zp1)*base.salary*num.reps, num.reps))

	twoBars(sprintf("With %.0f%% Good Fit", pct1*100), zp1, 
			sprintf("With %.0f%% Good Fit", pct2*100), zp2,
			x.title="Employee Profit Under 'Good Fit' Mix Scenarios")
}

runFigures <- function() {

	# calc with defaults
	dist.year <- calcDist()
	break.even <- calcBreakeven(dist.year)
	evh <- runPredNetCume()
	time.big <- genTimeline(500)
	time.med <- genTimeline(200)
	time.small <- genTimeline(50)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/histogram.png",
		   g.histogram(time.big, break.even), 
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/probTerm.png",
		   g.probTerm(dist.year, break.even,do.annotate=TRUE), 
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/costBenefit.png",
		   g.costBenefit(dist.year, break.even,do.annotate=TRUE), 
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/cumeCostBenefit.png",
		   g.cumeCostBenefit(dist.year, break.even,do.annotate=TRUE), 
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/cumeValue.png",
		   g.cumeValue(dist.year, break.even,do.annotate=TRUE), 
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/cumeValueTimeline.png",
		   g.cumeValueTimeline(dist.year, break.even, time.small),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/expCume.png",
		   g.expCume(dist.year, break.even, evh, do.annotate=TRUE), 
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/costArea.png",
		   g.costArea(dist.year, break.even,
					  main.title="Est Costs & Benefits for One Employee"),
		   height=6.75,width=6,dpi=100)
		   # height=4.5,width=12,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/costArea_good.png",
		   g.costArea(dist.year, break.even,
					  multiplier=1-cumsum(dist.year$prob.good)/100,
					  main.title="Est Costs & Benefits for 'Good Fit' Employee"),
		   height=6.75,width=6,dpi=100)
		   # height=4.5,width=12,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/costArea_bad.png",
		   g.costArea(dist.year, break.even,
					  multiplier=1-cumsum(dist.year$prob.bad)/100,
					  main.title="Est Costs & Benefits for 'Bad Fit' Employee"),
		   height=6.75,width=6,dpi=100)
		   # height=4.5,width=12,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/costArea_current.png",
		   g.costArea(dist.year, break.even,
					  multiplier=1-cumsum(dist.year$prob.good.wt + dist.year$prob.bad.wt)/100,
					  main.title="Est Costs & Benefits for Current Mix of Employees"),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/costArea_currten.png",
		   g.costArea(dist.year, break.even,
					  multiplier=1-cumsum(dist.year$prob.good.wt2 + dist.year$prob.bad.wt2)/100,
					  main.title="With 10% Improvement in Mix of Employees"),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/costAreaTimeline.png",
		   g.costAreaTimeline(dist.year, break.even, time.small,
					  main.title="Multiple Tenures in Employee Cost System"),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/survival.png",
		   g.survival(time.big, break.even),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/survival_multi.png",
		   g.survival(time.big, break.even, surv.split=TRUE),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/survivalCurve.png",
		   g.survivalCurve(dist.year, break.even, do.annotate=TRUE),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/survivalCurveGoodBad.png",
		   g.survivalCurveGoodBad(dist.year, break.even, do.annotate=TRUE),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/survivalCurveDelta.png",
		   g.survivalCurveDelta(dist.year, break.even, do.annotate=TRUE),
		   height=6.75,width=6,dpi=100)


	ggsave("~/gitInternal/ta_presentations/images/empCosts/timeline.png",
		   g.timeline(time.med),
		   height=6.75,width=6,dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/empCosts/deltaProfit.png",
		   g.deltaProfit(dist.year, def$good.bad.ratio, def$good.bad.ratio+0.1, 80000, 300),
		   height=6.75,width=6,dpi=100)

	# ggsave("plots/pat003_surv_split.png", 
	# 	   g.survival(time.big, break.even, 
	# 				  title="Multiple Survival Curves",
	# 				  surv.split=TRUE),
	# ggsave("plots/pat003_prob_term.png", 
	# 	   g.probTerm(dist.year, break.even, do.annotate=TRUE), 
	# 	   height=6.75, width=6, dpi=100)
	# ggsave("plots/pat003_cost_benefit.png", 
	# 	   g.costBenefit(dist.year, break.even, do.annotate=TRUE),
	# 	   height=6.75, width=6, dpi=100)
	# ggsave("plots/pat003_cume_value.png", 
	# 	   g.cumeValue(dist.year, break.even, do.annotate=TRUE),
	# 	   height=6.75, width=6, dpi=100)
	# ggsave("plots/pat003_exp_cume.png", 
	# 	   g.expCume(dist.year, break.even, evh, do.annotate=TRUE),
	# 	   height=6.75, width=6, dpi=100)

	# ggsave("plots/pat003_cume_time.png", 
	# 	   g.cumeValueTimeline(dist.year, break.even, genTimeline(100)),
	# 	   height=6.75, width=6, dpi=100)

	# ggsave("plots/pat003_time_line.png", 
	# 	   g.timeline(genTimeline(200)),
	# 	   height=6.75, width=6, dpi=100)


	# ggsave("plots/pat003_histogram.png", 
	# 	   g.histogram(time.big, break.even),
	# 	   height=6.75, width=6, dpi=100)
	
	# ggsave("plots/pat003_survival.png", 
	# 	   g.survival(time.big, break.even),
	# 	   height=6.75, width=6, dpi=100)

	# ggsave("plots/pat003_surv_split.png", 
	# 	   g.survival(time.big, break.even, 
	# 				  title="Multiple Survival Curves",
	# 				  surv.split=TRUE),
	# 	   height=6.75, width=6, dpi=100)
}


# TODO B: cumsum = doesn't affect paw


runSensitivityTests <- function() {
	# modify one variable through its range to calc sensitivity
	# limited analysis - just based on the "reasonable" starting point
	writeLines("Running Sensitivity Tests")

	# modify good.bad.ratio from 0-1 from base of 0.3
	writeLines(sprintf("  good.bad.ratio:\t1%% change from %.2f results in %.2f%% change in EVH",
					   def$good.bad.ratio, 
					   (runPredNetCume(good.bad.ratio=def$good.bad.ratio*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0, 1, 0.05)
	g.gbr <- sensitivityPlot("Good.Bad.Ratio", def$good.bad.ratio, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(good.bad.ratio=x)}))

	# modify shape.good from 1-5 from base of 2.5 
	writeLines(sprintf("  shape.good:\t1%% change from %.2f results in %.2f%% change in EVH",
					   def$shape.good, 
					   (runPredNetCume(shape.good=def$shape.good*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(1, 5, 0.05)
	g.shg <- sensitivityPlot("Shape.Good", def$shape.good, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(shape.good=x)}))

	# modify scale.good from 0.01-5 from base of 1.5 
	writeLines(sprintf("  scale.good:\t1%% change from %.2f results in %.2f%% change in EVH",
					   def$scale.good, 
					   (runPredNetCume(scale.good=def$scale.good*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.01, 5, 0.05)
	g.scg <- sensitivityPlot("Scale.Good", def$scale.good, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(scale.good=x)}))

	# modify shape.bad from 1-5 from base of 1.66 
	writeLines(sprintf("  shape.bad:\t1%% change from %.2f results in %.2f%% change in EVH",
					   def$shape.bad, 
					   (runPredNetCume(shape.bad=def$shape.bad*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(1, 5, 0.05)
	g.shb <- sensitivityPlot("Shape.Bad", def$shape.bad, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(shape.bad=x)}))

	# modify scale.bad from 0.01-5 from base of 0.33 
	writeLines(sprintf("  scale.bad:\t1%% change from %.2f results in %.2f%% change in EVH",
					   def$scale.bad, 
					   (runPredNetCume(scale.bad=def$scale.bad*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.01, 5, 0.05)
	g.scb <- sensitivityPlot("Scale.Bad", def$scale.bad, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(scale.bad=x)}))

	# modify max.benefit from 0.25-3, initial = 0.5
	writeLines(sprintf("  max.benefit:\t1%% change from %.2f results in %.2f%% change in EVH",
					   def$max.benefit, 
					   (runPredNetCume(max.benefit=def$max.benefit*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.25, 3, 0.05)
	g.mb <- sensitivityPlot("Max.Benefit", def$max.benefit, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(max.benefit=x)}))

	# modify cost.ramp from 0.5-5 from base of 3.5 
	writeLines(sprintf("  cost.ramp:\t1%% change from %.2f results in %.2f%% change in EVH",
					   def$cost.ramp, 
					   (runPredNetCume(cost.ramp=def$cost.ramp*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.5, 5, 0.05)
	g.cr <- sensitivityPlot("Cost.Ramp", def$cost.ramp, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(cost.ramp=x)}))

	# modify cost.scale from 0.5-5 from base of 2 
	writeLines(sprintf("  cost.scale:\t1%% change from %.2f results in %.2f%% change in EVH",
					   def$cost.scale, 
					   (runPredNetCume(cost.scale=def$cost.scale*1.01) / 
						runPredNetCume() - 1) * 100 ))
	z.in <- seq(0.5, 5, 0.05)
	g.cs <- sensitivityPlot("Cost.Scale", def$cost.scale, z.in, 
							 sapply(z.in, function(x) {runPredNetCume(cost.scale=x)}))

	# modify salary from 0-1 from base of 0.5
	writeLines(sprintf("  salary:\t1%% change from %.2f results in %.2f%% change in EVH",
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
						   geom_vline(xintercept=def.value, col=def$col.be, linetype="dashed") +
						   geom_hline(yintercept=0, col=def$col.be, linetype="dotted") +
						   geom_line(col=def$col.benefit, size=1) +
						   theme_bw() +
						   labs(x=label, y="Exp Net Cume Benefit"))
	return(zg)
}

twoBars <- function(var.a, val.a, var.b, val.b, x.title="Analysis Results") {
	# just a simple program to make quick bar charts as needed
	d.g <- data.frame(rbind(c(var.a, val.a), c(var.b, val.b)))
	names(d.g) <- c("variable", "value")
	d.g$variable <- factor(d.g$variable, levels=c(var.a, var.b))
	d.g$value <- as.numeric(levels(d.g$value)[as.numeric(d.g$value)])

	ggplot(d.g, aes(x=variable, y=value, fill=variable, col=variable)) +
	geom_bar(stat="identity") +
	scale_y_continuous(labels = percent) +
	labs(x=x.title,y="Percent") +
	theme_bw() +
	theme(legend.position="none") 
}

# everything below here runs on source
def <- setDefaults()

library(ggplot2)
library(gridExtra)
library(scales)
library(manipulate)

manipSim202 <- function() {
	manipulate(runSim202(max.yrs, max.benefit, 
						 cost.ramp, cost.scale, salary,
						 shape.good, scale.good, shape.bad, scale.bad, 
						 good.bad.ratio),
			   good.bad.ratio = slider(0, 1, initial=0.3),
			   shape.good = slider(1, 3, initial=1.8), 
			   scale.good = slider(0.01, 3, initial=1.5), 
			   shape.bad = slider(1, 3, initial=1.25), 
			   scale.bad = slider(0.01, 3, initial=0.9), 
			   max.benefit = slider(0.25, 10, initial = 1),
			   cost.ramp = slider(0.5, 10, initial=1.7), 
			   cost.scale = slider(0.5, 10, initial=1.5), 
			   salary = slider(0, 1, initial=0.5),
			   max.yrs = slider(1, 8, initial=3))
}

runSim202 <- function(max.yrs=3, max.benefit=1.5, 
					  cost.ramp=1.7, cost.scale=1.5, salary=0.5,
					  shape.good=1.8, scale.good=1.5, shape.bad=1.25, scale.bad=0.9, 
					  good.bad.ratio=0.3) {

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
	col.good <- "DarkOrange"
	col.bad <- "DarkGoldenrod"
	col.be <- "SteelBlue"
	col.be.cume <- "SteelBlue"

	fig1 <- ggplot(data=dist.year, aes(x=tenure)) + 
			geom_ribbon(fill=col.cost, size=0, aes(ymax=cost,ymin=benefit,alpha=cost>benefit)) + 
			scale_alpha_discrete(range=c(0,.1)) + 
			theme(legend.position="none") +
			geom_line(col=col.cost, size=1, aes(y=cost)) + 
			geom_line(col=col.benefit, size=1, aes(y=benefit)) +
			scale_y_continuous(labels = percent) +
			geom_vline(xintercept=be.pt, col=col.be, size=0.5, linetype="dashed") +
			geom_vline(xintercept=be.cume, col=col.be.cume, size=0.5, linetype="dashed") +
			theme_bw() +
			theme(legend.position="none") +
			labs(title="Monthly Benefit & Cost from One Employee", 
				 x="Tenure in Years", 
				 y="% Potential Monthly Value") 

	fig2 <- ggplot(data=dist.year, aes(x=tenure)) + 
			geom_line(aes(y=prob.good), col=col.good, size=1) +
			geom_line(aes(y=prob.bad), col=col.bad, size=1) +
			geom_vline(xintercept=be.pt, col=col.be, size=0.5, linetype="dashed") +
			geom_vline(xintercept=be.cume, col=col.be.cume, size=0.5, linetype="dashed") +
			scale_y_continuous(labels = percent) +
			theme_bw() +
			xlim(c(0,max.yrs)) +
			labs(title="Probability of Employee Tenure", 
				 x="Tenure in Years", 
				 y="Probability") 

	fig3 <- ggplot(data=dist.year, aes(x=tenure)) + 
			geom_ribbon(fill=col.good, size=0, alpha=0.5, ymin=0,
						aes(ymax=(benefit.cume-cost.cume)*prob.good)) + 
			geom_line(aes(y=(benefit.cume-cost.cume)*prob.good), col=col.good, size=1) +
			geom_ribbon(fill=col.bad, size=0, alpha=0.5, ymin=0,
						aes(ymax=(benefit.cume-cost.cume)*prob.bad)) + 
			geom_line(aes(y=(benefit.cume-cost.cume)*prob.bad), col=col.bad, size=1) +
			geom_vline(xintercept=be.pt, col=col.be, size=0.5, linetype="dashed") +
			geom_vline(xintercept=be.cume, col=col.be.cume, size=0.5, linetype="dashed") +
			geom_hline(yintercept=0, col=col.be.cume, size=0.5, linetype="dotted") +
			scale_y_continuous(labels = percent) +
			theme_bw() +
			xlim(c(0,max.yrs)) +
			labs(title="Expected Cumulative Net Benefit", 
				 x="Tenure in Years", 
				 y="Cumulative Net Benefit * Probability") 


	fig.all <- arrangeGrob(fig1, fig2, fig3, ncol=1)
	print(fig.all)
	
	writeLines("==== new sim ====")
	writeLines(sprintf("At this rate net benefit begins at year %.2f, breakeven at year %.2f",
					   be.pt, be.cume))

	writeLines(sprintf("Sim for max.benefit = %.2f, cost.ramp = %.2f, cost.scale = %.2f, salary = %.2f",
					   max.benefit, cost.ramp, cost.scale, salary))
	writeLines(sprintf("        shape.good = %.2f, scale.good = %.2f, shape.bad = %.2f, scale.bad = %.2f",
					   shape.good, scale.good, shape.bad, scale.bad))

	cume.good <- empPredNetCume(max.benefit, cost.ramp, cost.scale, salary, shape.good, scale.good) * 100
	cume.bad <- empPredNetCume(max.benefit, cost.ramp, cost.scale, salary, shape.bad, scale.bad) * 100
	cume.total <- cume.good + cume.bad

	writeLines(sprintf("cume.good = %.1f%%, cume.bad = %.1f%%, total = %.1f%%", 
					   cume.good, cume.bad, cume.total))

	return(cume.total)
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

empCostCume <- function(tenure, cost.ramp, cost.scale, salary) {
	# vector friendly cumulative cost, the integral of empCost
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
	

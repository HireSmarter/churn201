library(ggplot2)
library(gridExtra)
library(scales)
library(manipulate)

runSim202 <- function(max.yrs=3, max.benefit=1.5, cost.ramp=1.7, cost.scale=1.5, salary=0.5,
					  center.bad=0.9, center.good=2, good.bad.ratio=0.3, run.size=10000, run.seed=42) {

	set.seed(run.seed)
	writeLines(sprintf("Simulating %i samples with seed %i", run.size, run.seed))

	# get random dist of good and bad employees
	# lognormal is a nice estimate as it does not go below zero, and tails out long
	
	dist.good <- as.data.frame( rlnorm(run.size * good.bad.ratio, meanlog=log(center.good)) )
	names(dist.good) <- "tenure"
	dist.good$benefit <- empBenefit(dist.good$tenure, max.benefit)
	dist.good$benefit.cume <- empBenefitCume(dist.good$tenure, max.benefit)
	dist.good$cost <- empCost(dist.good$tenure, cost.ramp, cost.scale, salary)
	dist.good$cost.cume <- empCostCume(dist.good$tenure, cost.ramp, cost.scale, salary)

	dist.bad <- as.data.frame( rlnorm(run.size * (1-good.bad.ratio), meanlog=log(center.bad)) )
	names(dist.bad) <- "tenure"
	dist.bad$benefit <- empBenefit(dist.bad$tenure, max.benefit)
	dist.bad$benefit.cume <- empBenefitCume(dist.bad$tenure, max.benefit)
	dist.bad$cost <- empCost(dist.bad$tenure, cost.ramp, cost.scale, salary)
	dist.bad$cost.cume <- empCostCume(dist.bad$tenure, cost.ramp, cost.scale, salary)

	# divide our years uniformly, 100 pts a year
	dist.year <- as.data.frame( 0:(max.yrs*100)/100 )
	names(dist.year) <- "tenure"
	dist.year$benefit <- empBenefit(dist.year$tenure, max.benefit)
	dist.year$cost <- empCost(dist.year$tenure, cost.ramp, cost.scale, salary)

	# calc breakeven points
	be.pt.id <- which.max((dist.year$benefit - dist.year$cost)>0)
	be.pt <- dist.year$tenure[be.pt.id]
	be.cume.id <- which.max(cumsum(dist.year$benefit - dist.year$cost)>0)
	be.cume <- dist.year$tenure[be.cume.id]

	writeLines(sprintf("At this rate net benefit begins at year %.2f, breakeven at year %.2f",
					   be.pt, be.cume))

	fig1 <- ggplot(data=dist.year, aes(x=tenure)) + 
			geom_ribbon(fill="red", size=0, aes(ymax=cost,ymin=benefit,alpha=cost>benefit)) + 
			scale_alpha_discrete(range=c(0,.1)) + 
			theme(legend.position="none") +
			geom_line(col="red", size=1, aes(y=cost)) + 
			geom_line(col="green", size=1, aes(y=benefit)) +
			scale_y_continuous(labels = percent) +
			geom_vline(xintercept=be.pt,col="Blue", size=0.5, linetype="dashed") +
			geom_vline(xintercept=be.cume,col="Salmon", size=0.5, linetype="dashed") +
			theme_bw() +
			theme(legend.position="none") +
			labs(title="Monthly Benefit & Cost from One Employee", 
				 x="Tenure in Years", 
				 y="% Potential Monthly Value") 

	fig2 <- ggplot(data=dist.good, aes(x=tenure)) + 
			geom_histogram(fill="DarkGreen") +
			geom_vline(xintercept=be.pt,col="Blue", size=0.5, linetype="dashed") +
			geom_vline(xintercept=be.cume,col="Salmon", size=0.5, linetype="dashed") +
			theme_bw() +
			xlim(c(0,max.yrs)) +
			labs(title="Distribution of 'good.fit' Employee Tenure", 
				 x="Tenure in Years", 
				 y="Count") 

	fig3 <- ggplot(data=dist.bad, aes(x=tenure)) + 
			geom_histogram(fill="DarkRed") +
			geom_vline(xintercept=be.pt,col="Blue", size=0.5, linetype="dashed") +
			geom_vline(xintercept=be.cume,col="Salmon", size=0.5, linetype="dashed") +
			theme_bw() +
			xlim(c(0,max.yrs)) +
			labs(title="Distribution of 'bad.fit' Employee Tenure", 
				 x="Tenure in Years", 
				 y="Count") 

	fig.all <- arrangeGrob(fig1, fig2, fig3)

	# fig4 <- ggplot(data=dist.good, aes(x=tenure, y=benefit-cost)) + 
	# 		geom_point(fill="darkred") +
	# 		theme_bw() +
	# 		xlim(c(0,max.yrs)) +
	# 		labs(title="Foo", 
	# 			 x="Cost", 
	# 			 y="Benefit") 
	
	writeLines(sprintf("Sim for max.benefit = %.2f, cost.ramp = %.2f, cost.scale = %.2f, salary = %.2f",
					   max.benefit, cost.ramp, cost.scale, salary))
	writeLines(sprintf("        center.bad = %.2f, center.good = %.2f, good.bad.ratio = %.2f",
					   center.bad, center.good, good.bad.ratio))

	be.good <- mean(dist.good$benefit - dist.good$cost)*100
	be.bad <- mean(dist.bad$benefit - dist.bad$cost)*100

	writeLines(sprintf("sum.good = %.1f%%, sum.bad = %.1f%%, net = %.1f%%", 
					   be.good, be.bad, be.good + be.bad))

	cume.good <- mean(dist.good$benefit.cume - dist.good$cost.cume)*100
	cume.bad <- mean(dist.bad$benefit.cume - dist.bad$cost.cume)*100

	writeLines(sprintf("cume.good = %.1f%%, cume.bad = %.1f%%, net = %.1f%%", 
					   cume.good, cume.bad, cume.good + cume.bad))

	return(fig.all)
}

manipSim202 <- function() {
	manipulate(runSim202(max.yrs, max.benefit, cost.ramp, cost.scale, salary,
						 center.bad, center.good, good.bad.ratio, run.size=10000),
			   max.yrs = slider(1, 15, initial=3),
			   max.benefit = slider(0.25, 3, initial = 1),
			   cost.ramp = slider(0.5, 3, initial=1.7), 
			   cost.scale = slider(0.5, 3, initial=1.5), 
			   salary = slider(0, 1, initial=0.5),
			   center.bad = slider(0.01, 3, initial=0.9), 
			   center.good = slider(0.01, 3, initial=2),
			   good.bad.ratio = slider(0, 1, initial=0.3)) 

	# also can do checkbox, dropdown, button
}
	
empBenefit <- function(tenure, max.benefit) {
	# vector-friendly benefit from employee, modeled as a sigmoid function
	1/(1+exp(-(tenure/max.benefit*12-6)))
}

empBenefitCume <- function(tenure, max.benefit) {
	# vector friendly cumulative benefit, the integral of empBenefit
	# use sapply to make integrate is vector-friendly
	sapply(tenure, function(x) { integrate(empBenefit, 0, x, 
										   max.benefit=max.benefit)$value })
}

empCost <- function(tenure, cost.ramp, cost.scale, salary) {
	# vector-friendly cost of employee, modeled as a gompertz function
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

# probably some cute ways to draw this
# but the sum is what is the goal

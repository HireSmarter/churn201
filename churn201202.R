library(ggplot2)
library(gridExtra)
library(scales)
library(manipulate)
library(reshape)
library(survival)
library(GGally)

# special graphs for merged 201-202 article
# one big difference is that we are running in dollars now
# removed a lot of extra stuff from 202.
# might need to re-add some integration

# plots with % not prob   
# bigger text  
# no title  
# trademark graph   
# arrows   
# grey grid  
# Say Engagement in plots x x   

# to run these by hand:
#   zdist <- calcDist()
#	zbreak <- calcBreakeven(zdist)


setDefaults <- function() {
	gpal <- scales::hue_pal(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction=1)(8)

	# set up my defaults in a global list
	def <- list()
	def$benefit.full <- 1.5			# years to rull ramp up
	def$benefit.amt <- 50000		# dollars benefit per year
	def$benefit.slope <- 0.05		# apr-style annual interest

	def$cost.ramp <- 1.5
	def$cost.scale <- 400			# no relation to final depth
	def$salary.amt <- 25000
	def$salary.raise <- 0.05		# annual raise at 1-year marks
	
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

calcDist <- function() {
	
	### TIME
	# divide our years uniformly, 200 work days a year (from now one day = 1/200 year)
	cost.df <- data.frame( tenure=0:(def$max.yrs*200)/200 )

	### BENEFIT
	# daily employee benefit 
	cost.df$benefit <- empBenefit(cost.df$tenure)
	# cumulative emp benefit - can do this since there are 200 work days in a year
	cost.df$benefit.cume <- cumsum(cost.df$benefit)

	### COST
	# daily emp cost
	cost.df$cost <- empCost(cost.df$tenure)
	# cumulative emp cost
	cost.df$cost.cume <- cumsum(cost.df$cost)

	### NET
	cost.df$value <- cost.df$benefit - cost.df$cost
	cost.df$value.cume <- cumsum(cost.df$value)

	### PROBABILITIES
	# prob of good.fit terminating today, within good.fit sample
	cost.df$prob.good <- dweibull(cost.df$tenure, shape=def$shape.good, scale=def$scale.good)/200
	# prob of bad.fit terminating today, within bad.fit sample
	cost.df$prob.bad <- dweibull(cost.df$tenure, shape=def$shape.bad, scale=def$scale.bad)/200

	### CDF for survival
	cost.df$cdf.good <- cumsum(cost.df$prob.good)
	cost.df$cdf.bad <- cumsum(cost.df$prob.bad)

	return(cost.df)
}

calcBreakeven <- function(cost.df=calcDist()) {
	# calc breakeven points
	# TODO C: could solve for breakeven point in another way, to handle off-chart cases, 
	#         but in that case, they would be off the chart.... so who cares.

	break.even <- list()
	break.even$pt.id <- which.max(cost.df$benefit - cost.df$cost > 0)
	break.even$pt <- cost.df$tenure[break.even$pt.id]
	break.even$cume.id <- which.max(cost.df$benefit.cume - cost.df$cost.cume > 0)
	break.even$cume <- cost.df$tenure[break.even$cume.id]

	writeLines(sprintf("Daily breakeven at %.2f months, cume breakeven at %.2f months", 
					   break.even$pt * 12, break.even$cume * 12))
	return(break.even)
}

calcLTV <- function(cost.df=calcDist()) {
	full.ltv <- cost.df$value.cume[nrow(cost.df)]
	good.ltv <- sum(cost.df$value.cume * cost.df$prob.good)
	bad.ltv <- sum(cost.df$value.cume * cost.df$prob.bad)

	writeLines(sprintf("Full LTV at 4 years $%s, good LTV $%s, bad ltf $%s", 
					   format(round(full.ltv), big.mark=",", scientific=F),
					   format(round(good.ltv), big.mark=",", scientific=F),
					   format(round(bad.ltv), big.mark=",", scientific=F)))

	return(c(full.ltv, good.ltv, bad.ltv))
}

genTimeline <- function(emp=500) {
	set.seed(49)
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

###

empBenefit <- function(tenure) {

	# vector-friendly benefit from employee, modeled as a sigmoid function
	zb <- 1/(1+exp(-(tenure/def$benefit.full*12-6)))
	# multiply to dollar amount from annual salary
	zb <- zb * def$benefit.amt / 200
	# multiply to slope, but don't start slope until ramp to full
	zslope <- sapply(tenure, function(x) {ifelse( x < def$benefit.full,
												 1,
												 1 + def$benefit.slope/200)})
	# cumprod adds one at the end, so have to truncate
	zb <- zb * cumprod(zslope)[1:length(zb)]
	return(zb)	
}

empCost <- function(tenure) {
	# vector-friendly cost of employee, modeled as a gompertz function
	zcost <- exp(-exp(def$cost.ramp * tenure)) * def$cost.scale
	# salary
	zsal <- (1+def$salary.raise)^(as.integer(zdist$tenure)) * def$salary.amt/200

	return(zcost + zsal)
}

###

g.copyright <- function(g.input, text.size=2, x=4, y=-0.00015) {

	g.input +
		annotate("text", 
				x=x, y=y, hjust=0, vjust=0,
				size=text.size,
				color=def$col.be,
				label="© Copyright 2014, Talent Analytics Corp.")
}

g.probTerm <- function(cost.df=calcDist(), break.even=calcBreakeven(cost.df), 
					   max.yrs=def$max.yrs, do.annotate=TRUE, line.size=1, text.size=10 ) {
	zg <- ggplot(data=cost.df, aes(x=tenure)) + 
			# geom_vline(xintercept=break.even$pt, col=def$col.be, size=line.size/2, linetype="dashed") +
			# geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=line.size/2, linetype="dashed") +
			geom_line(aes(y=prob.bad), col=def$col.bad, size=line.size) +
			geom_line(aes(y=prob.good), col=def$col.good, size=line.size) +
			scale_y_continuous(labels = percent) +
			xlim(c(0, max.yrs)) +
			theme(legend.position="none", 
				  text = element_text(size = text.size*2) ) +
			labs(x="Tenure in Years", 
				 y="Daily Probability of Termination")

	if (do.annotate) {
		zg <- zg + 
				annotate("text", 
						x=2.01, y=0.0033, hjust=0, vjust=1,
						size=text.size,
						color=def$col.good,
						label="High\nEngagement") +
				annotate("text", 
						 x=0.7, y=0.0062, hjust=0, vjust=1,
						 size=text.size,
						 color=def$col.bad,
						 label="Low\nEngagement")
	}
	return(zg)
}

g.costBenefit <- function(cost.df=calcDist(), break.even=calcBreakeven(cost.df), 
						  max.yrs=def$max.yrs, do.annotate=TRUE, line.size=1, text.size=10 ) {
	zg <- ggplot(data=cost.df, aes(x=tenure)) + 
			geom_vline(xintercept=break.even$pt, col=def$col.be, size=line.size/2, linetype="dashed") +
			geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=line.size/2, linetype="dashed") +
			geom_ribbon(fill=def$col.cost, size=0, aes(ymax=cost,ymin=benefit,alpha=cost>benefit)) + 
			scale_alpha_discrete(range=c(0,.25)) + 
			theme(legend.position="none") +
			geom_line(col=def$col.cost, size=line.size, aes(y=cost)) + 
			geom_line(col=def$col.benefit, size=line.size, aes(y=benefit)) +
			scale_y_continuous(labels = dollar) +
			theme(legend.position="none", 
				  text = element_text(size = text.size*2) ) +
			labs(x="Tenure in Years", 
				 y="Daily Cost or Benefit")

	if (do.annotate) {
		zg <- zg +
			 annotate("text", 
					  x=2.5, y=240, hjust=0, vjust=0,
					  color=def$col.benefit,
					  size=text.size,
					  label="Benefit") +
			 annotate("text", 
					  x=2.5, y=110, hjust=0, vjust=0,
					  color=def$col.cost,
					  size=text.size,
					  label="Cost") +
			 annotate("text", 
					  x=break.even$pt, y=275, hjust=-0.1, vjust=0,
					  color=def$col.be,
					  size=text.size/2,
					  label="B/E") +
			 annotate("text", 
					  x=break.even$cume, y=275, hjust=-0.1, vjust=0,
					  color=def$col.be,
					  size=text.size/2,
					  label="B/E Cume") 
	}
	return(zg)
}

g.cumeValue <- function(cost.df=calcDist(), break.even=calcBreakeven(cost.df),
						max.yrs=def$max.yrs, do.annotate=TRUE, line.size=1, text.size=10) {

	zg <- ggplot(data=cost.df, aes(x=tenure)) + 
			geom_vline(xintercept=break.even$pt, col=def$col.be, size=line.size/2, linetype="dashed") +
			geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=line.size/2, linetype="dashed") +
			geom_hline(yintercept=0, col=def$col.be.cume, size=line.size/2, linetype="dotted") +
			geom_ribbon(size=0, alpha=0.5, ymin=0,
						aes(ymax=(benefit.cume-cost.cume)/1000, 
							fill=(benefit.cume-cost.cume)>0)) + 
			geom_line(size=line.size, 
					  aes(y=(benefit.cume-cost.cume)/1000,
						  col=(benefit.cume-cost.cume)>0)) +
			scale_y_continuous(labels = dollar) +
			scale_fill_manual(values=c(def$col.cost, def$col.benefit)) +
			scale_color_manual(values=c(def$col.cost, def$col.benefit)) +
			xlim(c(0,max.yrs)) +
			theme(legend.position="none", 
				  text = element_text(size = text.size*2) ) +
			labs(x="Tenure in Years", 
				 y="Cumulative Value in Thousands")

	if (do.annotate) {
		zg <- zg +
				 annotate("text", 
						  x=3, y=-7, hjust=0.5, vjust=0,
						  color=def$col.benefit,
						  size=text.size,
						  label="Net Benefit") +
				 annotate("text", 
						  x=0.8, y=7, hjust=0.5, vjust=1,
						  color=def$col.cost,
						  size=text.size,
						  label="Net Cost") +
				 annotate("text", 
						  x=break.even$pt, y=50, hjust=-0.1, vjust=0,
						  color=def$col.be,
						  size=text.size/2,
						  label="B/E") +
				 annotate("text", 
						  x=break.even$cume, y=50, hjust=-0.1, vjust=0,
						  color=def$col.be,
						  size=text.size/2,
						  label="B/E Cume")
	}
	return(zg)
}

g.survivalCurveGoodBad <- function(cost.df=calcDist(), break.even=calcBreakeven(cost.df), 
								   max.yrs=def$max.yrs, do.annotate=TRUE, line.size=1, text.size=6) {

	## get atttr at 1 year = 201
	z.rate.good <- 1 - cost.df$cdf.good[201]
	z.rate.bad <- 1 - cost.df$cdf.bad[201]
	writeLines(sprintf("Attrition rates good %.1f%% bad %.1f%%", (1-z.rate.good)*100, (1-z.rate.bad)*100))

	zg <- ggplot(data=cost.df, aes(x=tenure)) + 

		   # geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=line.size/2, linetype="dashed") +

		   geom_line(aes(y=1 - cdf.good), col=def$col.good, size=line.size) +
		   geom_line(aes(y=1 - cdf.bad), col=def$col.bad, size=line.size) +


		   xlim(c(0,max.yrs)) +
		   ylim(c(0,1)) +

		   scale_y_continuous(labels = percent) +
		   theme(legend.position="none", 
				 text = element_text(size = text.size*10/6*2) ) +

		   labs(x="Tenure in Years", 
				y="Probability of Reaching Tenure")

	if (do.annotate) {
		zg <- zg +
			   geom_segment(x = 1, y = 0, xend = 1, yend = z.rate.good, 
							linetype="dotted",
							col=def$col.bad,
							size=line.size) +
			   geom_segment(x = 0, y = z.rate.bad, xend = 1.2, yend = z.rate.bad, 
							linetype="dotted",
							col=def$col.bad,
							size=line.size) +
			   geom_segment(x = 1, y = 1, xend = 1, yend = z.rate.good, 
							linetype="dotted",
							col=def$col.good,
							size=line.size) +
			   geom_segment(x = 0, y = z.rate.good, xend = 1.5, yend = z.rate.good, 
							linetype="dotted",
							col=def$col.good,
							size=line.size) +

				 annotate("text", 
						  x=1.5, y=z.rate.good-0.05, hjust=0, vjust=0,
						  color=def$col.good,
						  size=text.size,
						  label=sprintf("High Engagement\n%.0f%% Survival\n%.0f%% Attrition", z.rate.good*100, (1-z.rate.good)*100)) +
				 annotate("text", 
						  x=1.2, y=z.rate.bad-0.06, hjust=-0, vjust=0,
						  color=def$col.bad,
						  size=text.size,
						  label=sprintf("Low Engagement\n%.0f%% Survival\n%.0f%% Attrition", z.rate.bad*100, (1-z.rate.bad)*100)) 
	}
	return(zg)
}

g.histogram <- function(d.timeline=genTimeline(), break.even=calcBreakeven(), 
						max.yrs=def$max.yrs, text.size=10) {

	zg <- ggplot(data=d.timeline, aes(x=tenure.yrs)) + 
			   geom_histogram(binwidth=1/12, fill=def$col.benefit) + 

			   geom_vline(xintercept=break.even$pt, col=def$col.be, size=0.5, linetype="dashed") +
			   geom_vline(xintercept=break.even$cume, col=def$col.be.cume, size=0.5, linetype="dashed") +

			   annotate("text", 
						x=break.even$pt, y=25, hjust=-0.1, vjust=0,
						color=def$col.be,
						size=text.size/2,
						label="B/E") +
			  annotate("text", 
					   x=break.even$cume, y=25, hjust=-0.1, vjust=0,
					   color=def$col.be,
					   size=text.size/2,
					   label="B/E Cume") +

			   xlim(c(0, max.yrs)) +

			   theme(text = element_text(size=text.size*2)) +
					 
			   labs(x="Tenure in Years", 
					y="Count")
	return(zg)
}

runFigures <- function() {

	# calc with defaults
	cost.df <- calcDist()
	break.even <- calcBreakeven(cost.df)

	ggsave("~/gitInternal/ta_presentations/images/churnMerge/probTerm.png",
		   g.copyright(g.probTerm(cost.df, break.even), x=3, y=-0.0001),
		   height=6, width=8, dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/churnMerge/costBenefit.png",
		   g.copyright(g.costBenefit(cost.df, break.even), x=3, y=-5),
		   height=6, width=8, dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/churnMerge/cumeValue.png",
		   g.copyright(g.cumeValue(cost.df, break.even), x=3, y=-26),
		   height=6, width=8, dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/churnMerge/survivalCurveGoodBad.png",
		   g.copyright(g.survivalCurveGoodBad(cost.df, break.even), x=3, y=-0.02),
		   height=6, width=8, dpi=100)

	ggsave("~/gitInternal/ta_presentations/images/churnMerge/histogram.png",
		   g.copyright(g.histogram(), x=3, y=-0.5),
		   height=6, width=8, dpi=100)
}

# everything below here runs on source
def <- setDefaults()

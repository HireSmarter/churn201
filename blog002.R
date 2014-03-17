library(reshape)
library(ggplot2)
library(ggthemes)
library(scales)

max.yrs <- 3		# max number of years to show on plot
max.benefit <- 1.5	# year at which employee delivers fully-trained value (asymptote) 

cost.ramp <- 1.7	# higher numbers speed up time before costs = salary
cost.scale <- 1.5		# higher numbers increase height of original training costs
salary <- 0.5		# monthly salary as a percent of fully trained value delivered to company
pts.yr <- 100		# how many points per year

# set up data frame with time series in months
emp.value <- as.data.frame(0:(max.yrs*pts.yr)/pts.yr)
names(emp.value)<- "tenure.yrs"

# set up benefit function, modeled as a sigmoid
# t<-seq(0,1,.1); p<-1/(1+exp(-t*12+6)) 
# p defines a nice sigmoid for 0..1
emp.value$benefit <- 1/(1+exp(-(emp.value$tenure.yrs/max.benefit*12-6)))

# set up cost function, modeled as a gompertz
emp.value$cost <- exp(-exp(cost.ramp * emp.value$tenure.yrs)) * cost.scale + salary

# calc breakeven points
be.pt.id <- which.max((emp.value$benefit - emp.value$cost)>0)
be.pt <- emp.value$tenure.yrs[be.pt.id]
be.cume.id <- which.max(cumsum(emp.value$benefit - emp.value$cost)>0)
be.cume <- emp.value$tenure.yrs[be.cume.id]

writeLines(sprintf("At this rate net benefit begins at year %.2f, breakeven at year %.2f",
				   be.pt, be.cume))

# plot time vs. monthly cost, benefit
fig1 <- ggplot(data=emp.value,
			   aes(x=tenure.yrs)) + 
	geom_ribbon(fill="red", size=0, aes(ymax=cost,ymin=benefit,alpha=cost>benefit)) + 
	scale_alpha_discrete(range=c(0,.1)) + 
	theme(legend.position="none") +

	geom_line(col="red", aes(y=cost)) + 
	geom_line(col="green", aes(y=benefit)) +
	theme(legend.position="bottom") +

	annotate("point", 
			 x=emp.value$tenure.yrs[be.pt.id], 
			 y=emp.value$cost[be.pt.id], 
			 color="Blue") +
	annotate("text", 
			 x=emp.value$tenure.yrs[be.pt.id]+0.02, 
			 y=emp.value$cost[be.pt.id], 
			 color="Blue",
			 size=4,
			 label="Monthly Breakeven",
			 hjust=0, vjust=0) +
	annotate("point", 
			 x=emp.value$tenure.yrs[be.cume.id], 
			 y=emp.value$cost[be.cume.id], 
			 color="DarkGreen") +
	annotate("text", 
			 x=emp.value$tenure.yrs[be.cume.id], 
			 y=emp.value$cost[be.cume.id] - 0.02, 
			 size=4,
			 color="DarkGreen",
			 label="Cumulative\nBreakeven",
			 hjust=0, vjust=1) +
	scale_y_continuous(labels = percent) +
	theme_bw() +
	theme(legend.position="none") +
	labs(title="Monthly Benefit & Cost from One Employee", 
		 x="Tenure in Years", 
		 y="% Potential Monthly Value") 

# png("figure_1.png")
# print(fig1)
# dev.off()

gen.timeline <- function() {
	yrs <- 4.2
	days <- yrs*365
	emp <- 150
	tenure.mean <- 1.5
	tenure.sd <- 1
	target <- 2014.2

	d.t <- as.data.frame(1:emp)
	names(d.t)[1] <- "id"

	d.t$hire <- sample(days, emp, replace=TRUE)/365 + 2010
	d.t$tenure.yrs <- rnorm(emp, tenure.mean, tenure.sd )
	d.t$tenure.yrs <- sapply(d.t$tenure.yrs, function(x) { max(0.1,x)})
	d.t$term <- d.t$hire + d.t$tenure.yrs
	d.t$term.class <- factor(ifelse(d.t$term <= target, "already.term",
									ifelse(d.t$hire <= target, "still.working",
										   "not.hired.yet" )))

	return(d.t)
}

d.timeline <- gen.timeline()

writeLines(sprintf("%.1f%% employees below monthly breakeven, %.1f%% below cume breakeven",
				  	sum(d.timeline$tenure.yrs<be.pt)/nrow(d.timeline)*100, 
					sum(d.timeline$tenure.yrs<be.cume)/nrow(d.timeline)*100))

fig2 <- ggplot(d.timeline,
		   aes(x=hire, xmin=hire, xmax=term, y=id, col=term.class)) + 
		geom_errorbarh(height=5) + 
		scale_color_hue(name="term.class") +
		geom_vline(xintercept=2014.2,col="red", size=1) +
		scale_x_continuous(limits=c(2010,2016)) +
		labs(x="Hire Date", y="Employees") +
		theme_bw() + 
		theme(legend.position="bottom",
			  axis.ticks.y=element_blank(),
			  axis.text.y=element_blank())
# print(fig2)


fig3 <- ggplot(d.timeline,
		   aes(x=tenure.yrs, xmin=0, xmax=tenure.yrs,
			   y=id, col=term.class)) + 
		geom_errorbarh(height=5) + 
		scale_color_hue(name="term.class") +
		geom_vline(xintercept=be.pt,col="Blue", size=1) +
		geom_vline(xintercept=be.cume,col="DarkGreen", size=1) +
		scale_x_continuous() +
		labs(x="Hire Date", y="Employees") +
		theme_bw() + 
		theme(legend.position="bottom",
			  axis.ticks.y=element_blank(),
			  axis.text.y=element_blank())
print(fig1)

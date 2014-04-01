library(reshape)
library(ggplot2)
library(ggthemes)
library(scales)

max.yrs <- 3		# max number of years to show on plot
max.benefit <- 1.5	# year at which employee delivers fully-trained value (asymptote) 

cost.ramp <- 1.5	# higher numbers speed up time before costs = salary
cost.scale <- 3		# higher numbers increase height of original training costs
salary <- 0.5		# monthly salary as a percent of fully trained value delivered to company

# set up data frame with time series in months
emp.value <- as.data.frame(0:(max.yrs*12)/12)
names(emp.value)<- "tenure.yrs"

# set up benefit function, modeled as a sigmoid
emp.value$benefit <- 1/(1+exp(-(emp.value$tenure.yrs/max.benefit*12-6)))

# set up cost function, modeled as a gompertz
emp.value$cost <- exp(-exp(cost.ramp * emp.value$tenure.yrs)) * cost.scale + salary

# calc breakeven points
be.mon.id <- which.max((emp.value$benefit - emp.value$cost)>0)
be.mon <- emp.value$tenure.yrs[be.mon.id]
be.cume.id <- which.max(cumsum(emp.value$benefit - emp.value$cost)>0)
be.cume <- emp.value$tenure.yrs[be.cume.id]

writeLines(sprintf("At this rate net benefit begins at year %.2f, breakeven at year %.2f", be.mon, be.cume))

# plot time vs. monthly cost, benefit
fig1<-ggplot(data=melt(emp.value, id.vars="tenure.yrs"), aes(x=tenure.yrs, y=value, col=variable)) +
	geom_hline(yintercept=1, size=1, linetype="dashed", col="white") +
	geom_line(size=1) +
	annotate("text", 
			 x=emp.value$tenure.yrs[be.mon.id]+0.02, 
			 y=emp.value$cost[be.mon.id], 
			 color=economist_pal()(6)[6], 
			 label="Monthly Breakeven",
			 hjust=0, vjust=0) +
	annotate("text", 
			 x=emp.value$tenure.yrs[be.cume.id], 
			 y=emp.value$cost[be.cume.id], 
			 color=economist_pal()(6)[6], 
			 label="Cumulative\nBreakeven",
			 hjust=0) +
	scale_y_continuous(labels = percent) +
	theme_economist(horizontal=FALSE) +
	scale_colour_economist(name="") + 
	labs(title="Monthly Benefit & Cost from One Employee", 
		 x="Tenure in Years", 
		 y="% Potential Monthly Value") 

# png("figure_1.png")
print(fig1)
# dev.off()

# plot time vs. net monthly benefit
fig2<-ggplot(data=emp.value, aes(x=tenure.yrs)) +
	geom_line(col=economist_pal()(1), size=1, aes(y=benefit-cost)) + 
	geom_hline(yintercept=0, size=1, linetype="dashed", col="white") +
	scale_y_continuous(labels = percent) +
	theme_economist(horizontal=FALSE) +
	scale_colour_economist() + 
	labs(title="Net Monthly Benefit from One Employee", 
		 x="Tenure in Years", 
		 y="Net Monthly Benefit (in % Potential)")

# png("figure_2.png")
print(fig2)
# dev.off()

# plot time vs cumulative benefit
fig3<-ggplot(data=emp.value, aes(x=tenure.yrs)) +
	geom_line(col=economist_pal()(1), size=1, aes(y=cumsum(benefit-cost))) + 
	geom_hline(yintercept=0, size=1, linetype="dashed", col="white") +
	scale_y_continuous(labels = percent) +
	theme_economist(horizontal=FALSE) +
	scale_colour_economist() + 
	labs(title="Cumulative Benefit from One Employee", 
		 x="Tenure in Years", 
		 y="Cumulative Value (in % Potential)")

# png("figure_3.png")
print(fig3)
# dev.off()

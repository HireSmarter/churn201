## Employee Churn 201: Calculating Employee Value

Welcome to the GitHub outpost of the [recent Predictive Analytics Times article][patimes] by Pasha Roberts from [Talent Analytics][ta].

Our goal here is to foster conversation and learning, including learning by us.

## The Quantitative Scissors
In the first blog entry, we put out a stylized model of employee value, plotting out monthly employee costs and benefits.
We define the way that these costs operate, to **show why high attrition is costly to an employer.**
We remark that this model acts as a "quantitative scissors" that limit our options to reduce the damage done by attrition.

We make a somewhat **bold claim that there are only 6 actions available to reduce attrition damage** - to support this claim,
we are publishing the code here for the model.  You can experiment, plot, present, and interact with the data *and with us.*

![Figure1][]

While this current graph could be done in excel, spreadsheets run out of analytics fuel quickly, and are not nearly as transparent as the attached code.
Spreadsheets are just the wrong tool for the job, especially given what is coming up in future installments.

### Modifying the Model

These are the six leverage points, that we claim are the only ways to escape the scissors.  Under each, we show how you can modify the model to change the graphs:

1. **Decrease hiring/onboarding costs**
	- `cost.scale` controls the height of the initial hiring/onboarding costs
	- `cost.ramp` controls the slope of the cost decrease; this pushes the cost asymptote to flatten sooner or later
2. **Decrease time to full productivity**
	- `max.benefit` sets the year at which the employee reaches full productivity.  This slides the top of the "S" sooner or later.
3. **Decrease salary**
	- `salary` sets the salary, in terms of 100% productivity
4. **Increase overall productivity** (which is at odds with reducing salary and cutting training)
	- Currently, nothing changes overall productivity - but you could change line 18 to do so, or plot a second curve to show a comparison
5. **Decrease employee turnover prior to the full productivity phase**
	- This would show up better on Figure 3, perhaps comparing two individuals
	- The corollary of this is would be to terminate likely-bad employees sooner
	- A future blog will model this out more visually!
6. **Hire to increase the proportion of employees who are likely to "survive" to the full productivity phase**
	- This is similar to 5 - we want to see more employees survive to breakeven, and the cheapest way to do so is to predictively admit a better class of employee.
	- A future blog will also model this out more visually!

Those are the variables that control the scissors.
You can change the position of the lines, but **there is no real way to escape the fact that employees are a net cost until they are up to speed.**
If you can find another way to escape the scissors, please do tell.

It is tempting to add inflation and wage increases.
But bear in mind that the unit listed here is **percent of total productivity**.
If there is inflation, the dollar value of everything goes up - productive gain, salary, training cost, everything.
They would inflate a different rates and time frames, but generally the graph would be unchanged.
Also, if you want to model increasing productivity as someone learns, then you are really talking about a longer "S" curve.
So... our choice of unit simplifies these cases considerably.

If you are a math geek, you can change the cost or benefit functions as well:

- The benefit curve is a [Sigmoid function][sigmoid], `1 / (1 + exp(-t))`
	- I was experimenting with a more skewed sigmoid, showing slower gains at first.  But, the time period is so short, that it really doesn't make much difference for the complexity.
- The cost curve is a [Gompertz function][gompertz], `a * exp(b * exp(c * t))`

You are welcome to embellish these to match your scenarios or fantastic imaginations.
I wanted to shade in the left side of the X to make the "cost region" more clear, or to show the problems when the employee terminates before the breakeven, but deadlines loomed.  Maybe on the next blog entry, or maybe one of you will code it up.

I'm curious what you will cook up!

### The Lost Plots

Also, you can see here two plots that did not make it in to the paper:

![Figure2][]

![Figure3][]

> Copyright &copy; 2014, Talent Analytics, Corp.

[patimes]: http://www.predictiveanalyticsworld.com/patimes/employee-churn-201-calculating-employee-value/
[ta]: http://www.talentanalytics.com
[sigmoid]: http://en.wikipedia.org/wiki/Sigmoid_function
[gompertz]: http://en.wikipedia.org/wiki/Gompertz_curve

[Figure1]: plots/pat201_fig1.png "Figure 1: Monthly Cost & Benefit from One Employee"
[Figure2]: plots/pat201_fig2.png "Figure 2: Net Monthly Benefit from One Employee"
[Figure3]: plots/pat201_fig3.png "Figure 3: Cumulative Benefit from One Employee"

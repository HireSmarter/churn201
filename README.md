## Employee Churn 201: Calculating Employee Value


>takes the simulation at the defaults set up in the code, and manipulates one variable at a time, to see what happens when you move that variable alone. It says that the main leverage points in the system are the mix of good/bad employees at the entry point, or to reduce salary. The latter has problems and side effects. Anyway It may be too wonky to show in detail in the article, but it’s the most powerful part of the analysis. Curious if this seems too wonky to you, or if you feel your CIO would look at you like you have three heads if you try showing him that…
 
>The main plot has three sections:
>The “quantitative scissors” from churn201 article.
>You can manipulate this with max.benefit, cost.ramp, cost.scale and salary
>A plot showing the probability of termination, across tenure, for two groups of employees:
>Orange is bad fit. They largely leave within a year. You can manipulate this with shape.bad and scale.bad.
>Blue is good fit. They stick around for 1-2 years. You can manipulate this with ‘shape.goodand 'scale.good.
>The bottom plot shows how the cumulative costs stack up for good+bad. The sum of the shaded area is your probability-weighted average employee net benefit. You can change the mix of good and bad employees with good.bad.ratio.

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

### Future Topics

Future blogs, and code, will address:

- Differentiating "good" and "bad" churn
- Variables, time windows, analytical methods and black boxes
- Survival analysis
- Intervention and uplift modeling - what is the employee analogy to "Sleeping Dogs" and "Persuadables" in [marketing churn][mktchurn]?
- Using cost information to tune models - are false negatives or false positives more expensive?

### Using R

To use this code, either download the single `blog001.R` file, or use the `Download ZIP` button, or best of all, use git to clone the project.
Running the code is easy, if you are familiar with R:

- Open R, R Studio, or whatever R platform you use.
- Set the working directory to your downloaded/cloned directory
- `source(blog001.R)` to run the numbers, based on the hardcoded values.
- Display the graph by printing `fig1`
- To change assumptions, edit the program and repeat.

This is designed to be short and simple, so there are no fancy sliders or D3 visualizations... yet.
As new blog articles come out, we will publish additional code to support the new ideas.

### Submitting Updates

We do encourage experimentation, and you are welcome to present useful ["pull requests"][pullreq] or ["issues"][issues] to this project to make it better.



[patimes]: http://www.predictiveanalyticsworld.com/patimes/employee-churn-201-calculating-employee-value/
[ta]: http://www.talentanalytics.com
[sigmoid]: http://en.wikipedia.org/wiki/Sigmoid_function
[gompertz]: http://en.wikipedia.org/wiki/Gompertz_curve
[mktchurn]: http://stochasticsolutions.com/pdf/SavedAndDrivenAway.pdf
[pullreq]: https://github.com/talentanalytics/churn201/pulls
[issues]: https://github.com/talentanalytics/churn201/issues 
[Figure1]: figure_1.png "Figure 1: Monthly Cost & Benefit from One Employee"
[Figure2]: figure_2.png "Figure 2: Net Monthly Benefit from One Employee"
[Figure3]: figure_3.png "Figure 3: Cumulative Benefit from One Employee"

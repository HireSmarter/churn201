## Employee Churn 201: Calculating Employee Value

Welcome to the GitHub outpost of the [recent Predictive Analytics Times article](http://www.predictiveanalyticsworld.com/patimes/) by Pasha Roberts from [Talent Analytics](http://www.talentanalytics.com).

Our goal here is to foster conversation and learning, including learning by us.
In the article, we put out a stylized model of employee value, plotting out monthly employee costs and benefits:

![Figure 1](figure_1.png)

## The Quantitative Scissors
In the blog, we define the way that these costs operate, to show why high attrition cause is costly to an employer.
We remark that this model acts as a "quantitative scissors" that define our options to reduce the damage done by attrition, to these choices:

- Decrease hiring/onboarding costs
- Decrease time to full productivity
- Decrease salary/productivity ratio
- Increase overall productivity (which is at odds with all above points)
- Decrease employee turnover prior to the full productivity phase
- Hire to increase the proportion of employees who are likely to "survive" to the full productivity phase

To support this bold claim, we are publishing the code for the model, for your own experimentation in R.
While it could be done in excel, spreadsheets run out of analytics fuel quickly, and are not nearly as transparent as the attached code.

### Modifying the Model

Different variables control the few leverage points in these scissors:

- `cost.scale` controls initial hiring/onboarding costs; the height of the time-zero costs
- `cost.ramp` controls the slope of the cost decrease; this pushes the cost asymptote forward and back
- `max.benefit` sets the year at which the employee reaches full productivity
- `salary` sets the salary, in terms of 100% productivity
- Currently, nothing changes overall productivity - but you could change line 18 to do so.

Those are the variables that control the scissors - the area to the left of the X, which makes up the cumulative costs seen in Figure 3.

The other options don't apply to an individual, but to the total stream of hired individuals:

- Decrease employee turnover prior to the full productivity phase
- Hire to increase the proportion of employees who are likely to "survive" to the full productivity phase

### Future Topics

Future blogs, and code, will address:

- Differentiating "good" and "bad" churn
- Variables, time windows, analytical methods and black boxes
- Survival analysis
- Intervention and uplift modeling - what is the employee analogy to "Sleeping Dogs" and "Persuadables" in [marketing churn](http://stochasticsolutions.com/pdf/SavedAndDrivenAway.pdf)?
- Using cost information to tune models - are false negatives or false positives more expensive?

### Using R

To use this code, either download the single `blog001.R` file, or use the `Download ZIP` button, or best of all, use git to clone the project.
Running the code is easy, if you are familiar with R:

- Open R, R Studio, or whatever R platform you use.
- Set the working directory to your downloaded/cloned directory
- `source blog001.R` to run the numbers, based on the hardcoded values.
- Display the graph by printing `fig1`
- To change assumptions, edit the program and repeat.

This is designed to be short and simple, so there are no fancy sliders or D3 visualizations... yet.
As new blog articles come out, we will publish additional code to support the new ideas.

### Submitting Updates

We do encourage experimentation, and you are welcome to present useful "pull requests" or "issues" to this project to make it better.

### The Lost Plots

Also, you can see here two plots that did not make it in to the paper:

![Figure 2](figure_2.png)

![Figure 3](figure_3.png)

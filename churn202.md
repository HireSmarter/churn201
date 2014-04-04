## Employee Churn 202: Good and Bad Churn

Welcome to the GitHub outpost of the recent Predictive Analytics Times ["Churn 202" article][patimes] by Pasha Roberts from [Talent Analytics][ta].

If you are impatient and just want to play with the graphs, you might want to just load the program in RStudio and go:

- `source("churn202.R")` 
- `manipSim202()`
- Read more in the [interactive model](#running-the-interactive-simulation) section.

### Libraries and Loading The Program
As before, this program uses the fairly common `ggplot2`, `gridExtra`, and `scales` libraries.
To enable live interaction in the `manipSim202()` function, the system uses the RStudio-only `manipulate` library.
[Manipulate][manip] is an interesting, cross-platform, powerful way to easily add sliders and other controls to R programs.
My main criticism is that it doesn't smoothly update the way that D3 does, but see below for my thoughts on issues with bringing this to the web.

To load the program, type `source("churn202.R")` .
If it works, it will say nothing, but you'll see code load into the variables section in the top right panel.

### Changing Defaults

Default values are stored in a global-variable list named `def`.
To change defaults, just change the values in the source code and re-source it.
Or, change the variable during runtime.

#### Benefit/Cost Variables:

- `max.benefit` = year at which employee benefit reaches 100% (default 0.5)
- `cost.ramp` = slope of cost function (default 3.5)
- `cost.scale` = size of initial cost function (default 2)
- `salary` = level of employee salary (default 0.5)

#### Distribution Variables:

These variables use the standard ["shape" and "scale" formulations of the Weibull distribution][weibull].
The shape tends to make the curve more or less skewed, and the scale tends to change its horizontal size.

- `shape.good` = shape of "good fit" distribution (default 2.5)
- `scale.good` = scale of "good fit" distribution (default 1.5)
- `shape.bad` = shape of "bad fit" distribution (default 1.66)
- `scale.bad` = scale of "bad fit" distribution (default 0.33)
- `good.bad.ratio` = percentage of employees who are "good fit" (default 0.6)

#### Graphing variables:

- `max.yrs` = number of years that show up on the chart (default 3)
- `col.benefit` = color of benefit lines
- `col.cost` = color of cost lines
- `col.good` = color of good lines
- `col.bad` = color of bad lines
- `col.be` = color of breakeven lines
- `col.be.cume` = color of cumulative breakeven lines

There is no way to save variables from a simulation into new defaults.
This would be nice - currently I admit to writing them down.
If you want to code a way to do this, please send me a pull request.

### Running Histograms

To output the histograms run `runHistograms()` .
The histograms seen in figures 1-3 show a sample of 1000 employees in "good fit" and "bad fit" roles.
The distribution is based from a Weibull distribution, as configured in the default variables.

Note the parameters to change the sample size, annotation, graph grobbing, and more.

![Figure1][]

![Figure2][]

![Figure3][]

### Running the Static Simulation

To output the four plots describing the static model at the default values, run `runSim202()` .
This does not require RStudio.
The plots are:

![Figure4][]

Figure 4 shows the above Weibull distributions as configured by `shape.good`, `scale.good`, `shape.bad`, and `scale.bad`.  Orange is "bad fit," and Blue is "good fit."

![Figure5][]

Figure 5 shows the benefit-cost curve and cutoff lines, as derived in Churn 201, and configured by `max.benefit`, `cost.ramp`, `cost.scale`, and `salary`.  These are the "quantitative scissors."

![Figure6][]

Figure 6 shows the Cumulative Net Benefit curve and cutoff lines, as configured by `max.benefit`, `cost.ramp`, `cost.scale`, and `salary`.

![Figure7][]

Figure 7 shows the Expected Cumulative Net Benefit curves for "good fit" and "bad fit", plus the value of the EVH in text.  These curves and EVH are derived from all above variables as well as `good.bad.ratio`.

Note the other parameters to change annotation, graph grobbing, and more.

Every time the model is run, it prints text to the console, such as:

	Daily breakeven at 0.28, cume breakeven at 0.76
	Good Fit: 48.1% net benefit * 60% weight = 28.8% overall contribution
	Bad Fit: -17.0% net benefit * 40% weight = -6.8% overall contribution
	Overall EVH = 22.0%

These mean what they say: 

- The year marks at which daily and cumulative breakeven is made
- The net benefit of "good fit" and "bad fit" populations, times their weights
- The overall EVH - our objective function


### Running the Interactive Simulation

To output the four plots with fancy sliders, using RStudio, run `manipSim202()`.
If you don't see the slider controls, click on the little gear in the graph area.
The output is exactly as in the [static model](#running-the-static-simulation), (in fact you will see that the program just runs the static model with new parameters), so please refer to that section for tips on modifying the output.

As with the static model, text is output with every run.
This way you have a running log of model results as you experiment.

### Web-Based Interactive Simulation

We are looking at options to put this up on the web, with one of the reactive web visualization platforms.
Normally we would look at a simpler, snazzier platform like D3, but as you can see in the code there are some rather intense numeric integrations involved.
Maybe we leave the EVH calculation aside and show the graphs with D3?

To use R directly, it looks like we could easily wrap this in Shiny, a R-based webvis platform.
But, that needs a special host with some restrictions and expensive proprietary software.
We're still figuring it out, but it isn't done yet.
Maybe by the time we get to Churn 204.
Please let us know if you have any ideas.

### Calculating EVH
If you just want numbers, and none of these heavy graphs, no problem.
From the command line, use `runPredNetCume()` to return the EVH value, overriding any of the defaults for the specific parameters that you want.

	> runPredNetCume(good.bad.ratio=0.2)
	[1] -0.03978231

To get the verbose output, specify `verbose=TRUE`:

	> runPredNetCume(verbose=TRUE)
	Good Fit: 48.1% net benefit * 60% weight = 28.8% overall contribution
	Bad Fit: -17.0% net benefit * 40% weight = -6.8% overall contribution
	Overall EVH = 22.0%
	[1] 0.2204147

	> runPredNetCume(verbose=TRUE, good.bad.ratio=0.8)
	Good Fit: 48.1% net benefit * 80% weight = 38.4% overall contribution
	Bad Fit: -17.0% net benefit * 20% weight = -3.4% overall contribution
	Overall EVH = 35.1%
	[1] 0.3505133


### Secret Preview: Sensitivity

I tend to write this kind of article by writing programs and numbers first, then the text.
In this case, the EVH concept took me well over the normal word count, therefore we threw some concepts overboard until next month.

One concept, already programmed, was sensitivity analysis.
I'll leave it as an exercise to the reader to run `runSensitivityTests()`, or to just wait until next month.
All I can say, is that computing power has sure come a long way since I was putting punched cards into the mainframe; this is pretty amazing.


### Feedback

As before, I encourage the community to engage with us on GitHub - comment, make issues, send pull requests.
We do find this of great value, and hope that we can connect with others who do so as well.

> Copyright &copy; 2014, Talent Analytics, Corp.  All Rights Reserved.

[patimes]: http://www.predictiveanalyticsworld.com/patimes/employee-churn-202-good-bad-churn/
[ta]: http://www.talentanalytics.com

[manip]: http://www.rstudio.com/ide/docs/advanced/manipulate
[weibull]: http://www.weibull.com/hotwire/issue14/relbasics14.htm

[Figure1]: plots/pat202_fig1.png
[Figure2]: plots/pat202_fig2.png
[Figure3]: plots/pat202_fig3.png
[Figure4]: plots/pat202_fig4.png
[Figure5]: plots/pat202_fig5.png
[Figure6]: plots/pat202_fig6.png
[Figure7]: plots/pat202_fig7.png

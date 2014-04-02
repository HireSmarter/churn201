## Employee Churn 202: Good and Bad Churn

Welcome to the GitHub outpost of the recent Predictive Analytics Times ["Churn 202" article][patimes] by Pasha Roberts from [Talent Analytics][ta].

If you are impatient and just want to play with the graphs, you might want to just load the program in RStudio and go:

- `source("blog202.R")` 
- `manipSim202()`
- Read more in the [interactive model][#inter] section.

### Libraries and Loading The Program
As before, this program uses the fairly common `ggplot2`, `gridExtra`, and `scales` libraries.
To enable live interaction in the `manipSim202()` function, the system uses the RStudio-only `manipulate` library.
[Manipulate][manip] is an interesting, cross-platform, powerful way to easily add sliders and other controls to R programs.
My main criticism is that it doesn't smoothly update the way that D3 does, but see below for my thoughts on issues with bringing this to the web.

To load the program, type `source("blog202.R")` .
If it works, it will say nothing, but you'll see code load into the variables section in the top right panel.

### Changing Defaults

Default values are stored in a global-variable list named `def`.
To change defaults, just change the values in the source code and re-source it.
Or, change the variable during runtime.

#### Benefit/Cost Variables:

- `max.benefit` = year at which employee benefit reaches 100% (default 0.5)
- `cost.ramp` = slope of cost function (default 3.5)
- `cost.scale` = size of initial cost function (default 2)
- `salary` = level of employee salary (default 50%)

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

- Figure 4 shows the above Weibull distributions as configured by `shape.good`, `scale.good`, `shape.bad`, and `scale.bad`.  Orange is "bad fit," and Blue is "good fit."
- Figure 5 shows the benefit-cost curve and cutoff lines, as derived in Churn 201, and configured by `max.benefit`, `cost.ramp`, `cost.scale`, and `salary`.  These are the "quantitative scissors."
- Figure 6 shows the Cumulative Net Benefit curve and cutoff lines, as configured by `max.benefit`, `cost.ramp`, `cost.scale`, and `salary`.
- Figure 7 shows the Expected Cumulative Net Benefit curves for "good fit" and "bad fit", plus the value of the EVH in text.  These curves and EVH are derived from all above variables as well as `good.bad.ratio`.

Note the parameters to change the sample size, annotation, graph grobbing, and more.

![Figure4][]

![Figure5][]

![Figure6][]

![Figure7][]

<a id="inter"></a>
### Running the Interactive Simulation

manipSim202() to run the 3-part graph with interactive sliders that change most everything in the simulation. Unfortunately I haven’t labeled all of them in english yet so until that window dressing is done, you can guess, ask me, or read the code… You can see what happens so it’s not hard to figure out.

The main plot has three sections:

The “quantitative scissors” from churn201 article.
You can manipulate this with max.benefit, cost.ramp, cost.scale and salary
A plot showing the probability of termination, across tenure, for two groups of employees:
Orange is bad fit. They largely leave within a year. You can manipulate this with shape.bad and scale.bad.
Blue is good fit. They stick around for 1-2 years. You can manipulate this with ‘shape.goodand 'scale.good.
The bottom plot shows how the cumulative costs stack up for good+bad. The sum of the shaded area is your probability-weighted average employee net benefit. You can change the mix of good and bad employees with good.bad.ratio.
### Web-Based Interactive Simulation

### Calculating EVH
enough of these graphs, give me the numbers

### Secret Preview: Sensitivity

>takes the simulation at the defaults set up in the code, and manipulates one variable at a time, to see what happens when you move that variable alone. It says that the main leverage points in the system are the mix of good/bad employees at the entry point, or to reduce salary. The latter has problems and side effects. Anyway It may be too wonky to show in detail in the article, but it’s the most powerful part of the analysis. Curious if this seems too wonky to you, or if you feel your CIO would look at you like you have three heads if you try showing him that…

runSensitivityTests() takes the simulation at the defaults set up in the code, and manipulates one variable at a time, to see what happens when you move that variable alone. It says that the main leverage points in the system are the mix of good/bad employees at the entry point, or to reduce salary. The latter has problems and side effects. Anyway It may be too wonky to show in detail in the article, but it’s the most powerful part of the analysis. Curious if this seems too wonky to you, or if you feel your CIO would look at you like you have three heads if you try showing him that…


> Copyright &copy; 2014, Talent Analytics, Corp.  All Rights Reserved.

[patimes]: http://www.predictiveanalyticsworld.com/patimes/employee-churn-201-calculating-employee-value/
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

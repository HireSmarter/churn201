## GitHub Companion to the Employee Churn 201 Series

The Predictive Analytics Times has been kind enough to publish a series articles by [Talent Analytics'][ta] Chief Scientist, [Pasha Roberts][pasha].
The articles focus is entirely on business and quantitative issues that impact **Employee Attrition**.

The primary articles are:

- [Churn 201][churn201]: Calculating Employee Value (2/20/2013)
- [Churn 202][churn202]: Good and Bad Churn (4/1/2013)
- Churn 203: Sensitivity Analysis and Experiment design *(upcoming)*
- Churn 204: Survival analysis for employee attrition *(upcoming)*
- Churn 205: Uplift modeling and intervention - what is the employee analogy to "Sleeping Dogs" and "Persuadables" in marketing churn? *(upcoming)*
- Churn 206: Variables, time windows, analytical methods and black boxes *(upcoming)*
- Churn 207: Using cost information to tune models - are false negatives or false positives more expensive? *(upcoming)*

An online blog can only go so far, technically.
So to foster conversation and learning, including learning by us, we are putting more content here on GitHub.
We believe that GitHub is an important and useful platform for collaboration between data researchers, enabling direct sharing of code, data, ideas, and results.

In this series we are posting the R code used to generate graphs, models, and insights.
In a way, this model is the unified engine for this Churn 20x Series.
We have put this code, currently over 500 lines of R, online so that other researchers (and clients) can download, experiment, and engage.
If you don't like our Weibull distributions, you can swap in a Log-Logit or whatever you want.
If you want to create a U-shaped cost curve, go ahead.

Each article has its own supporting article and code:

- [GitHub for Churn 201][github201] *(86 lines of code)*
	- Build plots
	- Modify employee cost/benefit models.
- [GitHub for Churn 202][github202] *(503 lines of code)*
	- Run interactive simulation of hiring costs
	- Calculate `Expected Value of Hiring`
	- Preview of sensitivity study

### Using GitHub

[Git][git] is a non-proprietary source control system, entirely separate from GitHub.
*GitHub* is more like a de facto central breeding ground for thousands of open software projects.
The best way to use the system is to [join and engage][joingh].
It's free, but there is some SSL configuration to use it properly.
Welcome to the new world!

If you are on GitHub, you just use git to "clone" a copy of the Churn 201 project to your local system.
Barring that, you can simply click on the "Zip" icon to the right.
You can even download the individual `blog201.R` and `blog202.R` files directly from the file manager above.

Once you have the files, let's say you have created an innovative new cost curve or dynamic system that you'd like to share.
Or, maybe you found a bug and want to send your fix.
You can share your these changes back to us with a ["pull request"][pullreq].
Or, you could "fork" your own variant of the project.
If you find a bug, create an ["issue."][issues] 

Keep us posted and engage!

### Using R

[R][rproject] is an open-source, free statistical platform that has taken the data science community by storm.
There are free libraries for almost anything one would ever want to do, and it is increasingly seen as the reference platform for most work in this domain.
We tend to treat R more like a statistically aware, vector-friendly programming language, than a pretty GUI system or graphing package.

The programs in this GitHub can be run either directly [from the console][rproject], or in the free and powerful [RStudio][rstudio], or likely on other platforms such as [RCommander][rcommander].
My personal preference is to use the console; but some may call it austere.
The interactive, slider-based model in [Churn 202][github202] does use the "manipulate" libraries that are only found in RStudio.

Running the code is easy:

- Open R, R Studio, or whatever R platform you use.
- Set the working directory to your downloaded/cloned directory with `setwd("/run/dir/name")` 
- Load the libraries and program with `source`
	- `source()` the relevant program, such as `source("blog201.R")` or `source("blog202.R")`.
	- This commands loads the relevant libraries and programs into memory.
	- Success is silent, but in RStudio you will see objects load into the top right panel.
- If you are missing libraries, you will need to install some packages.
	- For example, Churn202 uses the following packages: `ggplot2`, `gridExtra`, `scales`, and `manipulate`. 
	- In RStudio, use the Tools&gt;Install Libraries menu, or find the same option on the lower right panel.
	- From the console, simply run `install.packages()`.
- Follow the instructions on the companion page, [Churn 201][github201] or [Churn 202][github202]:
	- Run short commands as noted
	- Explore, view, modify objects
	- Create and modify plots
	- Modify the code, re-source, and re-run

As new blog articles come out, we will publish additional code to support the new ideas.

> Copyright &copy; 2014, Talent Analytics, Corp.

[ta]: http://www.talentanalytics.com
[pasha]: https://twitter.com/pasharoberts

[churn201]: http://www.predictiveanalyticsworld.com/patimes/employee-churn-201-calculating-employee-value/
[github201]: https://github.com/talentanalytics/churn201/blob/master/churn201.md

[churn202]: http://www.predictiveanalyticsworld.com/patimes/employee-churn-202-good-bad-churn/
[github202]: https://github.com/talentanalytics/churn201/blob/master/churn202.md

[statmethods]: http://statmethods.net
[rproject]: http://www.r-project.org/
[rstudio]: http://www.rstudio.com/
[rcommander]: http://socserv.mcmaster.ca/jfox/Misc/Rcmdr/

[git]: http://git-scm.com/
[joingh]: https://github.com/join
[pullreq]: https://github.com/talentanalytics/churn201/pulls
[issues]: https://github.com/talentanalytics/churn201/issues

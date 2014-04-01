## Employee Churn 202: Good and Bad Churn



>takes the simulation at the defaults set up in the code, and manipulates one variable at a time, to see what happens when you move that variable alone. It says that the main leverage points in the system are the mix of good/bad employees at the entry point, or to reduce salary. The latter has problems and side effects. Anyway It may be too wonky to show in detail in the article, but it’s the most powerful part of the analysis. Curious if this seems too wonky to you, or if you feel your CIO would look at you like you have three heads if you try showing him that…
 
>The main plot has three sections:
>The “quantitative scissors” from churn201 article.
>You can manipulate this with max.benefit, cost.ramp, cost.scale and salary
>A plot showing the probability of termination, across tenure, for two groups of employees:
>Orange is bad fit. They largely leave within a year. You can manipulate this with shape.bad and scale.bad.
>Blue is good fit. They stick around for 1-2 years. You can manipulate this with ‘shape.goodand 'scale.good.
>The bottom plot shows how the cumulative costs stack up for good+bad. The sum of the shaded area is your probability-weighted average employee net benefit. You can change the mix of good and bad employees with good.bad.ratio.

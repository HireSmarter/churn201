source("churn202.R")

dist.year <- calcDist()
break.even <- calcBreakeven(dist.year)
evh <- runPredNetCume()
time.big <- genTimeline(500)
time.med <- genTimeline(200)
time.small <- genTimeline(50)

ht <- 26.4 
wd <- 36
dpi <- 100


# 1a cost scissors = costBenefit
# ggsave("~/gitInternal/ta_presentations/images/banner/banner1a.png",
# 	   g.costBenefit(dist.year, break.even,do.annotate=TRUE, line.size=5, text.size=25), 
# 	   height=ht, width=wd,dpi=dpi)

# 1b cost sharkfin = costArea
# ggsave("~/gitInternal/ta_presentations/images/banner/banner1b.png",
# 	   g.costArea(dist.year, break.even,
# 				  main.title="Est Costs & Benefits for One Employee",
# 				  line.size=5, text.size=25),
# 	   height=ht,width=wd,dpi=dpi)

# 1c employee streams = timeline
# ggsave("~/gitInternal/ta_presentations/images/banner/banner1c.png",
# 	   g.timeline0(time.med, line.size=2, line.height=0.02, text.size=25),
# 	   height=ht,width=wd,dpi=dpi)

# 1d screen shot = redo,expand
# done

# 2a prediction scatter = make up
zp <- data.frame(x=rweibull(500,shape=3,scale=50),y=1,z=sample(c("a","b","c"), 500, replace=TRUE))
zp$y <- zp$x * rweibull(500,shape=9, scale=0.01)
zg <- ggplot(zp, aes(x=x, y=y, col=z)) +
		geom_point(size=10) +
		scale_color_brewer(palette="Set1") + 
		theme_bw() +
		theme(legend.position="none",
			  text=element_text(size = 100)) +
		labs(title="Top Performance is Predictable",
			 x="Performance KPI",
			 y="Prediction")
ggsave("~/gitInternal/ta_presentations/images/banner/banner2a.png",
	   zg, height=ht,width=wd,dpi=dpi)

# 2b survival bands = asb

# 2c uplift = asb

# 2d crisp
#   by jackie

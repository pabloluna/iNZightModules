library(gWidgets)
library(ggplot2)
options("guiToolkit"="RGtk2")

#Building window's structure.

win <- gwindow("Density Graphic Module")

big <- ggroup(container=win)
g <- ggroup(horizontal=FALSE, container=big, expand=TRUE)

#Creating text elements.
lbl1 <- glabel("Density Graphic Module", cont = g)
font(lbl1) <- list(weight = "bold", family = "normal", size   = 12)
add(g, lbl1, anchor = c(0, 0))
addSpace(g, 20)

#Reading data. Local needs to be changed.
mydata <- read.csv(file="C:/Users/Silas Yudi/Desktop/NZIncomes03_11000.csv")

#Creating plotting options.
lv1 <- glabel("Attribute of Interest:", cont = g)
dplist <- gcombobox(names(mydata), cont = g)
lv2 <- gcheckbox("Graphic fill", cont = g) #This needs to be checked, so the transparency can work.
lv3 <- glabel("Transparency:", cont = g)
transp <- gslider(0, 1, by = 0.1, cont = g)

#Creating function. The values will be related to the weekly income in this test.
newPlot = function(h,...)
{
	p1 <- switch(
		svalue(dplist), 
		"age_midpt" = ggplot(mydata, aes(x = weekly_income, colour = age_midpt)),
		"age_cat" = ggplot(mydata, aes(x = weekly_income, colour = age_cat)),
		"sex" = ggplot(mydata, aes(x = weekly_income, colour = sex)),
		"ethnicity" = ggplot(mydata, aes(x = weekly_income, colour = ethnicity)),
		"highest_qualification" = ggplot(mydata, aes(x = weekly_income, colour = highest_qualification)),
		"weekly_hrs" = ggplot(mydata, aes(x = weekly_income, colour = weekly_hrs)),
		"weekly_income" = ggplot(mydata, aes(x = weekly_income, colour = weekly_income)) 
	)
	
	if (svalue(lv2, index = TRUE)) {
		print(p1 + geom_density(fill = svalue(lv2, index = TRUE), alpha = svalue(transp)))
	} else {
		print(p1 + geom_density())
	}
}

#Creating button to activate the plot function.
plot_button <- gbutton(
  text = "Generate Plot",
  container = g,
  handler = newPlot
)

#Building container for the plot.
parg <- ggroup(horizontal=FALSE, cont=big, label="plot args")
qpg <- ggraphics(container=big, label="plot")

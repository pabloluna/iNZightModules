##' iNZight Model Fitting Module
##'
##' A GUI add-on for fitting regression models.
##'
##' @title iNZight Model Fitting Module
##'
##' @author Tom Elliott
##'
##' @import iNZightRegression
##'
##' @export iNZightRegMod
##' @exportClass iNZightRegMod
iNZightRegMod <- setRefClass(
    "iNZightRegMod",
    fields = list(
        GUI         = "ANY",
        mainGrp     = "ANY",
        activeData  = "data.frame",
        smryOut     = "ANY", smryInd = "numeric",
        regPlots    = "ANY", plotInd = "numeric",
        nbIndex     = "ANY"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI)

            activeData <<- GUI$getActiveData()

            GUI$initializeModuleWindow(.self)
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(5)

            GUI$plotToolbar$update(NULL, refresh = NULL)

            # addSpace(mainGrp, 15)

            lbl1 <- glabel("Model Fitting Module")
            font(lbl1) <- list(weight = "bold",
                               family = "normal",
                               size   = 11)
            add(mainGrp, lbl1, anchor = c(0, 0))
            addSpace(mainGrp, 20)


            addSpring(mainGrp)
            bot <- ggroup(container = mainGrp)

            helpButton <- gbutton("Help",
                                  handler = function(h, ...) {
                                      browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=multiple_response")
                                  })
            homeButton <- gbutton("Home",
                                handler = function(h, ...) {
                                    ## delete the module window
                                    delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                    ## display the default view (data, variable, etc.)
                                    GUI$plotToolbar$restore()
                                    visible(GUI$gp1) <<- TRUE
                                })

            add(bot, helpButton, expand = TRUE, fill = TRUE)
            add(bot, homeButton, expand = TRUE, fill = TRUE)


            visible(GUI$moduleWindow) <<- TRUE


            ## Now create new tab for SUMMARY output:
            smryOut <<- gtext("Regression model output...\n",
                              font.attr = list(weight = "bold"))
            add(GUI$plotWidget$plotNb, smryOut, label = "Model Output", close.button = FALSE)
            smryInd <<- svalue(GUI$plotWidget$plotNb)

            regPlots <<- ggraphics(expand = TRUE)
            add(GUI$plotWidget$plotNb, regPlots, label = "Model Plots", close.button = FALSE)
            plotInd <<- svalue(GUI$plotWidget$plotNb)
            plot(1:10)

            ## So now, can swith between text and plot tabs ...
            svalue(GUI$plotWidget$plotNb) <<- smryInd

            for (i in 1:5) {
                insert(smryOut, paste0("\nModel ", i, ":"))

                fit <- lm(height ~ armspan + gender, data = activeData)
                smry <- capture.output(summary(fit))
                insert(smryOut, smry, font.attr = list(family = "monospace"))
            }
        }
    )
)

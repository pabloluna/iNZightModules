##' iNZight Time Series Module
##'
##' A GUI add-on for visualising and doing basic inference and prediction of time series data.
##'
##' @title iNZight Time Series Module
##'
##' @author Eric Lim
##'
##' @import iNZightTS
##'
##' @export iNZightTSMod
##' @exportClass iNZightTSMod
iNZightTSMod <- setRefClass(
    "iNZightTSMod",
    fields = list(
        GUI         = "ANY",
        mainGrp     = "ANY",
        activeData  = "data.frame",
        timeVar     = "ANY",
        patternType = "numeric",
        smthSlider  = "ANY", smoothness = "numeric",
        tsObj       = "ANY",
        yLab        = "ANY", xLab = "ANY",
        plottype    = "numeric",
        compare     = "numeric",
        animateBtn  = "ANY", pauseBtn = "ANY",
        recomposeBtn = "ANY", recomposeResBtn = "ANY", decomp = "ANY",
        forecastBtn = "ANY", forecasts   = "ANY"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI, patternType = 1, smoothness = 10, tsObj = NULL,
                       plottype = 1, compare = 1)

            dat = GUI$getActiveData()
            activeData <<- tsData(dat)
            timeVar <<- getTime(activeData, index = FALSE)

            GUI$initializeModuleWindow(.self)
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(5)

            ## playBtn <- iNZight:::gimagebutton(stock.id = "media-play", handler = function(h, ...) updatePlot(animate = TRUE))
            GUI$plotToolbar$update(NULL, refresh = "updatePlot")#, extra = list(playBtn))

            ################
            ###  fields  ###
            ################
            frameFont = list(weight = "bold")

            #################################
            ###  set up frame containers  ###
            #################################
            g1 = gframe("Time Information", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            g2 = gframe("Model Settings", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            addSpring(mainGrp)

            midGrp <- ggroup(container = mainGrp, fill = TRUE)
            g3 = gframe("Series Variables", pos = 0.5, horizontal = FALSE,
                        container = midGrp, fill = TRUE)
            g5 = gframe("Plot Type Options", pos = 0.5, horizontal = FALSE,
                        container = midGrp, fill = TRUE, expand = TRUE)

            g4 = gframe("Customize Labels", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)


            g1$set_borderwidth(8)
            g2$set_borderwidth(8)
            g3$set_borderwidth(8)
            g4$set_borderwidth(8)

            g5$set_borderwidth(8)

            ## bold-faced title for the frames
            frames = getToolkitWidget(mainGrp)$getChildren()
            mainGrp$set_rgtk2_font(frames[[1]]$getChildren()[[2]], frameFont)
            mainGrp$set_rgtk2_font(frames[[2]]$getChildren()[[2]], frameFont)
            midGrp$set_rgtk2_font(getToolkitWidget(midGrp)$getChildren()[[1]]$getChildren()[[2]], frameFont)
            midGrp$set_rgtk2_font(getToolkitWidget(midGrp)$getChildren()[[2]]$getChildren()[[2]], frameFont)
            mainGrp$set_rgtk2_font(frames[[5]]$getChildren()[[2]], frameFont)

            ############
            ###  g1  ###
            ############
            ## FOR MAIN LAYOUT
            g1_layout = glayout(container = g1)
            g1_opt1   = gradio(c("Select time variable", "Provide time manually"),
                               selected = 1, horizontal = FALSE)
            g1_layout[1, 1:2, expand = TRUE] = g1_opt1

            ## FOR LAYOUT A
            g1a_layout = glayout(container = g1)
            ## g1a options

            g1a_opt1   = gcombobox(names(activeData),
                                   selected = match(timeVar, names(activeData)),
                                   handler = function(h, ...) {
                                       timeVar <<- svalue(h$obj)
                                       print(timeVar)
                                       updatePlot()
                                   })
            ## g1a labels
            g1a_lab1   = glabel("Select time variable:")
            ## g1a layout
            g1a_layout[2, 1, expand = TRUE, anchor = c(-1, 0)] = g1a_lab1
            g1a_layout[2, 2, expand = TRUE]   = g1a_opt1

            ## FOR LAYOUT B
            # g1b_layout <- glabel("Not implemented yet.", container = g1)
            # visible(g1b_layout) <- FALSE
            g1b_layout = glayout(container = g1)
            visible(g1b_layout) = FALSE
            ## g1b options
            g1b_opt1  = gedit("")
            g1b_opt2  = gedit("")
            g1b_opt3  = gedit("")
            size(g1b_opt1) = c(120, 21)
            size(g1b_opt2) = c(120, 21)
            size(g1b_opt3) = c(120, 21)
            ## g1b labels
            g1b_lab1  = glabel("Specify start date:")
            g1b_lab2  = glabel("Specify season:")
            g1b_lab3  = glabel("Specify frequency:")
            ## g1b layout
            g1b_layout[2, 1, expand = TRUE, anchor = c(-1, 0)] = g1b_lab1
            g1b_layout[3, 1, expand = TRUE, anchor = c(-1, 0)] = g1b_lab2
            g1b_layout[4, 1, expand = TRUE, anchor = c(-1, 0)] = g1b_lab3
            g1b_layout[2, 2, expand = TRUE] = g1b_opt1
            g1b_layout[3, 2, expand = TRUE] = g1b_opt2
            g1b_layout[4, 2, expand = TRUE] = g1b_opt3

            addHandlerChanged(g1_opt1, handler = function(h,...) {
                if (svalue(h$obj, index = TRUE) == 1) {
                    visible(g1a_layout) = TRUE
                    visible(g1b_layout) = FALSE
                } else {
                    visible(g1a_layout) = FALSE
                    visible(g1b_layout) = TRUE
                }

            })

            ############
            ###  g2  ###
            ############
            g2_layout = glayout(container = g2, spacing = 5)
            g2_opt1   = gradio(c("Multiplicative", "Additive"), selected = patternType,
                               horizontal = TRUE,
                               handler = function(h, ...) {
                                   patternType <<- svalue(h$obj, index = TRUE)
                                   updatePlot()
                               })

            g2_layout[1, 1, anchor = c(1, 0), expand = TRUE] <- glabel("Seasonal pattern :")
            g2_layout[1, 2, expand = TRUE] = g2_opt1

            ## Smoother
            smthSlider <<- gslider(0, 100, by = 0.1, value = smoothness,
                                   handler = function(h, ...) {
                                       smoothness <<- svalue(h$obj)
                                       updatePlot()
                                   })

            g2_layout[2, 1, anchor = c(1, 0), expand = TRUE] <- glabel("Smoothness :")
            g2_layout[2, 2, fill = TRUE, expand = TRUE] <- smthSlider

            ############
            ###  g3  ###
            ############
            ## NOTE:
            ##   need to change the variable selection widget for when there
            ##   are many variables which will expand the widget.
            g3_layout = glayout(container = g3)
            g3_opt1 = gtable(names(activeData)[! names(activeData) %in% timeVar],
                             multiple = TRUE)
            size(g3_opt1) <- c(floor(size(GUI$leftMain)[1] * 0.5), 200)
            g3_layout[1, 1, expand = TRUE] = g3_opt1

            addHandlerSelectionChanged(g3_opt1, function(h, ...) {
                if (length(svalue(g3_opt1)) == 0) {
                    visible(novar) <- TRUE
                    return()
                }
                visible(novar) <- FALSE

                ## make dataset an iNZightTS object
                var_ind <- which(names(activeData) %in% svalue(h$obj))
                if (length(var_ind) == 1) {
                    visible(onevar) <- TRUE
                    visible(multivar) <- FALSE
                } else {
                    visible(onevar) <- FALSE
                    visible(multivar) <- TRUE
                }
                tsObj <<- iNZightTS::iNZightTS(data = activeData, var = var_ind,
                                               time.col = which(colnames(activeData) == timeVar))
                updatePlot()
            })


            ############
            ###  g5  ###
            ############

            onevar <- gvbox(container = g5)
            addSpring(onevar)
            plotType <- gradio(c("Standard", "Decomposition", "Seasonal", "Forecast"), selected = plottype,
                               container = onevar, expand = TRUE,
                               handler = function(h, ...) {
                                   plottype <<- svalue(h$obj, index = TRUE)
                                   visible(animateBtn) <<- svalue(h$obj, TRUE) == 1
                                   visible(recomposeBtn) <<- FALSE
                                   visible(recomposeResBtn) <<- FALSE
                                   visible(forecastBtn) <<- FALSE
                                   updatePlot()
                               })

            tsenv <- new.env()
            assign("stopAnimation", FALSE, envir = tsenv)
            runAnimation <- gaction("Animate", icon = "gtk-media-play",
                                    handler = function(h, ...) {
                                        assign("stopAnimation", FALSE, envir = tsenv)
                                        enabled(animateBtn) <<- FALSE
                                        enabled(pauseBtn) <<- TRUE
                                        iNZightTS::rawplot(tsObj, multiplicative = (patternType == 1),
                                                           ylab = svalue(yLab), xlab = svalue(xLab), animate = TRUE, t = smoothness,
                                                           e = tsenv)
                                        enabled(pauseBtn) <<- FALSE
                                        enabled(animateBtn) <<- TRUE
                                    })
            pauseAnimation <- gaction("End Animation", icon = "gtk-media-stop",
                                      handler = function(h, ...) {
                                          assign("stopAnimation", TRUE, envir = tsenv)
                                      })
            animateBtn <<- gbutton(action = runAnimation, container = onevar)
            pauseBtn <<- gbutton(action = pauseAnimation, container = onevar)
            enabled(pauseBtn) <<- FALSE

            recomposeBtn <<- gbutton("Recompose", container = onevar,
                                     handler = function(h, ...) {
                                        #  iNZightTools::newdevice(width = 6, height = 8)
                                         decomp <<- decompositionplot(tsObj, multiplicative = (patternType == 1),
                                                                          xlab = svalue(xLab), ylab = svalue(yLab), t = smoothness)
                                         iNZightTS::recompose(decomp)
                                     })
            visible(recomposeBtn) <<- FALSE
            recomposeResBtn <<- gbutton("Recompose Result", container = onevar)
            addHandlerClicked(recomposeResBtn, function(h, ...) {
                blockHandlers(h$obj)
                if (svalue(h$obj) == "Re-decompose") {
                    updatePlot()
                    svalue(recomposeResBtn) <<- "Recompose Result"
                } else {
                    iNZightTS::recompose(decomp, animate = FALSE)
                    svalue(recomposeResBtn) <<- "Re-decompose"
                }
                unblockHandlers(h$obj)
            })
            visible(recomposeResBtn) <<- FALSE

            forecastBtn <<- gbutton("Forecasted Values", container = onevar,
                                    handler = function(h, ...) {
                                        w <- gwindow("Time Series Forecasts", parent = GUI$win,
                                                     width = 400, height = 300)
                                        g <- gvbox(container = w)
                                        t <- gtext(text = "", container = g, expand = TRUE,
                                                   wrap = FALSE, font.attr = list(family = "monospace"))
                                        insert(t, capture.output(print(forecasts)))
                                    })
            visible(forecastBtn) <<- FALSE

            multivar <- ggroup(container = g5)
            compareChk <- gradio(c("Single graph",
                                   "Separate graphs"),
                                 checked = compare,
                                 container = multivar,
                                 handler = function(h, ...) {
                                     compare <<- svalue(h$obj, index = TRUE)
                                     updatePlot()
                                 })

            visible(onevar) <- FALSE
            visible(multivar) <- FALSE

            novar <- gvbox(container = g5)
            glabel("Select a Variable.", container = novar)
            lb <- glabel("(Hold CTRL to select multiple)", container = novar)
            font(lb) <- list(size = 8)



            ############
            ###  g4  ###
            ############
            g4_layout = glayout(container = g4)
            g4_lab1   = glabel("x-axis")
            g4_lab2   = glabel("y-axis")
            xLab <<- gedit(timeVar)
            yLab <<- gedit("")

            addHandlerKeystroke(xLab, function(h, ...) updatePlot())
            addHandlerKeystroke(yLab, function(h, ...) updatePlot())

            #size(xLab) <<- c(150, 21)
            #size(yLab) <<- c(150, 21)

            g4_layout[1, 1:2, expand = TRUE, anchor = c(-1, 0)] = g4_lab1
            g4_layout[2, 1:2, expand = TRUE, anchor = c(-1, 0)] = g4_lab2
            g4_layout[1, 3, expand = TRUE] = xLab
            g4_layout[2, 3, expand = TRUE] = yLab

            clearXlab <- iNZight:::gimagebutton(stock.id = "reset", handler = function(h, ...) {
                svalue(xLab) <<- timeVar
            })
            g4_layout[1, 4] <- clearXlab
            clearYlab <- iNZight:::gimagebutton(stock.id = "reset", handler = function(h, ...) {
                svalue(yLab) <<- ""
            })
            g4_layout[2, 4] <- clearYlab


            btmGrp <- ggroup(container = mainGrp)

            helpButton <- gbutton("Help", expand = TRUE, fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=time_series")
                                  })
            homeButton <- gbutton("Home", expand = TRUE, fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      ## delete the module window
                                      delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                      ## display the default view (data, variable, etc.)
                                      GUI$plotToolbar$restore()
                                      visible(GUI$gp1) <<- TRUE
                                  })

            ## Make the module visible
            visible(GUI$moduleWindow) <<- TRUE

            ## IF time series variable is chosen, plot first variable.
            svalue(g3_opt1, index = TRUE) <- 1
        },

        # ========
        # METHODS
        # ========
        ## returns the time variable index
        getTime = function(data, index = TRUE) {
            ## look for time or date
            time_re = "([Tt][Ii][Mm][Ee])|([Dd][Aa][Tt][Ee])"
            ind     = grep(time_re, names(data))
            if (index) return(ind)
            else if (length(ind) == 0) return(NA)
            else return(names(data)[ind])
        },

        ## checks for a time variable in dataset
        isTS = function(data) {
            return(length(getTime(data)) != 0)
        },

        ## drops categorical variables (except the time variable)
        tsData = function(data) {
            time_index = getTime(data)

            data[, c(time_index, which(sapply(data, is.numeric)))]
            ## index      = numeric()
            ## ## find non-numeric variables and
            ## ## store their indices in "index"
            ## for (i in 1:ncol(data)) {
            ##     if (i != time_index) {
            ##         if (!is.numeric(data[, i])) {
            ##             index = c(index, i)
            ##         }
            ##     }
            ## }
            ## ## drop the "index" from dataset
            ## if (length(index) != 0) {
            ##     data[, -index]
            ## } else {
            ##     data
            ## }
        },

        ## draw the plot, depending on the settings
        updatePlot = function(animate = FALSE) {
            ## plot the TS object setup by the GUI

            if (animate) gmessage("Animation not yet implemented :(")
            animate <- FALSE

            decomp <<- NULL
            forecasts <<- NULL

            can.smooth <- TRUE
            smooth.t <- smoothness

            if (is.null(tsObj)) {
                cat("Nothing to plot ...\n")
                plot.new()
            } else if (inherits(tsObj, "iNZightMTS")) { ## multiple vars
                switch(compare,
                       compareplot(tsObj, multiplicative = (patternType == 1),
                                   xlab = svalue(xLab), ylab = svalue(yLab), t = smooth.t),
                       multiseries(tsObj, multiplicative = (patternType == 1),
                                   xlab = svalue(xLab), ylab = svalue(yLab), t = smooth.t))
            } else { ## single var
                switch(plottype, {
                    ## 1 >> standard plot
                    ## patternType = 1 >> 'multiplicative'; 2 >> 'additive'
                    iNZightTS::rawplot(tsObj, multiplicative = (patternType == 1),
                                       ylab = svalue(yLab), xlab = svalue(xLab), animate = animate, t = smooth.t)
                }, {
                    ## 2 >> decomposed plot
                    decomp <<- iNZightTS::decompositionplot(tsObj, multiplicative = (patternType == 1),
                                                            xlab = svalue(xLab), ylab = svalue(yLab),
                                                            t = smooth.t)
                    visible(recomposeBtn) <<- TRUE
                    visible(recomposeResBtn) <<- TRUE
                }, {
                    ## 3 >> season plot
                    iNZightTS::seasonplot(tsObj, multiplicative = (patternType == 1),
                                          xlab = svalue(xLab), ylab = svalue(yLab), t = smooth.t)
                }, {
                    ## 4 >> forecast plot
                    forecasts <<- iNZightTS::forecastplot(tsObj, multiplicative = (patternType == 1),
                                                          xlab = svalue(xLab), ylab = svalue(yLab))
                    visible(forecastBtn) <<- TRUE
                    can.smooth <- FALSE
                })

            }

            enabled(smthSlider) <<- can.smooth

        }
    )
)

## #iNZightTimeSeries()

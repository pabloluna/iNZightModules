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
        tsObj       = "ANY",
        yLab        = "ANY", xLab        = "ANY",
        decompose   = "logical",
        plottype    = "numeric",
        compare     = "logical",
        forecastBtn = "ANY", forecasts   = "ANY"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI, patternType = 1,
                       decompose = FALSE, plottype = 1, compare = TRUE)
            
            dat = GUI$getActiveData()
            activeData <<- tsData(dat)
            timeVar <<- getTime(activeData, index = FALSE)

            GUI$initializeModuleWindow(.self)
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(5)

            playBtn <- iNZight:::gimagebutton(stock.id = "media-play", handler = function(h, ...) updatePlot(animate = TRUE))
            GUI$plotToolbar$update(NULL, refresh = "updatePlot", extra = list(playBtn))

            ################
            ###  fields  ###
            ################
            frameFont = list(weight = "bold")

            #################################
            ###  set up frame containers  ###
            #################################
            g1 = gframe("Time Information", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            g2 = gframe("Seasonal Pattern", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            addSpring(mainGrp)
            g3 = gframe("Series Variables", pos = 0.5, horizontal = FALSE,
                        container = mainGrp, fill = TRUE)
            
            g5 = gframe("Plot Type Options", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            size(g5) <- c(100, 80)
            
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
            mainGrp$set_rgtk2_font(frames[[4]]$getChildren()[[2]], frameFont)
            mainGrp$set_rgtk2_font(frames[[5]]$getChildren()[[2]], frameFont)
            mainGrp$set_rgtk2_font(frames[[6]]$getChildren()[[2]], frameFont)

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
                                   selected = 1, #match(timeVar, names(activeData)),
                                   handler = function(h, ...) {
                                       timeVar <<- svalue(h$obj)
                                       updatePlot()
                                   })
            ## g1a labels
            g1a_lab1   = glabel("Select time variable:")
            ## g1a layout
            g1a_layout[2, 1, expand = TRUE, anchor = c(-1, 0)] = g1a_lab1
            g1a_layout[2, 2, expand = TRUE]   = g1a_opt1

            ## FOR LAYOUT B
            g1b_layout <- glabel("Not implemented yet.", container = g1)
            visible(g1b_layout) <- FALSE
            ## g1b_layout = glayout(container = g1)
            ## visible(g1b_layout) = FALSE
            ## ## g1b options
            ## g1b_opt1  = gedit("")
            ## g1b_opt2  = gedit("")
            ## g1b_opt3  = gedit("")
            ## size(g1b_opt1) = c(120, 21)
            ## size(g1b_opt2) = c(120, 21)
            ## size(g1b_opt3) = c(120, 21)
            ## ## g1b labels
            ## g1b_lab1  = glabel("Specify start date:")
            ## g1b_lab2  = glabel("Specify season:")
            ## g1b_lab3  = glabel("Specify frequency:")
            ## ## g1b layout
            ## g1b_layout[2, 1, expand = TRUE, anchor = c(-1, 0)] = g1b_lab1
            ## g1b_layout[3, 1, expand = TRUE, anchor = c(-1, 0)] = g1b_lab2
            ## g1b_layout[4, 1, expand = TRUE, anchor = c(-1, 0)] = g1b_lab3
            ## g1b_layout[2, 2, expand = TRUE] = g1b_opt1
            ## g1b_layout[3, 2, expand = TRUE] = g1b_opt2
            ## g1b_layout[4, 2, expand = TRUE] = g1b_opt3

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
            g2_layout = glayout(container = g2)
            g2_opt1   = gradio(c("Multiplicative", "Additive"), selected = patternType,
                               horizontal = TRUE,
                               handler = function(h, ...) {
                                   patternType <<- svalue(h$obj, index = TRUE)
                                   updatePlot()
                               })

            g2_layout[1, 1, expand = TRUE] = g2_opt1

            ############
            ###  g3  ###
            ############
            ## NOTE:
            ##   need to change the variable selection widget for when there
            ##   are many variables which will expand the widget.
            g3_layout = glayout(container = g3)
            g3_opt1 = gtable(names(activeData)[-getTime(activeData)],
                             multiple = TRUE)
            size(g3_opt1) <- c(100, 150)
            g3_layout[1, 1, expand = TRUE] = g3_opt1

            addHandlerSelectionChanged(g3_opt1, function(h, ...) {
                if (length(svalue(g3_opt1)) == 0) return()
                
                ## make dataset an iNZightTS object
                var_ind <- which(names(activeData) %in% svalue(h$obj))
                if (length(var_ind) == 1) {
                    visible(onevar) <- TRUE
                    visible(multivar) <- FALSE
                } else {
                    visible(onevar) <- FALSE
                    visible(multivar) <- TRUE
                }
                tsObj <<- iNZightTS::iNZightTS(data = activeData, var = var_ind)
                updatePlot()
            })


            ############
            ###  g5  ###
            ############

            onevar <- gvbox(container = g5)
            addSpring(onevar)
            plotType <- gradio(c("Standard", "Season", "Forecast"), selected = plottype,
                               horizontal = TRUE, container = onevar, expand = TRUE,
                               handler = function(h, ...) {
                                   plottype <<- svalue(h$obj, index = TRUE)
                                   visible(decompBtn) <- plottype == 1
                                   visible(forecastBtn) <<- FALSE ## plottype == 3
                                   updatePlot()
                               })

            decompBtn <- gbutton("Decompose", container = onevar,
                                 handler = function(h, ...) {
                                     decompose <<- !decompose
                                     blockHandlers(h$obj)
                                     svalue(h$obj) <- ifelse(decompose, "Re-compose", "Decompose")
                                     unblockHandlers(h$obj)
                                     updatePlot()
                                 })
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
            compareChk <- gcheckbox("Draw each variable in the same plot", checked = compare,
                                    container = multivar,
                                    handler = function(h, ...) {
                                        compare <<- svalue(h$obj)
                                        updatePlot()
                                    })

            visible(onevar) <- FALSE
            visible(multivar) <- FALSE
            
            

            ############
            ###  g4  ###
            ############
            g4_layout = glayout(container = g4)
            g4_lab1   = glabel("x-axis")
            g4_lab2   = glabel("y-axis")
            yLab <<- gedit(timeVar)
            xLab <<- gedit("")

            size(xLab) <<- c(150, 21)
            size(yLab) <<- c(150, 21)

            g4_layout[1, 1:2, expand = TRUE, anchor = c(-1, 0)] = g4_lab1
            g4_layout[2, 1:2, expand = TRUE, anchor = c(-1, 0)] = g4_lab2
            g4_layout[1, 3, expand = TRUE] = yLab
            g4_layout[2, 3, expand = TRUE] = xLab



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

            visible(GUI$moduleWindow) <<- TRUE
        },

        # ========
        # METHODS
        # ========
        ## returns the time variable index
        getTime = function(data, index = TRUE) {
            ## look for time or date
            time_re = "([Tt][Ii][Mm][Ee])|([Dd][Aa][Tt][Ee])"
            ind     = grep(time_re, names(data))
            if (index) { return(ind) }
            else       { return(names(data)[ind]) }
        },

        ## checks for a time variable in dataset
        isTS = function(data) {
            return(length(getTime(data)) != 0)
        },

        ## drops categorical variables (except the time variable)
        tsData = function(data) {
            time_index = getTime(data)
            index      = numeric()
            ## find non-numeric variables and
            ## store their indices in "index"
            for (i in 1:ncol(data)) {
                if (i != time_index) {
                    if (!is.numeric(data[, i])) {
                        index = c(index, i)
                    }
                }
            }
            ## drop the "index" from dataset
            if (length(index) != 0) {
                data[, -index]
            } else {
                data
            }
        },

        ## draw the plot, depending on the settings
        updatePlot = function(animate = FALSE) {
            ## plot the TS object setup by the GUI

            if (animate) gmessage("Animation not yet implemented :(")
            animate <- FALSE

            forecasts <<- NULL

            if (is.null(tsObj)) {
                cat("Nothing to plot ...\n")
                plot.new()
            } else if (inherits(tsObj, "iNZightMTS")) { ## multiple vars
                if (compare)
                    compareplot(tsObj, multiplicative = (patternType == 1), ylab = svalue(yLab))
                else
                    multiseries(tsObj, multiplicative = (patternType == 1), ylab = svalue(yLab))
            } else { ## single var
                switch(plottype, {
                    ## 1 >> standard plot
                    if (decompose) {
                        cat("Draw decomposed plot ...\n")
                        plot.new()
                    } else {
                        ## patternType = 1 >> 'multiplicative'; 2 >> 'additive'
                        iNZightTS::rawplot(tsObj, multiplicative = (patternType == 1),
                                           ylab = svalue(yLab), xlab = svalue(xLab), animate = animate)
                    }
                }, {
                    ## 2 >> season plot
                    iNZightTS::seasonplot(tsObj, multiplicative = (patternType == 1))
                }, {
                    ## 3 >> forecast plot
                    forecasts <<- iNZightTS::forecastplot(tsObj, multiplicative = (patternType == 1))
                    visible(forecastBtn) <<- TRUE
                })
                       
            }
            
        }
    )
)

## #iNZightTimeSeries()

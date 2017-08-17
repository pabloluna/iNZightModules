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
        smryOut     = "ANY",
        regPlots    = "ANY",
        response    = "character", responseType = "numeric",
        variableList = "ANY",
        variables   = "character", explanatoryList = "ANY",
        confounding = "character", confounderList = "ANY",
        modelName   = "ANY", modelList = "ANY",
        fit         = "ANY", summaryOutput = "character",
        fits        = "list",
        working     = "logical",
        plottype    = "numeric", showBoots = "ANY"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI, working = TRUE, plottype = 1)

            GUI$initializeModuleWindow(.self)
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(5)

            GUI$plotToolbar$update(NULL, refresh = "updatePlot")

            if (!is.null(GUI$moduledata) && !is.null(GUI$moduledata$regression) &&
                !is.null(GUI$moduledata$regression$fits))
                fits <<- GUI$moduledata$regression$fits

            # addSpace(mainGrp, 15)

            lbl1 <- glabel("Model Fitting Module")
            font(lbl1) <- list(weight = "bold",
                               family = "normal",
                               size   = 11)
            add(mainGrp, lbl1, anchor = c(0, 0))
            addSpace(mainGrp, 5)

            
            ## ---------------------------------------------------------------------------------------------------------
            ## Here lies the left panel
            responseGp <- gframe("Response Options", horizontal = FALSE, container = mainGrp)
            responseGp$set_borderwidth(10)
            responseTbl <- glayout(container = responseGp)
            ii <-  1

            lbl <- glabel("Variable")
            yVars <- names(data()[, sapply(data(), function(x) is.numeric(x) || length(levels(x)) == 2)])
            responseBox <- gcombobox(yVars, selected = 0,
                                     handler = function(h, ...) {
                                         working <<- TRUE
                                         response <<- svalue(h$obj)
                                         ## detect framework
                                         y <- data()[[response]]
                                         if (is.numeric(y)) {
                                             svalue(responseTypeBox, index = TRUE) <- 1
                                         } else if (length(levels(y)) == 2) {
                                             svalue(responseTypeBox, index = TRUE) <- 2
                                         }
                                         ## set explanatory variables
                                         setAvailVars()
                                         working <<- FALSE
                                         updateModel()
                                     })
            responseTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            responseTbl[ii, 2:3, expand = TRUE] <- responseBox
            ii <- ii + 1

            lbl <- glabel("Framework")
            responseTypeBox <- gcombobox(c("Least Squares", "Logistic Regression (binary)", "Poisson Regression (counts)"),
                                         selected = 0,
                                         handler = function(h, ...) {
                                             responseType <<- svalue(h$obj, index = TRUE)
                                             updateModel()
                                         })
            responseTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            responseTbl[ii, 2:3, expand = TRUE] <- responseTypeBox
            ii <- ii + 1




            variableGp <- gframe("Explanatory Variables (drag+drop/double-click)", horizontal = FALSE, container = mainGrp)
            variableGp$set_borderwidth(10)
            variableTbl <- glayout(homogeneous = TRUE, container = variableGp)
            
            variableList <<- gtable("")#, multiple = TRUE)
            setAvailVars()
            variableTbl[1:3, 1, expand = TRUE] <- variableList
            size(variableList) <<- c(-1, 300)

            explanatoryList <<- gtable("")
            setExplVars()
            variableTbl[1:2, 2, expand = TRUE] <- explanatoryList

            confounderList <<- gtable("")
            setConfVars()
            variableTbl[3, 2, expand = TRUE] <- confounderList

            ## Right-click menus
            transforms <- list("I(x^2)", "I(x^3)", "POWER", "POLY", "log(x)", "sqrt(x)", "OTHER")
            transformList <- lapply(transforms,
                                    function(x) {
                                        if (x == "POWER") {
                                            return(gaction("x^z: other power ...", handler = function(h, ...) {
                                                xname <- svalue(variableList, index = FALSE)
                                                if (!is.numeric(data()[[xname]])) {
                                                    gmessage("Transformations only available for numeric variables",
                                                             "error", parent = GUI$win)
                                                    return()
                                                }
                                                zw <- gbasicdialog("Choose order of power", handler=function(h,...) {
                                                    z <- as.numeric(svalue(zval))
                                                    if (is.na(z)) gmessage("Order must be a number.", "Invalid Value",
                                                                           "error", parent = h$obj)
                                                    else
                                                        addTransform(xname, paste0("I(x^", z, ")"))
                                                }, parent = GUI$win)
                                                zg <- ggroup(cont=zw)
                                                zt <- glayout(homogeneous = TRUE, container = zg)
                                                zt[1,1,anchor=c(1,0), expand = TRUE] = glabel(paste0(xname, "^"))
                                                zt[1,2] = (zval <- gedit(4, width = 4, container = zt))
                                                out <- visible(zw)
                                            }))
                                        }
                                        if (x == "POLY") {
                                            return(gaction("polynomial to degree ...", handler = function(h, ...) {
                                                xname <- svalue(variableList, index = FALSE)
                                                if (!is.numeric(data()[[xname]])) {
                                                    gmessage("Transformations only available for numeric variables",
                                                             "error", parent = GUI$win)
                                                    return()
                                                }
                                                zw <- gbasicdialog("Choose order of polynomial", handler=function(h,...) {
                                                    z <- as.numeric(svalue(zval))
                                                    if (is.na(z)) gmessage("Order must be a number.", "Invalid Value",
                                                                           "error", parent = h$obj)
                                                    else
                                                        addTransform(xname, paste0("poly(x, ", z, ")"))
                                                }, parent = GUI$win)
                                                zg <- ggroup(cont=zw)
                                                zt <- glayout(homogeneous = TRUE, container = zg)
                                                zt[1,1,anchor=c(1,0), expand = TRUE] = glabel(paste0("poly(", xname, ","))
                                                zt[1,2] = (zval <- gspinbutton(1, 50, 1, value = 2, container = zt))
                                                zt[1,3, anchor= c(-1,0), expand=TRUE] = glabel(")")
                                                out <- visible(zw)
                                            }))
                                        }
                                        if (x == "OTHER") {
                                            return(gaction("Other ...", handler = function(h, ...) {
                                                xname <- svalue(variableList, index = FALSE)
                                                if (!is.numeric(data()[[xname]])) {
                                                    gmessage("Transformations only available for numeric variables",
                                                             "error", parent = GUI$win)
                                                    return()
                                                }
                                                zw <- gbasicdialog("Specify other transformation", handler=function(h,...) {
                                                    try(addTransform(sprintf("%s(%s)", svalue(zfun), svalue(zargs))))
                                                    invisible(NULL)
                                                }, parent = GUI$win)
                                                zg <- gvbox(cont=zw)
                                                zt <- glayout(homogeneous = FALSE, container = zg)
                                                zfun <- gedit("", width = 10, initial.msg = "function")
                                                zargs <- gedit(xname, width = 15)
                                                zt[1,1] <- glabel("Specify transformation :")
                                                zt[1,2] <- zfun
                                                zt[1,3] <- glabel("(")
                                                zt[1,4] <- zargs
                                                zt[1,5] <- glabel(")")
                                                glabel("Include comma-separated arguments if necessary", container = zg)
                                                out <- visible(zw)
                                            }))
                                        }
                                        lbl <- x
                                        if (grepl("I(", lbl, fixed = TRUE))
                                            lbl <- gsub(":.+", "", gsub(")", "", gsub("I(", "", lbl, fixed = TRUE), fixed = TRUE))
                                        gaction(lbl, handler = function(h, ...) {
                                            xname <- svalue(variableList, index = FALSE)
                                            if (!is.numeric(data()[[xname]])) {
                                                gmessage("Transformations only available for numeric variables",
                                                         "error", parent = GUI$win)
                                                return()
                                            }
                                            addTransform(xname, x)
                                        })
                                    })
            factorTransformList <- list(
                gaction("Interact ...", handler = function(h, ...) {
                    xname <- svalue(variableList, index = FALSE)
                    zw <- gbasicdialog("Create Interaction", handler = function(h, ...) {
                        vs <- c(xname, svalue(intvar, index = FALSE))
                        if (svalue(zval) == 0) {
                            vtxt <- paste(vs, collapse = " * ")
                        } else {
                            vtxt <- sprintf("(%s)^%s", paste(vs, collapse = " + "), svalue(zval)+1)
                        }
                        addTransform(vtxt)
                    })
                    zg <- gvbox(container = zw)
                    size(zw) <- c(280, 400)
                    glabel(sprintf("Choose variables to interact with %s\n(CTRL to select many)", xname),
                           container = zg)
                    vs <- variableList$get_items()
                    intvar <- gtable(vs[vs != xname], multiple = TRUE, container = zg)
                    zg2 <- ggroup(container = zg)
                    glabel("How many interaction terms?\n(0 = all) :", container = zg2)
                    zval <- gspinbutton(0, length(svalue(intvar)), 1, container = zg2)
                    enabled(zval) <- length(svalue(intvar)) > 0
                    ## when interactions chosen, change upper limit of zval
                    addHandlerSelectionChanged(intvar, handler = function(h, ...) {
                        n <- length(svalue(h$obj))
                        zval$set_items(0:n)
                        enabled(zval) <- n > 0
                        invisible(NULL)
                    })
                    out <- visible(zw)
                })
            )
            addRightclickPopupMenu(variableList, c(transformList, list(gseparator()), factorTransformList))
            
            ## Drag-and-drop behaviour
            addDropSource(variableList, type="object",handler = function(h, ...) {
                varname <- svalue(h$obj)
                attr(varname, "from") <- "avail"
                varname
            })
            addDropSource(explanatoryList, handler = function(h, ...) {
                varname <- svalue(h$obj)
                variables <<- variables[variables != varname]
                working <<- TRUE
                setExplVars()
                working <<- FALSE
                varname
            })
            addDropSource(confounderList, handler = function(h, ...) {
                varname <- svalue(h$obj)
                confounding <<- confounding[confounding != varname]
                working <<- TRUE
                setConfVars()
                working <<- FALSE
                varname
            })
            
            addDropTarget(explanatoryList, handler =  function(h, ...) {
                varname <- h$dropdata
                if (varname %in% c(variables, confounding)) return()
                variables <<- c(variables, varname)
                setExplVars()
            })
            addDropTarget(confounderList, handler = function(h, ...) {
                varname <- h$dropdata
                if (varname %in% c(variables, confounding)) return()
                confounding <<- c(confounding, varname)
                setConfVars()
            })

            ## double click behaviour
            addHandlerDoubleclick(variableList, handler = function(h, ...) {
                varname <- svalue(h$obj)
                if (length(varname) != 1) return()
                if (varname %in% c(variables, confounding)) return()
                variables <<- c(variables, varname)
                setExplVars()
            })
            addHandlerDoubleclick(explanatoryList, handler = function(h, ...) {
                if (length(svalue(h$obj)) != 1) return()
                variables <<- variables[variables != svalue(h$obj)]
                setExplVars()
            })
            addHandlerDoubleclick(confounderList, handler = function(h, ...) {
                if (length(svalue(h$obj)) != 1) return()
                confounding <<- confounding[confounding != svalue(h$obj)]
                setConfVars()
            })



            ## ---------------------------------------------------------------------------------------------------------
            ## Model options
            
            modelGp <- gexpandgroup("Models", horizontal = FALSE, container = mainGrp)
            modelGp$set_borderwidth(10)
            modelTbl <- glayout(homogeneous = TRUE, container = modelGp)
            ii <- 1

            lbl <- glabel("Select Model :")
            modelList <<- gcombobox(c("(new)", names(fits)), ## if (length(fits) > 0) names(fits) else "(new)",
                                    handler = function(h, ...) {
                                        ## reset response, framework, variables ...
                                        if (svalue(h$obj, index = TRUE) == 1) {
                                            newBtn$invoke_change_handler()
                                            return()
                                        }
                                        obj <- fits[[svalue(h$obj, index = TRUE) - 1]]
                                        working <<- TRUE
                                        svalue(responseBox) <- obj$response
                                        svalue(responseTypeBox, index = TRUE) <- obj$responseType
                                        variables <<- obj$variables
                                        setExplVars()
                                        confounding <<- obj$confounding
                                        setConfVars()
                                        svalue(modelName) <<- svalue(h$obj)
                                        blockHandlers(saveBtn)
                                        svalue(saveBtn) <- "Update"
                                        unblockHandlers(saveBtn)
                                        working <<- FALSE
                                        fit <<- obj$fit
                                        updateModel(new = FALSE)
                                    })

            newBtn <- gbutton("New",
                              handler = function(h, ...) {
                                  svalue(modelName) <<- paste("Model", length(fits) + 1)
                                  blockHandlers(modelList)
                                  svalue(modelList, index = TRUE) <<- 1
                                  unblockHandlers(modelList)
                                  blockHandlers(saveBtn)
                                  svalue(saveBtn) <- "Save Model"
                                  unblockHandlers(saveBtn)
                              })
            modelTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            modelTbl[ii, 2, expand = TRUE, fill = TRUE] <- modelList
            modelTbl[ii, 3, expand = TRUE, fill = TRUE] <- newBtn
            ii <- ii + 1

            lbl <- glabel("Name :")
            modelName <<- gedit(paste("Model", length(fits) + 1))
            saveBtn <- gbutton("Save Model",
                               handler = function(h, ...) {
                                   updateModel(save = TRUE)
                                   blockHandler(h$obj)
                                   svalue(h$obj) <- "Update"
                                   unblockHandler(h$obj)
                               })
            modelTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            modelTbl[ii, 2, expand = TRUE, fill = TRUE] <- modelName
            modelTbl[ii, 3, expand = TRUE, fill = TRUE] <- saveBtn
            ii <- ii + 1

            ## ---------------------------------------------------------------------------------------------------------
            ## Plot options
            
            plotGp <- gexpandgroup("Additional Options (Plots, Comparisons, ...)", horizontal = FALSE, container = mainGrp)
            visible(plotGp) <- FALSE
            plotGp$set_borderwidth(10)
            plotTbl <- glayout(homogeneous = TRUE, container = plotGp)
            ii <- 1

            lbl <- glabel("Residual plots :")
            plotTypeList <- gcombobox(c("Residual", "Scale-Location", "Leverage", "Cook's Distance",
                                        "Normal Q-Q", "Histogram", "Summary Matrix", "Partial Residual"),
                                      handler = function(h, ...) {
                                          plottype <<- svalue(h$obj, index = TRUE)
                                          updatePlot()
                                      })
            plotTbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
            plotTbl[ii, 3:6, expand = TRUE, fill = TRUE] <- plotTypeList
            ii <- ii + 1

            showBoots <<- gcheckbox("Show bootstraps", checked = nrow(data()) >= 30 && nrow(data()) < 4000,
                                    handler = function(h, ...) updatePlot())
            plotTbl[ii, 3:6, anchor = c(-1, 0), fill = TRUE] <- showBoots
            ii <- ii + 1


            compMatrix <- gbutton("Comparison Matrix",
                                  handler = function(h, ...) {

                                  })
            compPlot <- gbutton("Comparison Plot",
                                handler = function(h, ...) {

                                })
            #partialResPlot <- gbutton("Partial Residual Plot",
            #                          handler = function(h, ...) {
            #
            #})
            plotTbl[ii, 1:3, expand = TRUE, fill = TRUE] <- compMatrix
            plotTbl[ii, 4:6, expand = TRUE, fill = TRUE] <- compPlot
            ii <- ii + 1
            #plotTbl[ii, 4:6, expand = TRUE, fill = TRUE] <- partialResPlot
            #ii <- ii + 1
            
            

            ## ---------------------------------------------------------------------------------------------------------
            ## Here lies the bottom piece
            addSpring(mainGrp)
            bot <- ggroup(container = mainGrp)

            helpButton <- gbutton("Help",
                                  handler = function(h, ...) {
                                      browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=model_fitting")
                                  })
            homeButton <- gbutton("Home",
                                  handler = function(h, ...) {
                                      ## save fits
                                      GUI$moduledata$regression <<- list(fits = fits)
                                      
                                      ## clean up tabs ...
                                      showTab("plot")
                                      GUI$plotWidget$closePlot()
                                      GUI$plotWidget$addPlot()
                                      showTab("summary")
                                      GUI$plotWidget$closePlot()
                                      
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
            pb.i <- svalue(GUI$plotWidget$plotNb)
            smryOut <<- gtext()
            add(GUI$plotWidget$plotNb, smryOut, label = "Model Output", close.button = FALSE)
            svalue(GUI$plotWidget$plotNb) <<- pb.i
            GUI$plotWidget$closePlot()

            regPlots <<- ggraphics(expand = TRUE)
            add(GUI$plotWidget$plotNb, regPlots, label = "Model Plots", close.button = FALSE)
            plot(1:10)

            ## So now, can swith between text and plot tabs ...
            showTab("summary")

            ## Some nice text to start off with ...
            addOutput("\nWelcome to the iNZight Model Fitting Module!",
                      font.attr = list(weight = "bold"))

            ## Instructions!
            addOutput(font.attr = list(), "",
                      "1. Select a response variable from the drop down.",
                      "2a. Double-click variables in the Available Variables box to add them to the model.",
                      "2b. or Drag-and-drop variables from the Available Variables box to either of the explanatory or counfounding variables boxes.",
                      "3. Right-click variables to select and apply a transformation.",
                      "4. Drag-and-drop a variable in the Explanatory Variables box to another in the same box to create an interaction.",
                      "5. Double-click variables in the Explanatory Variables box to remove them from the model.")
            rule()

            working <<- FALSE
            

            ## tmpfit <- lm(height ~ armspan + gender, data = activeData)
            ## smry <- capture.output(summary(tmpfit))
            ## addOutput(paste0("Model 1:"), smry)
        },
        data = function() GUI$getActiveData(),
        setAvailVars = function() {
            if (is.null(response)) {
                vars <- "Select response"
            } else {
                vars <- names(data()[,-which(names(data()) == response)])
                if (length(variables) && response %in% variables) {
                    variables <<- variables[variables != response]
                    setExplVars()
                }
                if (length(confounding) && response %in% confounding) {
                    confounding <<- confounding[confounding != response]
                    setConfVars()
                }
            }
            variableList$set_items(structure(data.frame(vars, stringsAsFactors = FALSE),
                                             names = "Available Variables"))            
        },
        setExplVars = function () {
            explanatoryList$set_items(structure(data.frame(variables, stringsAsFactors = FALSE),
                                                names = "Variables of Interest"))
            updateModel()
        },
        setConfVars = function() {
            confounderList$set_items(structure(data.frame(confounding, stringsAsFactors = FALSE),
                                               names = "Confounding Variables"))
            updateModel()
        },
        addTransform = function(var, fun) {
            if (!missing(fun)) {
                fn <- gsub("x", "%s", fun)
                nv <- sprintf(fn, var)
            } else nv <- var
            if (! nv %in% variables )
                variables <<- c(variables, nv)
            setExplVars()
        },
        showTab = function(x = c("plot", "summary")) {
            x <- match.arg(x)
            svalue(GUI$plotWidget$plotNb) <<-
                which(names(GUI$plotWidget$plotNb) ==
                      ifelse(x == "plot", "Model Plots", "Model Output"))
        },
        addOutput = function(..., font.attr = list(family = "monospace")) {
            ## showTab("summary")
            sapply(list(...), insert, obj = smryOut, font.attr = font.attr)
        },
        rule = function(char = "-") {
            addOutput("", paste0(rep(char, 80), collapse = ""), "")
        },
        updateModel = function(new = TRUE, save = FALSE) {
            if (working) return()

            xexpr <- paste(c(if (length(variables) > 0) variables else "1", confounding), collapse = " + ")
            dataset <- data()
            if (new) {
                mcall <- iNZightTools::fitModel(response, xexpr, data = "dataset",
                                                family = switch(responseType, "gaussian", "binomial", "poisson"),
                                                na.action = na.exclude)
                fit <<- try(eval(parse(text = mcall)), TRUE)
            }
            
            modelname <- svalue(modelName)
                

            svalue(smryOut) <<- ""
            addOutput(summaryOutput)
            rule()

            addOutput(paste0("# Summary of ", modelname, ": ", response, " ~ ", xexpr))
            if (inherits(fit, "try-error")) {
                addOutput("Unable to fit model.")
                rule()
                return()
            } else {
                wd <- options()$width
                options(width = 200)
                addOutput(capture.output(iNZightRegression::iNZightSummary(fit, exclude = if (length(confounding) > 0) confounding else NULL)))
                options(width = wd)
                ## plot it
                
                if (save) {
                    obj <- list(fit = fit, response = response, responseType = responseType,
                                variables = variables, confounding = confounding)
                    if (svalue(modelList, index = TRUE) == 1) {
                        ## Creating a new model
                        fits <<- c(fits, structure(list(obj), .Names = modelname))
                    } else {
                        ## Updating an existing model
                        fits[[svalue(modelList, index = TRUE) - 1]] <<- obj
                        names(fits)[svalue(modelList, index = TRUE) - 1] <<- modelname
                    }
                    blockHandlers(modelList)
                    modelList$set_items(c("(new)", names(fits)))
                    svalue(modelList) <<- modelname
                    unblockHandlers(modelList)
                    summaryOutput <<- svalue(smryOut)
                }
            }
            
            rule()


            updatePlot()
        },
        updatePlot = function() {                  
            if (plottype %in% 1:7) {
                if (svalue(showBoots) && plottype %in% 5:6) {
                    if (plottype == 5) {
                        iNZightRegression::iNZightQQplot(fit)
                    } else {
                        iNZightRegression::histogramArray(fit)
                    }
                } else {
                    iNZightRegression::plotlm6(fit, which = plottype, showBootstraps = svalue(showBoots))
                }
            } else if (plottype == 8) {
                plot(10:1, main = "Partial residual plot")
            } else {
                plot(1:10)
            }
        }
    )
)

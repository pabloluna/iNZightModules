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
        fit         = "ANY", summaryOutput = "character",
        fits        = "list",
        working     = "logical"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI, working = TRUE)

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




            variableGp <- gframe("Variables", horizontal = FALSE, container = mainGrp)
            variableGp$set_borderwidth(10)
            variableTbl <- glayout(homogeneous = TRUE, container = variableGp)
            
            variableList <<- gtable("")
            setAvailVars()
            variableTbl[1:3, 1, expand = TRUE] <- variableList
            size(variableList) <<- c(-1, 300)

            explanatoryList <<- gtable("")
            setExplVars()
            variableTbl[1:2, 2, expand = TRUE] <- explanatoryList

            confounderList <<- gtable("")
            setConfVars()
            variableTbl[3, 2, expand = TRUE] <- confounderList


            ## Drag-and-drop behaviour
            addDropSource(variableList, handler = function(h, ...) {
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
                if (varname %in% variables) return()
                variables <<- c(variables, varname)
                setExplVars()
            })
            addDropTarget(confounderList, handler = function(h, ...) {
                varname <- h$dropdata
                if (varname %in% confounding ) return()
                confounding <<- c(confounding, varname)
                setConfVars()
            })

            ## double click behaviour
            addHandlerDoubleclick(variableList, handler = function(h, ...) {
                varname <- svalue(h$obj)
                if (varname %in% c(variables, confounding)) return()
                variables <<- c(variables, varname)
                setExplVars()
            })
            addHandlerDoubleclick(explanatoryList, handler = function(h, ...) {
                variables <<- variables[variables != svalue(h$obj)]
                setExplVars()
            })
            addHandlerDoubleclick(confounderList, handler = function(h, ...) {
                confounding <<- confounding[confounding != svalue(h$obj)]
                setConfVars()
            })
            
            

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
            if (is.null(response)) vars <- "Select response"
            else vars <- names(data()[,-which(names(data()) == response)])
            
            variableList$set_items(structure(data.frame(vars, stringsAsFactors = FALSE),
                                             names = "Available Variables"))
        },
        setExplVars = function () {
            explanatoryList$set_items(structure(data.frame(variables, stringsAsFactors = FALSE),
                                                names = "Explanatory Variables"))
            updateModel()
        },
        setConfVars = function() {
            confounderList$set_items(structure(data.frame(confounding, stringsAsFactors = FALSE),
                                               names = "Confounding Variables"))
            updateModel()
        },
        showTab = function(x = c("plot", "summary")) {
            x <- match.arg(x)
            svalue(GUI$plotWidget$plotNb) <<-
                which(names(GUI$plotWidget$plotNb) ==
                      ifelse(x == "plot", "Model Plots", "Model Output"))
        },
        addOutput = function(..., font.attr = list(family = "monospace")) {
            showTab("summary")
            sapply(list(...), insert, obj = smryOut, font.attr = font.attr)
        },
        rule = function(char = "-") {
            addOutput("", paste0(rep(char, 80), collapse = ""), "")
        },
        updateModel = function(save = FALSE) {
            if (working) return()
            dataset <- data()
            fit <<- eval(parse(
                text = iNZightTools::fitModel(response, paste(c(if (length(variables) > 0) variables else "1", confounding), collapse = " + "),
                                              data = "dataset",
                                              family = switch(responseType, "gaussian", "binomial", "poisson"))
            ))
            svalue(smryOut) <<- summaryOutput
            addOutput(capture.output(iNZightRegression::iNZightSummary(fit, exclude = if (length(confounding) > 0) confounding else NULL)))
            
            if (save) {
                fits <<- c(fits, fit)
                summaryOutput <<- svalue(smryOut)
            }
            
            rule()
        }
    )
)

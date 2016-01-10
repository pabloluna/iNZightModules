iNZightMultiRes <- setRefClass(
    "iNZightMultiRes",
    fields = list(
        GUI        = "ANY",
        mainGrp    = "ANY",
        activeData = "data.frame",
        vars       = "character",
        binaryVar  = "numeric",
        mrObject   = "ANY",
        byMRObject = "ANY"
    ),
    
    methods = list(
        initialize = function(GUI) {
            # g = gwindow(width = 300, height = 600)
            # mainGrp <<- gvbox(spacing = 10, container = g, expand = TRUE)
            # activeData <<- read.csv("//Users/eric/Desktop/CaS.csv")
            
            initFields(GUI = GUI)
            activeData <<- GUI$getActiveData()
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            
            mainGrp$set_borderwidth(2)
            top = ggroup(container = mainGrp,  expand = TRUE)
            mid = glayout(container = mainGrp, expand = TRUE)
            bot = glayout(container = mainGrp, expand = FALSE)
            
            # ==========
            # top panel
            # ==========
            binaryVar <<- getVars(activeData)
            vars      <<- names(activeData)
            gtab = gtable(paste("(b)", vars[binaryVar]), multiple = TRUE, container = top)
            names(gtab) = "VARIABLES (b = binary)"
            size(gtab)  = c(280, 280)
            
            top.timer = NULL
            addHandlerClicked(gtab, handler = function(h, ...) {
                if (!is.null(top.timer))
                    top.timer$stop_timer()
                top.timer = gtimer(500, function(...) {
                    if (length(svalue(gtab)) >= 2) {
                        enabled(box1)      = TRUE
                        enabled(sumButton) = TRUE
                        enabled(comButton) = TRUE
                    } else {
                        enabled(box1)      = FALSE
                        enabled(sumButton) = FALSE
                        enabled(comButton) = FALSE
                        svalue(box1, index = TRUE) = 1
                        svalue(box2, index = TRUE) = 1
                    }
                    updatePlot(gtab)
                }, one.shot = TRUE)
            })
            
            
            # =============
            # mid panel
            # =============
            box1 = gcombobox(c("Select Subset Variable 1", vars))
            box2 = gcombobox(c("Select Subset Variable 2", vars))
            
            size(box1) = c(230, 25)
            size(box2) = c(230, 25)
            slider1 = gslider(0, 1)
            slider2 = gslider(0, 1)
            
            ## cancel buttons
            cancelButton1 = gbutton("", handler = function(h,...) {
                svalue(box1, index = TRUE) = 1
                updatePlot(gtab)
            })
            cancelButton2 = gbutton("", handler = function(h,...) {
                svalue(box2, index = TRUE) = 1
                by.formula = paste("~", svalue(box1))
                byMRObject <<- byMRO(mrObject, by.formula, mroPara)
                barplot(byMRObject)
            })
            cancelButton1$set_icon("Cancel")
            cancelButton2$set_icon("Cancel")
            
            ## check box
            chk = gcheckbox("Side-by-side")
            
            ## default is unabled
            enabled(box1)    = FALSE
            enabled(box2)    = FALSE
            enabled(slider1) = FALSE
            enabled(slider2) = FALSE
            enabled(chk)     = FALSE
            
            box1.timer = NULL
            addHandlerChanged(box1, handler = function(h,...) {
                if (!is.null(box1.timer))
                    box1.timer$stop_timer()
                box1.timer = gtimer(500, function(...) {
                    s1 = svalue(box1, index = TRUE) - 1
                    if (s1 == 0) {
                        enabled(box2)    = FALSE
                        enabled(slider1) = FALSE
                        enabled(slider2) = FALSE
                        slider1[] = 0:1
                        updatePlot(gtab)
                    } else {
                        enabled(box2)    = TRUE
                        enabled(slider1) = TRUE
                        subsetVar = iNZightPlots::convert.to.factor(activeData[, s1])
                        slider1[] = 0:nlevels(subsetVar)
                        if (svalue(box2,index = TRUE) > 1) {
                            by.formula = paste("~", paste(svalue(box1), "+", svalue(box2)))
                            byMRObject <<- byMRO(mrObject, by.formula, mroPara)
                            barplot(between(byMRObject))
                        } else {
                            by.formula = paste("~", svalue(box1))
                            byMRObject <<- byMRO(mrObject, by.formula, mroPara)
                            barplot(byMRObject)
                        }
                        # byMRObject <<- byMRO(mrObject, by.formula, mroPara)
                        # barplot(byMRObject)
                    }
                }, one.shot = TRUE)
            })
            
            box2.timer = NULL
            addHandlerChanged(box2, handler = function(h,...) {
                if (!is.null(box2.timer))
                    box2.timer$stop_timer()
                box2.timer = gtimer(500, function(...) {
                    s2 = svalue(box2, index = TRUE) - 1
                    if (s2 == 0) {
                        enabled(chk) = FALSE
                        svalue(chk)  = FALSE
                        enabled(slider2)   = FALSE
                        enabled(comButton) = TRUE
                        slider2[] = 0:1
                        by.formula = paste("~", svalue(box1))
                        byMRObject <<- byMRO(mrObject, by.formula, mroPara)
                        barplot(byMRObject)
                    } else {
                        enabled(chk)       = TRUE
                        enabled(slider2)   = TRUE
                        enabled(comButton) = FALSE
                        subsetVar = iNZightPlots::convert.to.factor(activeData[, s2])
                        slider2[] = 0:nlevels(subsetVar)
                        by.formula = paste("~", paste(svalue(box1), "+", svalue(box2)))
                        byMRObject <<- byMRO(mrObject, by.formula, mroPara)
                        # barplot(between(byMRObject))
                        barplot(byMRObject)
                    }
                }, one.shot = TRUE)
            })
            
            chk.timer = NULL
            addHandlerChanged(chk, handler = function(h,...) {
                if (!is.null(chk.timer))
                    chk.timer$stop_timer()
                chk.timer = gtimer(500, function(...) {
                    if (svalue(chk))
                        barplot(between(byMRObject))
                    else
                        barplot(byMRObject)
                }, one.shot = TRUE)
            })
            
            mid[1, 1:7, anchor = c(0,0), expand = TRUE] = box1
            mid[3, 1:7, anchor = c(0,0), expand = TRUE] = box2
            mid[2, 1:8, anchor = c(0,0), expand = TRUE] = slider1
            mid[4, 1:8, anchor = c(0,0), expand = TRUE] = slider2
            mid[1, 8,   anchor = c(0,0)] = cancelButton1
            mid[3, 8,   anchor = c(0,0)] = cancelButton2
            mid[5, 1,   anchor = c(0,0)] = chk
            
            ## summary button
            sumButton = gbutton("Summary", handler = function(h,...) {
                s1 = svalue(box1, index = TRUE)
                s2 = svalue(box2, index = TRUE)
                if (s1 == 1) {
                    # summaryWindow(capture.output(summary(mroPara(mrObject))), mode = 1)
                    txt = capture.output(summary(mroPara(mrObject)))
                    summaryWindow(txt, mode = 1)
                } else if (s1 != 1 & s2 == 1) {
                    txt = capture.output(summary(byMRObject, "within"))
                    summaryWindow(txt, mode = 2)
                } else if (s1 != 1 & s2 != 1) {
                    txt = capture.output(summary(byMRObject, "between"))
                    summaryWindow(txt, mode = 3)
                }
            })
            enabled(sumButton) = FALSE
            
            ## combinations
            comButton = gbutton("Combinations", handler = function(h,...) {
                s1 = svalue(box1, index = TRUE)
                s2 = svalue(box2, index = TRUE)
                if (s1 == 1) {
                    summaryWindow(capture.output(plotcombn(mrObject)), mode = 3)
                } else if (s1 != 1 & s2 == 1) {
                    gmessage("Not yet supported")
                } else if (s1 != 1 & s2 != 1) {
                    gmessage("Not yet supported")
                }
            })
            enabled(comButton) = FALSE
            
            ## back button
            back = gbutton("Back", handler = function(h,...) {
                # tooltip = "Click to go back"
                # icon = "gtk-go-back",
                visible(GUI$moduleWindow) <<- FALSE
                visible(GUI$gp1)          <<- TRUE
            })
            font(back) <- list(weight="bold", family="normal", color="navy")
            
            
            bot[1, 1, anchor = c(0,0), expand = TRUE] = sumButton
            bot[1, 2, anchor = c(0,0), expand = TRUE] = comButton
            bot[2, 1:2, anchor = c(0,0), expand = TRUE] = back
            
            
            
        },
        
        ## isBinary() checks for a single vector.
        isBinary = function(x) {
            ## NAs are ignored as they are handled by MR
            tab = table(x, useNA = "no")[table(x)!=0]
            n   = length(names(tab))
            ## if not binary, return FALSE
            if (n != 2) { return(FALSE) }
            ## regular expressions for "yes, no, 0, 1, true, false"
            re1 = "([Yy][Ee][Ss])|([Nn][Oo])|([Yy])|([Nn])"
            re2 = "(0)|(1)"
            re3 = "([Tt][Rr][Uu][Ee])|([Ff][Aa][Ll][Ss][Ee])|([Tt])|([Ff])"
            re  = paste(re1, re2, re3, sep = "|")
            ## do those patterns match?
            l = grepl(re, names(tab))
            ## do BOTH binary values match the patterns?
            return(all(l))
        },
        ## getVars() checks for every variable in data.
        getVars = function(data) {
            which(apply(data, 2, function(x) isBinary(x)))
        },
        ## create an MR object and plot it
        updatePlot = function(obj) {
            ## create an MR object
            clicked   = svalue(obj, index = TRUE)
            if (length(clicked) == 1) { return() }
            selected  = binaryVar[clicked]
            varPieces = paste(vars[selected], collapse = " + ")
            MRformula = as.formula(paste("mrobj", "~", varPieces))
            mrObject <<- iNZightMR(MRformula, data = activeData, Labels = substrsplit)
            mro       = mroPara(mrObject)
            # mro$Topic: change this or not?
            ## plot the MR object
            barplot(mro)
        },
        ## summary window
        summaryWindow = function(text, mode) {
            if (mode == 1) {
                # w = 500
                # h = 350
                w = 680; h = 350
            } else if (mode == 2) {
                # w = 670
                # h = 500
                w = 680; h = 350
            } else if (mode == 3) {
                # w = 300
                # h = 200
                w = 680; h = 350
            }
            text = paste0(text, collapse = "\n")
            win  = gwindow("Summary Output", width = w, height = h)
            gtext(text, font.attr = list(family = "monospace"), container = win)
        }
    )
)

# iNZightMultiRes()

iNZightMultiRes <- setRefClass(
    "iNZightMultiRes",
    fields = list(
        GUI        = "ANY",
        mainGrp    = "ANY",
        activeData = "data.frame",
        vars       = "character",
        binaryVar  = "numeric"
    ),

    methods = list(
        initialize = function(GUI) {
            g = gwindow(width = 300, height = 600)
            mainGrp <<- gvbox(spacing = 10, container = g, expand = TRUE)
            
            # initFields(GUI = GUI)
            # dat = GUI$getActiveData()
            # activeData <<- tsData(dat)
            activeData <<- read.csv("//Users/eric/Desktop/CaS.csv")
            # mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(2)
            top = ggroup(container = mainGrp)
            bot = glayout(container = mainGrp, expand = FALSE)
            
            # ==========
            # top panel
            # ==========
            binaryVar <<- getVars(activeData)
            vars      <<- names(activeData)
            gtab = gtable(paste("(b)", vars[binaryVar]), multiple = TRUE, container = top)
            names(gtab) = "VARIABLES (b = binary)"
            size(gtab)  = c(280, 300)
            
            top.timer = NULL
            addHandlerClicked(gtab, handler = function(h, ...) {
                if (!is.null(top.timer))
                    top.timer$stop_timer()
                top.timer = gtimer(500, function(...) {
                    updatePlot(gtab)
                }, one.shot = TRUE)
            })
            
            
            # =============
            # bottom panel
            # =============
            box1 = gcombobox(c("Select Variable 1", vars))
            box2 = gcombobox(c("Select Variable 2", vars))
            size(box1) = c(230, 25)
            size(box2) = c(230, 25)
            slider1 = gslider(0, 1)
            slider2 = gslider(0, 1)
            
            box1.timer = NULL
            addHandlerChanged(box1, handler = function(h, ...) {
                if (!is.null(top.timer))
                    top.timer$stop_timer()
                box1.timer = gtimer(500, function(...) {
                    s = svalue(box1, index = TRUE) - 1
                    if (s == 0) {
                        slider1[] = 0:1
                    } else {
                        subsetVar = iNZightPlots::convert.to.factor(activeData[, s])
                        slider1[] = 0:nlevels(subsetVar)
                    }
                    
                    
                }, one.shot = TRUE)
            })
            
            bot[1, 1:5, anchor = c(0,0), expand = TRUE] = box1
            bot[3, 1:5, anchor = c(0,0), expand = TRUE] = box2
            bot[2, 1:7, anchor = c(0,0), expand = TRUE] = slider1
            bot[4, 1:7, anchor = c(0,0), expand = TRUE] = slider2
            
            closeButton1 = gbutton("")
            closeButton2 = gbutton("")
            closeButton1$set_icon("Cancel")
            closeButton2$set_icon("Cancel")
            bot[1, 7, anchor = c(0,0)] = closeButton1
            bot[3, 7, anchor = c(0,0)] = closeButton2
            
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
            MRobj1    = iNZightMR(MRformula, data = activeData, Labels = substrsplit)
            MRobj2    = mroPara(MRobj1)
            # MRobj2$Topic: change this or not?
            ## plot the MR object
            barplot(MRobj2)
        }
    )
)

iNZightMultiRes()

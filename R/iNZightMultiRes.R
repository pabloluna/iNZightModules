iNZightMultiRes <- setRefClass(
    "iNZightMultiRes",
    fields = list(
        GUI         = "ANY",
        mainGrp     = "ANY",
        activeData = "data.frame"
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
            # mainGrp$set_borderwidth(15)
            top = ggroup(container = mainGrp)
            bot = ggroup(container = mainGrp)
            
            # ==========
            # top panel
            # ==========
            b  = getVars(activeData)
            top.1 = gtable(paste("(b)", names(activeData)[b]), multiple = TRUE, container = top)
            names(top.1) = "VARIABLES (b = binary)"
            size(top.1)  = c(280, 280)
            
            addHandlerShiftclick(top.1, handler = function(h,...) {
                cat(svalue(top.1))
                cat("\n")
            })
#             addHandlerClicked(top.1, handler = function(h,...) {
#                 cat(svalue(top.1))
#                 cat("\n")
#             })
#             addHandlerControlclick(top.1, handler = function(h,...) {
#                 cat(svalue(top.1))
#                 cat("\n")
#             })
            
            
            
            # =============
            # bottom panel
            # =============
            #bot.1 = gcombobox()
            #bot.2 = gcombobox()
            
            
            
            
#             g1 = gframe("Select response", pos = 0.5, horizontal = FALSE, container = mainGrp)
#             g2 = gframe("Group by", pos = 0.5, horizontal = FALSE, container = mainGrp)
#             g3 = gframe("Group by", pos = 0.5, horizontal = FALSE, container = mainGrp)
#             g4 = gvbox(container = mainGrp)
#             g1$set_borderwidth(6)
#             g2$set_borderwidth(6)
#             g3$set_borderwidth(6)
#             
#             frames = getToolkitWidget(mainGrp)$getChildren()
#             mainGrp$set_rgtk2_font(frames[[1]]$getChildren()[[2]], frameFont)
#             mainGrp$set_rgtk2_font(frames[[2]]$getChildren()[[2]], frameFont)
#             mainGrp$set_rgtk2_font(frames[[3]]$getChildren()[[2]], frameFont)
#             
#             g1_layout = glayout(container = g1)
#             g1_opt1   = gtable(names(activeData)[getVars(activeData)[[1]]], multiple = TRUE)
#             size(g1_opt1) = c(120, 120)
#             g1_layout[1, 1, expand = TRUE] = g1_opt1
#             
#             
#             g2_layout = glayout(container = g2)
#             g2_opt1   = gcombobox(names(activeData)[getVars(activeData)[[1]]], multiple = TRUE)
#             g2_opt2   = gslider(0, 2)
#             size(g2_opt1) = c(120, 25)
#             size(g2_opt2) = c(120, 25)
#             
#             g2_layout[1, 1, expand = TRUE] = g2_opt1
#             g2_layout[2, 1, expand = TRUE] = g2_opt2
#             
#             g3_layout = glayout(container = g3)
#             g3_opt1   = gcombobox(names(activeData)[getVars(activeData)[[1]]], multiple = TRUE)
#             g3_opt2   = gslider(0, 2)
#             size(g3_opt1) = c(120, 25)
#             size(g3_opt2) = c(120, 25)
#             
#             g3_layout[1, 1, expand = TRUE] = g3_opt1
#             g3._ayout[1, 2, exapnd = TRUE] = g3_opt2
#             
#             
#             g4_layout = glayout(container = g4)
#             plotButton = gbutton("Plot", handler = function(h,...) {
#                 
#             }, container = g4)
#             summaryButton = gbutton("Summary", handler = function(h,...) {
#                 
#             }, container = g4)
#             g4_layout[1, 1, expand = TRUE] = plotButton
#             g4_layout[1, 2, expand = TRUE] = summaryButton
            
            
            
        },
        
        ## isBinary() checks for a single vector.
        ## getVars() checks for every variable in data.
        getVars = function(data) {
            apply(data, 2, function(x) isBinary(x))
        },
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
        }
        
        
    )
)

iNZightMultiRes()

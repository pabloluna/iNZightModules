## Template for creating a module object
iNZightMaps <- setRefClass(
    
    # ===================================================================
    # REFERENCE DEFINITION
    #   + A character value used as a reference for the object
    #   + Best to keep it consistent with object name to avoid confusion
    # ===================================================================
    "iNZightMaps",
    
    
    # =================================================================================
    # FIELD DEFINITION
    #   + Pre-defined fields:
    #     - GUI       : main GUI
    #     - mainGrp   : main container in which all buttons and sub-groups are defined
    #     - activeData: imported data retrieved from the main iNZight GUI
    #   + Can change names
    #   + Set as many fields as needed
    # =================================================================================
    fields = list(
        GUI         = "ANY",
        mainGrp     = "ANY",
        activeData  = "data.frame",
        map.vars    = "ANY"
    ),
    
    
    # ================================================================================
    # METHOD DEFINITION
    #   + Pre-defined method:
    #     - initialize(): all that relate to module window GUI should be defined here
    # ================================================================================
    methods = list(
        ## Function with all GUI specifications
        initialize = function(GUI) {
            ## GUI
            initFields(GUI = GUI)
            
            ## Configure the data / variables for mapping:
            ## activeData
            activeData <<- GUI$getActiveData()
            
            w <- gwindow("Define Geographical Variables", width = 400, height = 300, parent = GUI$win, visible = FALSE)
            gv <- gvbox(cont = w, expand = TRUE, fill = TRUE)
            gv$set_borderwidth(15)

            title <- glabel("Mapping Variables")
            font(title) <- list(weight = "bold", size = 12, family = "normal")
            add(gv, title, anchor = c(-1, 0))

            
            ## latitude and longitude
            tbl <- glayout()
            ii <- 1

            lbl <- "Latitude :"
            latVar <- gcombobox(c("", numericVars()))
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2, anchor = c(-1, 0), expand = TRUE] <- latVar
            ii <- ii + 1

            lbl <- "Longitude :"
            lonVar <- gcombobox(c("", numericVars()))
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2, anchor = c(-1, 0), expand = TRUE] <- lonVar
            ii <- ii + 1


            addSpace(gv, 10)
            add(gv, tbl, expand = TRUE, fill = TRUE)
            addSpring(gv)

            ## OK Button
            btnGrp <- ggroup(cont = gv)

            addSpring(btnGrp)
            okbtn <- gbutton("OK", expand = TRUE,
                             cont = btnGrp,
                             handler = function(h, ...) {
                                 if (svalue(latVar, TRUE) > 1 && svalue(lonVar, TRUE) > 1) {
                                     setVars(list(latitude = svalue(latVar),
                                                  longitude = svalue(lonVar)))

                                     dispose(w)
                                 } else {
                                     gmessage("Please select a variable for latitude and longitude")
                                 }
                             })
            

            visible(w) <- TRUE

            

            
            ## Reconfigure the Plot Toolbar:

            ###### (to do)

            
            ## mainGrp
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(5)
            
            
            ## close buton
            addSpring(mainGrp)
            
            btmGrp <- ggroup(cont = mainGrp)
            
            helpButton <- gbutton("Help", expand = TRUE, fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=maps")
                                  })
            okButton <- gbutton("Close", expand = TRUE, fill = TRUE,
                                cont = btmGrp,
                                handler = function(h, ...) {
                                    ## delete the module window
                                    delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                    ## display the default view (data, variable, etc.)
                                    visible(GUI$gp1) <<- TRUE
                                })
            
        },
        
        ## Supplementary functions to be used in initialize()
        ##   - Can create as many as needed
        setVars = function(names) {
            map.vars <<- names
        },
        ## get only numeric type variables
        numericVars = function() {
            colnames(activeData)[sapply(activeData, is.numeric)]
        }
    )
    
)

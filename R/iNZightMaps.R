## Template for creating a module object
iNZightMapMod <- setRefClass(

    # ===================================================================
    # REFERENCE DEFINITION
    #   + A character value used as a reference for the object
    #   + Best to keep it consistent with object name to avoid confusion
    # ===================================================================
    "iNZightMapMod",


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
        map.vars    = "ANY",
        map.object  = "ANY",
        map.type    = "ANY"
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
                                     initiateModule()
                                     dispose(w)
                                 } else {
                                     gmessage("Please select a variable for latitude and longitude")
                                 }
                             })
            cnclBtn <- gbutton("Cancel", expand = TRUE, cont = btnGrp,
                               handler = function(h, ...) {
                                  dispose(w)
                               })


            visible(w) <- TRUE






        },

        ## Supplementary functions to be used in initialize()
        ##   - Can create as many as needed
        setVars = function(names) {
            map.vars <<- names
            map.type <<- "roadmap"

            createMapObject()
        },
        createMapObject = function() {
          map.object <<- iNZightMaps::iNZightMap(lat = eval(parse(text = paste("~", map.vars$latitude))),
                                                 lon = eval(parse(text = paste("~", map.vars$longitude))),
                                                 data = activeData,
                                                 name = GUI$dataNameWidget$datName)
        },
        ## get only numeric type variables
        numericVars = function() {
            colnames(activeData)[sapply(activeData, is.numeric)]
        },
        ## initiate the module only when the data has been set
        initiateModule = function() {
          GUI$initializeModuleWindow()


          ## Reconfigure the Plot Toolbar:

          ###### (to do)


          ## mainGrp
          mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
          mainGrp$set_borderwidth(5)

          addSpace(mainGrp, 15)

          lbl1 <- glabel("iNZight Mapping Module")
          font(lbl1) <- list(weight = "bold",
                             family = "normal",
                             size   = 12)
          add(mainGrp, lbl1)
          addSpace(mainGrp, 20)


          tbl <- glayout()
          ii <- 1
          
          lbl <- glabel("Colour by :")
          colVarList <- gcombobox(c("", names(GUI$getActiveData())),
                                  selected = ifelse(
                                      is.null(map.vars$colby),
                                      1, which(names(GUI$getActiveData()) ==
                                                   map.vars$colby)[1] + 1
                                      )
                                  )
          tbl[ii, 1, anchor = c(-1, -1)] <- lbl
          tbl[ii, 2, expand = TRUE] <- colVarList
          ii <- ii + 1
          
          ## lvlCols <- gbutton("Specify colours")
          ## tbl[ii, 2, expand = TRUE] <- lvlCols
          ## visible(lvlCols) <- svalue(grpVarList, index = TRUE) != 1
          ## ii <- ii + 1
          
          ## addHandlerClicked(lvlCols, function(h, ...) {
          ##                       variable <- GUI$getActiveData()[, svalue(grpVarList, index = FALSE)]
          ##                       if (is.numeric(variable)) {
          ##                           gmessage("Set colour of numeric ... not yet implemented.", "Not ready yet.", icon = "warning")
          ##                       } else {
          ##                           specifyColours(variable)
          ##                       }
          ##                   })
          
          
          lbl <- glabel("Size by :")
          rszVarList <- gcombobox(
              c("", rszNames <- names(activeData)[sapply(activeData, is.numeric)]),
              selected = ifelse(
                  is.null(map.vars$sizeby),
                  1, which(rszNames == map.vars$sizeby)[1] + 1
                    )
              )
          tbl[ii, 1, anchor = c(-1, -1)] <- lbl
          tbl[ii, 2, expand = TRUE] <- rszVarList
          ii <- ii + 1


          lbl <- glabel("Opacify by :")
          opctyVarList <- gcombobox(
              c("", numNames <- names(activeData)[sapply(activeData, is.numeric)]),
              selected = ifelse(
                  is.null(map.vars$opacity),
                  1, which(numNames == map.vars$opacity)[1] + 1
                    )
              )
          tbl[ii, 1, anchor = c(-1, -1)] <- lbl
          tbl[ii, 2, expand = TRUE] <- opctyVarList
          ii <- ii + 1

          ii <- ii + 1

          lbl <- glabel("Map type :")
          typeOpts <- c("roadmap", "satellite", "terrain", "hybrid")
          typeList <- gcombobox(typeOpts)
          tbl[ii, 1, anchor = c(-1, -1)] <- lbl
          tbl[ii, 2, expand = TRUE] <- typeList
          
          
          ## Maintain a single function that is called whenever anything is updated:
          updateEverything <- function() {
              if (svalue(colVarList, TRUE) > 1) map.vars$colby <<- svalue(colVarList) else map.vars$colby <<- NULL
              if (svalue(rszVarList, TRUE) > 1) map.vars$sizeby <<- svalue(rszVarList) else map.vars$sizeby <<- NULL
              if (svalue(opctyVarList, TRUE) > 1) map.vars$opacity <<- svalue(opctyVarList) else map.vars$opacity <<- NULL

              map.type <<- svalue(typeList)

              updatePlot()
          }
          
          ## in this case, no point in having a separate "show" button
          addHandlerChanged(colVarList, handler = function(h, ...) updateEverything())
          addHandlerChanged(rszVarList, handler = function(h, ...) updateEverything())
          addHandlerChanged(opctyVarList, handler = function(h, ...) updateEverything())
          addHandlerChanged(typeList, handler = function(h, ...) updateEverything())
          
          ## addHandlerChanged(grpVarList,
          ##                   handler = function(h, ...) {
          ##                       updateEverything()
          ##                       visible(lvlCols) <- svalue(grpVarList, index = TRUE) != 1 &&
          ##                           is.factor(activeData[[svalue(grpVarList)]])
          ##                   })
          
          add(mainGrp, tbl)

          
          
          
          
          
          ## close buton
          addSpring(mainGrp)
          
          btmGrp <- ggroup(cont = mainGrp)
          
          helpButton <- gbutton("Help", expand = TRUE, fill = TRUE,
                                cont = btmGrp,
                                handler = function(h, ...) {
                                    browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=maps")
                                })
          okButton <- gbutton("Back", expand = TRUE, fill = TRUE,
                              cont = btmGrp,
                              handler = function(h, ...) {
                                  ## delete the module window
                                  delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                  ## display the default view (data, variable, etc.)
                                  visible(GUI$gp1) <<- TRUE
                              })

          visible(GUI$moduleWindow) <<- TRUE

          updatePlot()
        },
        ## update plot function
        updatePlot = function() {
            args <- list(x = map.object, varnames = list())
            if (!is.null(map.vars$colby)) {
                args$colby <- activeData[[map.vars$colby]]
                args$varnames$colby = map.vars$colby
            }
            if (!is.null(map.vars$sizeby)) {
                args$sizeby <- activeData[[map.vars$sizeby]]
                args$varnames$sizeby = map.vars$sizeby
            }
            if (!is.null(map.vars$opacity)) {
                args$opacity <- map.vars$opacity
                args$varnames$opacity = map.vars$opacity
            }

            #if (!is.null(map.type))
            args$type <- map.type
            
            do.call("plot.inzightmap", args)
        }
    )

)

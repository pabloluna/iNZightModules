plot3D = function(e){
  
  currentDevice = dev.cur()
  defaultText =  "Drop name here"
  
  noConflicts = TRUE
  
  scatterPlot3DWindow = gwindow("3D Scatter Plot Controls", parent = e$win)  # when you close this window make the current device the embedded plot
  scatterPlot3DGp = ggroup(horizontal = FALSE)
  
  dropGroup = ggroup(horizontal = TRUE)
  
  dropLayout = glayout()
  
  responseLabel =   glabel("  Response     :")
  font(responseLabel) <- list(weight="bold", family = "normal")
  covariateLabel1 = glabel("  Covariate 1  :")
  font(covariateLabel1) <- list(weight="bold", family = "normal")
  covariateLabel2 = glabel("  Covariate 2  :")
  font(covariateLabel2) <- list(weight="bold", family = "normal")
  groupLabel = glabel("  Group by     :")
  font(groupLabel) <- list(weight="bold", family = "normal")
  
  responseDrop = glabel(defaultText)
  covariateDrop1 = glabel(defaultText)
  covariateDrop2 = glabel(defaultText)
  groupDrop = glabel(defaultText)
  
  dropLayout[2,1] = responseLabel
  dropLayout[2,2] = responseDrop
  dropLayout[2,3] = gbutton("clear", handler = function(h,...) svalue(responseDrop) = defaultText)
  dropLayout[3,1] = covariateLabel1
  dropLayout[3,2] = covariateDrop1
  dropLayout[3,3] = gbutton("clear", handler = function(h,...) svalue(covariateDrop1) = defaultText)
  dropLayout[4,1] = covariateLabel2
  dropLayout[4,2] = covariateDrop2
  dropLayout[4,3] = gbutton("clear", handler = function(h,...) svalue(covariateDrop2) = defaultText)
  dropLayout[3,5] = groupLabel
  dropLayout[3,6] = groupDrop
  dropLayout[3,7] = gbutton("clear", handler = function(h,...) svalue(groupDrop) = defaultText)
  
  add(dropGroup, dropLayout)
  
  
  controlsGroupMain = ggroup(horizontal = TRUE)
  plot3DImage = gimage(filename = system.file("images/scatter3dunmoving2.jpg", package = "iNZight"), size = "dialog")
  add(controlsGroupMain, plot3DImage)
  controlsGroup = ggroup(horizontal = FALSE)
  controlsGroupLayout = glayout()
  
  
  
  sectionTitle1 = glabel("Surfaces to Fit")
  font(sectionTitle1) <- list(weight="bold", family = "normal"
  )
  #
  #    controlsGroupLayout[2,1] = glabel("Show surface grid lines")
  #    controlsGroupLayout[3,1] = glabel("Show surface residuals")
  #    controlsGroupLayout[4,1] = sectionTitle1
  #    controlsGroupLayout[5,1] = glabel("Linear least-squares")
  #    controlsGroupLayout[6,1] = glabel("Quadratic least-squares")
  
  
  
  gridLines = gcheckbox("Show surface grid lines", checked = TRUE)
  surfaceResiduals = gcheckbox("Show surface residuals")
  plotSurface = gcheckbox("Plot surface(s)", checked = TRUE)
  linearLS = gcheckbox("Linear least-squares", checked = TRUE)
  quadraticLS = gcheckbox("Quadratic least-squares")
  multipleSurfaceParallel = gcheckbox("Parallel surfaces\n(when plotting surfaces by groups)", checked = TRUE)
  
  e$canIdentify = FALSE
  #e$deviceNumber = -100
  
  
  plotButton = gbutton("Plot in 3D", handler = function(h, ...){
    
    X = svalue(covariateDrop1)
    Z = svalue(covariateDrop2)
    Y = svalue(responseDrop)
    G = svalue(groupDrop)
    
    groups = NULL
    
    if(defaultText %in% c(X,Y,Z)){
      noConflicts = FALSE
      gmessage("3 variables are needed to create a 3-dimensional plot" , parent = e$win)
    }else{
      
      xData = eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(covariateDrop1), collapse = "")))
      zData = eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(covariateDrop2), collapse = "")))
      yData = eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(responseDrop), collapse = "")))
      gData = NULL
      
      if(G != defaultText)
        gData = eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(groupDrop), collapse = "")))
      
      
      if(!is.factor(xData) && !is.factor(yData) && !is.factor(zData)){
        noConflicts = TRUE
        if(!is.null(gData)){
          if(!is.factor(gData)){
            gmessage("The grouping variable MUST be a factor!", parent = e$win)
            noConflicts = FALSE
          }
        }
        
      }else{
        gmessage("One or more of the main variables is a factor!", parent = e$win)
        noConflicts = FALSE
      }
    }
    
    if(noConflicts){
      fit = c("linear", "quadratic")
      
      if(any(c(svalue(linearLS),svalue(quadraticLS))))
        fit = fit[c(svalue(linearLS), svalue(quadraticLS))]
      else{
        svalue(linearLS) = TRUE
        fit = "linear"
        
      }
      e$deviceNumber = dev.cur()
      scatter3d(xData, yData, zData, xlab = X, ylab = Y, zlab = Z, bg.col = "black", fit = fit, sphere.size = 1, surface = svalue(plotSurface), groups = gData, parallel = svalue(multipleSurfaceParallel), grid = svalue(gridLines), residuals = svalue(surfaceResiduals))
      
      
      #        scatter3d(eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(covariateDrop1), collapse = ""))), eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(responseDrop), collapse = ""))), eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(covariateDrop2), collapse = ""))),
      #                  xlab = svalue(covariateDrop1), ylab = svalue(responseDrop), zlab = svalue(covariateDrop2),bg.col = "black", fit = fit, grid = svalue(gridLines), residuals = svalue(surfaceResiduals))
      #
      e$canIdentify = TRUE
      
    }
    
  })
  
  identifyButton = gbutton("Identify Points", handler = function(h,...){
    
    #if(e$deviceNumber != dev.cur())  e$canIdentify = FALSE
    
    if(e$canIdentify)
      identify3d(eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(covariateDrop1), collapse = ""))),
                 eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(responseDrop), collapse = ""))), 
                 eval(parse(text = paste("tag(e$obj, \"dataSet\")", "$", svalue(covariateDrop2), collapse = ""))),
                 col = "white")
    #identify3d(x, y, z, axis.scales=TRUE, groups=NULL, labels=1:length(x),
    #                      col=c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
    #                      offset = ((100/length(x))^(1/3)) * 0.02)
    
    
    
    
  })
  
  identifyGp = ggroup(horizontal = FALSE)
  #addSpace(identifyGp, 10, horizontal = FALSE)
  addSpace(identifyGp, 10)
  #font(plotButton) = list(style="bold")
  add(identifyGp, plotButton)
  #font(identifyButton) = list(style="bold")
  #addSpace(identifyGp, 5, horizontal = FALSE)
  addSpace(identifyGp, 5)
  add(identifyGp, identifyButton)
  add(identifyGp, glabel("Drag right mouse button to identify points,\nclick right button to exit"), expand = TRUE)
  
  controlsGroupLayout[2,2] = gridLines
  controlsGroupLayout[3,2] = surfaceResiduals
  controlsGroupLayout[4,2] = sectionTitle1
  controlsGroupLayout[5,2] = plotSurface
  controlsGroupLayout[6,2] = linearLS
  controlsGroupLayout[7,2] = quadraticLS
  controlsGroupLayout[8,2] = multipleSurfaceParallel
  #controlsGroupLayout[8,3] = plotButton
  
  
  addDropTarget(responseDrop, handler = function(h,...) svalue(responseDrop) = gWidgets::id(h$dropdata))
  addDropTarget(covariateDrop2, handler = function(h,...) svalue(covariateDrop2) = gWidgets::id(h$dropdata))
  addDropTarget(covariateDrop1, handler = function(h,...) svalue(covariateDrop1) = gWidgets::id(h$dropdata))
  addDropTarget(groupDrop, handler = function(h,...) svalue(groupDrop) = gWidgets::id(h$dropdata))
  
  add(controlsGroup, controlsGroupLayout)
  add(controlsGroup, identifyGp)
  add(scatterPlot3DGp, dropGroup)
  add(controlsGroupMain, controlsGroup)
  add(scatterPlot3DGp, controlsGroupMain)
  
  add(scatterPlot3DWindow, scatterPlot3DGp)
  
}

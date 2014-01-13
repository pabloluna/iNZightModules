scatterPlotMatrix = function(e,...){
  e$scatterPlotWinOpened = TRUE
  varList = names(tag(e$obj, "dataSet"))
  
  scatterPlotWin = gwindow("Scatter Plot Matrix", parent = e$win, width = 250, height = 350)
  addHandlerUnrealize(scatterPlotWin, handler = function(h,...){e$scatterPlotWinOpened = FALSE; dispose(scatterPlotWin)})
  
  scatterPlotMain = ggroup(horizontal = FALSE, container = scatterPlotWin, expand = TRUE)
  #addSpace(scatterPlotMain, 5, horizontal = FALSE)
  addSpace(scatterPlotMain, 5)
  lbl1 = glabel("Dataset Variables", container = scatterPlotMain)
  font(lbl1) <- list(weight="bold", family = "normal")
  lbl2 = glabel("(Hold Ctrl to choose many)", container = scatterPlotMain)
  font(lbl2) <- list(weight="bold", family = "normal")
  
  listOfVars = gtable(varList, multiple = TRUE, container = scatterPlotMain, expand = TRUE)
  names(listOfVars) = "Variables"
  
  #addSpace(scatterPlotMain, 5, horizontal = FALSE)
  addSpace(scatterPlotMain, 5)
  plotButton = gbutton("- Plot -", container = scatterPlotMain, handler = function(h,...) {
    
    chosenVars = svalue(listOfVars)
    chosenVarIndices = which(names(tag(e$obj,"dataSet")) %in% chosenVars)
    
    #dev.new()
    gpairs(tag(e$obj, "dataSet")[,chosenVarIndices])
    #dispose(scatterPlotWin)
  })
  font(plotButton) = list(weight = "bold")
  
}
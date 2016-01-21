##' Draws a matrix of all plot-combinations based on selected variables.
##'
##' User selects variables, they are drawn.
##' 
##' @title Scatterplot Matrix
##' @param e going, going, going ...
##' @param ... more opts
##' 
##' @return NULL
##'
##' @importFrom gpairs gpairs
##' 
##' @author iNZight Team
##' 
##' @export
scatterPlotMatrix <- function(e, ...) {
    e$scatterPlotWinOpened <- TRUE
    varList <- names(tag(e$obj, "dataSet"))
    d <- tag(e$obj, "dataSet")
  
    scatterPlotWin <- gwindow("Scatter Plot Matrix", parent = e$win,
                              width = 250, height = 350)
    addHandlerUnrealize(scatterPlotWin,
                        handler = function(h,...) {
                            e$scatterPlotWinOpened = FALSE
                            dispose(scatterPlotWin)
                        })
  
    scatterPlotMain <- ggroup(horizontal = FALSE, container = scatterPlotWin, expand = TRUE)
    addSpace(scatterPlotMain, 5)
    lbl1 <- glabel("Dataset Variables", container = scatterPlotMain)
    font(lbl1) <- list(weight = "bold", family = "normal")
    lbl2 <- glabel("(Hold Ctrl to choose many)", container = scatterPlotMain)
    font(lbl2) <- list(weight = "bold", family = "normal")
      
    listOfVars <- gtable(varList, multiple = TRUE, container = scatterPlotMain, expand = TRUE)
    names(listOfVars) <- "Variables"

    addSpace(scatterPlotMain, 5)
    
    lbl3 <- glabel("Grouping Variable", container = scatterPlotMain)
    font(lbl3) <- list(weight = "bold", family = "normal")

    facVars <- listOfVars[sapply(d, class) == "factor"]
    grpVar <- gcombobox(c("", facVars), selected = 1, container = scatterPlotMain)
    
    
    addSpace(scatterPlotMain, 5)
    plotButton <- gbutton("- Plot -", container = scatterPlotMain,
                          handler = function(h,...) {
                              chosenVars <- svalue(listOfVars)
                              grpVar <- svalue(grpVar)
                              if (grpVar == "") {
                                  scatter.pars <- NULL
                                  stripplot.pars <- list(jitter.data = TRUE)
                              } else {
                                  scatter.pars <- list(col = as.numeric(d[, grpVar]))
                                  stripplot.pars <- list(jitter.data = TRUE,
                                                         col = as.numeric(d[, grpVar]))
                              }
                              chosenVarIndices <-
                                  which(names(d) %in% chosenVars)
    
                              dev.new()
                              dev.hold()
                              gpairs(d[, chosenVarIndices],
                                     upper.pars = list(conditional = "stripplot"),
                                     scatter.pars = scatter.pars,
                                     stripplot.pars = stripplot.pars)
                              dev.flush()
                          })
    font(plotButton) <- list(weight = "bold")
}

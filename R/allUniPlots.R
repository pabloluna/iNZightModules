##' Explore all univariate plots
##'
##' Opens a pop-up window that allows users to click through all univariate plots,
##' which is useful for quickly exploring large data sets.
##' 
##' @title Explore All Univariate Plots
##' 
##' @param e (depreciated ... will be updated in future)
##' 
##' @return NULL
##' 
##' @author Tom Elliott
##' 
##' @export
allUniPlots <- function(e) {
    win <- e$win
    data <- tag(e$obj, "dataSet")

    w <- gwindow("Explore All 1-way Plots", width = 600, height = 600,
                 visible = FALSE, parent = win)

    noteb <- gstackwidget(expand = TRUE)
    add(w, noteb, expand = TRUE)
    fig <- ggraphics(expand = TRUE)
    add(noteb, fig)

    visible(w) <- TRUE

    ploti <- 0
    addHandlerClicked(fig,
                      handler = function(h, ...) {
                          ploti <<- ploti + 1
                          i <- ploti
                          if (i == ncol(data) + 1) {
                              grid::grid.newpage()
                              grid::pushViewport(grid::viewport())
                              grid::grid.text(paste("Click to close."))
                           } else if (i > ncol(data) + 1) {
                               dispose(w)
                           } else {
                              iNZightPlots::iNZightPlot(data[, i],
                                                        varnames =
                                                        list(x = colnames(data)[i]))
                          }
                      })
    
    grid::grid.newpage()
    grid::pushViewport(grid::viewport())

  # Title window:
    grid::grid.text(paste("Click the window for the next plot."))
}

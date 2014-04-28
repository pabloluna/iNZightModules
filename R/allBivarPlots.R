allBivarPlots <- function(e) {
    win <- e$win
    data <- tag(e$obj, "dataSet")

    w1 <- gwindow("Explore Bivariate Plots", width = 250, height = 100,
                  visible = FALSE, parent = win)
    gg <- ggroup(horizontal = FALSE, container = w1, expand = TRUE)

    lbl1 <- glabel("Select Variable 1")
    font(lbl1) <- list(weight = "bold", family = "normal")
    
    xvar <- gcombobox(names(data), selected = 1, expand = FALSE)
    btn <- gbutton("Plot Variables", handler = function(h, ...) {
        x.name <- svalue(xvar, index = FALSE)
        o.name <- names(data)[names(data) != x.name]

        w <- gwindow("Explore Bivariate Plots", width = 600, height = 600,
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
                              if (i == ncol(data)) {
                                  grid::grid.newpage()
                                  grid::pushViewport(grid::viewport())
                                  grid::grid.text(paste("Click to close."))
                              } else if (i > ncol(data)) {
                                  dispose(w)
                              } else if (i > 1) {
                                  X <- data[, x.name]
                                  Y <- data[, o.name[i]]
                                # Flip X and Y for numeric variables:
                                  if (is.numeric(X) & is.numeric(Y)) {
                                      iNZightPlots::iNZightPlot(Y, X,
                                                            varnames =
                                                            list(x = o.name[i],
                                                                 y = x.name))
                                  } else {
                                      iNZightPlots::iNZightPlot(X, Y,
                                                            varnames =
                                                            list(x = x.name,
                                                                 y = o.name[i]))
                                  }
                              } else {
                                  iNZightPlots::iNZightPlot(data[, x.name],
                                                            varnames =
                                                            list(x = x.name))
                              }
                          })
        
        grid::grid.newpage()
        grid::pushViewport(grid::viewport())
        
      # Title window:
        grid::grid.text(paste("Click the window for the next plot."))
    })
    
    tbl <- glayout()
    tbl[1, 1:2, expand = TRUE] <- lbl1
    tbl[2, 1:2, expand = FALSE] <- xvar
    tbl[3, 1:2, expand = FALSE] <- btn

    add(gg, tbl)
    
    visible(w1) <- TRUE
    
}

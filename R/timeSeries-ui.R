# This is a module used for graphical time series analysis.
timeSeries <- function(e) {

  
  tsenv <- new.env()
  tsStructure <- list(start = NA, frequency = NA)
  tag(e$obj, "tsStructure") = list(start = NA, frequency = NA)
  fully.loaded <- FALSE
  tswin <- gwindow(title = "Time Series", expand = FALSE)
  main.group <- ggroup(horizontal = FALSE, expand = TRUE, container = tswin)
  main.layout <- glayout(container = main.group)
  main.layout[1:2, 1] <- (ts.select <- gradio(c("Select *TIME* Variable", "Provide Time Information"),
                                              handler = function(h, ...) {
                                                if (svalue(ts.select, index = TRUE) == 1) {
                                                  enabled(createTimeVar) <- FALSE
                                                  enabled(e$tsTimeVar) <- TRUE
                                                } else {
                                                  enabled(e$tsTimeVar) <- FALSE
                                                  enabled(createTimeVar) <- TRUE
                                                }
                                              }))
  
  main.layout[1, 2, expand = TRUE] <- (e$tsTimeVar <- gcombobox(names(tag(e$obj, "dataSet")),
                                                                handler = function(h, ...) {
                                                                  loadTimeInfo()
                                                                }))
  
  loadTimeInfo <- function() {
    vardata <- tag(e$obj, "dataSet")[, svalue(e$tsTimeVar)]
    ts.info <- get.ts.structure(vardata)
    tsStructure <<- ts.info
    if (any(is.na(ts.info$start))) {
      if (! fully.loaded) {
        fully.loaded <- TRUE
      } else {
        gmessage("Invalid date", title = "Error", icon = "error")
      }
      return()
    }
  }
  
  # Just run once
  loadTimeInfo()
  
  main.layout[2, 2, expand = TRUE] <- (createTimeVar <- gbutton("Create",
                                                                handler = function(h, ...) {
                                                                  #createTSInfo()
                                                                  tsconvwin <- gwindow("Provide Time Series Information")
                                                                  tsgroup <- ggroup(horizontal = FALSE, container = tsconvwin)
                                                                  tsvar.layout <- glayout(container = tsgroup)
                                                                  tsvar.layout[1, 1, expand = FALSE] <- (tsStart1.lab <- glabel("Start Date"))
                                                                  tsvar.layout[1, 2, expand = TRUE] <- (tsVarStart1 <- gedit(width = 30, height = 20))
                                                                  tsvar.layout[2, 1, expand = FALSE] <- (tsStart2.lab <- glabel("Season Number"))
                                                                  tsvar.layout[2, 2, expand = TRUE] <- (tsVarStart2 <- gedit(width = 30, height = 20))
                                                                  tsvar.layout[3, 1, expand = FALSE] <- (tsFrequency.lab <- glabel("Frequency"))
                                                                  tsvar.layout[3, 2, expand = TRUE] <- (tsVarFrequency <- gedit(width = 30, height = 20))
                                                                  saveButton <- gbutton("Provide TS Info", container = tsgroup,
                                                                                        handler = function(h, ...) {
                                                                                          valid.params <- logical(3)
                                                                                          valid.ts.args <- function(arg) {
                                                                                            suppressWarnings(tmp <- as.integer(arg))
                                                                                            ! is.na(tmp) && length(tmp) > 0
                                                                                          }
                                                                                          # Start Part 1 - starting value of the series (could be years)
                                                                                          start.part1 <- svalue(tsVarStart1)
                                                                                          valid.params[1] <- valid.ts.args(start.part1)
                                                                                          # Start Part 2 - how far through the cycle of the first value we are
                                                                                          start.part2 <- svalue(tsVarStart2)
                                                                                          valid.params[2] <- valid.ts.args(start.part2)
                                                                                          # Frequency - how many values does it take for us to increment "Start Part 1"
                                                                                          frequency <- svalue(tsVarFrequency)
                                                                                          valid.params[3] <- valid.ts.args(frequency)
                                                                                          if (! all(valid.params)) {
                                                                                            gmessage("One of your time series parameters is not an integer", icon = "error")
                                                                                            return()
                                                                                          }
                                                                                          # Now we know all params are valid, cast to ints
                                                                                          start.part1 <- as.integer(start.part1)
                                                                                          start.part2 <- as.integer(start.part2)
                                                                                          frequency <- as.integer(frequency)
                                                                                          tsStructure <<- list(start = c(start.part1, start.part2),
                                                                                                              frequency = frequency)
                                                                                          # Get rid of the window for now
                                                                                          dispose(tsconvwin)
                                                                                        })
                                                                }))
  
  enabled(createTimeVar) <- FALSE
  allow.recompose <- FALSE
  
  
  # Need to create an environment, within which we are able to
  # allow for a somewhat "global" variable

  assign("stopAnimation", FALSE, envir = tsenv)
  astart.anime <- gaction("Time Series Plot - Animate", icon = "gtk-media-play",
                         handler = function(h, ...) {
                           assign("stopAnimation", FALSE, envir = tsenv)
                           ts.info <- tsStructure
                           valid.ts <- valid.ts.info(ts.info)
                           # We always know that there will be at least one selected value
                           # otherwise we would not be able to run this handler
                           var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                           valid.var <- valid.vars(var.df)
                           allow.recompose <- FALSE
                           enabled(single.recompose) <- allow.recompose
                           enabled(single.recomp.result) <- allow.recompose
                           if (valid.ts && valid.var) {
                             enabled(single.label) <- FALSE
                             #enabled(single.tsplot.animated) <- TRUE  
                             var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                  start = ts.info$start,
                                                  freq = ts.info$frequency,                                                                           
                                                  var=svalue(tsVarselect,index= TRUE))
                             enabled(start.anime) <- FALSE
                             enabled(skip.anime) <- TRUE
                             enabled(single.tsplot) <- FALSE
                             enabled(single.decompose) <- FALSE
                             enabled(single.seasonal) <- FALSE
                             enabled(single.forecast) <- FALSE
                             enabled(recompose.start.anime) <- FALSE
                             enabled(single.recomp.result) <- FALSE
                             
                             rawplot(var.df2, ylab = svalue(ylab.input), xlab = svalue(xlab.input),
                                     animate = TRUE, e= tsenv)
                             #tsPlot(var.df = var.df, start = ts.info$start,
                            #        frequency = ts.info$frequency, animate = TRUE, env = tsenv)
                             enabled(single.label) <- TRUE
                             enabled(single.tsplot) <- TRUE
                             enabled(start.anime) <- TRUE
                             enabled(skip.anime) <- FALSE
                             enabled(single.decompose) <- TRUE
                             enabled(recompose.start.anime) <- FALSE
                             enabled(single.recomp.result) <- FALSE
                             enabled(single.seasonal) <- length(ts.info$start) > 1
                             enabled(single.forecast) <- length(ts.info$start) > 1
                             
                           }
                         })
  start.anime <- gbutton(action = astart.anime)
  askip.anime <- gaction("Skip to End", icon = "gtk-media-next", 
                        handler = function(h, ...) {
                          ts.info <- tsStructure
                          assign("stopAnimation", TRUE, envir = tsenv)
                          enabled(single.label) <- TRUE
                          enabled(single.tsplot) <- TRUE
                          enabled(single.decompose) <- TRUE
                          enabled(single.seasonal) <- length(ts.info$start) > 1
                          enabled(single.forecast) <- length(ts.info$start) > 1
                          enabled(single.recompose) <- FALSE
                          enabled(single.recomp.result) <- FALSE
                          allow.recompose <- FALSE
                          enabled(single.recompose) <- allow.recompose
                          enabled(single.recomp.result) <- allow.recompose
                          enabled(start.anime) <- TRUE
                          enabled(skip.anime) <- FALSE
                        })
  skip.anime<- gbutton(action = askip.anime)
  main.layout[3, 1:2, anchor = c(-0.9,-0.5), expand= TRUE] <- glabel("Select variable(s)\n(Use Ctrl for multiple selection)")
  tsVarselect <- gtable(names(tag(e$obj, "dataSet")), multiple = TRUE)
  main.layout[4, 1:2] <- tsVarselect
  size(tsVarselect) <- c(300, 200)
  middle.layout <- glayout()
  radio.group <- gradio(c("Additive","Multiplicative"), selected=1, horizontal=TRUE, label = "model type")
  middle.layout[1,1] <- radio.group
  ylab.input <- gedit()
  xlab.input <- gedit("Time")
  middle.layout[2,1] <- glabel("x label")
  middle.layout[3,1] <- xlab.input
  middle.layout[2,2] <- glabel("y label")
  middle.layout[3,2] <- ylab.input
  main.layout[5, 1:2] <- middle.layout
  main.layout[6, 1:2] <- (single.label <- glabel("Single Series"))
  small.layout <- glayout()
  small.layout[1,1] <- start.anime
  small.layout[1,2] <- skip.anime
  main.layout[7, 1] <- small.layout
  
  # the old single.ts.plot.animated
  # I personally still want to use one button and flip to change its ability
  # but in gWidgets2 right now (13/01/2014) is not working..
  (single.tsplot.animated <- gbutton("Time Series Plot - Animate",
                                     handler = function(h, ...) {
                                       if (svalue(single.tsplot.animated) == "Time Series Plot - Animate") {
                                         assign("stopAnimation", FALSE, envir = tsenv)
                                         ts.info <- tsStructure
                                         valid.ts <- valid.ts.info(ts.info)
                                         # We always know that there will be at least one selected value
                                         # otherwise we would not be able to run this handler
                                         var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                         valid.var <- valid.vars(var.df)
                                         allow.recompose <- FALSE
                                         enabled(single.recompose) <- allow.recompose
                                         enabled(single.recomp.result) <- allow.recompose
                                         if (valid.ts && valid.var) {
                                           enabled(single.label) <- FALSE
                                           enabled(single.tsplot.animated) <- TRUE
                                           svalue(single.tsplot.animated) <- "Skip Animation"
                                           enabled(single.tsplot) <- FALSE
                                           enabled(single.decompose) <- FALSE
                                           enabled(single.seasonal) <- FALSE
                                           enabled(single.forecast) <- FALSE
                                           enabled(single.recompose) <- FALSE
                                           enabled(single.recomp.result) <- FALSE
                                           tsPlot(var.df = var.df, start = ts.info$start,
                                                 frequency = ts.info$frequency, animate = TRUE, env = tsenv)
                                           enabled(single.label) <- TRUE
                                           enabled(single.tsplot.animated) <- TRUE
                                           svalue(single.tsplot.animated) <- "Time Series Plot - Animate"
                                           enabled(single.tsplot) <- TRUE
                                           enabled(single.decompose) <- TRUE
                                           enabled(single.recompose) <- FALSE
                                           enabled(single.recomp.result) <- FALSE
                                           enabled(single.seasonal) <- length(ts.info$start) > 1
                                           enabled(single.forecast) <- length(ts.info$start) > 1
                                         }
                                       } else if (svalue(single.tsplot.animated) == "Skip Animation") {
                                         ts.info <- tsStructure
                                         assign("stopAnimation", TRUE, envir = tsenv)
                                         enabled(single.label) <- TRUE
                                         enabled(single.tsplot.animated) <- TRUE
                                         svalue(single.tsplot.animated) <- "Time Series Plot - Animate"
                                         enabled(single.tsplot) <- TRUE
                                         enabled(single.decompose) <- TRUE
                                         enabled(single.seasonal) <- length(ts.info$start) > 1
                                         enabled(single.forecast) <- length(ts.info$start) > 1
                                         enabled(single.recompose) <- FALSE
                                         enabled(single.recomp.result) <- FALSE
                                       }
                                     }))
  
  main.layout[7, 2] <- (single.tsplot <- gbutton("Time Series Plot",
                                                 handler = function(h, ...) {
                                                   ts.info <- tsStructure
                                                   valid.ts <- valid.ts.info(ts.info)
                                                   # We always know that there will be at least one selected value
                                                   # otherwise we would not be able to run this handler
                                                   var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                                   valid.var <- valid.vars(var.df)
                                                   allow.recompose <- FALSE
                                                   enabled(single.recompose) <- allow.recompose
                                                   enabled(single.recomp.result) <- allow.recompose
                                                   if (valid.ts && valid.var) {
                                                     var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                                          start = ts.info$start,
                                                                          freq = ts.info$frequency,                                                                           
                                                                          var=svalue(tsVarselect,index= TRUE))
                                                     assign("stopAnimation", FALSE, envir = tsenv)
                                                     #tsPlot(var.df = var.df, start = ts.info$start,
                                                    #        frequency = ts.info$frequency, animate = FALSE, env = tsenv)
                                                    rawplot(var.df2, ylab = svalue(ylab.input), xlab = svalue(xlab.input),
                                                            multiplicative = svalue(radio.group) == "Multiplicative",
                                                            animate=FALSE, e = tsenv)
                                                   }
                                                 }))
  main.layout[8, 1:2] <- (single.decompose <- gbutton("Decompose",
                                                      handler = function(h, ...) {
                                                        ts.info <- tsStructure
                                                        valid.ts <- valid.ts.info(ts.info)
                                                        var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                                        valid.var <- valid.vars(var.df)
                                                        if (valid.ts && valid.var) {
                                                          allow.recompose <- TRUE
                                                          enabled(single.recompose) <- allow.recompose
                                                          enabled(single.recomp.result) <- allow.recompose
                                                          #svalue(single.recompose) <- "Recompose - Animate"
                                                          enabled(recompose.start.anime) <- TRUE
                                                          var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                                               start = ts.info$start,
                                                                               freq = ts.info$frequency,                                                                           
                                                                               var=svalue(tsVarselect,index= TRUE))
                                                          #tsDecompose(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                          decompositionplot(var.df2, ylab = svalue(ylab.input), xlab = svalue(xlab.input),
                                                                            multiplicative = svalue(radio.group) == "Multiplicative")
                                                        }
                                                      }))
  
  arecompose.start.anime <- gaction("Recompose - Animate", icon = "gtk-media-play",
                                   handler = function(h, ...) {
                                     ts.info <- tsStructure
                                     valid.ts <- valid.ts.info(ts.info)
                                     var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                     valid.var <- valid.vars(var.df)               
                                     allow.recompose <- FALSE
                                     #enabled(single.recompose) <- allow.recompose
                                     enabled(recompose.start.anime) <- allow.recompose
                                     enabled(single.recomp.result) <- allow.recompose
                                     enabled(recompose.skip.anime) <- TRUE
                                     if (valid.ts && valid.var) {
                                       enabled(single.label) <- FALSE
                                       
                                       #enabled(single.tsplot.animated) <- FALSE
                                       enabled(start.anime) <- FALSE
                                       enabled(single.tsplot) <- FALSE
                                       enabled(single.decompose) <- FALSE
                                       enabled(single.seasonal) <- FALSE
                                       enabled(single.forecast) <- FALSE
                                       enabled(recompose.start.anime) <- FALSE
                                       assign("stopAnimation", FALSE, envir = tsenv)
                                       var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                            start = ts.info$start,
                                                            freq = ts.info$frequency,                                                                           
                                                            var=svalue(tsVarselect,index= TRUE))
                                       #tsRecompose(var.df = var.df, start = ts.info$start,
                                      #             frequency = ts.info$frequency, env = tsenv)
                                      iNZightTS:::recompose(decompositionplot(var.df2, ylab = svalue(ylab.input), xlab = svalue(xlab.input),
                                                                              multiplicative = svalue(radio.group) == "Multiplicative"), animate=TRUE,e=tsenv)
                                       enabled(single.label) <- TRUE
                                       #enabled(single.tsplot.animated) <- TRUE
                                       enabled(start.anime) <- TRUE
                                       enabled(single.tsplot) <- TRUE
                                       enabled(single.decompose) <- TRUE
                                       enabled(recompose.start.anime) <- TRUE
                                       enabled(recompose.skip.anime) <- FALSE
                                       enabled(single.seasonal) <- length(ts.info$start) > 1
                                       enabled(single.forecast) <- length(ts.info$start) > 1
                                     }
                                   })
  recompose.start.anime <- gbutton(action = arecompose.start.anime)
  arecompose.skip.anime <- gaction("Skip to End", icon = "gtk-media-next",
                                  handler = function(h, ...) {
                                    ts.info <- tsStructure
                                    enabled(single.label) <- TRUE
                                    #enabled(single.tsplot.animated) <- TRUE
                                    enabled(start.anime) <- TRUE
                                    enabled(single.tsplot) <- TRUE
                                    enabled(single.decompose) <- TRUE
                                    enabled(single.seasonal) <- length(ts.info$start) > 1
                                    enabled(single.forecast) <- length(ts.info$start) > 1
                                    enabled(recompose.start.anime) <- FALSE 
                                    #enabled(single.recompose) <- FALSE
                                    enabled(single.recomp.result) <- FALSE
                                    assign("stopAnimation", TRUE, envir = tsenv)
                                    allow.recompose <- FALSE
                                    #enabled(single.recompose) <- allow.recompose
                                    enabled(recompose.start.anime) <- allow.recompose
                                    enabled(single.recomp.result) <- allow.recompose
                                    enabled(recompose.start.anime) <- TRUE
                                    enabled(recompose.skip.anime) <- FALSE
                                    enabled(single.recomp.result) <- TRUE
                                  })
  recompose.skip.anime <- gbutton(action = arecompose.skip.anime)
  small.layout2 <- glayout()
  main.layout[9, 1] <- small.layout2
  small.layout2[1, 1] <- recompose.start.anime
  small.layout2[1, 2] <- recompose.skip.anime 
  enabled(recompose.start.anime) <- FALSE   # %
  enabled(recompose.skip.anime) <- FALSE    # %
  (single.recompose <- gbutton("Recompose - Animate", handler = function(h, ...) { 
    if (svalue(single.recompose) == "Recompose - Animate")
      KK = TRUE
    else
      KK = FALSE
    if (KK) {
      ts.info <- tsStructure
      valid.ts <- valid.ts.info(ts.info)
      var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
      var.df2 <- iNZightTS(tag(e$obj, "dataSet"), var = svalue(tsVarselect,index=TRUE))
      valid.var <- valid.vars(var.df)
      allow.recompose <- FALSE
      enabled(single.recompose) <- allow.recompose
      enabled(single.recomp.result) <- allow.recompose
      if (valid.ts && valid.var) {
        enabled(single.label) <- FALSE
        enabled(single.tsplot.animated) <- FALSE
        enabled(single.tsplot) <- FALSE
        enabled(single.decompose) <- FALSE
        enabled(single.seasonal) <- FALSE
        enabled(single.forecast) <- FALSE
        enabled(single.recompose) <- TRUE
        #svalue(single.recompose) <- "Skip Animation"   # this makes the iteratively change between "Recompose - Animate" and "between animation"
        assign("stopAnimation", FALSE, envir = tsenv)
        tsRecompose(var.df = var.df, start = ts.info$start,
                    frequency = ts.info$frequency, env = tsenv)
        enabled(single.label) <- TRUE
        enabled(single.tsplot.animated) <- TRUE
        enabled(single.tsplot) <- TRUE
        enabled(single.decompose) <- TRUE
        enabled(single.recompose) <- FALSE
        svalue(single.recompose) <- "Recompose - Animate"
        enabled(single.seasonal) <- length(ts.info$start) > 1
        enabled(single.forecast) <- length(ts.info$start) > 1
      } 
    } else if (!KK) {
      ts.info <- tsStructure
      enabled(single.label) <- TRUE
      enabled(single.tsplot.animated) <- TRUE
      enabled(single.tsplot) <- TRUE
      enabled(single.decompose) <- TRUE
      enabled(single.seasonal) <- length(ts.info$start) > 1
      enabled(single.forecast) <- length(ts.info$start) > 1
      enabled(single.recompose) <- FALSE
      svalue(single.recompose) <- "Recompose - Animate"
      enabled(single.recomp.result) <- FALSE
      assign("stopAnimation", TRUE, envir = tsenv)
    }
  }))
  main.layout[9, 2] <- (single.recomp.result <- gbutton("Recompose - Result",
                                                        handler = function(h, ...) {
                                                          ts.info <- tsStructure
                                                          valid.ts <- valid.ts.info(ts.info)
                                                          var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                                          valid.var <- valid.vars(var.df)
                                                          allow.recompose <- FALSE
                                                          enabled(recompose.start.anime) <- allow.recompose
                                                          enabled(single.recomp.result) <- allow.recompose
                                                          if (valid.ts && valid.var) {
                                                            assign("stopAnimation", FALSE, envir = tsenv)
                                                            var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                                                 start = ts.info$start,
                                                                                 freq = ts.info$frequency,                                                                           
                                                                                 var=svalue(tsVarselect,index= TRUE))
                                                            #tsRecompResult(var.df = var.df, start = ts.info$start,
                                                            #               frequency = ts.info$frequency, env = tsenv)
                                                            iNZightTS:::recompose(decompositionplot(var.df2, ylab = svalue(ylab.input), xlab = svalue(xlab.input),
                                                                                                    multiplicative = svalue(radio.group) == "Multiplicative"), animate=FALSE,e=tsenv)
                                                          }
                                                        }))
  main.layout[10, 1:2] <- (single.seasonal <- gbutton("Seasonplot",
                                                     handler = function(h, ...) {
                                                       ts.info <- tsStructure
                                                       valid.ts <- valid.ts.info(ts.info)
                                                       var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                                       valid.var <- valid.vars(var.df)
                                                       allow.recompose <- FALSE
                                                       enabled(recompose.start.anime) <- allow.recompose
                                                       enabled(single.recomp.result) <- allow.recompose
                                                       if (valid.ts && valid.var)
                                                         var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                                              start = ts.info$start,
                                                                              freq = ts.info$frequency,                                                                           
                                                                              var=svalue(tsVarselect,index= TRUE))
                                                         #tsSeasonal(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                         seasonplot(var.df2,  multiplicative = svalue(radio.group) == "Multiplicative",
                                                                    ylab = svalue(ylab.input))
                                                     }))
  main.layout[11, 1:2] <- (single.forecast <- gbutton("Forecast",
                                                      handler = function(h, ...) {
                                                        ts.info <- tsStructure
                                                        valid.ts <- valid.ts.info(ts.info)
                                                        var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                                        valid.var <- valid.vars(var.df)
                                                        allow.recompose <- FALSE
                                                        enabled(recompose.start.anime) <- allow.recompose
                                                        enabled(single.recomp.result) <- allow.recompose
                                                        if (valid.ts && valid.var) {
                                                          #hwPredictionWindow(var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                          #tsForecast(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                                          var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                                               start = ts.info$start,
                                                                               freq = ts.info$frequency,                                                                           
                                                                               var=svalue(tsVarselect,index= TRUE))
                                                          forecastWindow(forecastplot(var.df2,  multiplicative = svalue(radio.group) == "Multiplicative",
                                                                                      ylab = svalue(ylab.input), 
                                                                                      show = FALSE))
                                                          forecastplot(var.df2,  multiplicative = svalue(radio.group) == "Multiplicative",
                                                                       ylab = svalue(ylab.input))
                                                        }
                                                      }))
  main.layout[12, 1:2] <- (several.label <- glabel("Several Series"))
  several.compare.series <- gbutton("Multi-Plot",
                                     handler = function(h, ...) {
                                       ts.info <- tsStructure
                                       valid.ts <- valid.ts.info(ts.info)
                                       var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                       valid.var <- valid.vars(var.df)
                                       allow.recompose <- FALSE
                                       enabled(recompose.start.anime) <- allow.recompose
                                       enabled(single.recomp.result) <- allow.recompose
                                       if (valid.ts && valid.var)
                                         #compareSeries(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                         var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                              start = ts.info$start,
                                                              freq = ts.info$frequency,                                                                           
                                                              var=svalue(tsVarselect,index= TRUE))
                                         multiseries(var.df2, ylab = svalue(ylab.input), 
                                                     multiplicative = svalue(radio.group) == "Multiplicative")
                                     })
  main.layout[14, 1:2] <- several.compare.series
  several.compare.series2 <- gbutton("Single-Plot",
                                     handler = function(h, ...) {
                                       ts.info <- tsStructure
                                       valid.ts <- valid.ts.info(ts.info)
                                       var.df <- tag(e$obj, "dataSet")[, svalue(tsVarselect), drop = FALSE]
                                       valid.var <- valid.vars(var.df)
                                       allow.recompose <- FALSE
                                       enabled(recompose.start.anime) <- allow.recompose
                                       enabled(single.recomp.result) <- allow.recompose
                                       if (valid.ts && valid.var)
                                         #compareSeries(var.df = var.df, start = ts.info$start, frequency = ts.info$frequency)
                                         var.df2 <- iNZightTS(tag(e$obj, "dataSet"), 
                                                              start = ts.info$start,
                                                              freq = ts.info$frequency,                                                                           
                                                              var=svalue(tsVarselect,index= TRUE))
                                         compareplot(var.df2, ylab = svalue(ylab.input), 
                                                     multiplicative = svalue(radio.group) == "Multiplicative")
                                     })
  main.layout[13, 1:2] <- several.compare.series2
  
  multiple.select.handler <- function(...) {
    ts.info <- tsStructure
    invalid.time <- any(is.na(ts.info$start))
    if (! invalid.time) {
      show.annual <- length(ts.info$start) > 1
      n <- length(svalue(tsVarselect))
    } else {
      n <- 0
    }
    if (n > 1) {
      enabled(single.label) <- FALSE
      #enabled(single.tsplot.animated) <- FALSE
      enabled(start.anime) <- FALSE  # %
      enabled(skip.anime) <- FALSE   # %
      enabled(single.tsplot) <- FALSE
      enabled(single.decompose) <- FALSE
      #enabled(single.recompose) <- FALSE
      enabled(recompose.start.anime) <- FALSE   # %
      enabled(recompose.skip.anime) <- FALSE    # %
      enabled(single.recomp.result) <- FALSE
      enabled(single.seasonal) <- FALSE
      enabled(single.forecast) <- FALSE
      enabled(several.label) <- TRUE
      enabled(several.compare.series) <- TRUE
      enabled(several.compare.series2) <- TRUE
    } else if (n == 1) {
      enabled(single.label) <- TRUE
      #enabled(single.tsplot.animated) <- TRUE
      enabled(start.anime) <- TRUE  # %
      enabled(skip.anime) <- FALSE  # %
      enabled(single.tsplot) <- TRUE
      enabled(single.decompose) <- TRUE
      enabled(recompose.start.anime) <- allow.recompose   # %
      #enabled(single.recompose) <- allow.recompose
      enabled(single.recomp.result) <- allow.recompose
      enabled(single.seasonal) <- show.annual
      enabled(single.forecast) <- show.annual
      enabled(several.label) <- FALSE
      enabled(several.compare.series) <- FALSE
      enabled(several.compare.series2) <- FALSE
    } else {
      enabled(single.label) <- FALSE
      #enabled(single.tsplot.animated) <- FALSE
      enabled(start.anime) <- FALSE # %
      enabled(skip.anime) <- FALSE  # %
      enabled(single.tsplot) <- FALSE
      enabled(single.decompose) <- FALSE
      #enabled(single.recompose) <- FALSE
      enabled(recompose.start.anime) <- FALSE   # %
      enabled(recompose.skip.anime) <- FALSE    # %
      enabled(single.recomp.result) <- FALSE
      enabled(single.seasonal) <- FALSE
      enabled(single.forecast) <- FALSE
      enabled(several.label) <- FALSE
      enabled(several.compare.series) <- FALSE
      enabled(several.compare.series2) <- FALSE
    }
  }
  
  # Each time that the gtable is clicked, check how many
  # of the elements are selected so that we can enable the
  # correct buttons
  addHandlerSelectionChanged(tsVarselect, handler = multiple.select.handler)
  multiple.select.handler()
}




valid.ts.info <- function(ts.info) {
  valid <- TRUE
  
  # If we haven't set a time variable, inform the user
  if (any(is.na(ts.info$start))) {
    valid <- FALSE
    gmessage("Please set a valid time variable", title = "Error", icon = "error")
  }
  
  valid
}

valid.vars <- function(var.df) {
  var.classes <- lapply(var.df, class)
  # Only allow numeric values
  valid <- all(var.classes %in% c("numeric", "integer", "double"))
  if (! valid) {
    gmessage("A data variable is not numeric, no categorical variables are allowed.", title = "Error", icon = "error")
  }
  valid
}


createTSInfo <- function() {
  tsconvwin <- gwindow("Provide Time Series Information")
  tsgroup <- ggroup(horizontal = FALSE, container = tsconvwin)
  tsvar.layout <- glayout(container = tsgroup)
  tsvar.layout[1, 1, expand = FALSE] <- (tsStart1.lab <- glabel("Start Date"))
  tsvar.layout[1, 2, expand = TRUE] <- (tsVarStart1 <- gedit(width = 30, height = 20))
  tsvar.layout[2, 1, expand = FALSE] <- (tsStart2.lab <- glabel("Season Number"))
  tsvar.layout[2, 2, expand = TRUE] <- (tsVarStart2 <- gedit(width = 30, height = 20))
  tsvar.layout[3, 1, expand = FALSE] <- (tsFrequency.lab <- glabel("Frequency"))
  tsvar.layout[3, 2, expand = TRUE] <- (tsVarFrequency <- gedit(width = 30, height = 20))
  saveButton <- gbutton("Provide TS Info", container = tsgroup,
                        handler = function(h, ...) {
                          valid.params <- logical(3)
                          valid.ts.args <- function(arg) {
                            suppressWarnings(tmp <- as.integer(arg))
                            ! is.na(tmp) && length(tmp) > 0
                          }
                          # Start Part 1 - starting value of the series (could be years)
                          start.part1 <- svalue(tsVarStart1)
                          valid.params[1] <- valid.ts.args(start.part1)
                          # Start Part 2 - how far through the cycle of the first value we are
                          start.part2 <- svalue(tsVarStart2)
                          valid.params[2] <- valid.ts.args(start.part2)
                          # Frequency - how many values does it take for us to increment "Start Part 1"
                          frequency <- svalue(tsVarFrequency)
                          valid.params[3] <- valid.ts.args(frequency)
                          if (! all(valid.params)) {
                            gmessage("One of your time series parameters is not an integer", icon = "error")
                            return()
                          }
                          # Now we know all params are valid, cast to ints
                          start.part1 <- as.integer(start.part1)
                          start.part2 <- as.integer(start.part2)
                          frequency <- as.integer(frequency)
                          tsStructure <- list(start = c(start.part1, start.part2),
                                                frequency = frequency)
                          # Get rid of the window for now
                          dispose(tsconvwin)
                        })
}


forecastWindow <- function(forecastoutcome) {
  
  pw <- gwindow(title = "Forecast Output", width = 600, height = 400)
  pg <- ggroup(horizontal = FALSE, use.scrollwindow = TRUE, container = pw)
  predtext <- gtext("", font.attr = list(family = "monospace"), wrap = FALSE,
                    expand = TRUE, container = pg)
  printed.text <- capture.output(print(forecastoutcome))
  insert(predtext, printed.text)
}

get.ts.structure <- iNZightTS:::get.ts.structure

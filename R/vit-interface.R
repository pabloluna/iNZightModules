## iNZightVIT (gWidgets2)

iNZightVIT <- function(disposeR = TRUE) {
  e <- new.vit.env()
  e$disposeR <- disposeR
  e$homefun <- iNZightVIT
  
  
  permvar.handler <- function(){
    dispose(home)
    graphics.off()
    newdevice(height = 7.5, width = 11.25)
    plot.new()
    e$title <- "Randomisation variation"
    setupGUI(e)
    permvarGUIHandler(e)
    dataGUI(e)
  }
  permutation.handler <- function(){
    dispose(home)
    graphics.off()
    newdevice(height = 7.5, width = 11.25)
    plot.new()
    e$title <- "Randomisation tests"
    setupGUI(e)
    permGUIHandler(e)
    dataGUI(e)
  }
  sampvar.handler <- function() {
    dispose(home)
    graphics.off()
    newdevice(height = 7.5, width = 7.5)
    plot.new()
    e$title <- "Sampling variation"
    setupGUI(e)
    sampvarGUIHandler(e)
    dataGUI(e)
  }
  bootstrap.handler <- function(){
    dispose(home)
    graphics.off()
    newdevice(height = 7.5, width = 11.25)
    plot.new()
    e$title <- "Bootstrap confidence intervals"
    setupGUI(e)
    bootstrapGUIHandler(e)
    dataGUI(e)
  }
  confint.handler <- function(){
    dispose(home)
    graphics.off()
    newdevice(height = 7.5, width = 7.5)
    e$title <- "Confidence interval coverage"
    setupGUI(e)
    CIGUIHandler(e)
    dataGUI(e)
  }
  ## Generates home page
  home <- gwindow("The iNZight and Visual Inference Tool Systems", visible = TRUE)
  
  
  # We want to just quit R any time the home screen is closed (but only when iNZight hasn't been loaded)
  addHandlerUnrealize(home, handler = function(h, ...) {
    if (e$disposeR) {
      e$confirmDialog("Do you wish to quit iNZightVIT?",
                      handler = function(...) {
                        q(save = "no")
                      })
    } else {
      dispose(home)
      dev.off()
    }
    return(TRUE)
  })
  
  
  group <- ggroup(horizontal = FALSE, container = home)
  title <- glabel("The iNZight and VIT Systems", container = group)
  addSpace(group, 10, horizontal = FALSE)
  
  
  inzight.group <- ggroup(horizontal = FALSE, container = group)
  
  
  have.inzight <- "iNZight" %in% rownames(installed.packages())
  if (have.inzight) {
    datestamp <- strsplit(packageDescription("iNZight")$Version, "-")[[1]][2]
    inz.frame.title <- paste("iNZight", " (v", datestamp, ")", sep = "")
  } else {
    inz.frame.title <- "iNZight (not installed)"
  }
  
  
  inzight.frame <- gframe(inz.frame.title, container = inzight.group, horizontal = FALSE)
  inzight.banner <- gimage(system.file("images/inzight-banner.png", package = "vit"),
                           handler = function(h, ...) browseURL("http://www.stat.auckland.ac.nz/~wild/iNZight/"))
  add(inzight.frame, inzight.banner)
  addSpace(inzight.frame, 5, horizontal = FALSE)
  inzight.title <- glabel("iNZight is a program for analysing data")
  add(inzight.frame, inzight.title)
  home.inzight <- gbutton("Run iNZight", expand = TRUE,
                          handler = function(h, ...) {
                            # We first check presence of the iNZight package. The idea is so that we have
                            # a reasonable idea of whether we should provide a splash screen when loading
                            # iNZight (as it takes a while...)
                            if ("iNZight" %in% rownames(installed.packages())) {
                              lib.png <- suppressWarnings(require(png))
                              if (lib.png) {
                                iNZight.splash <- png::readPNG(system.file("images", "inzight-splash.png",
                                                                           package = "vit"), exists("rasterImage"))
                                dev.new(width = 3.5, height = 2)
                                grid.newpage()
                                grid.raster(iNZight.splash, width = unit(3.5, "inches"), height = unit(2, "inches"))
                              }
                              suppressPackageStartupMessages({
                                suppressWarnings(library(iNZight))
                              })
                              if (lib.png)
                                dev.off()
                              e$disposeR <- FALSE
                              iNZight::iNZight()
                            } else {
                              gmessage("You must install the iNZight package to run iNZight.",
                                       title = "Error - iNZight required", icon = "error")
                            }
                          })
  enabled(home.inzight) <- have.inzight
  add(inzight.frame, home.inzight)
  addSpace(group, 10, horizontal = FALSE)
  
  
  vit.group <- ggroup(horizontal = FALSE, container = group)
  
  
  datestamp <- strsplit(packageDescription("vit")$Version, "-")[[1]][2]
  vit.frame.title <- paste("Visual Inference Tools", " (v", datestamp, ")", sep = "")
  vit.frame <- gframe(vit.frame.title, container = vit.group, horizontal = FALSE)
  vit.banner <- gimage(system.file("images/vit-banner.png", package = "vit"),
                       handler = function(h, ...) browseURL("http://www.stat.auckland.ac.nz/~wild/VIT/"))
  add(vit.frame, vit.banner)
  addSpace(vit.frame, 5, horizontal = FALSE)
  vit.title <- glabel("Visual Inference Tools (VIT) contains programs for developing concepts")
  add(vit.frame, vit.title)
  vit.functions <- list(permvar.handler,
                        permutation.handler,
                        sampvar.handler,
                        bootstrap.handler,
                        confint.handler)
  vit.modules <- c("Randomisation variation",
                   "Randomisation tests",
                   "Sampling variation",
                   "Bootstrap confidence interval construction",
                   "Confidence interval coverage")
  vit.radio <- gradio(vit.modules, horizontal = FALSE)
  add(vit.frame, vit.radio)
  vit.run <- gbutton("Run selected VIT module", expand = TRUE,
                     handler = function(h, ...){
                       vit.functions[[which(vit.modules == svalue(vit.radio))]]()
                     })
  add(vit.frame, vit.run)
  addSpace(vit.group, 10, horizontal = FALSE)
  logo <- gimage(system.file("images/uoa-logo.png", package = "vit"),
                 handler = function(h, ...) browseURL("http://www.stat.auckland.ac.nz/"))
  add(group, logo)
}

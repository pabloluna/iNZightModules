# This is a module used for graphical multiple response analysis.
multipleResponseWindow <- function(e) {
    mrWin <- gwindow(title = "Multiple Response Analysis",
                     width = 600,
                     expand = TRUE)
    ## Pull out the existing dataset from the main window and assign
    ## as a copy
    tag(mrWin, "dataSet") <- tag(e$obj, "dataSet")
    tag(mrWin, "mrObjects") <- list()
    tag(mrWin, "mrNames") <- list()
    mrExists <- function() {
        if (! length(tag(mrWin, "mrObjects"))) {
            gmessage("A multiple response object must first be created")
            FALSE
        } else {
            TRUE
        }
    }
    codeHistory <- "library(iNZightMR)"
  
    mainGroup <- ggroup(horizontal = FALSE, expand = TRUE, container = mrWin)
    mainLayout <- glayout(container = mainGroup)
    mainLayout[1, 1, expand = TRUE] <-
        (topLayout <- glayout(container = mainLayout))
    mainLayout[2, 1, expand = TRUE] <-
        (bottomLayout <- glayout(container = mainLayout))
      
    title <- glabel("Make Multiple Response Object")
    font(title) <- list(weight = "bold")
    topLayout[1, 1:2, anchor = c(-1,-1),expand=TRUE] <- title
    chooseVarsTitle <- glabel("Choose Variables")
    font(chooseVarsTitle) <- list(weight = "bold")
    topLayout[1, 3] <- chooseVarsTitle
      
    topLayout[2:6, 3, expand = TRUE] <-
        (varSelect <- gtable(names(tag(mrWin, "dataSet")), multiple = TRUE))
    topLayout[2, 1:2,anchor = c(-1,-1),expand=TRUE] <-
        glabel("Type the name for a new MR object")
    topLayout[4, 1] <- (mrName <- gedit(initial.text = "mro1"))
    topLayout[4, 2] <-
        gbutton("Create MR object",
                handler = function(h, ...) {
                    objs <- tag(mrWin, "mrObjects")
                    if (! nzchar(svalue(mrName)))
                        gmessage("A name must first be given to create an object")
                    if (length(svalue(varSelect)) < 1)
                        gmessage("At least one variable must be selected")
                    
                    varPieces <- paste(svalue(varSelect), collapse = " + ")
                    ## a pre test is adding here through the UI system
                    pre.frm <-as.formula(paste("~", varPieces)) 
                    test.frm <- model.frame(pre.frm,tag(mrWin, "dataSet"))
                    test.result <- any(sapply(lapply(test.frm,nlevels),"[",1)>2)
                    test.index <- which(sapply(lapply(test.frm,nlevels),"[",1)>2)
                    if(test.result)
                        gmessage(paste("the",names(test.frm)[test.index[1]],
                                       "make the process down!"))
                    ## end note:only give the first problem variable name here...
                    frm <- as.formula(paste(svalue(mrName), "~", varPieces))
                    objs[[svalue(mrName)]] <- mro2(frm, data = tag(mrWin, "dataSet"))
                    tag(mrWin, "mrObjects") <- objs
                    mrSelect[] <- c(svalue(mrName), mrSelect[])
                    if(length(mrSelect)>1)
                        enabled(breakOutSelect)<-TRUE 
                    svalue(mrSelect) <- svalue(mrName)
                    
                    tag(mrWin, "mrNames") <- svalue(varSelect)
                })
    topLayout[5, 1:2,anchor = c(-1,-1),expand=TRUE] <-
        glabel("Choose multiple response object")
    mrSelect <- gcombobox("   ",editable = TRUE)
    topLayout[6, 1:2] <- mrSelect
  
    topLayout[7, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Alone")
    topLayout[8, 1, expand = TRUE] <-
        gbutton(" Plot", handler = function(h, ...) {
            if (! mrExists())
                return()
            MR <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
            dev.new()
            barplot(mroPara(MR))
        })
    topLayout[8, 2, expand = TRUE] <-
        gbutton("Summary", handler = function(h, ...) {
            if (! mrExists())
                return()
            currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
            mrSummaryWindow(capture.output(summary(mroPara(currentObj))))
        })
    topLayout[8, 3, expand = TRUE] <-
        gbutton("Combinations", handler = function(h, ...) {
            if (! mrExists())
                return()
            currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
            dev.new()
            mrSummaryWindow(capture.output(plotcombn(currentObj)))
        })
    topLayout[9, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Broken out by:")
    
    (breakOutSelect <-
     gcombobox(c("   ",names(tag(mrWin, "dataSet"))),selected=0,
               editable = FALSE,
               handler = function(h, ...) {
                   subsetVar <- tag(mrWin, "dataSet")[[svalue(breakOutSelect)]]
                   subsetSlider[] <-
                       if (is.numeric(subsetVar))
                           0
                       else
                           0:length(levels(subsetVar))
                   if(svalue(breakOutSelect)==""){
                       enabled(subsetSlider) <- FALSE
                       svalue(breakOutSelect2)<-""
                       svalue(subsetSlider)<-0
                       enabled(breakOutSelect2) <- FALSE
                   }
                   else{
                       enabled(subsetSlider) <- TRUE
                       enabled(breakOutSelect2)<-TRUE
                   }
               }))
    enabled(breakOutSelect)<-FALSE
    topLayout[9, 2:3, expand = TRUE] <- breakOutSelect
    topLayout[10, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Filtered by:")
    topLayout[10, 2:3, expand = TRUE] <-
        (subsetSlider <- gslider(from = 0, to = 1, by = 1))
    enabled(subsetSlider) <- FALSE
    topLayout[11, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Broken out by:")
    topLayout[11, 2:3, expand = TRUE] <-
        (breakOutSelect2 <-
         gcombobox(c("   ",names(tag(mrWin, "dataSet"))),selected=0,
                   editable = FALSE,
                   handler = function(h, ...) {
                       subsetVar2 <- tag(mrWin, "dataSet")[[svalue(breakOutSelect2)]]
                       subsetSlider2[] <-
                           if (is.numeric(subsetVar2))
                               0
                           else
                               0:length(levels(subsetVar2))
                       if(svalue(breakOutSelect2)==""){
                           enabled(subsetSlider2) <- FALSE
                           svalue(subsetSlider2)<-0
                           
                       }
                       else{
                           enabled(subsetSlider2) <- TRUE
                           if(svalue(breakOutSelect2)==svalue(breakOutSelect)){
                               gmessage("STOP!You are trying to use the same variable in both subsetting slot")
                               svalue(breakOutSelect2)<-""
                               enabled(breakOutSelect2)<-TRUE
                           }
                       }
                   }))
    enabled(breakOutSelect2)<-FALSE
    topLayout[12, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Filtered by:")
    topLayout[12, 2:3, expand = TRUE] <-
        (subsetSlider2 <- gslider(from = 0, to = 1, by = 1))
    enabled(subsetSlider2) <- FALSE
    
    topLayout[13, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Profiles by group")
    topLayout[14, 1, expand = TRUE] <-
        gbutton(" Plot", handler = function(h, ...) {
            if (! mrExists())
                return()
            currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
            dev.new()
            barchart4(by(currentObj,
                         as.formula(paste("~", svalue(breakOutSelect))),
                         mroPara))
        })
    topLayout[14, 2, expand = TRUE] <-
        gbutton("Summary", handler = function(h, ...) {
            if (! mrExists())
                return()
            currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
            txt <- capture.output(summary(by(currentObj,
                                             as.formula(paste("~", svalue(breakOutSelect))),
                                             mroPara), "within"))
            mrSummaryWindow(txt)
        })
    
    topLayout[14, 3, expand = TRUE] <-
        gbutton("Combinations", handler = function(h, ...) {
            gmessage("Not yet supported")
            return()
            if (! mrExists())
                return()
            currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
            browser()
            mrSummaryWindow(capture.output(plotcombn(by(currentObj,
                                                        as.formula(paste("~",
                                                                         svalue(breakOutSelect))),
                                                        mroPara))
                                           ))
        })
    bottomLayout[1, 1:2, anchor = c(-1,-1),expand=TRUE] <- glabel("Compare groups")
    bottomLayout[2, 1, expand = TRUE] <-
        gbutton(" Plot", handler = function(h, ...) {
            if (! mrExists())
                return()
            dev.new()
            currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
            currSubset <- as.numeric(svalue(subsetSlider))
            ##if (currSubset != 0)
            ##    barplot(between(by(currentObj,
            3#                       as.formula(paste("~", svalue(breakOutSelect))),
            ##                       mroPara)))
            ##else
            barplot.between(between(by(currentObj,
                                       as.formula(paste("~", svalue(breakOutSelect))),
                                       mroPara)))
        })
    bottomLayout[2, 2, expand = TRUE] <-
        gbutton("Summary", handler = function(h, ...) {
            if (! mrExists())
                return()
            currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
            mybymro <- by(currentObj,
                          as.formula(paste("~", svalue(breakOutSelect))),
                          mroPara)
            mrSummaryWindow(capture.output(summary(between(mybymro), mybymro)))
        })
    ## what might need in the future
    ##tbl <- list()
    ##aRenew <- gaction(label="Renew",icon="refresh",handler=function(...) print("Renew"))
    ##aHistory<- gaction(label="History",icon="print",handler=function(...) print("History"))
    ##tbl$Renew <- aRenew
    ##tbl$History <- aHistory
    ##tb <- gtoolbar(tbl,style="text",container=mainGroup)
    sb <- gstatusbar("Showing the right now process, such as 'mranalysis(online~gender+age)'",
                     container = mainGroup)
}


mrSummaryWindow <- function(text) {
    text <- paste0(text, collapse = "\n")
    sumWin <- gwindow("Summary Output", width = 800, height = 800)
    gtext(text, font.attr = c(family = "monospace"), container = sumWin)
}

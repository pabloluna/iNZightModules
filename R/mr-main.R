
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
      gmessage("A multiple response object must first be created", parent = mrWin)
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
              
              # check the duplicated name here first...
              # block the same name 
              inputMROname <- svalue(mrName)
              if ( inputMROname %in% mrSelect[])
                inputMROname <- paste0(svalue(mrName), ".2")
              svalue(mrName) <- inputMROname
              
              #print(svalue(mrName))
              
              objs <- tag(mrWin, "mrObjects")
              if (! nzchar(svalue(mrName)))
                gmessage("A name must first be given to create an object", parent = mrWin)
              if (length(svalue(varSelect)) < 1)
                gmessage("At least one variable must be selected", parent = mrWin)
              
              varPieces <- paste(svalue(varSelect), collapse = " + ")
              ## a pre test is adding here through the UI system
              if (length(varPieces) == 0)  #%
                return() #%
              pre.frm <-as.formula(paste("~", varPieces)) 
              test.frm <- model.frame(pre.frm,tag(mrWin, "dataSet"))
              test.result <- any(sapply(lapply(test.frm,nlevels),"[",1)>2)
              test.index <- which(sapply(lapply(test.frm,nlevels),"[",1)>2)
              if(test.result)
                gmessage(paste("the",names(test.frm)[test.index[1]],
                               "make the process down!"), parent = mrWin)
              ## end note:only give the first problem variable name here...
              #if (length(svalue(mrName))==0)  #%
              #  return() #%
              frm <- as.formula(paste(inputMROname, "~", varPieces))
              #frm <- as.formula( "~", varPieces)
              objs[[inputMROname]] <- try(iNZightMR(frm, data = tag(mrWin, "dataSet")), silent = FALSE)
              if (inherits(objs[[inputMROname]], "try-error")){
                ttt <- unclass(objs[[inputMROname]])
                gmessage(ttt)
                return()
              }
              
              tag(mrWin, "mrObjects")[[inputMROname]] <- objs[[inputMROname]]
              
              mrSelect[] <- c(inputMROname, mrSelect[])
              if(length(mrSelect)>1)
                enabled(breakOutSelect)<-TRUE 
              svalue(mrSelect) <- inputMROname
              
              
              #print(str(tag(mrWin, "mro")))
              tag(mrWin, "mrNames") <- svalue(varSelect)
            })
  topLayout[5, 1:2,anchor = c(-1,-1),expand=TRUE] <-
    glabel("Choose multiple response object")
  mrSelect <- gcombobox(" ", editable = TRUE)
  topLayout[6, 1:2] <- mrSelect
  
  topLayout[7, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Alone")
  topLayout[8, 1, expand = TRUE] <-
    gbutton(" Plot", handler = function(h, ...) {
      if (! mrExists())
        return()
      MR <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
      dev.new()
      par(mfrow = c(1,1), oma = rep(0,4), mar = c(5.1, 4.1, 4.1, 2.1))
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
     gcombobox(c(" ",names(tag(mrWin, "dataSet"))),selected=0,
               editable = FALSE))
  
  addHandlerChanged(breakOutSelect, handler = function(h, ...) {
    subsetVar <- iNZightPlots::convert.to.factor(tag(mrWin, "dataSet")[[svalue(breakOutSelect)]])
    
    
    if(svalue(breakOutSelect)==" "){
      
      
      svalue(breakOutSelect2) <- " "
      subsetSlider[] <- 0:1
      svalue(subsetSlider) <- 0
      enabled(subsetSlider) <- FALSE
      enabled(breakOutSelect2) <- FALSE
    }
    else{
      enabled(subsetSlider) <- TRUE
      subsetSlider[] <-
        if (svalue(breakOutSelect) == " " || is.numeric(subsetVar))
          0:1
      else
        0:nlevels(subsetVar)
      enabled(breakOutSelect2)<-TRUE
    }
  })
  enabled(breakOutSelect)<-FALSE
  topLayout[9, 2:3, expand = TRUE] <- breakOutSelect
  topLayout[10, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Filtered by:")
  topLayout[10, 2:3, expand = TRUE] <-
    (subsetSlider <- gslider(from = 0, to = 1, by = 1))
  enabled(subsetSlider) <- FALSE
  topLayout[11, 1, anchor = c(-1,-1),expand=TRUE] <- glabel("Broken out by:")
  topLayout[11, 2:3, expand = TRUE] <-
    (breakOutSelect2 <-
       gcombobox(c(" ",names(tag(mrWin, "dataSet"))),selected=0,
                 editable = FALSE))
  addHandlerChanged(breakOutSelect2,  handler = function(h, ...) {
    subsetVar2 <- iNZightPlots::convert.to.factor(tag(mrWin, "dataSet")[[svalue(breakOutSelect2)]])
    
    #subsetSlider2[] <- seq(0, nlevels(subsetVar2))
    if(svalue(breakOutSelect2)==" " || length(svalue(breakOutSelect2)) == 0){
      #print(1)
      svalue(subsetSlider2) <- 0
      subsetSlider2[] <- 0:1
      # if (svalue(breakOutSelect2) == " ")
      #    0:1
      #else
      #  0:nlevels(subsetVar2)
      enabled(subsetSlider2) <- FALSE
      
      
    }
    else{
      enabled(subsetSlider2) <- TRUE
      subsetSlider2[] <- 
        if (svalue(breakOutSelect2) == " ")
          0:1
      else
        0:nlevels(subsetVar2)
      if(svalue(breakOutSelect2)==svalue(breakOutSelect)){
        gmessage("STOP!You are trying to use the same variable in both subsetting slot", parent = mrWin)
        svalue(breakOutSelect2)<-" "
        enabled(breakOutSelect2)<-TRUE
      }
    }
  })
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
      vars <- svalue(breakOutSelect)
      
      
      
      if (length(svalue(breakOutSelect2))!=0 && svalue(breakOutSelect2) != " ")
        vars <- paste(vars, svalue(breakOutSelect2), sep = "+")
      
      test1 <<- vars
      test2 <<- currentObj
      
      sub1 <- NULL
      if (svalue(subsetSlider)>0)
        sub1 <- svalue(subsetSlider)
      
      sub2 <- NULL
      !enabled(subsetSlider2)
      if (svalue(subsetSlider2)>0)
        sub2 <- svalue(subsetSlider2)
      
      
      if ( length(vars) != 0 && vars != " ") {
        by.formula <- as.formula(paste("~", vars))
        OBJ = byMRO(currentObj, by.formula, mroPara)
        
        if (!is.null(sub1)){
          sub1 <- dimnames(OBJ)[[1]][sub1]
        }
        
        if (is.null(sub2)){
          sub2 <- "_MULTI"
        }
        else {
          sub2 <- dimnames(OBJ)[[2]][sub2]
        } 
        
        barplot(OBJ, g1.level = sub1, g2.level = sub2)
      }
      else{
        gmessage("Pick up at least one subset variable first", parent = mrWin)
        
      }
      
    })
  topLayout[14, 2, expand = TRUE] <-
    gbutton("Summary", handler = function(h, ...) {
      if (! mrExists())
        return()
      
      currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
      if ( length(svalue(breakOutSelect)) != 0 && svalue(breakOutSelect) != " ") {
        
        vars <- svalue(breakOutSelect)
        
        if (length(svalue(breakOutSelect2)) != 0 && svalue(breakOutSelect2) != " ")
          vars <- paste(vars, svalue(breakOutSelect2), sep = "+")
        
        by.formula <- as.formula(paste("~", vars))
        
        
        txt <- capture.output(summary(byMRO(currentObj,
                                         by.formula,
                                         mroPara), "within"))
        mrSummaryWindow(txt)
      }
      else{
        gmessage("Pick up at least one subset variable first", parent = mrWin)
        
      }
      
      
    })
  
  topLayout[14, 3, expand = TRUE] <-
    gbutton("Combinations", handler = function(h, ...) {
      gmessage("Not yet supported", parent = mrWin)
      return()
      if (! mrExists())
        return()
      currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
      mrSummaryWindow(capture.output(plotcombn(byMRO(currentObj,
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
      
      vars <- svalue(breakOutSelect)
      
      currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
      
      numOfExtVar = 1
      if (length(svalue(breakOutSelect2))!=0 && svalue(breakOutSelect2) != " "){
        vars <- paste(vars, svalue(breakOutSelect2), sep = "+")
        numOfExtVar = numOfExtVar + 1
      }
      
      test1 <<- vars
      test2 <<- currentObj
      
      sub1 <- NULL
      if (svalue(subsetSlider)>0)
        sub1 <- svalue(subsetSlider)
      
      sub2 <- NULL
      !enabled(subsetSlider2)
      if (svalue(subsetSlider2)>0)
        sub2 <- svalue(subsetSlider2)
      
      
      if ( length(vars) != 0 && vars != " ") {
        by.formula <- as.formula(paste("~", vars))
        OBJ = byMRO(currentObj, by.formula, mroPara)
        

        if (!is.null(sub2)){

          sub2 <- dimnames(OBJ)[[2]][sub2]

        } 
        
        if (numOfExtVar>1)
          barplot(between(OBJ), g1.level = sub2)
        else
          barplot(between(OBJ))
      }
      
      
      else{
        gmessage("Pick up at least one subset variable first", parent = mrWin)
        
      }
      
    })
  bottomLayout[2, 2, expand = TRUE] <-
    gbutton("Summary", handler = function(h, ...) {
      if (! mrExists())
        return()
      currentObj <- tag(mrWin, "mrObjects")[[svalue(mrSelect)]]
      
      if ( length(svalue(breakOutSelect)) != 0 && svalue(breakOutSelect) != " ") {
        
        vars <- svalue(breakOutSelect)
        
        if (length(svalue(breakOutSelect2)) != 0 && svalue(breakOutSelect2) != " ")
          vars <- paste(vars, svalue(breakOutSelect2), sep = "+")
        
        by.formula <- as.formula(paste("~", vars))
        
        mybymro <- byMRO(currentObj,
                      by.formula ,
                      mroPara)
        
        
        Itest <<- mybymro
        mrSummaryWindow(capture.output(summary(mybymro, "between")))
      }
      else{
        gmessage("Pick up at least one subset variable first", parent = mrWin)
        
      }
    })
  ## what might need in the future
  ##tbl <- list()
  ##aRenew <- gaction(label="Renew",icon="refresh",handler=function(...) print("Renew"))
  ##aHistory<- gaction(label="History",icon="print",handler=function(...) print("History"))
  ##tbl$Renew <- aRenew
  ##tbl$History <- aHistory
  ##tb <- gtoolbar(tbl,style="text",container=mainGroup)
  
}


mrSummaryWindow <- function(text) {
  text <- paste0(text, collapse = "\n")
  sumWin <- gwindow("Summary Output", width = 800, height = 800)
  gtext(text, font.attr = list(family = "monospace"), container = sumWin)
}

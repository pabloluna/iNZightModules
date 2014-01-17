fit.model <-
  function(y, x, data, family = 'gaussian',
           design = 'simple', svydes = NA, ...) {
    
    ##################################################################
    # This function takes input from iNZight software, and perpares
    # it for iNZightRegression package.
    # y            character string representing the response,
    # x            character string of the explanatory variables,
    # family       gaussian, binomial, poisson (so far, no others
    #              will be added)
    # design       data design specification. one of 'simple',
    #              'survey' or 'experiment'
    # data         name of the object containing the data.
    # svydes       a vector of arguments to be passed to the svydesign
    #              function, excluding data (defined above)
    # ...          further arguments to be passed to lm, glm, svyglm,
    #              such as offset, etc.
    #
    # Value: a fitted model object, either an lm, glm, or svyglm,
    # depending on the design and family of the model.
    #
    # Details: 
    ##################################################################
    
    
    Formula <- paste(y, x, sep = ' ~ ')
    dat <- paste("data", data, sep = ' = ')
    fam <- paste("family", family, sep = ' = ')
    
    # Deal with extra arguments (eg. weights, offset ...)
    xarg <- list(...)
    xargs <- paste(names(xarg), xarg, sep = ' = ', collapse = ', ')
    
    if (design == 'simple') {
      ## simple IID data:
      if (family == 'gaussian') {
        ## Simple linear regression model:
        args <- paste(Formula, dat, sep = ', ')
        if (xargs != "")
          args <- paste(args, xargs, sep = ', ')
        call <- paste('lm(', args, ')', sep = '')
      } else {
        ## general linear model:
        #args <- paste(Formula, ",", fam, xargs)
        args <- paste(Formula, dat, fam, sep = ', ')
        if (xargs != "")
          args <- paste(args, xargs, sep = ', ')
        call <- paste('glm(', args, ')', sep = '')
      }
    } else if (design == 'survey') {
      ## complex survey design:
      
      
      # set up the survey design
      #svy.des <- paste('svydesign(',
      #                 paste(svydes, collapse = ', '),
      #                 ', data = mydata)', sep = '')
      
      #svy.design <<- eval(parse(text = svy.des))
      
      # set up the svyglm function call
      args <- paste(Formula, fam, "design = svy.design", sep = ', ')
      if (xargs != "")
        args <- paste(args, xargs, sep = ', ')
      call <- paste('svyglm(', args, ')', sep = '')
    } else if (design == 'experiment') {
      ## experimental design:
      stop('Experiments are not yet implemented. \n')
    }
    
    
    
    # save the call to the history, and return 
    #histScript <<- c(histScript, call)  # this *will* change
    #fit <- cHist(call)
    
    # at this stage we just return the call
    call
    
  }

fit.design <- function(svydes, dataset.name) {
  if(all(svydes==""))
    return()
  svy.des <- paste0('svydesign(',
                    paste(svydes, collapse = ', '),
                    ', data = ', dataset.name, ')') 
  
  eval(parse(text = svy.des), .GlobalEnv)
}

square <- function(x) x^2

root <- function(x, k) x^(1/k) 

addHist <-
  function(txt) {
    # This simple function adds text (a function call)
    # to the history object, and returns the call.
    
    e$hist <<- c(e$hist, txt)
    txt
  }


cHist <-
  function(txt) {
    # A wrapper for addHist if the call is already made,
    # and simply needs to be added to the history.
    
    eval(parse(text = addHist(txt)))
  }

as.catergory <- function(x, ...) 
  factor(x, ...)


modelFitting = function(e) {
  
  mydata <- tag(e$obj, "dataSet")
  classset <- sapply(mydata, class)
  numset <- Filter(is.numeric, mydata)
  factorset <- Filter(is.factor, mydata)
  test.numset <- names(numset)
  test.factorset <- lapply(factorset, levels)
  
  varnames <- names(tag(e$obj, "dataSet"))
  if (length(varnames) == 1 && varnames == "empty") {
    gmessage(msg = "A dataset is required to use the model fitting module", title = "No data", icon = "error")
    return()
  }
  
  modellingWinOpened <- TRUE
  modellingWin <- gwindow("Model Fitting", height = 800, 
                          width = 815)
  tag(modellingWin, "dataSet") <- tag(e$obj, "dataSet")
  
  addHandlerUnrealize(modellingWin, handler = function(h,...) {
    modellingWinOpened <- FALSE
    dispose(modellingWin)
  })
  
  # control the data in the environment
  # sort of temp file and save in the global environment 
  code.history <- "library(iNZightRegression)"
  
  ## here is some possible solution to handle if there is something mydata in the global environment
  if (exists("mydata", .GlobalEnv))
    dataset.name <- "mydata.new"
  else
    dataset.name <- "mydata"
  
  assign(dataset.name, tag(modellingWin, "dataSet"), envir = globalenv())
  mainGp <- gvbox(container = modellingWin,use.scrollwindow = TRUE)
  
  
  title <- glabel("Model Response (Y) using Variables of Interest and Confounders",cont=mainGp)
  font(title) <- list(weight = "bold",size=15)
  
  
  #resp.label <- glabel("Variable")
  #font(resp.label) <- list(weight = "bold")
  paned <- gpanedgroup(cont = mainGp)
  lgroup <- ggroup(cont=paned,horizontal=FALSE,expand=TRUE)
  original.label <- NULL
  response.varlist.panel <- gframe("Variables",cont=lgroup)
  # new gtable is coming from here
  ## find icons by class
  myClass <- function(df) {
    df[,2] <- as.character(df[,2])
    df[,2][df[,2] %in% c("numeric","integer")] <- "Num"
    df[,2][df[,2] %in% c("character", "factor")] <- "Cat"
    df[,2][df[,2] %in% c("logical", "Complex")] <- "Unknown"
    original.label <<- df[,2]
    df
  }
  
  icon.FUN <- function(items) {
    
    raw = as.character(items[,2,drop=TRUE])
    raw[raw %in% "Num"] <- "gw-numeric"  # always numeric
    raw[raw %in% "Cat"] <- "gw-factor"
    raw[raw %in% "Unknown"] <- "gw-error"
    raw
  }
  
  m <- names(mydata)
  type <- sapply(mydata, class)
  response.type.df = data.frame(Name=m, Type=type)
  response.type.df[,1] <- as.character(response.type.df[,1])
  response.type.df <- myClass(response.type.df)
  response.varlist <- gtable(response.type.df, icon.FUN=icon.FUN, 
                             container=response.varlist.panel, expand=TRUE,
                             multiple=TRUE)
  ## add handlers
  addHandlerSelectionChanged(response.varlist, handler = function(h,...) {
    #print(svalue(h$obj))
  })
  TableMenulist <- list(
    one = gaction("Numeric", tooltip = "switch to numeric type",icon = "numeric", handler = function(h,...) {
      response.varlist[svalue(response.varlist,index=TRUE),2] <- "Num"
    }),
    two = gaction("Category", tooltip = "switch to catergory type", icon = "factor", handler = function(h,...) {
      response.varlist[svalue(response.varlist,index=TRUE),2] <- "Cat"
    })
  ) 
  add3rdmousePopupMenu(response.varlist, TableMenulist)
  addHandlerSelect(response.varlist, handler = function(h, ...) {
    Class = response.varlist[svalue(response.varlist, index = TRUE),2]
    Var = mydata[,svalue(response.varlist,indxe= TRUE)]
    if (Class == "Cat") 
      Var = as.factor(Var)
    if (Class == "Num")
      Var = as.numeric(Var)
    print(summary(Var))
  })
  # end
  
  # the old response.varlist gtable, only names of the input dataSet
  # response.varlist <- gtable(names(tag(modellingWin, "dataSet")), 
  #                           cont = response.varlist.panel,
  #                           multiple = TRUE, expand = TRUE)
  size(response.varlist) <- c(170, 400)
  addDropSource(response.varlist, handler=function(h,...) svalue(h$obj))
  
  ## not sure to keep this
  ## it would be most useful to use it to calculate weight,
  ## changing some value and see how this change the linear model
  ## or drop observations.... (needed!)
  ## or do subsetting...(needed!!)
  ## or giving a place to let user to type the name of the data frame they want
  ## transforming variables here is ok, but don't too much
  showDF <- gbutton("Show Data",cont=lgroup, handler = function(h,...) {
    mydata.window <- gwindow("showing mydata",parent = modellingWin)
    mydata.frame <- gdf(mydata, cont = mydata.window)
  })
  
  
  main.layout <- glayout(cont = paned)
  y.label <- glabel("Response (Y)")
  main.layout[1, 2] <- y.label
  moving.y <- gcombobox(c("", names(tag(modellingWin, "dataSet"))))
  size(moving.y) <- c(100,20)
  addDropTarget(moving.y, handler = function(h,...) {
    svalue(h$obj) <- h$dropdata  
    ## status bar information
    if (svalue(transform.drop.list) == "NULL")
      svalue(statusbar) <- paste("choosing", h$dropdata, "now")
    else{
      #if (h$dropdata %in% names(test.factorset) 
      #    & 
      #      (svalue(transform.drop.list) == "log"| svalue(transform.drop.list) == "root"))
      #  return(svalue(statusbar) <- paste(h$dropdata,"is factor and does not support numeric transformation"))
      
      extra <- paste(",",svalue(argument.type))
      if (extra==", ")
        extra <- NULL
      
      if (svalue(argument.type)!=""
          & 
            (svalue(transform.drop.list) == "log"))
        return(svalue(statusbar) <- paste("we are not provide extra argument of log so far; 
                                          This may leads to a wrong fitting model."))
      
      svalue(statusbar) <- paste("choosing", 
                                 paste0(svalue(transform.drop.list),"(",h$dropdata,extra,")") 
                                 ,"now")
    }
  })
  addHandlerChanged(moving.y, handler = function(h, ...) {
    ## extra information should provide here to tell user to change factor to integer for poisson..
    if (svalue(moving.y) %in% test.numset)
      svalue(m.f.list) <- "Least Squares"
    
    if (length(test.factorset[[svalue(moving.y)]]) > 2 & classset[svalue(moving.y)] != "numeric" )
      svalue(m.f.list) <- "Poisson regreesion (count data)"
    
    if (length(test.factorset[[svalue(moving.y)]]) == 2)
      svalue(m.f.list) <- "Logistic Regression (Y binary)"
  })
  main.layout[1, 3] <- moving.y
  
  f.w.label <- glabel("Freq.Weights")
  main.layout[2, 2] <- f.w.label
  f.w.list <- gcombobox("                -              ")
  size(f.w.list) <- c(100,20)
  main.layout[2, 3, expand=T] <- f.w.list
  
  p.w.label <- glabel("Presc.Weights")
  main.layout[3, 2] <- p.w.label
  p.w.list <- gcombobox("              -                ")
  size(p.w.list) <- c(100,20)
  main.layout[3, 3] <- p.w.list
  
  t.y.label <- glabel("Transform \n   y")
  t.y.layout <- glayout()
  transform.drop.list <- gcombobox(c("NULL", "log", "sqrt", "^ argument"))
  size(transform.drop.list) <- c(80, 20)
  addHandlerChanged(transform.drop.list, handler = function(h, ...) {
    
    if (svalue(moving.y)=="")
      return(svalue(statusbar) <- paste("notice to choose response before you fit model"))
    else{
      extra <- paste(",",svalue(argument.type))
      if (extra==", ")
        extra <- NULL
      
      if (svalue(argument.type)!=""
          & 
            (svalue(transform.drop.list) == "log"))
        return(svalue(statusbar) <- paste("we are not provide extra argument of log so far; This may leads to a wrong fitting model."))
      
      #if (svalue(moving.y) %in% names(test.factorset) 
      #    & 
      #      (svalue(transform.drop.list) == "log"| svalue(transform.drop.list) == "root"))
      #  return(svalue(statusbar) <- paste(svalue(moving.y),"is factor and does not support numeric transformation"))
      
      if(svalue(h$obj)=="NULL")
        return(svalue(statusbar) <- paste("choosing", svalue(moving.y), "now"))
      
      if (svalue(h$obj)!="NULL")
        svalue(statusbar) <- paste("choosing", 
                                   paste0(svalue(transform.drop.list),"(", svalue(moving.y), extra,")") 
                                   ,"now")
      else
        svalue(statusbar) <- paste("choosing", svalue(moving.y), "now")
    }
  })
  t.y.layout[1, 1] <- transform.drop.list
  
  
  t.y.layout[1, 2] <- glabel("argument")
  argument.type <- gedit()
  size(argument.type) <- c(20, 20)
  t.y.layout[1, 3, expand=FALSE] <- argument.type
  addHandlerChanged(argument.type, handler = function(h, ...) {
    if (svalue(transform.drop.list) == "NULL" & svalue(h$obj)!="")
      return(svalue(statusbar) <- paste("notice NULL does not provide any argument"))
    
    if (svalue(moving.y)=="")
      return(svalue(statusbar) <- paste("notice to choose response before you fit model"))
    else{
      extra <- paste(",",svalue(h$obj))
      if (extra==", ")
        extra <- NULL
      
      if (svalue(h$obj)!=""
          & 
            (svalue(transform.drop.list) == "log"))
        return(svalue(statusbar) <- paste("we are not provide extra argument of log so far; 
                                          This may leads to a wrong fitting model."))
      
      if (svalue(h$obj)=="" & svalue(transform.drop.list) == "^ argument")
        return(svalue(statusbar) <- paste("please provide the argument to control how degree you want"))
      
      if (svalue(transform.drop.list) != "NULL")
        svalue(statusbar) <- paste("choosing", 
                                   paste0(svalue(transform.drop.list),"(", svalue(moving.y), extra,")") 
                                   ,"now")
      else
        svalue(statusbar) <- paste("choosing", svalue(moving.y), "now")
    }
  })
  main.layout[1, 4] <- t.y.label
  main.layout[1, 5] <- t.y.layout
  
  # we place an object "place" here is to fix "glm" to "glm" we do nothing.
  # but we add sth if "lm" to "glm" or delete sth if "glm" to "lm"
  place <- "0"
  m.f.label <- glabel("Modeling \nframework")
  m.f.list <-gcombobox(c("Least Squares","Logistic Regression (Y binary)","Poisson Regression (Y counts)"),
                       handler = function(h, ...) {
                         
                         
                         if ((svalue(h$obj) == "Logistic Regression (Y binary)" | svalue(h$obj) == "Poisson Regression (Y counts)") & place == "0"){ 
                           enabled(tblist$`Normality Checks`$f3) <- FALSE
                           enabled(tblist$`Normality Checks`$f4) <- FALSE
                           add(nonstandard.layout, extraargs.frame)
                           place <<- "1"
                         }
                         if ((svalue(h$obj) == "Logistic Regression (Y binary)" | svalue(h$obj) == "Poisson Regression (Y counts)") & place == "1"){
                           return()
                         }
                         if ( svalue(h$obj) == "Least Squares" ){
                           place <<- "0"
                           enabled(tblist$`Normality Checks`$f3) <- TRUE
                           enabled(tblist$`Normality Checks`$f4) <- TRUE
                           delete(nonstandard.layout, extraargs.frame)
                           
                         }
                       })
  size(m.f.list) <- c(100,20)
  main.layout[2, 4] <- m.f.label
  main.layout[2, 5, expand=T] <- m.f.list 
  
  d.s.label <- glabel("Data \nStructure")
  d.s.list <- gcombobox(c("Standard", "Complex Survey"),
                        handler = function(h, ...) {
                          if (svalue(h$obj) == "Standard") {
                            svalue(statusbar) <- "Use modeling framework to select lm/glm"
                            svalue(svycluster.edit, index = TRUE) <- 1
                            svalue(svystrata.edit, index = TRUE) <- 1
                            svalue(svycluster.edit, index = TRUE) <- 1
                            svalue(svyfpc.edit, index = TRUE) <- 1
                            delete(nonstandard.layout, svy.frame) 
                            delete(nonstandard.layout, aov.frame)
                          }
                          if (svalue(h$obj) == "Designed Expt"){
                            svalue(statusbar) <- "Experiments are not yet implemented."
                            delete(nonstandard.layout, svy.frame)
                            add(nonstandard.layout, aov.frame)
                          }
                          if (svalue(h$obj) == "Complex Survey"){
                            svalue(statusbar) <- "survey structure currently only supports strata and 1-level cluster designs."
                            delete(nonstandard.layout, aov.frame)
                            add(nonstandard.layout, svy.frame)  
                          }
                        })
  size(d.s.list) <- c(100,20)
  main.layout[3, 4] <- d.s.label
  main.layout[3, 5] <- d.s.list
  
  
  nonstandard.layout <- ggroup(horizontal = FALSE)
  main.layout[1:10, 7:9] <- nonstandard.layout
  
  ## extra arguments for glm
  quasi.label <- glabel("quasi")
  quasi.edit <- gcombobox(c(FALSE, TRUE))
  offset.label <- glabel("offset")
  offset.edit <- gedit(initial.msg = "accept expression here only")
  extraargs.frame <- gframe("Extra arguments: ")
  extraargs.layout <- glayout(cont = extraargs.frame)
  extraargs.layout[1, 1] <- quasi.label
  extraargs.layout[1, 2] <- quasi.edit
  extraargs.layout[2, 1] <- offset.label
  extraargs.layout[2, 2] <- offset.edit
  ## extra arguments for glm end
  
  ## complex survey 
  svydes.vec <- character(4)
  svycluster.label <- glabel("Cluster")
  svycluster.edit <- gcombobox(c("", "1", names(tag(modellingWin, "dataSet"))), 
                               handler = function(h, ...){
                                 if (svalue(svycluster.edit)=="") 
                                   svydes.vec[1] <<- ""
                                 else
                                   svydes.vec[1] <<- paste0("id = ~",svalue(svycluster.edit))
                               })
  svystrata.label <-  glabel("Strata")
  svystrata.edit <- gcombobox(c("", names(tag(modellingWin, "dataSet"))), 
                              handler = function(h, ...){
                                if (svalue(svystrata.edit)=="") 
                                  svydes.vec[2] <<- ""
                                else
                                  svydes.vec[2] <<- paste0("strata = ~",svalue(svystrata.edit))
                              })
  
  svyweights.label <- glabel("Weights")
  svyweights.edit <- gcombobox(c("", names(tag(modellingWin, "dataSet"))), 
                               handler = function(h, ...){
                                 if (svalue(svyweights.edit)=="") 
                                   svydes.vec[3] <<- ""
                                 else
                                   svydes.vec[3] <<- paste0("weights = ~",svalue(svyweights.edit))
                               })
  
  svyfpc.label <- glabel("fpc") 
  svyfpc.edit <- gcombobox(c("", names(tag(modellingWin, "dataSet"))), 
                           handler = function(h, ...){
                             if (svalue(svyfpc.edit)=="") 
                               svydes.vec[4] <<- ""
                             else
                               svydes.vec[4] <<- paste0("fpc = ~",svalue(svyfpc.edit))
                           })
  svyextra.button <- gbutton("More", handler = function(h,...) {
    svalue(statusbar) <- "For more complex survey design and provide this functionality in the future."
  })
  
  svy.design <- NULL
  svy.frame <- gframe("survey design")
  svy.layout <- glayout(cont = svy.frame)
  svy.layout[1, 1, anchor =c(0, 0)] <- svycluster.label
  svy.layout[1, 2, anchor =c(0, 0)] <- svycluster.edit
  svy.layout[2, 1, anchor =c(0, 0)] <- svystrata.label
  svy.layout[2, 2, anchor =c(0, 0)] <- svystrata.edit
  svy.layout[3, 1, anchor =c(0, 0)] <- svyweights.label
  svy.layout[3, 2, anchor =c(0, 0)] <- svyweights.edit
  svy.layout[4, 1, anchor =c(0, 0)] <- svyfpc.label
  svy.layout[4, 2, anchor =c(0, 0)] <- svyfpc.edit
  svy.layout[5, 2, anchor =c(0, 0)] <- svyextra.button
  
  
  ## complex survey end
  
  ## designed experiment
  aovproj.label <- glabel("projections")
  aovproj.edit <- gcombobox(c("FALSE", "TRUE"))
  
  aovqr.label <-  glabel("QR decomp")
  aovqr.edit <- gcombobox(c(TRUE,FALSE))
  
  aovcontr.label <- glabel("contrasts")
  aovcontr.edit <- gedit()
  
  aov.frame <- gframe("Experiment design")
  aov.layout <- glayout(cont = aov.frame)
  aov.layout[1, 1, anchor=c(0, 0)] <- aovproj.label
  aov.layout[1, 2, anchor=c(0, 0)] <- aovproj.edit
  aov.layout[2, 1, anchor=c(0, 0)] <- aovqr.label
  aov.layout[2, 2, anchor=c(0, 0)] <- aovqr.edit
  aov.layout[3, 1, anchor=c(0, 0)] <- aovcontr.label
  aov.layout[3, 2, anchor=c(0, 0)] <- aovcontr.edit 
  ## designed experiment end
  
  storage.effect.of.interest <- character(0) 
  x.var.choice <-gradio(c("Variables of Interest                  ", "Confounders"), horizontal = TRUE)
  main.layout[5, 2:5] <- x.var.choice
  
  EffectOfInterests <- gtable(character(0),multiple=TRUE)
  addHandlerSelectionChanged(EffectOfInterests, handler = function(h,...) {
    #print(svalue(h$obj))
    #EffectOfInterests.select <<- svalue(h$obj)
    svalue(x.var.choice) <- "Variables of Interest                  "
  })
  size(EffectOfInterests) <- c(100, 265)
  Confounders <- gtable(character(0),multiple=TRUE)
  size(Confounders) <- c(100, 265)
  addHandlerSelectionChanged(Confounders, handler = function(h,...) {
    #Confounders.select <<- svalue(h$obj)
    svalue(x.var.choice) <- "Confounders"  
  })
  
  aswitch.button <- gaction(label = "<=>", tooltip = "switch panel", 
                            handler = function(h, ...) {
                              select <- Entry()
                              if (any(as.character(select) == "")) return()
                              start <- svalue(x.var.choice, index = TRUE)
                              remove.action()
                              mid <- ifelse(start - 1, 1, 2)
                              svalue(x.var.choice, index = TRUE) <- mid
                              addVarstoPanel(select)
                              svalue(x.var.choice, index = TRUE) <- start
                            })
  switch.button <- gbutton(action = aswitch.button)
  
  main.layout[6:10, 2:3, anchor = c(0, 0)] <- EffectOfInterests
  main.layout[6:10, 5:6, anchor = c(0, 0)] <- Confounders
  main.layout[8, 4, ancohr = c(0, 0)] <- switch.button
  
  ## it is quite complex here because names(gtable(...)) does not return the list
  ## of the table variables names  
  ## names for gtable is just return the column name
  ## svalue return the select var name
  ## we can use this ability in here but only can use the nice panel structure like gtable...
  ## i.e use gtable to display the variables name rather than gdf or gtext...
  # EffectOfInterests.select <- ""
  # Confounders.select <- ""
  EffectOfInterests.list <- character(0)
  Confounders.list <- character(0)
  total.vars.list <- character(0)
  
  aAdd.button <- gaction("Add",tooltip = "Add Variables from Variable Name list",
                         handler = function(h,...) {
                           addVarstoPanel()
                         })
  Add.button <- gbutton(action = aAdd.button)
  
  
  addVarstoPanel = function(input=svalue(response.varlist), interact = FALSE) {
    entry <- input
    ## for each entry we need to do control , so that
    ## no same name in the same panel, no same name across the panel
    
    if (svalue(x.var.choice) == "Variables of Interest                  ") {
      # we need to control no repeated variables in the same table or across table
      if ((length(total.vars.list) == 0)) {
        EffectOfInterests.list <<- append(EffectOfInterests.list, entry)
        total.vars.list <<- append(total.vars.list, EffectOfInterests.list)
        # gWidgets1: annoying!
        #delete(main.layout, EffectOfInterests)
        #EffectOfInterests <- gtable(EffectOfInterests.list, multiple = TRUE)
        #addHandlerClicked(EffectOfInterests,handler = function(h, ...) { 
        #  svalue(x.var.choice) <- "Variables of Interest                  "
        #  
        #  EffectOfInterests.select <<- svalue(h$obj)
        #})
        #size(EffectOfInterests) <- c(200, 265)
        #main.layout[6:10, 2:3, anchor = c(0, 0)] <- EffectOfInterests
        # gWidgets2: 
        EffectOfInterests[] <- EffectOfInterests.list
      }
      else{
        if (any(entry %in% EffectOfInterests.list)) 
          entry <- entry[!entry %in% EffectOfInterests.list]
        
        if (interact & any(entry %in% Confounders.list)) 
          return()
        
        if (length(Confounders.list)!=0)
          #if (!is.null(Confounders.list)) 
          entry <- entry[!entry %in% Confounders.list]
        
        if (any(entry %in% all.vars(as.formula(paste0("~", paste0(total.vars.list, collapse = "+")))))) 
          return()
        
        if (length(entry) == 0) 
          return()
        
        EffectOfInterests.list <<- append(EffectOfInterests.list, entry)
        total.vars.list <<- unique(append(total.vars.list, EffectOfInterests.list))
        EffectOfInterests[] <- EffectOfInterests.list
      }  
    }
    else{
      if (length(total.vars.list) == 0){
        Confounders.list <<- append(Confounders.list, entry)
        total.vars.list <<- append(total.vars.list, Confounders.list)
        Confounders[] <- Confounders.list
      }
      else{
        if (any(entry %in% Confounders.list)) 
          entry <- entry[!entry %in% Confounders.list]
        
        if (interact & any(entry %in% EffectOfInterests.list)) 
          return()
        
        if (length(EffectOfInterests.list)!=0)
          #if (!is.null(EffectOfInterests.list)) 
          entry <- entry[!entry %in% EffectOfInterests.list]
        
        if (any(entry %in% all.vars(as.formula(paste0("~", paste0(total.vars.list, collapse = "+")))))) 
          return()
        if (length(entry) == 0) return()
        
        Confounders.list <<- append(Confounders.list, entry)
        total.vars.list <<- unique(append(total.vars.list, Confounders.list))
        Confounders[] <- Confounders.list
      }
    }
  }  
  
  
  ## Macro button list
  aInteract.button = gaction(label = "Interact",
                             tooltip = "Interact set of variables selected from variable name list")
  Interact.button <- gbutton(action = aInteract.button)
  Interact.button.list <- {
    list(one = gaction("Full Factorial", 
                       handler = function(h, ...) {
                         if (length(svalue(response.varlist))<2) 
                           return(svalue(statusbar) <- "Select at least two variables using 'Ctrl' or 'Shift'")
                         temp1 <- paste0(svalue(response.varlist), collapse = "*")
                         if (temp1 == "") 
                           return() 
                         Interact.buttonI <- as.formula(paste0("~", temp1))
                         Interact.buttonO <- attr(terms(Interact.buttonI), "term.labels")
                         addVarstoPanel(Interact.buttonO, interact = TRUE)
                       }),
         
         two = gaction("All interactions <= Degree (below)", 
                       handler = function(h, ...) {
                         if (length(svalue(response.varlist))<2) 
                           return(svalue(statusbar) <- "Select at least two variables using 'Ctrl' or 'Shift'")
                         temp1 <- paste0(svalue(response.varlist),collapse="*")
                         if (temp1 == "") 
                           return()
                         Interact.buttonI <- as.formula(paste0("~", temp1))
                         Interact.buttonO <- attr(terms(Interact.buttonI), "term.labels")
                         test.expression <- paste0(rep(":", svalue(degreeI)), collapse = ".*")
                         lessthan <- grep(test.expression,Interact.buttonO)
                         if (length(lessthan) == 0) 
                           return(addVarstoPanel(Interact.buttonO, interact = TRUE))
                         addVarstoPanel(Interact.buttonO[-lessthan], interact = TRUE)  
                       })
    )}
  addPopupMenu(Interact.button, Interact.button.list)
  ## Macro button list end
  
  degree.layout <- glayout()
  degree.layout[1, 1, anchor = c(0, 0)] <- glabel("Degree")
  degreeI <- gedit(2, width = 2)
  degree.layout[1, 2, anchor = c(0, 0)] <- degreeI
  
  ## transform button
  Transform.button <- gbutton(action = gaction("Transform", 
                                               tooltip = "only accept the variable in two right panels"))
  
  ## the strategy of Entry and addVarspanel is different
  ## the former use the vars names in the EffectofInterests or Confounders Panel...
  ## the latter use the vars names in the response variables list...
  ## Entry is a function help distinguish which panel object was chosena and return that object
  Entry <- function(){
    if (svalue(x.var.choice) == "Variables of Interest                  ") {
      entry <- svalue(EffectOfInterests)
    }
    else{
      entry <- svalue(Confounders)
    }
    if (all(entry == "")) {
      svalue(statusbar)<-"select variable first" ## introduce error message here...
      return()
    }
    entry
  }
  
  Transform.button.list <- {
    list(one = gaction("log",
                       handler = function(h, ...) {
                         entry <- Entry()
                         if (any(is.null(entry)))
                           return()
                         Transform.buttonI <- paste0("log(", entry, ")")
                         remove.action()
                         addVarstoPanel(Transform.buttonI) 
                         
                       }),
         
         two = gaction("sqrt", 
                       handler = function(h, ...) {
                         entry <- Entry()
                         if (any(is.null(entry)))
                           return()
                         Transform.buttonI <- paste0("sqrt(", entry, ")")
                         remove.action()
                         addVarstoPanel(Transform.buttonI) 
                         
                       }),
         
         three = gaction("^ Degree (above)",
                         tooltip = "raise to power of Degree (above)",
                         handler = function(h, ...) {
                           entry <- Entry()
                           if (any(is.null(entry)))
                             return()
                           Transform.buttonI <- paste0("I(", entry, "^", svalue(degreeI), ")")
                           addVarstoPanel(Transform.buttonI) 
                           #remove.action()
                         }),
         
         four = gaction("Polynomial of Degree", 
                        handler=function(h, ...) {
                          entry <- Entry()
                          if (any(is.null(entry)))
                            return()
                          Transform.buttonI <- paste0("poly(", entry, ",", svalue(degreeI), ")")
                          remove.action()
                          addVarstoPanel(Transform.buttonI) 
                          
                        }),
         
         #five = gaction("smooth",
         #                handler = function(h, ...) {
         #                  entry <- Entry()
         #                  Transform.buttonI <- paste0("smooth(", entry, ")")
         #                  addVarstoPanel(Transform.buttonI) 
         #                  remove.action() 
         #                }),
         #five = gaction("offset", 
         #                handler = function(h, ...) {
         #                  entry <- Entry()
         #                  if (any(is.null(entry)))
         #                    return()
         #                  Transform.buttonI <- paste0("offset(", entry, ")")
         #                  remove.action() 
         #                  addVarstoPanel(Transform.buttonI) 
         #                  
         #                }),
         six = gaction("reset baseline", 
                       handler = function(h, ...){
                         # This one have to be single choice
                         entry <- Entry()
                         if (any(is.null(entry)))
                           return()
                         if (length(entry) > 1) 
                           return(svalue(statusbar)<-"relevel only support single selected variable")
                         selected.level <- NULL
                         #if(!(entry %in% names(test.factorset))) 
                         #   return(svalue(statusbar)<- paste(entry, "is not factor and no level can be reorder."))
                         relevel.window <- gwindow("relevel", width=300, height=20, visible=TRUE, parent = modellingWin)
                         relevel.window.group <- ggroup(cont = relevel.window)
                         relevel.window.label1 <- glabel(paste("relevel(", entry, ","), 
                                                         cont=relevel.window.group,
                                                         hanlder=function(h, ...) {h$obj})
                         print(test.factorset[[entry]])
                         relevel.window.droplist <- gcombobox(test.factorset[[entry]],
                                                              cont=relevel.window.group)
                         size(relevel.window.droplist) <- c(100, -1) 
                         relevel.window.label2 <- glabel(")", cont=relevel.window.group)
                         submit.button <- gbutton("sumbit", cont = relevel.window.group, 
                                                  handler = function(h, ...) {
                                                    Transform.buttonI <- paste0("relevel(", entry ,", ref=","'",svalue(relevel.window.droplist),"'",")")
                                                    remove.action() 
                                                    addVarstoPanel(Transform.buttonI) 
                                                    dispose(relevel.window)
                                                  })
                       })
    )} 
  addPopupMenu(Transform.button, Transform.button.list)
  
  
  ## Remove button 
  rmVars <- function(input, place) {
    rmVarsI <- input
    place <- place[!(place %in% rmVarsI)] 
    total.vars.list <<- total.vars.list[!(total.vars.list %in% rmVarsI)]
    place
  }
  
  remove.action <- function() {
    if (svalue(x.var.choice) == "Variables of Interest                  ") {
      entry <- rmVars(svalue(EffectOfInterests), EffectOfInterests.list)
      EffectOfInterests.list <<- entry
      #EffectOfInterests.select <<- ""   ## clean the select processs
      EffectOfInterests[] <- EffectOfInterests.list
    }
    else {
      entry <- rmVars(svalue(Confounders), Confounders.list)
      Confounders.list <<- entry
      #Confounders.select <<- ""   ## clean the select processs
      Confounders[] <- Confounders.list
    } 
  }
  
  aRemove.button <- gaction("Remove", tooltip = "Remove variables from two right panels (multiple selection allowed)",
                            handler = function(h, ...){
                              remove.action()
                            })
  Remove.button <- gbutton(action = aRemove.button)
  
  main.layout[6,  1, anchor = c(0, 0)] <- Add.button
  main.layout[7,  1, anchor = c(0, 0)] <- Interact.button
  main.layout[8,  1, anchor = c(0, 0)] <- degree.layout
  main.layout[9,  1, anchor = c(0, 0)] <- Transform.button
  main.layout[10, 1, anchor = c(0, 0)] <- Remove.button
  
  menuGp <- ggroup(container = mainGp, expand = FALSE, horizontal = FALSE)
  
  
  model.buttons.group <- ggroup()
  
  temp.data.frame.id <- 0
  fit.model.button <- gbutton("Fit model", handler = function(h, ...) {
    
    
    ## update the variable type by variable name list once press the fit model 
    original.class = sapply(tag(modellingWin, "dataSet"), class)
    original.order = names(tag(modellingWin, "dataSet"))
    newlabel.class = response.varlist[,]
    Id <- newlabel.class[original.order,2] != original.label
    
    if (any(Id)) {
      target.var.change = newlabel.class[original.order,][Id,]
      target.var.change[target.var.change[,2] == "Num",2] <- "numeric" 
      target.var.change[target.var.change[,2] == "Cat",2] <- "factor"
      transform.command.core <- paste0(target.var.change[,1]," = ","as.", target.var.change[,2],"(",target.var.change[,1],")", collapse = " , ")
      transform.command <- paste0("transform(", dataset.name, ",", transform.command.core,")")
      
      dataset.name.f <- paste0("temp", temp.data.frame.id)
      temp.data.frame.id <<- temp.data.frame.id + 1 
      tag(modellingWin, "name") <<- dataset.name.f
      
      assign(dataset.name.f, eval(parse(text = transform.command)), .GlobalEnv)
      code.history <<- c(code.history, paste(dataset.name.f, " <- ", transform.command))
      ##
    }
    else 
      dataset.name.f <- dataset.name
    
    if (svalue(transform.drop.list) == "NULL")
      y <- svalue(moving.y)
    if (svalue(transform.drop.list) == "log")
      y <- paste0("log(", svalue(moving.y), ")")
    if (svalue(transform.drop.list) == "^ argument" & svalue(argument.type)!="")
      y <- paste0("I(", svalue(moving.y), "^", svalue(argument.type), ")")
    if (svalue(transform.drop.list) == "^ argument" & svalue(argument.type)=="")
      return(svalue(statusbar) <- paste("provide arguement before fitting the model"))
    #if (svalue(transform.drop.list) == "as.numeric")
    #  y <- paste0("as.numeric(", svalue(moving.y), ")")
    
    x <-  paste0(c(EffectOfInterests.list, Confounders.list), collapse = " + ")
    
    target.family <- svalue(m.f.list) 
    if (target.family == "Least Squares") 
      target.family = "gaussian"
    if (target.family == "Logistic Regression (Y binary)" & !svalue(quasi.edit)) 
      target.family = "\"binomial\""
    if (target.family == "Logistic Regression (Y binary)" & svalue(quasi.edit)) 
      target.family = "\"quasibinomial\""
    
    if (target.family == "Poisson Regression (Y counts)" & !svalue(quasi.edit)) 
      target.family = "\"poisson\""
    if (target.family == "Poisson Regression (Y counts)" & svalue(quasi.edit))
      target.family = "\"quasipoisson\""
    
    target.design <- svalue(d.s.list) 
    if (target.design == "Standard") 
      target.design = "simple"
    if (target.design == "Complex Survey") 
      target.design = "survey"
    if (target.design == "Designed Expt") 
      target.design = "experiment"
    
    
    svy.design <<- fit.design(svydes.vec, dataset.name.f)
    if (target.design == "survey") {
      code.history <<- c(code.history,
                         paste("svy.design", paste0(deparse(svy.design$call),collapse = " "), sep = " = "))
    }
    current.fit <- fit.model(y, x, data = dataset.name.f, 
                             family = target.family, 
                             design = target.design)
    
    #print(eval(parse(text = current.fit),envir = mydata))
    #print(names(mydata))
    #print(eval(as.formula(current.fit), envir = mydata))
    # In the case where we might not want to keep adding
    # to our list of models, uncomment this block.
    #model.exists <- listOfModels %in% fm
    #if (any(model.exists)) {
    #  match.ind <- min(which(model.exists))
    #  svalue(modelChooser, index = TRUE) <- match.ind
    #  currentModelIndex <- match.ind
    #  gmessage(paste("The model", names(listOfModels)[match.ind], "already contains the model you want to fit, using that instead"),
    #           title = "Model exists", icon = "warning", container = TRUE, parent = modellingWin)
    #  # Now by default produce summary output
    #  getModelSummary()
    #  getModelAnova()
    #  return()
    #}
    
    # Here we work out what is the best name to give a model.
    # We want the newest model to have the highest index, but
    # we can have gaps due to deletion of rubbish models.
    fm <- current.fit
    #mnames <- strsplit(names(listOfModels), "_") 
    #nums <- sapply(mnames, function(words) {
    #  if (length(words) > 1)
    #    as.numeric(words[2])
    #  else numeric(0)
    #})
    
    #nums <- unlist(nums)
    
    if (length(listOfModels) > 1)
      model.name <- paste("model", length(listOfModels) , sep = "_")
    else
      model.name <- "model_1"
    
    code.history <<- c(code.history,
                       paste(model.name, makeQualifiedLM(fm), sep = " = "))
    listOfModels <<- c(listOfModels, fm)
    nmodels <- length(listOfModels)
    names(listOfModels)[nmodels] <<- model.name
    modelChooser[] <- names(listOfModels)
    listOfDroppedObs[[nmodels]] <<- numeric(0)
    currentModelIndex <<- nmodels
    svalue(modelChooser, index = TRUE) <- nmodels
    
    # Now by default produce summary output
    getModelSummary()
  })
  
  
  fit.model.button2 <- gbutton("Fit model", handler = function(h, ...) {
    
    
    if (trim(svalue(int.text)) == "" & trim(svalue(other.text)) == "")
      return()
    if (trim(svalue(int.text)) == "")
      fm <- paste(svalue(response.varlist), "~", svalue(other.text))
    else if (trim(svalue(other.text)) == "")
      fm <- paste(svalue(response.varlist), "~", svalue(int.text))
    else
      fm <- paste(svalue(response.varlist), "~", svalue(int.text), "+", svalue(other.text))
    fm <- paste("lm(", fm, ")", sep = "")
    # In the case where we might not want to keep adding
    # to our list of models, uncomment this block.
    #model.exists <- listOfModels %in% fm
    #if (any(model.exists)) {
    #  match.ind <- min(which(model.exists))
    #  svalue(modelChooser, index = TRUE) <- match.ind
    #  currentModelIndex <- match.ind
    #  gmessage(paste("The model", names(listOfModels)[match.ind], "already contains the model you want to fit, using that instead"),
    #           title = "Model exists", icon = "warning", container = TRUE, parent = modellingWin)
    #  # Now by default produce summary output
    #  getModelSummary()
    #  getModelAnova()
    #  return()
    #}
    
    # Here we work out what is the best name to give a model.
    # We want the newest model to have the highest index, but
    # we can have gaps due to deletion of rubbish models.
    mnames <- strsplit(names(listOfModels), "_")
    nums <- sapply(mnames, function(words) {
      if (length(words) > 1)
        as.numeric(words[2])
      else numeric(0)
    })
    nums <- unlist(nums)
    if (length(nums) > 0)
      model.name <- paste("model", max(nums) + 1, sep = "_")
    else
      model.name <- "model_1"
    code.history <- c(code.history,
                      paste(model.name, makeQualifiedLM(fm), sep = " = "))
    listOfModels <- c(listOfModels, fm)
    nmodels <- length(listOfModels)
    names(listOfModels)[nmodels] <- model.name
    modelChooser[] <- names(listOfModels)
    listOfDroppedObs[[nmodels]] <- numeric(0)
    currentModelIndex <- nmodels
    svalue(modelChooser, index = TRUE) <- nmodels
    
    # Now by default produce summary output
    getModelSummary()
  })
  
  rename.model.button <- gbutton("Rename model",handler = function(h,...){
    rename.window = gwindow("Rename Current Model", , parent = modellingWin, 
                            width=500, height=30)
    
    rename.group = ggroup(cont = rename.window)
    oldmodel.label = glabel(svalue(modelChooser), cont = rename.group)
    sign.label = glabel("==>>", cont = rename.group)
    newmodel.label <- gedit(text = "", initial.msg = "type the new name you want and press submit",
                            cont = rename.group)
    size(newmodel.label) <- c(300,30)
    anewmode.button <- gaction(label = "rename", tooltip = "Not accept name start by Number; Not accept '(','[','{','SPACE';
                               Or, you can use `your named model`.",handler = function(h, ...) {
                                 if (svalue(newmodel.label)=="")
                                   return()
                                 new.label <- svalue(newmodel.label)
                                 code.history <<- c(code.history, paste0(svalue(newmodel.label), "<-", svalue(modelChooser)))
                                 names(listOfModels)[currentModelIndex] <<- svalue(newmodel.label)
                                 ## notice bug!
                                 modelChooser[currentModelIndex] <<- svalue(newmodel.label)
                                 svalue(modelChooser, index = TRUE) <- currentModelIndex
                                 #print("new")
                                 #print(currentModelIndex)
                                 #print(listOfModels)
                                 #print(svalue(modelChooser, index = TRUE) <- currentModelIndex)
                                 #print(svalue(modelChooser))
                                 
                                 #temp.storage.name <<- svalue(newmodel.label)
                                 dispose(rename.window)
  })
    newmode.button <- gbutton(action = anewmode.button,
                              cont = rename.group)
    
    
    
    
  })
  
  delete.model.button <- gbutton("Delete model", handler = function(h, ...) {
    if (length(listOfModels) == 1)
      return()
    listOfModels <<- listOfModels[-currentModelIndex]
    listOfDroppedObs <<- listOfDroppedObs[-currentModelIndex]
    currentModelIndex <<- length(listOfModels)
    modelChooser[] <- names(listOfModels)
    svalue(modelChooser, index = TRUE) <- currentModelIndex
  })
  
  
  add(model.buttons.group, fit.model.button)
  add(model.buttons.group, rename.model.button)
  add(model.buttons.group, delete.model.button)
  
  main.layout[11,4:5 ] <- model.buttons.group   ###%%%###
  
  #toolbar list
  
  
  listOfModels <- "Current model not yet chosen"
  names(listOfModels) <-  "        - "
  listOfDroppedObs <- list()
  listOfDroppedObs[[1]] <- numeric(0)
  currentModel <- ""
  currentModelIndex <- 1
  currentModelLabel <- glabel("Current model not yet chosen")
  
  modelChooser = gcombobox(names(listOfModels), selected = 1, handler = function(h,...) {
    if(! is.null(svalue(h$obj, index = TRUE)))
      currentModelIndex <<- svalue(h$obj, index = TRUE)
    
    if (length(listOfDroppedObs[[currentModelIndex]]) == 0)
      svalue(currentModelLabel) <<- listOfModels[currentModelIndex]
    else
      svalue(currentModelLabel) <<- gsub('\\n+', "",
                                         paste(listOfModels[currentModelIndex],
                                               "with dropped observations",
                                               paste(listOfDroppedObs[[currentModelIndex]],
                                                     collapse = " , ")), perl = TRUE)
  })
  
  
  main.layout[11, 1, anchor = c(0, 0)] <- glabel("Current Model: ")
  main.layout[11, 2:3] <- modelChooser
  
  gseparator(horizontal = TRUE, container = mainGp)
  add(mainGp, currentModelLabel)
  gseparator(horizontal = TRUE, container = mainGp)
  
  tblist = list()
  tblist$Summary <- list(a1 = gaction(label = "iNZightSummary(current.model)", tooltip = "try1", 
                                      icon = "symbol_diamond", handler = function(h, ...) getModelSummary()))
  tblist[2] <- gseparator(parent = modellingWin, horizontal = FALSE)
  tblist$`Category Comparison` = list(#c1 = gaction("Factor Means", tooltip = NULL, icon = NULL,
    #             handler = function(h, ...) print("factorMeans(model_1)")),
    #c2 = gaction("Adjusted Means", tooltip = NULL, icon = NULL,
    #             handler = function(h, ...) print("adjustedMeans(model_1)")),
    c3 = gaction("Comparison Plot", tooltip = NULL, icon = "symbol_diamond",
                 handler = function(h, ...) chooseFactorComparisonVars(printMat = FALSE)),
    c4 = gaction("Comparison Matrix", tooltip = NULL, icon = "symbol_square",
                 handler = function(h, ...) chooseFactorComparisonVars(printMat = TRUE))
  )
  basicplot.list <- list(e1 = gaction("All Plots", tooltip = NULL, icon = NULL,
                                      handler = function(h, ...) getBasicPlots(1:4)),
                         e2 = gaction("Residuals vs Fitted", tooltip = NULL, icon = NULL,
                                      handler = function(h, ...) getBasicPlots(1)),
                         e3 = gaction("Scale-Location", tooltip = NULL, icon = NULL,
                                      handler = function(h, ...) getBasicPlots(2)),
                         e4 = gaction("Residuals vs Leverage", tooltip = NULL, icon = NULL,
                                      handler = function(h, ...) getBasicPlots(3)),
                         e5 = gaction("Cooks Distance", tooltip = NULL, icon = NULL,
                                      handler = function(h, ...) getBasicPlots(4)),
                         e6 = gaction("Summary Plot", tooltip = NULL, icon = NULL,
                                      handler = function(h, ...) getBasicPlots(7))
  )
  tblist[4] <- gseparator(parent = modellingWin, horizontal = FALSE)
  tblist$`Graphical Diagnostics` = list(d1 = gaction("Scatter Plot Matrix", tooltip = NULL, icon = "symbol_diamond",
                                                     handler = function(h, ...) {
                                                       getScatterplotMatrix() }),
                                        "Basic Plots (4 types)" = basicplot.list,
                                        d3 = gaction("Partial Residual Plot", tooltip = NULL, icon = NULL,
                                                     handler = function(h, ...) choosePartialResid()),
                                        d4 = gaction("All Partial Residual Plots", tooltip = NULL, icon = NULL,
                                                     handler = function(h, ...) showAllPartialResids())
  )
  tblist[6] <- gseparator(parent = modellingWin, horizontal = FALSE)
  tblist$`Normality Checks` = list(f1 = gaction("Normal Q-Q", tooltip = NULL, icon = NULL,
                                                handler = function(h, ...) getBasicPlots(5)),
                                   f2 = gaction("Norm Check Histogram", tooltip = NULL, icon = NULL,
                                                handler = function(h, ...) getBasicPlots(6)),
                                   f3 = gaction("Histogram Array", tooltip = NULL, icon = NULL,
                                                handler = function(h, ...) getHistArray()),
                                   f4 = gaction("Q-Q Plot Inference", tooltip = NULL, icon = NULL,
                                                handler = function(h, ...) getQQPlotArray())
  )
  tblist[8] <- gseparator(parent = modellingWin, horizontal = FALSE)
  tblist$`History` <- list(g1 = gaction("Show code history", 
                                        handler = function(h, ...) showCodeHistory()))
  
  mb <- gmenu(tblist) 
  
  size(mb) <- c(100,30)
  ##%%89
  add(mainGp, mb, expand = FALSE)
  
  outputLabel = glabel("OUTPUT", expand = TRUE)
  font(outputLabel) <- list(weight = "bold", family = "normal")
  outputTxt = gtext("", font.attr=list(family="monospace"))
  add(mainGp, outputTxt, expand = TRUE)
  
  statusbar = gstatusbar("You can try drag-drop the variable from 'Variables' panel to 'Response(Y)' panel, also Hold 'Ctrl' or 'Shift' to do multiple selection.",cont=mainGp)
  
  #specified functional
{
    getModelSummary = function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        if (length(Confounders.list)!=0) {
          if (length(Confounders.list) > 1) {
            exclude.list <- paste0("c(", paste0("\"", Confounders.list, "\"", collapse = " , "), ")")
            result = try(capture.output(eval(parse(text = paste0("iNZightSummary(", listOfModels[modelIndex], ",", "exclude = ", exclude.list, ")", collapse = "")))))
          }
          else 
            result = try(capture.output(eval(parse(text = paste0("iNZightSummary(", listOfModels[modelIndex], ",", "exclude = ", "\"", Confounders.list,"\"", ")", collapse = "")))))
          #result = try(capture.output(eval(parse(text = paste0("with(tag(modellingWin, \"dataSet\"), iNZightSummary(", listOfModels[modelIndex], ",", "exclude = ", "\"", Confounders.list,"\"", ")", ")", collapse = "")))))
        }
        else
          result = try(capture.output(eval(parse(text = paste("iNZightSummary(", listOfModels[modelIndex], ")", collapse = "")))))
        #result = try(capture.output(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), iNZightSummary(", listOfModels[modelIndex], "))", collapse = "")))))
        if (class(result)[1] == "try-error")
          svalue(statusbar) <- result
        #gmessage(title = "ERROR", message = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          svalue(statusbar) <- "Model fitting successful!"
          insert(outputTxt, paste("> iNZightSummary(", svalue(modelChooser),  ")", sep = ""), 
                 font.attr=list(family="monospace"))
          insert(outputTxt, result, font.attr=list(family="monospace"))
          insert(outputTxt, "----------------------------------------------------------------------------------")
          code.history <<- c(code.history, paste("iNZightSummary(", svalue(modelChooser), ")", sep = ""))
        }
        options(show.error.messages = TRUE)
        
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    # havn't used right now? 
    getModelAnova = function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), anova(", listOfModels[modelIndex], "))", collapse = ""))))
        if (inherits(result, "try-error"))
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          insert(outputTxt, paste("> anova(", svalue(modelChooser),  ")", sep = ""))
          insert(outputTxt, capture.output(eval(result)))
          insert(outputTxt, "----------------------------------------------------------------------------------")
          code.history <<- c(code.history, paste("anova(", svalue(modelChooser), ")", sep = ""))
        }
        options(show.error.messages = TRUE)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    getVIFs = function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), vif(", listOfModels[modelIndex], "))", collapse = ""))))
        if (inherits(result, "try-error"))
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          insert(outputTxt, paste("> vif(", svalue(modelChooser),  ")", sep = ""))
          insert(outputTxt, capture.output(eval(result)))
          insert(outputTxt, "----------------------------------------------------------------------------------")
          code.history <<- c(code.history, paste("vif(", svalue(modelChooser), ")", sep = ""))
        }
        options(show.error.messages = TRUE)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    getBasicPlots = function(plotInds = 1:6) {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = listOfModels[modelIndex] )))
        #result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"),", listOfModels[modelIndex], ")", collapse = ""))))
        if (inherits(result, "try-error"))
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current.model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          dev.new()
          plot.new()
          modelString = paste("with(tag(modellingWin, \"dataSet\"),", listOfModels[modelIndex], ")", collapse = "")
          theModel = eval(parse(text = modelString))
          whichArg <-
            if (all(plotInds %in% 1:6))
              ""
          else
            paste(", which = ", plotInds, sep = "")
          plotlm6(theModel, which = plotInds)
          code.history <<- c(code.history, paste("plotlm6(", svalue(modelChooser), ", which = ", plotInds, ")", sep = ""))
        }
        options(show.error.messages = TRUE)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    getScatterplotMatrix = function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = listOfModels[modelIndex] )))
        #result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), ", listOfModels[modelIndex], ")", collapse = ""))))
        if (class(result)[1] == "try-error")
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          dev.new()
          gpairs(result$model)
          code.history <<- c(code.history, paste0("gpairs(", paste0(svalue(modelChooser),"$model"),")"))
          #modelIndex = svalue(modelChooser, index = TRUE)
          #if (modelIndex != 1) {
          
          #  names_list = names(tag(modellingWin, "dataSet"))
          #  logical_list = logical(length(names_list))
          
          #  for(i in 1:length(names_list))
          #    logical_list[i] = grepl(names_list[i], listOfModels[modelIndex])
          
          
          #  dev.new()
          #  gpairs(tag(modellingWin, "dataSet")[,logical_list])
          
          #  obj.lm <- with(tag(modellingWin, "dataSet"), eval(parse(text = listOfModels[modelIndex])))
          #  vars.in.use <- names(attr(terms(obj.lm), "dataClasses"))
          #  vars.in.use <- paste("\"", paste(vars.in.use, collapse = "\", \""), "\"", sep = "")
          #code.history <<- c(code.history, paste("gpairs(", dataset.name.f, "[, c(", vars.in.use, ")])", sep = ""))
        } }
      else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    choosePartialResid <- function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = listOfModels[modelIndex] )))
        #result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), ", listOfModels[modelIndex], ")", collapse = ""))))
        if (class(result)[1] == "try-error")
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          assign(x = "svy.design", value = svy.design, envir = .GlobalEnv)
          xVarterms = attr(result$terms, "term.labels")
          xVartypes = attr(result$terms, "dataClasses")[-1]
          xVarnames = xVarterms[ ! grepl(":", xVarterms) & ! xVartypes %in% c("factor", "ordered")]  ## problem
          ### poly is not suppoort for xVarnames now......also the smooth....
          pr.window = gwindow("Select Variable(s) to Plot", , parent = modellingWin)
          pr.group <- ggroup(horizontal = FALSE, container = pr.window)
          var.table <- gtable(xVarnames, container = pr.group, expand = TRUE, multiple = TRUE)
          plotEachVar <- function(h, ...) {
            plotVars <- svalue(var.table)
            if (! length(plotVars))
              return()
            dev.new()
            danp <- devAskNewPage(length(plotVars) > 1)
            for (v in plotVars) {
              partialResPlot(result, v)
              code.history <<- c(code.history, paste("partialResPlot(", svalue(modelChooser), ", \"", v, "\")", sep = ""))
            }
            devAskNewPage(danp)
            
          }
          
          plot.button <- gbutton("Plot Variable(s)", container = pr.group, handler = plotEachVar)
        }
        options(show.error.messages = TRUE)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    chooseFactorMeans <- function() {
      
      
    }
    
    chooseadjustedMeans <- function() {
      
      
    }
    
    
    showAllPartialResids <- function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = listOfModels[modelIndex] )))
        #result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), ", listOfModels[modelIndex], ")", collapse = ""))))
        if (class(result)[1] == "try-error")
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          dev.new()
          allPartialResPlots(result)
          code.history <<- c(code.history, paste("allPartialResPlots(", svalue(modelChooser), ")", sep = ""))
        }
        options(show.error.messages = TRUE)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    getHistArray <- function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = listOfModels[modelIndex] )))
        #result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), ", listOfModels[modelIndex], ")", collapse = ""))))
        if (class(result)[1] == "try-error")
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          dev.new()
          histogramArray(result)
          code.history <<- c(code.history, paste("histogramArray(", svalue(modelChooser), ")", sep = ""))
        }
        options(show.error.messages = TRUE)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    getQQPlotArray <- function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = listOfModels[modelIndex] )))
        #result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), ", listOfModels[modelIndex], ")", collapse = ""))))
        if (class(result)[1] == "try-error")
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          dev.new()
          #qqplotArray(result)
          iNZightQQplot(result)
          code.history <<- c(code.history, paste("iNZightQQplot(", svalue(modelChooser), ")", sep = ""))
        }
        options(show.error.messages = TRUE)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    chooseFactorComparisonVars <- function(printMat = FALSE) {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        options(show.error.messages = FALSE)
        result = try(eval(parse(text = listOfModels[modelIndex] )))
        #result = try(eval(parse(text = paste("with(tag(modellingWin, \"dataSet\"), ", listOfModels[modelIndex], ")", collapse = ""))))
        if (class(result)[1] == "try-error")
          gmessage(title = "ERROR", msg = "No output produced.\nCheck current model for errors", icon = "error", container = TRUE, parent = modellingWin)
        else {
          xVarterms = attr(result$terms, "term.labels")
          xVartypes = attr(result$terms, "dataClasses")[-1]
          #assign("test", result, .GlobalEnv)
          # Remove any > two-way interactions
          validOrder <- attr(result$terms, "order") <= 2
          
          # Only allow numeric interactions and single factors, thus allowed type reduce the multi factor interactions
          allowedType <- function(orders, types) {
            n <- length(orders) # should be same as types
            allowed <- logical(n)
            names(allowed) <- xVarterms
            checked <- logical(n)
            names(checked) <- xVarterms
            
            maxOrder <- max(orders)
            for (i in maxOrder:1) {
              # Already done
              if (all(checked))
                break
              
              for (j in 1:n) {
                # We've already checked this variable from a higher order
                if (checked[j])
                  next
                
                lvls <- strsplit(xVarterms[j], ":")[[1]]
                lvll <- length(lvls)
                if (i > 2 & lvll > 2) {
                  # Generate all possible combinations of the factor
                  all.factors <- vector("list", length(lvls))
                  for (k in 1:length(lvls)) {
                    all.factors[[k]] <- sapply(
                      combn(lvls, k, simplify=FALSE),
                      function(x) paste(x, collapse = ":"))
                  }
                  all.factors <- unlist(all.factors)
                  factor.present <- all.factors %in% xVarterms
                  all.factors <- all.factors[factor.present]
                  allowed[all.factors] <- FALSE
                  checked[all.factors] <- TRUE
                } else if (i == 2 & lvll == 2) {
                  levelTypes <- types[lvls]
                  levelTypes <- levelTypes %in% c("factor", "ordered")
                  allowed[j] <- xor(levelTypes[1], levelTypes[2])
                  allowed[lvls] <- allowed[j] & levelTypes
                  checked[lvls] <- TRUE
                } else if (i == 1 & lvll == 1) {
                  # Should be case where no higher order interactions
                  # are present, i.e. order == 1, i
                  allowed[j] <- types[j] %in% c("factor", "ordered")
                  checked[j] <- TRUE
                }
              }
            }
            allowed
          }
          
          validTypes <- allowedType(attr(result$terms, "order"), xVartypes)
          
          xVarnames <- xVarterms[validTypes]
          if (length(xVarnames) == 0 ) 
            xVarnames <- "Only allow numeric interactions and single factors"
          
          winTitle <- if (printMat) "Print" else "Plot"
          fc.window <- gwindow(paste("Select Variable to", winTitle), , parent = modellingWin)
          fc.group <- ggroup(horizontal = FALSE, container = fc.window)
          var.table <- gtable(xVarnames, container = fc.group, expand = TRUE, multiple = FALSE)
          
          plotVar <- function(h, ...) {
            xvar <- svalue(var.table)
            if (xvar == "Only allow numeric interactions and single factors") return()
            if (! length(xvar))
              return()
            dev.new()
            plot(moecalc(result, xvar))
            code.history <<- c(code.history, paste("plot(moecalc(", svalue(modelChooser), ", \"", xvar, "\"))", sep = ""))
          }
          
          printVar <- function(h, ...) {
            xvar <- svalue(var.table)
            if (xvar == "Only allow numeric interactions and single factors") return()
            if (! length(xvar))
              return()
            insert(outputTxt, paste("> multicomp(moecalc(", svalue(modelChooser), ", \"", xvar, "\"))", sep = ""),
                   , font.attr=list(family="monospace"))
            insert(outputTxt, capture.output(multicomp(moecalc(result, xvar))),
                   , font.attr=list(family="monospace"))
            insert(outputTxt, "----------------------------------------------------------------------------------")
            code.history <<- c(code.history, paste("multicomp(moecalc(", svalue(modelChooser), ", \"", xvar, "\"))", sep = ""))
          }
          
          if (printMat)
            print.button <- gbutton("Print Variable", container = fc.group, handler = printVar)
          else
            plot.button <- gbutton("Plot Variable", container = fc.group, handler = plotVar)
        }
        options(show.error.messages = TRUE)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    
    # havn't used right now?
    dropObs = function() {
      modelIndex = svalue(modelChooser, index = TRUE)
      if (modelIndex != 1) {
        dropObsWin = gwindow("Drop Observations", container = TRUE, parent = modellingWin)
        dropObsMainGp = ggroup(container = dropObsWin, horizontal = FALSE)
        dropObsSecondaryGp = ggroup(container = dropObsMainGp)
        droplt = glayout()
        glbl1 = glabel("Current Model : ", container = dropObsSecondaryGp)
        font(glbl1) <- list(weight="bold", family = "normal")
        glbl2 = glabel(svalue(currentModelLabel), container = dropObsSecondaryGp, expand = TRUE)
        
        add(dropObsMainGp, gseparator(horizontal = TRUE))
        
        glbl3 = glabel("Observations to be dropped from the current model :")
        font(glbl3) <- list(weight="bold", family = "normal")
        gt = gedit("eg:  1, 86, 7")
        droplt[2,1] = glbl3
        droplt[2,2:3] = gt
        
        dropObsTertiaryGp = ggroup()
        lbl1 = glabel("Save model as :", container = dropObsTertiaryGp)
        font(lbl1) <- list(weight="bold", family = "normal")
        nameTxt = gedit(paste(svalue(modelChooser), ".obsDropped",sep=""), container = dropObsTertiaryGp)
        
        dropObsFourthGp = ggroup()
        lbl2 = glabel("Set as current.model?", container = dropObsFourthGp)
        font(lbl2) <- list(weight="bold", family = "normal")
        lbl2checkBox = gcheckbox("", checked = TRUE, container = dropObsFourthGp)
        addSpring(dropObsFourthGp, horizontal = TRUE)
        
        is.wholenumber = function(x, tol = .Machindouble.eps^0.5)  abs(x - round(x)) < tol
        
        saveButtonInDropGp = gbutton(". SAVE .", container = dropObsFourthGp,  handler = function(h,...) {
          listOfModels = c(listOfModels, x = listOfModels[length(listOfModels)])
          names(listOfModels)[length(names(listOfModels))] = gsub('\\n+', "", svalue(nameTxt), perl = TRUE) #what if the user's keep it blank?
          
          x = getDroppedObs(gsub('\\n+', "", svalue(gt), perl = TRUE))
          listOfDroppedObs[[length(names(listOfModels))]] = unique(c(listOfDroppedObs[[svalue(modelChooser, index = TRUE)]], round(x[is.wholenumber(x)])))
          
          if(svalue(lbl2checkBox)) {
            modelChooser[] = names(listOfModels)
            svalue(modelChooser) = names(listOfModels)[length(listOfModels)]
            currentModelIndex = svalue(modelChooser, index = TRUE)
          } else {
            i = svalue(modelChooser, index = TRUE) # this works so currentModelIndex seems to be redundant
            modelChooser[] =  names(listOfModels)
            svalue(modelChooser, index = TRUE) = i
            currentModelIndex = i
          }
          saveModelWinOpened = FALSE
          dispose(dropObsWin)
        })
        visible(droplt) = TRUE
        add(dropObsMainGp, droplt)
        add(dropObsMainGp, dropObsTertiaryGp)
        add(dropObsMainGp, dropObsFourthGp)
      } else {
        gmessage(title = "ERROR", msg = "Select a model first then try again", icon = "error", container = TRUE, parent = modellingWin)
      }
    }
    
    # havn't used right now?
    getDroppedObs = function(charString) {
      x = strsplit(charString, split = ",", perl = TRUE)
      y = strsplit(x[[1]], " +")
      z = character(0)
      
      for(i in 1:length(y))
        z = c(z, y[[i]])
      
      z = na.omit(as.numeric(unique(z)))
      z = z[!is.nan(z)]
      z = z[!is.infinite(z)]
    }
    
    # havn't used right now?
    makePredictions = function(){
      predictionsWin = gwindow("Prediction", parent = modellingWin)
      predictionsMain = ggroup(horizontal = FALSE, container = predictionsWin, expand = TRUE)
      addSpace(predictionsMain, 10, horizontal = FALSE)
      lbl1 = glabel(paste("Current Model :", listOfModels[svalue(modelChooser, index = TRUE)]), container = predictionsMain)
      font(lbl1) <- list(weight="bold", family = "normal")
      addSpace(predictionsMain, 10, horizontal = FALSE)
      lbl2 = glabel("Type expressions needed for prediction and submit the command", container = predictionsMain)
      font(lbl2) <- list(weight="bold", family = "normal")
      lbl3 = glabel("Submit one expression at a time", container = predictionsMain)
      addSpace(predictionsMain, 5, horizontal = FALSE)
      predictionsText = gtext(container = predictionsMain)
      submitButtGroup = ggroup(container = predictionsMain)
      addSpring(submitButtGroup)
      submitButt = gbutton("Submit", handler = function(h,...) submitExpression())
      add(submitButtGroup, submitButt)
      outputText = gtext(container = predictionsMain, expand = TRUE)
      helpButtGroup = ggroup(container = predictionsMain)
      addSpring(helpButtGroup)
      helpButt = gbutton(" Help ", handler = function(h,...) {}) # NEED TO ADD A SCREEN CAP IMAGE FOR HELP
      add(helpButtGroup, helpButt)
    }
    
    submitExpression = function() {
      inputExpression = svalue(predictionsText)
      result = try(eval(parse(text = inputExpression)))
      if (inherits(result, "try-error"))
        gmessage(title = "ERROR", msg = "No output produced.\nCheck submitted expression for errors", icon = "error", container = TRUE, parent = predictionsWin)
      else
        insert(outputText, capture.output(eval(result)))
    }
    
    # initial is to indicate the the first time open code history
    # without initial, every time click code history, header.text will type again.
    initial <- 0
    showCodeHistory <- function() {
      hist.win <- gwindow("Code History", width = 450, height = 300, parent = modellingWin)
      hist.group <- ggroup(use.scrollwindow = TRUE, container = hist.win)
      header.text <- c("# To make this code work outside of iNZight, read in your data like so:",
                       paste("# ", dataset.name, " = read.table(file.choose(), header = TRUE)", sep = ""),
                       "# iNZight has done this step for you, just run the",
                       "# following code in your R console:")
      if (!initial) {
        code.history <<- c(header.text, code.history)
        initial <<- initial + 1
      }
      code.history <<- paste(code.history, collapse = "\n")
      hist.text <- gtext(code.history, container = hist.group, expand = TRUE,
                         wrap = FALSE, font.attr=list(family="monospace"))
    }
    
    makeQualifiedLM <- function(lmtext) {
      #first.piece <- substr(lmtext, 1, nchar(lmtext) - 1)
      #qual.lm <- paste(first.piece, ", data = ", dataset.name, ")", sep = "")
      qual.lm <- lmtext
      qual.lm
    }
  }
}


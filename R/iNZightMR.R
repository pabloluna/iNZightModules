iNZightMR <- setRefClass(
    "iNZightMR",
    fields = list(
        GUI         = "ANY",
        mainGrp     = "ANY",
        activeData = "data.frame"
    ),
    
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI)
            dat = GUI$getActiveData()
            activeData <<- tsData(dat)
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(15)
            
            
    )
    
    
    
)
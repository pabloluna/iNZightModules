### read.infoshare reads in a CSV/Excel file from the Stats NZ infoshare website.
### It makes a data frame and sets the start value and frequency for ts
### (time series) objects. Currently only the data frame is returned.

read.infoshare = function(file.loc) {

    dat = read.csv(file.loc, header = TRUE, skip = 1, as.is = TRUE,
                   na.strings = c("NULL","NA","N/A","#N/A","","<NA>"), check.names = TRUE)
    names(dat)[1] = "time"

    ### The stats NZ files have some info at the end and this
    ### has been read into our data frame - so remove it
    cutOff = with(dat, which(time == "Table information:"))
    selectRow = rep(TRUE, nrow(dat))
    selectRow[cutOff:nrow(dat)] = FALSE
    dat = subset(dat, selectRow)

    ### We need to find the first row that numbers start at
    num.patt = "^[0-9]+(\\.[0-9]+)?$"
    i = 1
    while (length(grep(num.patt, dat[i,2])) == 0)
        i = i + 1
    numbers.start = i


    ### What frequency is the data at (monthly, quarterly, yearly)
    firstTime = dat$time[numbers.start]
    if (nchar(firstTime) > 4) {
        interval = substring(firstTime, 5, 5)
        freq = ifelse(interval == "Q", 4, 12)
    }
    else {
        interval = "A"
        freq = 1
    }

    ### form the start value (used when we create ts objects)
    start = as.numeric(substring(firstTime, 1 ,4))
    if (interval != "A") {
        start = c(start, as.numeric(substring(firstTime, 6)))
    }

    # Just return the data for now
    dat
}



### read.data reads in a standard csv format, top row contains column
### names, 1st column is the time variable and the other columns contain
### the data. Returns the data frame in a list called vars

read.data = function() {
    vars = list()
    vars$data = read.csv(file.choose(), header = TRUE, as.is = TRUE)
    vars
}



### This function adds start and frequency elements to the vars list,
### for data that has been read in by read.data(). Returns the modified
### vars list.

setup.ts = function(vars, start, frequency) {
    vars$start = start
    vars$freq = frequency
    vars
}




### make.ts returns a ts object. which.var is the column number/name of
### the data frame called 'data', inside the list called vars. Returns a
### modified list vars with a time series object in it, and the name of the
### corresponding variable.

make.ts = function(vars, which.var) {
    vars$tsObj = ts(vars$data[,which.var], start = vars$start, frequency = vars$freq)
    vars$currentName = names(vars$data)[which.var]
    vars
}





### The function get.x computes the 'x values' of the time series
### object inside vars. Returns a list, one element is a numeric
### vector and the other is a native units vector

get.x = function(tsObj) {
    ### figure out the limits and step size along the x axis
	f = frequency(tsObj)
	s = start(tsObj)
    if (f == 1) {
      start.x = s[1]
      step.x = 1
      end.x = start.x + length(tsObj) - 1
    }
    else {
      step.x = 1/f
      start.x = s[1] + (s[2] - 1) * step.x
      end.x = start.x + step.x * (length(tsObj) - 1)
    }

    x = seq(start.x, end.x, by = step.x)
    x.units = unit(x, "native")
    list(x = x, x.units = x.units)
}




### The get.line.coords function returns a list of various coordinates
### for a lineGrob. This is necessary because we draw the line copies
### in the plots.vp viewport - the original lines are drawn within the 3
### children viewports of plot.vp.

get.line.coords = function(vars.decomp, vpName, lineGrobName) {
    decomp = vars.decomp$decompVars
    seekViewport(vpName)
    line = getGrob(decomp$tree, lineGrobName)
    line.y = convertUnit(line$y, attr(line$y[1], "unit"), valueOnly = TRUE)
    line.vp.yrange = current.viewport()$yscale
    line.y.npc = (line.y - line.vp.yrange[1]) / diff(line.vp.yrange)
    line.y.parent = switch(vpName,
                           season = decomp$props["remainder"] +
                                      line.y.npc * decomp$props["seasonal"],
                           random = line.y.npc * decomp$props["remainder"],
                            trend = line.y.npc * decomp$props["trend"] +
                                      decomp$props["seasonal"] +
                                      decomp$props["remainder"])
    line.x = convertUnit(line$x, "native", valueOnly = TRUE)
    line.vp.xrange = current.viewport()$xscale
    line.x.npc = (line.x - line.vp.xrange[1]) / diff(line.vp.xrange)
    x.parent = line.x.npc

    list(line.y = line.y, line.vp.yrange = line.vp.yrange,
         line.y.npc = line.y.npc, line.y.parent = line.y.parent,
         line.x = line.x, line.vp.xrange = line.vp.xrange,
         line.x.npc = line.x.npc, x.parent = x.parent,
         line.col = line$gp$col)
}





### The function add.line.plots.vp adds a copy of a lineGrob to the decomposition
### plot. The original is positioned within the child viewports of plots.vp, the copy
### is drawn within plots.vp so that we can shift the lines upwards (later) more easily

add.line.plots.vp = function(vars.decomp, vpName, lineCol = "red",
                             name = paste(vpName, "copy", sep = ".")) {
    z = get.line.coords(vars.decomp, vpName, paste(vpName, "Line", sep = ""))
    lineCopy = linesGrob(unit(z$x.parent, "npc"),
                         unit(z$line.y.parent, "npc"),
                         name = name,
                         vp = vpPath("parent", "plots"),
                         gp = gpar(col = lineCol))
    updated.tree = addGrob(vars.decomp$decompVars$tree, lineCopy)
    vars.decomp$decompVars$tree = updated.tree
    vars.decomp
}

# Borrowed from VIT, use it for TS decomp & recomp
# because otherwise we end up with "flashy" animation
newdevice <- function(width, height, ...) {
    # The windows device works fine (for now), only attempt to speed up
    # any other devices that we're going to be using.
    # We speed them up by getting rid of bufferring.
    if ("Acinonyx" %in% rownames(installed.packages())) {
        # Acinonyx uses pixels rather than inches, convert inches to
        # pixels to determine dims. Assume 90 dpi.
        width.in <- round(width * 90)
        height.in <- round(height * 90)
        Acinonyx::idev(width = width.in, height = height.in)
    } else {
        if (.Platform$OS.type != "windows" && Sys.info()["sysname"] != "Darwin")
            dev.new(width = width, height = height, type = "nbcairo", ...)
        else
            dev.new(width = width, height = height, ...)
    }
}

drawImage = function(image) {
  if ("Acinonyx" %in% rownames(installed.packages()))
    plot.new()

  # Draws current image in device.
  grid.newpage()
  grid.draw(image)

  # On some devices (notably on Mac) we end up being unable to
  # see anything besides a single frame due to buffering.
  # dev.flush() will force the device to show what it has
  # currently buffered.
  if (exists("dev.flush"))
    dev.flush()
}

pauseImage = function(image, pause = 1) {
  for (i in 1:pause)
    drawImage(image)
}

rmGrobs = function(image, grobs) {
  for (i in grobs) {
    if (i %in% childNames(image)) {
      image <- removeGrob(image, gPath(i))
    }
  }
  image
}

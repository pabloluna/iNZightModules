## .onAttach <- function(...) {
##     if (!"package:rgl" %in% search())
##         require(rgl)
## }

scatter3d <- function(x, y, z,
                      xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
                      axis.scales=TRUE,
                      zlab=deparse(substitute(z)), revolutions=0, bg.col=c("white", "black"),
                      axis.col=if (bg.col == "white") c("darkmagenta", "black", "darkcyan")
                      else c("darkmagenta", "white", "darkcyan"),
                      surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
                      neg.res.col="red", pos.res.col="green",
                      square.col=if (bg.col == "white") "black" else "gray", point.col="yellow",
                      text.col=axis.col, grid.col=if (bg.col == "white") "black" else "gray",
                      fogtype=c("exp2", "linear", "exp", "none"),
                      residuals=(length(fit) == 1), surface=TRUE, fill=TRUE, grid=TRUE, grid.lines=26,
                      df.smooth=NULL, df.additive=NULL,
                      sphere.size=1, threshold=0.01, speed=1, fov=60,
                      fit="linear", groups=NULL, parallel=TRUE, ellipsoid=FALSE, level=0.5,
                      model.summary = FALSE) {
  ##  Load "rgl" only when the "scatter3d" function is called..
  require(rgl)                      
  use.gams <- suppressPackageStartupMessages(require(mgcv))
  if (residuals == "squares"){
    residuals <- TRUE
    squares <- TRUE
  }
  else squares <- FALSE
  summaries <- list()
  if ((!is.null(groups)) && (nlevels(groups) > length(surface.col)))
    stop(sprintf(gettext("Number of groups (%d) exceeds number of colours (%d)."),
                 nlevels(groups), length(surface.col)))
  if ((!is.null(groups)) && (!is.factor(groups))) stop(gettext("groups variable must be a factor."))
  bg.col <- match.arg(bg.col)
  fogtype <- match.arg(fogtype)
  if ((length(fit) > 1) && residuals && surface)
    stop(gettext("cannot plot both multiple surfaces and residuals"))
  #        xlab   cause these arguments to be evaluated
  #        ylab
  #        zlab
  rgl.clear()
  rgl.viewpoint(fov=fov)
  rgl.bg(color=bg.col, fogtype=fogtype)
  valid <- if (is.null(groups)) complete.cases(x, y, z)
  else complete.cases(x, y, z, groups)
  x <- x[valid]
  y <- y[valid]
  z <- z[valid]
  minx <- min(x)
  maxx <- max(x)
  miny <- min(y)
  maxy <- max(y)
  minz <- min(z)
  maxz <- max(z)
  if (axis.scales){
    #                lab.min.x <- nice(minx)
    #                lab.max.x <- nice(maxx)
    #                lab.min.y <- nice(miny)
    #                lab.max.y <- nice(maxy)
    #                lab.min.z <- nice(minz)
    #                lab.max.z <- nice(maxz)


    lab.min.x <- minx
    lab.max.x <- maxx
    lab.min.y <- miny
    lab.max.y <- maxy
    lab.min.z <- minz
    lab.max.z <- maxz


    minx <- min(lab.min.x, minx)
    maxx <- max(lab.max.x, maxx)
    miny <- min(lab.min.y, miny)
    maxy <- max(lab.max.y, maxy)
    minz <- min(lab.min.z, minz)
    maxz <- max(lab.max.z, maxz)
    min.x <- (lab.min.x - minx)/(maxx - minx)
    max.x <- (lab.max.x - minx)/(maxx - minx)
    min.y <- (lab.min.y - miny)/(maxy - miny)
    max.y <- (lab.max.y - miny)/(maxy - miny)
    min.z <- (lab.min.z - minz)/(maxz - minz)
    max.z <- (lab.max.z - minz)/(maxz - minz)
  }
  if (!is.null(groups)) groups <- groups[valid]
  x <- (x - minx)/(maxx - minx)
  y <- (y - miny)/(maxy - miny)
  z <- (z - minz)/(maxz - minz)
  size <- sphere.size*((100/length(x))^(1/3))*0.015
  if (is.null(groups)){
    if (size > threshold) rgl.spheres(x, y, z, color=point.col, radius=size)
    else rgl.points(x, y, z, color=point.col)
  }
  else {
    if (size > threshold) rgl.spheres(x, y, z, color=surface.col[as.numeric(groups)], radius=size)
    else rgl.points(x, y, z, color=surface.col[as.numeric(groups)])
  }
  if (!axis.scales) axis.col[1] <- axis.col[3] <- axis.col[2]
  rgl.lines(c(0,1), c(0,0), c(0,0), color=axis.col[1])
  rgl.lines(c(0,0), c(0,1), c(0,0), color=axis.col[2])
  rgl.lines(c(0,0), c(0,0), c(0,1), color=axis.col[3])
  rgl.texts(1, 0, 0, xlab, adj=1, color=axis.col[1])
  rgl.texts(0, 1.05, 0, ylab, adj=1, color=axis.col[2])
  rgl.texts(0, 0, 1, zlab, adj=1, color=axis.col[3])
  if (axis.scales){
    rgl.texts(min.x, -0.05, 0, lab.min.x, col=axis.col[1])
    rgl.texts(max.x, -0.05, 0, lab.max.x, col=axis.col[1])
    rgl.texts(0, -0.1, min.z, lab.min.z, col=axis.col[3])
    rgl.texts(0, -0.1, max.z, lab.max.z, col=axis.col[3])
    rgl.texts(-0.05, min.y, -0.05, lab.min.y, col=axis.col[2])
    rgl.texts(-0.05, max.y, -0.05, lab.max.y, col=axis.col[2])
  }
  if (ellipsoid) {
    dfn <- 3
    if (is.null(groups)){
      dfd <- length(x) - 1
      radius <- sqrt(dfn * qf(level, dfn, dfd))
      ellips <- ellipsoid(center=c(mean(x), mean(y), mean(z)),
                          shape=cov(cbind(x,y,z)), radius=radius)
      if (fill) shade3d(ellips, col=surface.col[1], alpha=0.1, lit=FALSE)
      if (grid) wire3d(ellips, col=surface.col[1], lit=FALSE)
    }
    else{
      levs <- levels(groups)
      for (j in 1:length(levs)){
        group <- levs[j]
        select.obs <- groups == group
        xx <- x[select.obs]
        yy <- y[select.obs]
        zz <- z[select.obs]
        dfd <- length(xx) - 1
        radius <- sqrt(dfn * qf(level, dfn, dfd))
        ellips <- ellipsoid(center=c(mean(xx), mean(yy), mean(zz)),
                            shape=cov(cbind(xx,yy,zz)), radius=radius)
        if (fill) shade3d(ellips, col=surface.col[j], alpha=0.1, lit=FALSE)
        if (grid) wire3d(ellips, col=surface.col[j], lit=FALSE)
        coords <- ellips$vb[, which.max(ellips$vb[1,])]
        if (!surface) rgl.texts(coords[1] + 0.05, coords[2], coords[3], group,
                                col=surface.col[j])
      }
    }
  }
  if (surface){
    vals <- seq(0, 1, length.out=grid.lines)
    dat <- expand.grid(x=vals, z=vals)
    for (i in 1:length(fit)){
      f <- match.arg(fit[i], c("linear", "quadratic", "smooth", "additive"))
      if (! use.gams & f %in% c("smooth", "additive")) {
        f <- "linear"
        gmessage("To use 'smooth' or 'additive' fits you must install the 'mgcv' package for R.\n\nUsing a linear fit instead.",
                 title = "Error - mgcv not installed", icon = "error")
      }
      if (is.null(groups)){
        mod <- switch(f,
                      linear = lm(y ~ x + z),
                      quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2)),
                      smooth = if (is.null(df.smooth)) gam(y ~ s(x, z))
                      else gam(y ~ s(x, z, fx=TRUE, k=df.smooth)),
                      additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z))
                      else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                 s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)))
        )
        if (model.summary) summaries[[f]] <- summary(mod)
        yhat <- matrix(predict(mod, newdata=dat), grid.lines, grid.lines)
        if (fill) rgl.surface(vals, vals, yhat, color=surface.col[i], alpha=0.5, lit=FALSE)
        if(grid) rgl.surface(vals, vals, yhat, color=if (fill) grid.col
                             else surface.col[i], alpha=0.5, lit=FALSE, front="lines", back="lines")
        if (residuals){
          n <- length(y)
          fitted <- fitted(mod)
          colors <- ifelse(residuals(mod) > 0, pos.res.col, neg.res.col)
          rgl.lines(as.vector(rbind(x,x)), as.vector(rbind(y,fitted)), as.vector(rbind(z,z)),
                    color=as.vector(rbind(colors,colors)))
          if (squares){
            res <- y - fitted
            xx <- as.vector(rbind(x, x, x + res, x + res))
            yy <- as.vector(rbind(y, fitted, fitted, y))
            zz <- as.vector(rbind(z, z, z, z))
            rgl.quads(xx, yy, zz, color=square.col, alpha=0.5, lit=FALSE)
            rgl.lines(xx, yy, zz, color=square.col)
          }
        }
      }
      else{
        if (parallel){
          mod <- switch(f,
                        linear = lm(y ~ x + z + groups),
                        quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2) + groups),
                        smooth = if (is.null(df.smooth)) gam(y ~ s(x, z) + groups)
                        else gam(y ~ s(x, z, fx=TRUE, k=df.smooth) + groups),
                        additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z) + groups)
                        else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                   s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)) + groups)
          )
          if (model.summary) summaries[[f]] <- summary(mod)
          levs <- levels(groups)
          for (j in 1:length(levs)){
            group <- levs[j]
            select.obs <- groups == group
            yhat <- matrix(predict(mod, newdata=cbind(dat, groups=group)), grid.lines, grid.lines)
            if (fill) rgl.surface(vals, vals, yhat, color=surface.col[j], alpha=0.5, lit=FALSE)
            if (grid) rgl.surface(vals, vals, yhat, color=if (fill) grid.col
                                  else surface.col[j], alpha=0.5, lit=FALSE, front="lines", back="lines")
            rgl.texts(1, predict(mod, newdata=data.frame(x=1, z=1, groups=group)), 1,
                      paste(group, " "), adj=1, color=surface.col[j])
            if (residuals){
              yy <- y[select.obs]
              xx <- x[select.obs]
              zz <- z[select.obs]
              fitted <- fitted(mod)[select.obs]
              res <- yy - fitted
              rgl.lines(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                        col=surface.col[j])
              if (squares) {
                xxx <- as.vector(rbind(xx, xx, xx + res, xx + res))
                yyy <- as.vector(rbind(yy, fitted, fitted, yy))
                zzz <- as.vector(rbind(zz, zz, zz, zz))
                rgl.quads(xxx, yyy, zzz, color=surface.col[j], alpha=0.5, lit=FALSE)
                rgl.lines(xxx, yyy, zzz, color=surface.col[j])
              }
            }
          }
        }
        else {
          levs <- levels(groups)
          for (j in 1:length(levs)){
            group <- levs[j]
            select.obs <- groups == group
            mod <- switch(f,
                          linear = lm(y ~ x + z, subset=select.obs),
                          quadratic = lm(y ~ (x + z)^2 + I(x^2) + I(z^2), subset=select.obs),
                          smooth = if (is.null(df.smooth)) gam(y ~ s(x, z), subset=select.obs)
                          else gam(y ~ s(x, z, fx=TRUE, k=df.smooth), subset=select.obs),
                          additive = if (is.null(df.additive)) gam(y ~ s(x) + s(z), subset=select.obs)
                          else gam(y ~ s(x, fx=TRUE, k=df.additive[1]+1) +
                                     s(z, fx=TRUE, k=(rev(df.additive+1)[1]+1)), subset=select.obs)
            )
            if (model.summary) summaries[[paste(f, ".", group, sep="")]] <- summary(mod)
            yhat <- matrix(predict(mod, newdata=dat), grid.lines, grid.lines)
            if (fill) rgl.surface(vals, vals, yhat, color=surface.col[j], alpha=0.5, lit=FALSE)
            if (grid) rgl.surface(vals, vals, yhat, color=if (fill) grid.col
                                  else surface.col[j], alpha=0.5, lit=FALSE, front="lines", back="lines")
            rgl.texts(1, predict(mod, newdata=data.frame(x=1, z=1, groups=group)), 1,
                      paste(group, " "), adj=1, color=surface.col[j])
            if (residuals){
              yy <- y[select.obs]
              xx <- x[select.obs]
              zz <- z[select.obs]
              fitted <- fitted(mod)
              res <- yy - fitted
              rgl.lines(as.vector(rbind(xx,xx)), as.vector(rbind(yy,fitted)), as.vector(rbind(zz,zz)),
                        col=surface.col[j])
              if (squares) {
                xxx <- as.vector(rbind(xx, xx, xx + res, xx + res))
                yyy <- as.vector(rbind(yy, fitted, fitted, yy))
                zzz <- as.vector(rbind(zz, zz, zz, zz))
                rgl.quads(xxx, yyy, zzz, color=surface.col[j], alpha=0.5, lit=FALSE)
                rgl.lines(xxx, yyy, zzz, color=surface.col[j])
              }
            }
          }
        }
      }
    }
  }
  if (revolutions > 0) {
    for (i in 1:revolutions){
      for (angle in seq(1, 360, length.out=360/speed)) rgl.viewpoint(-angle, fov=fov)
    }
  }
  if (model.summary) return(summaries) else return(invisible(NULL))
}




Rcmdr.select3d <-
  function (...)
  {
    .check3d()
    rect <- rgl.select(...)
    llx <- rect[1]
    lly <- rect[2]
    urx <- rect[3]
    ury <- rect[4]
    if (llx > urx) {
      temp <- llx
      llx <- urx
      urx <- temp
    }
    if (lly > ury) {
      temp <- lly
      lly <- ury
      ury <- temp
    }
    proj <- rgl.projection()
    function(x, y, z) {
      pixel <- rgl.user2window(x, y, z, projection = proj)
      apply(pixel, 1, function(p) (llx <= p[1]) && (p[1] <=
                                                      urx) && (lly <= p[2]) && (p[2] <= ury) && (0 <= p[3]) &&
              (p[3] <= 1))
    }
  }
identify3d  <-
  function (x, y, z, axis.scales=TRUE, groups = NULL, labels = 1:length(x),
            col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
            offset = ((100/length(x))^(1/3)) * 0.02)
  {
    valid <- if (is.null(groups))
      complete.cases(x, y, z)
    else complete.cases(x, y, z, groups)
    labels <- labels[valid]
    x <- x[valid]
    y <- y[valid]
    z <- z[valid]
    minx <- min(x)
    maxx <- max(x)
    miny <- min(y)
    maxy <- max(y)
    minz <- min(z)
    maxz <- max(z)
    if (axis.scales){
      #                  lab.min.x <- nice(minx)
      #                  lab.max.x <- nice(maxx)
      #                  lab.min.y <- nice(miny)
      #                  lab.max.y <- nice(maxy)
      #                  lab.min.z <- nice(minz)
      #                  lab.max.z <- nice(maxz)


      lab.min.x <- minx
      lab.max.x <- maxx
      lab.min.y <- miny
      lab.max.y <- maxy
      lab.min.z <- minz
      lab.max.z <- maxz




      minx <- min(lab.min.x, minx)
      maxx <- max(lab.max.x, maxx)
      miny <- min(lab.min.y, miny)
      maxy <- max(lab.max.y, maxy)
      minz <- min(lab.min.z, minz)
      maxz <- max(lab.max.z, maxz)
      min.x <- (lab.min.x - minx)/(maxx - minx)
      max.x <- (lab.max.x - minx)/(maxx - minx)
      min.y <- (lab.min.y - miny)/(maxy - miny)
      max.y <- (lab.max.y - miny)/(maxy - miny)
      min.z <- (lab.min.z - minz)/(maxz - minz)
      max.z <- (lab.max.z - minz)/(maxz - minz)
    }
    x <- (x - minx)/(maxx - minx)
    y <- (y - miny)/(maxy - miny)
    z <- (z - minz)/(maxz - minz)
    rgl.bringtotop()
    identified <- character(0)
    groups <- if (!is.null(groups))
      as.numeric(groups[valid])
    else rep(1, length(x))
    repeat {
      f <- Rcmdr.select3d(button="right")
      which <- f(x, y, z)
      if (!any(which))
        break
      rgl.texts(x[which], y[which] + offset, z[which], labels[which],
                color = col[groups][which])
      identified <- c(identified, labels[which])
    }
    unique(identified)
  }

# iNZightModules
### Add-on modules for iNZight

This R package contains add-on modules for iNZight, each of which targets some particular application.
The modules connect to a related `R` package, which will be downloaded as it is needed (to reduce the size of the iNZightVIT download).

*******

The modules are as follows:

- __Quick Explore__ modules:  
  These include `allBivarPlots.R`, `allUniPlots.R`, and `scatterPlotMatrix.R`,
  which provide UI's for viewing all bivariate plots, all univariate plots, and
  generating a scatter plot matrix, respectively.

- __3D Plot__:  
  `plot3Dmode.R` - a UI for producing 3D plots of data, and `supportPlot3D.R` - support functions for the 3D plot.

- __Time Series__:  
  `iNZightTS.R` - a UI for visualising and doing basic forecasts of time series data.

- __Model Fitting__:  
  `modelFitting.R` - a UI for fitting ([survey] generalised) linear models to data, and generating model checking plots.

- __Multiple Response__:  
  `iNZightMultiRes.R` - a UI for visualising multiple response type data (e.g., check all that apply).

- __Maps__:  
  `iNZightMaps.R` - a UI for visualising geographical data.

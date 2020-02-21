library(tidyverse)
library(png)

# Function to create plots ------------------------------------------------

buildFan <- function(hue = 0.5, saturation = 0.20, value = 0.55, alpha = 0.5,
                     hue.range = 0, saturation.range = 0, value.range = 0, alpha.range = 0,
                     x.end = NULL, y.end = NULL, x.center = NULL, y.center = NULL,
                     random.center = FALSE, segment.width = 8, n.segments = 200,
                     depth = FALSE, rescale = FALSE){
  
  #function builds a single fan plot
  #random vectors are generated if no vectors are provided
  #hue, saturation, value, and alpha should be between 0:1
  
  # set the center points of the segments
  x.center <- if (!is.null(x.center)) x.center else if(random.center == FALSE) 0 else if(is.null(x.end)) runif(1, -1, 1) else runif(1, min(x.end), max(x.end))
  y.center <- if (!is.null(y.center)) y.center else if(random.center == FALSE) 0 else if(is.null(y.end)) runif(1, -1, 1) else runif(1, min(y.end), max(y.end))
  
  # set the end of the segments. If vectors are provided then add some randomness
  x.end <- if(is.null(x.end)) rnorm(n.segments, 0, 1) else x.end * runif(n.segments, 0.3)
  y.end <- if(is.null(y.end)) rnorm(n.segments, 0, 1) else y.end * runif(n.segments, 0.3)
  
  # set the hue, saturation, value, and alpha per segment
  generate.variables <- function(variable, variable.range){
    runif(n = n.segments,
          min = max(0, variable - variable.range),
          max = min(1, variable + variable.range))
  }
  
  h <- generate.variables(hue, hue.range)
  s <- generate.variables(saturation, saturation.range)
  v <- generate.variables(value, value.range)
  alpha <- generate.variables(alpha, alpha.range)
  
  lwd <- segment.width * runif(n.segments)
  
  if (depth) {
    # reorganize the segments to give them depth. Layer the segments by length and then increase the color strength
    # effect should be the longer ones look like they're further back, darker, less colorful, thiner
    # shorter ones will be brighter, more colorful, thicker
    
    # sort vector by eucledian distance from their center (longest first)
    vector.order <- sqrt(abs(x.end - x.center) ^ 2 + abs(y.end - y.center) ^ 2) %>% order(decreasing = TRUE)
    x.end <- x.end[vector.order]
    y.end <- y.end[vector.order]
    
    # sort the colors by strength
    # leave out hsv so there is still some color variance per plot
    # h <- sort(h)
    # s <- sort(s)
    # v <- sort(v)
    alpha <- sort(alpha)
    lwd <- sort(lwd)
  }
  
  #rescale the x, y vectors so they're between -1 and 1
  if (rescale){
    rescale.vector <- function(x){(x - min(x))/(max(x) - min(x)) * 2 - 1}
    x.end <- rescale.vector(x.end)
    y.end <- rescale.vector(y.end)
  }
  
  #set graphical parameters
  par(mar = rep(1, 4),
      bty = "n")
  
  # draw the plot template
  plot(x = 0,
       y = 0,
       xlim = c(-1, 1),
       ylim = c(-1, 1),
       type = 'n',
       xaxt = 'n',
       yaxt = 'n',
       ann = FALSE)
  
  # draw the segments
  segments(x0 = x.center,
           y0 = y.center,
           x1 = x.end,
           y1 = y.end,
           col = hsv(h = h,
                     s = s,
                     v = v,
                     alpha = alpha),
           lwd = lwd)
}


# function wraps around buildFan to automate the printing of multiple plots ----------------------------------------------------

printFans <- function(hsv.array = seq(0, 1, length.out = 100), filename = "fans.png",
                      output.image.width = 4608, output.image.height = 2880, background = "white", ...){
  
  # function automates the printing and saving of buildFan
  # hsv.array is the array colors to map the buildFan function over. If only one vector is provided then it is
  # assumed to be hue. If two, then hue and saturation. If three, hue then saturation then value
  # length / nrow of hsv.array determines how many plots there will be
  # n.segments is number of segments per plot
  
  # n.plots is the number of plots to be printed determined by the length/nrow of hsv.array
  n.plots <- if (!is.null(nrow(hsv.array))) nrow(hsv.array) else length(hsv.array)
  
  if (sqrt(n.plots) %% 1 != 0) stop("Square root of the number of plots must be a whole number.")
  
  # set where to save
  png(filename = filename,
      width = output.image.width,
      height = output.image.height)
  
  # set graphical parameters
  par(bg = background,
      mar = rep(2, 4),
      mfrow = c(sqrt(n.plots), sqrt(n.plots)),
      bty = "n")
  
  #ensure hsv list to map over is dataframe and rename columns
  hsv.array <- as.data.frame(hsv.array)
  names(hsv.array) <- c("hue", "saturation", "value")[1:ncol(hsv.array)]
  
  # run the function over the hsv.array, saves plots
  pwalk(hsv.array, buildFan, ...)
  
  # reset graphics
  invisible(dev.off())
  
}

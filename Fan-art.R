library(tidyverse)
library(png)

#origin article here: http://www.r-graph-gallery.com/57-rays-abstract-painting/

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

# test the fan functions --------------------------------------------------

# build a single fan
buildFan()

# build a panel of fans and save it
printFans()


# build different designs -------------------------------------------------

png(filename = paste0('Plotted_images/singlefan.png', name, '.png'),
    width = 1000,
    height = 1000)
buildFan()
dev.off()

png(filename = paste0('Plotted_images/singlefanDepth.png', name, '.png'),
    width = 1000,
    height = 1000)
buildFan(n.segments = 1000,
         x.end = runif(1000, -1, 1),
         y.end = runif(1000, -1, 1),
         x.center = rep(0, 1000),
         y.center = rep(0, 1000),
         alpha = 0.3,
         depth = TRUE)
dev.off()

#random
buildFan(n.segments = 1000,
         x.end = runif(1000, -1, 1),
         y.end = runif(1000, -1, 1),
         x.center = rep(0, 1000),
         y.center = rep(0, 1000),
         alpha = 0.4,
         depth = TRUE)



# define the colors to map the function over
n.plots <- 9^2
hues <- tibble(hue = seq(0.35, 0.55, length = n.plots),
               saturation = seq(0.25, 0.1, length = n.plots))

# specific colors with random segments
printFans(hsv.array = hues,
          n.segments = 250,
          depth = TRUE,
          hue.range = 0.5,
          saturation.range = 0.10,
          value = 0.45,
          value.range = 0.20,
          alpha = 0.3,
          alpha.range = 0.30,
          segment.width = 10,
          random.center = TRUE)

# semi circle
n.segments <- 150
base.vector <- seq(0, pi, length = n.segments)
x.vector <- cos(base.vector)
y.vector <- sin(base.vector) * 2 - 1
printFans(hsv.array = hues,
          n.segments = n.segments,
          x.end = x.vector,
          y.end = y.vector,
          x.center = 0,
          y.center = -1,
          hue.range = 0.20,
          saturation.range = 0.20,
          value.range = 0.20,
          alpha.range = 0.40,
          alpha = 0.3,
          segment.width = 20)

# circle
n.segments <- 200
base.vector <- seq(0, pi*2, length = n.segments)
x.vector <- cos(base.vector) * 2
y.vector <- sin(base.vector) * 2
printFans(hsv.array = hues,
          n.segments = n.segments,
          x.end = x.vector,
          y.end = y.vector,
          hue.range = 0.30,
          saturation.range = 0.20,
          value.range = 0.40,
          alpha.range = 0.40,
          alpha = 0.3,
          output.image.width = 2500,
          output.image.height = 2500,
          depth = TRUE,
          background = "seashell2")


# create images for gif ---------------------------------------------------

name <- 1
for (i in c(1:40, 39:1)){
  n <- floor(i^2)
  png(filename = paste0('Plotted_images/Gif/fan', name, '.png'),
      width = 500,
      height = 500)
  buildFan(hue = i/40, 
           saturation = 1- i/40, 
           value = 0.5, 
           alpha = 1 - (i/40),
           n.segments = n,
           x.end = runif(n, -1, 1),
           y.end = runif(n, -1, 1),
           x.center = rep(0, n),
           y.center = rep(0, n),
           depth = TRUE)
  dev.off()
  name <- name + 1
}

# now in GIMP, file-> open 'fan1.png' -> Image->Mode->RBG->File->
# Open as layers-> select the rest of the files -> File -> export as -> 'name'.gif

n.plots <- 80
hues <- seq(0, 1, length = 12)
logis.curve <- 1 / ( 1 + exp(seq(3, -3, length = n.plots/2)))
logis.curve <- c(logis.curve, rev(logis.curve))

name <- 1

for (i in 1:n.plots){
  # set where to save
  png(filename = paste0('Plotted_images/multiGif/fan', name, '.png'),
      width = 1000,
      height = 1000)
  
  # set graphical parameters
  par(bg = 'white',
      mar = rep(2, 4),
      mfrow = c(3, 4),
      bty = "n")

  
  for (j in 1:12) {
    n <-  floor(logis.curve[i] * 100)
    
    if (i %% j == 0){
      buildFan(alpha = 0)
      next
    }
    
    # build the plot
    buildFan(
      hue = hues[j],
      saturation = 1 - logis.curve[i],
      value = 0.5,
      alpha = 1 - logis.curve[i],
      n.segments = n,
      x.end = runif(n,-1, 1),
      y.end = runif(n,-1, 1),
      x.center = rep(0, n),
      y.center = rep(0, n),
      depth = TRUE
    )
  }
    
  # save it
  dev.off()
  name <- name + 1
}




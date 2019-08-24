library(tidyverse)
library(png)

#origin article here: http://www.r-graph-gallery.com/57-rays-abstract-painting/

# Function to create plots ------------------------------------------------

buildFan <- function(hue = 0.5, saturation = 0.20, value = 0.55, alpha = 0.5,
                     hue.range = 0, saturation.range = 0, value.range = 0, alpha.range = 0,
                     x.end = NULL, y.end = NULL, x.center = NULL, y.center = NULL,
                     random.center = FALSE, segment.width = 8, n.segments = 150, depth = FALSE){
  
  #function builds a single fan plot
  #random vectors are generated if no vectors are provided
  #hue, saturation, value, and alpha should be between 0:1
  
  # set the center points of the segments
  x.center <- if (!is.null(x.center)) x.center else if(random.center == FALSE) 0 else if(is.null(x.end)) runif(1, -1, 1) else runif(1, min(x.end), max(x.end))
  x.center <- if (!is.null(x.center)) x.center else rep(x.center, n.segments)
  y.center <- if (!is.null(y.center)) y.center else if(random.center == FALSE) 0 else if(is.null(y.end)) runif(1, -1, 1) else runif(1, min(y.end), max(y.end))
  y.center <- if (!is.null(y.center)) y.center else rep(y.center, n.segments)
  
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
    # h <- sort(h) # leave out hue so there is still some color variance per plot
    s <- sort(s)
    v <- sort(v)
    alpha <- sort(alpha)
    lwd <- sort(lwd)
  }
  
  # set the base vector for determining the plot size
  x <- if(is.null(x.end)) seq(-1, 1, length = n.segments) else x.end
  y <- if(is.null(y.end)) seq(-1, 1, length = n.segments) else y.end
  
  # draw the plot template
  plot(x, y,
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
                     alpha = alpha
                     ),
           lwd = lwd)
}


# function wraps around buildFan to automate the printing of multiple plots ----------------------------------------------------

printFans <- function(hsv.array = seq(0, 1, length.out = 100), image = FALSE, filename = "fans.png",
                      output.image.width = 4608, output.image.height = 2880, avg.block.width = 4, ...){
  
  # function automates the printing and saving of buildFan
  # hsv.array is the array colors to map the buildFan function over. If only one vector is provided then it is
  # assumed to be hue. If two, then hue and saturation. If three, hue then saturation then value
  # length / nrow of hsv.array determines how many plots there will be
  # image is a logical. If true then hsv.array should be an image that is then converted to an hsv.array
  # n.segments is number of segments per plot
  # avg.block.width is the width (in pixels) that will averaged to reduce the resolution of the image
  # it should be divisble by width and height
  
  # n.plots is the number of plots to be printed determined by the length/nrow of hsv.array
  n.plots <- if (!is.null(nrow(hsv.array))) nrow(hsv.array) else length(hsv.array)
  
  if (sqrt(n.plots) %% 1 != 0) stop("Square root of the number of plots must be a whole number. If trying an image then check that the pixel dimensions are square and each side has a sqrt.")
  
  # set where to save
  png(filename = filename,
      width = output.image.width,
      height = output.image.height)
  
  # if statement transforms image into an HSV array
  if(image){
    
    image.dim <- dim(hsv.array)
    
    if (image.dim[[1]] != image.dim[[2]]) stop("Picture resolution must be square")
    
    # reshape image into a data frame
    rgbDF <- data.frame(red = matrix(hsv.array[,,1], ncol = 1),
                     green = matrix(hsv.array[,,2], ncol = 1),
                     blue = matrix(hsv.array[,,3], ncol = 1)
    )
    
    # convert colors from rgb to hsv
    hsvDF <- rgb2hsv(r = rgbDF$red,
                     g = rgbDF$green,
                     b = rgbDF$blue, maxColorValue = 1) %>%
      t() %>%
      as_tibble()
    
    # add col and row identifiers
    hsvDF$col <- 1:image.dim[2]
    hsvDF$row <-lapply(1:image.dim[1], function(x){rep(x, image.dim[1])}) %>% unlist()
    
    # flip the rows (image otherwise flipped horizontally)
    hsvDF$row <- image.dim[2] + 1 - hsvDF$row
    
    # rotate image clockwise 90d
    hsvDF$col2 <- image.dim[1] + 1 - hsvDF$row
    hsvDF$row2 <- hsvDF$col
    
    # reduce resolution of the image by averaging values across the avg.block.width
    hsv.array <- hsvDF %>%
      mutate(col = col2,
             row = row2) %>%
      select(-col2, -row2) %>%
      mutate(col.group = ceiling(col / avg.block.width),
             row.group = ceiling(row / avg.block.width)) %>%
      group_by(col.group, row.group) %>%
      summarize(h = mean(h),
                s = mean(s),
                v = mean(v)) %>%
      ungroup() %>%
      arrange(row.group, col.group) %>%
      select(h, s, v)
    
    # set graphical parameters for the image
    par(bg = "seashell",
        mar = rep(0.04, 4),
        mfrow = c(image.dim[1] / avg.block.width,
                  image.dim[2] / avg.block.width),
        bty = "n")
    
    hsv.array
    
  } else{
    
    # set graphical parameters if it isn't an image
    par(bg = "grey15",
        mar = rep(2, 4),
        mfrow = c(sqrt(n.plots), sqrt(n.plots)),
        bty = "n")
  }
  
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

# Build different designs --------------------------------------

#
buildFan(n.segments = 1000,
          x.end = runif(1000, 0, 3),
          y.end = runif(1000, 0, 3),
          x.center = rep(0, 1000),
          y.center = rep(0, 1000))

# define the colors to map the function over
n.plots <- 100
hues <- tibble(hue = seq(0.10, 0.90, length = n.plots),
               saturation = seq(0.40, 0.15, length = n.plots))

# random
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
          

# random lengths
n.segments <- 150
x.vector <- rnorm(n.segments, 0.5, 1)
y.vector <- rnorm(n.segments, 0.5, 1)
printFans(hsv.array = hues,
          n.segments = n.segments,
          x = x.vector,
          y = y.vector,
          hue.range = 0.35,
          saturation.range = 0.20,
          value.range = 0.40,
          alpha.range = 0.50)

# semi circle
n.segments <- 100
base.vector <- seq(0, pi, length = n.segments)
x.vector <- cos(base.vector)
y.vector <- sin(base.vector)
printFans(hsv.array = hues,
          n.segments = n.segments,
          x.end = x.vector,
          y.end = y.vector,
          hue.range = 0.30,
          saturation.range = 0.20,
          value.range = 0.40,
          alpha.range = 0.40)

# circle
n.segments <- 100
base.vector <- seq(0, pi*2, length = n.segments)
x.vector <- cos(base.vector)
y.vector <- sin(base.vector)
printFans(hsv.array = hues,
          n.segments = n.segments,
          x.end = x.vector,
          y.end = y.vector,
          hue.range = 0.30,
          saturation.range = 0.20,
          value.range = 0.40,
          alpha.range = 0.40,
          output.image.width = 5000,
          output.image.height = 5000,
          depth = TRUE)

# diamond
n.segments <- 100
x.vector <- rep(seq(0, pi, length = n.segments / 2), 2)
x.vector <- x.vector - mean(x.vector) #rescale so center is 0
y.vector <- c(seq(pi / 2, 0, length = n.segments / 4),
              seq(0, pi, length = n.segments / 2),
              seq(pi, pi / 2, length = n.segments / 4))
y.vector <- y.vector - mean(y.vector) #rescale so center is 0
printFans(hsv.array = hues,
          n.segments = n.segments,
          x.end = x.vector,
          y.end = y.vector,
          hue.range = 0.30,
          saturation.range = 0.20,
          value.range = 0.40,
          alpha.range = 0.40,
          output.image.width = 5000,
          output.image.height = 5000)

# S curve
n.segments <- 100
x.vector <- seq(-1, 1, length.out = n.segments)
# x.vector <- x.vector - mean(x.vector) #rescale so center is 0
y.vector <- pnorm(x.vector, 0, .4)
# y.vector <- y.vector - mean(y.vector) #rescale so center is 0
buildFan(x.end = x.vector,
          y.end = y.vector,
          n.segments = n.segments)
buildFan(x.end = x.vector,
          x.center = x.vector + 0.1,
          y.end = y.vector,
          y.center = y.vector)
printFans(x = x.vector,
          y = y.vector,
          random.center = TRUE)

#issue is that the end of the segments need to be define by a similar function but offset;
  # not by the random inputs currently defining it


# image -------------------------------------------------------------------

# load the PNG into an RGB image object
image <- readPNG("Spanish_moss.png")

#print it
printFans(hsv.array = image,
          image = TRUE,
          output.image.width = 5000,
          output.image.height = 5000,
          avg.block.width = 10,
          random.center = TRUE,
          segment.width = 4,
          alpha.range = 0.3)

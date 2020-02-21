source('Functions.R')

#origin article here: http://www.r-graph-gallery.com/57-rays-abstract-painting/

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
         alpha = 0.7,
         depth = TRUE,
         segment.width = 1.5)
dev.off()

#random
buildFan(n.segments = 1000,
         x.end = runif(1000, -1, 1),
         y.end = runif(1000, -1, 1),
         x.center = rep(0, 1000),
         y.center = rep(0, 1000),
         depth = TRUE,
         segment.width = 3,
         hue.range = 0.5, 
         saturation.range = 0.3, 
         value = 0.4,
         value.range = 0.5,
         alpha = 0.1,
         alpha.range = 0.7)



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
# note requires imagemagick to be installed: https://imagemagick.org/
#  or brew install imagemagick 
setwd("Plotted_images/Gifs")

# gif of single fan
png(filename = paste0('fan%02d.png'),
    width = 500,
    height = 500)
for (i in c(1:40, 39:1)){
  n <- floor(i^2)
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
}
dev.off()
system("convert -delay 20 *.png single_fan.gif") # this calls imagemagik via shell
file.remove(list.files(pattern = ".png"))


# gif of single fan with depth and variation

# gif parameters
n.frames <- 100
logis.curve <- 1 / ( 1 + exp(seq(4, -5, length = n.frames/2)))
logis.curve <- c(logis.curve, rev(logis.curve))
rescale.vector <- function(x, min = .1, max = .9) (x - min(x))/(max(x) - min(x)) * (max - min) + min

# set where to save
png(filename = paste0('fan%02d.png'),
    width = 1250,
    height = 1250)
for (i in 1:n.frames){
  n <- floor(logis.curve[i]*250)
  buildFan(hue = rescale.vector(logis.curve)[i], 
           saturation = .6, 
           value = 0.5, 
           hue.range = 0.20,
           saturation.range = 0.20,
           value.range = 0.20,
           alpha =  1 - rescale.vector(logis.curve)[i],
           alpha.range = 0.10,
           segment.width = 6,
           n.segments = n,
           x.end = runif(n, -1, 1),
           y.end = runif(n, -1, 1),
           x.center = rep(0, n),
           y.center = rep(0, n),
           depth = TRUE
  )
}
dev.off()
system("convert -delay 20 *.png depth_fan.gif")
file.remove(list.files(pattern = ".png"))


# gif of semi-circle fan

# set where to save
png(filename = paste0('fan%02d.png'),
    width = 1250,
    height = 800)

for (i in 1:n.frames){
  n <- floor(logis.curve[i]*400)
  
  # semi circle
  base.vector <- seq(0, pi, length = n)
  x.vector <- cos(base.vector)
  y.vector <- sin(base.vector) * 2 - 1
  
  buildFan(hue = rescale.vector(logis.curve, min = 0.3, max = .6)[i], 
           saturation = .45, 
           value = 0.4, 
           hue.range = 0.20,
           saturation.range = 0.20,
           value.range = 0.20,
           alpha =  1 - rescale.vector(logis.curve, min = .6, max = .95)[i],
           alpha.range = 0.30,
           segment.width = 20,
           n.segments = n,
           x.end = x.vector,
           y.end = y.vector,
           x.center = 0,
           y.center = -1
  )
}
dev.off()
system("convert -delay 20 *.png semi_fan.gif")
file.remove(list.files(pattern = ".png"))


# gif of many fans with sequencing pattern
# gif parameters
n.frames <- 80
hues <- rep(seq(0, 1, length = 12), 12)
logis.curve <- 1 / ( 1 + exp(seq(4, -4, length = n.frames/2)))
logis.curve <- c(logis.curve, rev(logis.curve))

# set where to save
png(filename = 'fan%02d.png',
    width = 1250,
    height = 1250)

for (i in 1:n.frames){

  # set graphical parameters
  par(bg = 'white',
      mar = rep(2, 4),
      mfrow = c(3, 4),
      bty = "n")
  
  for (j in 1:12) {
    n <-  floor(logis.curve[i] * 250)
    
    # logic controls the plot sequence
    # if (i %% 12 == j | i %% 6 == j | i %% 3 == j){
    if (i %% 12 == j | i %% 12 == j - 1 | i %% 12 == j - 2 | i %% 12 == j - 3){
      buildFan(
        hue = hues[j + i],
        saturation = .7,
        value = 0.5,
        alpha = 1 - logis.curve[i],
        n.segments = n,
        x.end = runif(n, -1, 1),
        y.end = runif(n, -1, 1),
        # x.center = rep(0, n),
        # y.center = rep(0, n),
        depth = TRUE,
        random.center = TRUE
      )
      next
    }
    
    # build a blank plot
    buildFan(alpha = 0)
  }
}
dev.off()
system("convert -delay 20 *.png multi_fan.gif")
file.remove(list.files(pattern = ".png"))

# reset the working directory
setwd(normalizePath('../..'))



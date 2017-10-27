
## -----------------------------------------------
## Just another random walk - Pollock style! ;-)
##
## by João Gonçalves
##
## Use, enhance and/or distribute this code freely
## -----------------------------------------------


require(RColorBrewer)


# Global parameters

# Output folder where images will be placed
# Change this according to your system
outFolder <- "./Algo1/OUT"


# Window/canvas size
xmin <- 0
xmax <- 100
ymin <- 0
ymax <- 100

# Number of drawing steps
nSteps <- 1000

# Number of individual drawings (each round will generate a diferent image file)
nTries <- 10

# Random start/ starting point
x0 <- runif(1,xmin,xmax)
y0 <- runif(1,ymin,ymax)

# Color palettes from RColorBrewer
# This will be randomly selected at each drawing
colorPalettes <- brewer.pal.info
numColorPalettes <- nrow(colorPalettes)

# Define min, max and step line widths
# This will be used to randomly vary the line width
lineWidths <- seq(0.5,10,by=0.5)

# Select one option
# This will affect the 2D distribution of lines in the
# drawing space/canvas
#
# Available options are:
# 1 - uniform distribution
# 2 - bivariate Gaussian
#
optionSet <- 2


# Depending on the option selected above you have to choose
# the related parameters of the distributions
#
#
# ----- Option #1 ----- #
# Parameters
#
# Multipliers
useMult <- FALSE
mult <- seq(1,10,by=0.1)
fuzzyMultiplier <- c(0,0.5,1)



# ----- Option #2 ----- #
# Parameters
#
x_meanVal <- 50
x_sdVal <- 20
y_meanVal <- 50
y_sdVal <- 20
sortIt <- FALSE

# -------------------------------------------------------------------------------------- #
# Draw it!
# -------------------------------------------------------------------------------------- #



for(nT in 1:nTries){
  
  cat("Performing trial #",nT,"...................")
  
  # Start the device
  # NOTE! Don't forget to change the target folder according to your local system
  #
  jpeg(paste(outFolder,"/rand_walk_",nT,".jpg",sep=""),width=2500,height=2500,res=300,quality=100)
  
  
  # Start the plot
  #
  plot(0,xlim=c(xmin,xmax),ylim=c(ymin,ymax),type="n",axes=FALSE,xlab="",ylab="")
  
  
  # Generate data for option #2
  if(optionSet==2){
    
    xValues <- sample(rnorm(nSteps,x_meanVal,x_sdVal),nSteps) 
    yValues <- sample(rnorm(nSteps,y_meanVal,y_sdVal),nSteps)
    
    if(sortIt){
      xValues <- sort(xValues)
      yValues <- sort(yValues)
    }
  }
  
  
  # Color palette generation
  pal_n <- sample(1:numColorPalettes,1)
  pal <- rownames(colorPalettes)[pal_n]
  pal_ncolors <- colorPalettes[pal_n,1]
  mypalette <- brewer.pal(pal_ncolors,pal)
  
  
  # ---------------- #
  # Drawing steps
  # ---------------- #
  
  
  for(i in 1:nSteps){
    
    if(optionSet==1){
      # Get the coordinates
      x1 <- runif(1,xmin,xmax)*ifelse(useMult,sample(fuzzyMultiplier,1)*sample(mult,1),1)
      y1 <- runif(1,ymin,ymax)*ifelse(useMult,sample(fuzzyMultiplier,1)*sample(mult,1),1)
      
    }else if(optionSet==2){
      # Get coordinates when using option #2
      x1 <- xValues[i]  
      y1 <- yValues[i]
    }else{
      stop("Unknown option!")
    }
    
    # Select color and line width
    lineColor <- sample(mypalette,1)
    lineWidth <- sample(lineWidths,1)
    
    # Do plot
    lines(c(x0,x1),c(y0,y1),col=lineColor,lwd=lineWidth)
    
    # Store the last point coordinates
    x0 <- x1
    y0 <- y1
    
  }
  
  dev.off()
  
  cat("done.\n\n")
  
}




## -------------------------------------------------------
## Just another random walk - uses straight line segments
##
## by João Gonçalves
##
## Use, enhance and/or distribute this code freely
## -------------------------------------------------------


# Canvas/window size
mSize<-c(xmax=200, ymax=200)

# Number of drawing rouns
nRounds <- 100000

# Starting point coordinates
startPoint.x <- 100
startPoint.y <- 100

# Line color and width
lineCol <- "red"
lineWidth <- 1

# Put a frame around the drawing?
useBox <- FALSE


png(filename = "./algo2/OUT/rWalk-1.png", res=300, width = 2500, height = 2500)

# Initiate the plot 
plot(0,type="n",xlim=c(0,mSize[1]),ylim=c(0,mSize[2]),xlab="",ylab="",xaxt="n",axes=FALSE)

# -------------------------------------------------------------------------------------- #
# Draw it!
# -------------------------------------------------------------------------------------- #


for(i in 1:nRounds){
  
  selDir<-1:4
  
  repeat{
  
    dir<-sample(selDir,1)
    
    if(dir==1){
      nextPoint.x <- startPoint.x
      nextPoint.y <- startPoint.y + 1
    }else if(dir==2){
      nextPoint.x <- startPoint.x + 1
      nextPoint.y <- startPoint.y
    }else if(dir==3){
      nextPoint.x <- startPoint.x
      nextPoint.y <- startPoint.y - 1
    }else if(dir==4){
      nextPoint.x <- startPoint.x - 1 
      nextPoint.y <- startPoint.y
    }else{
      stop()
    }
    
    if(nextPoint.x>=0 && nextPoint.x<=mSize[1] && nextPoint.y>=0 && nextPoint.y<=mSize[2]){
      break
    }else{
      selDir <- selDir[-dir]
	    #cat("Get back!\n\n")
    }
    
    
  }

  lines(c(startPoint.x, nextPoint.x),c(startPoint.y, nextPoint.y), col=lineCol, lwd=lineWidth)
  startPoint.x <- nextPoint.x
  startPoint.y <- nextPoint.y
  
  #print(c(nextPoint.x,nextPoint.y))

}

if(useBox) box()

dev.off()



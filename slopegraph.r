# via FlowingData
# Make it a function for easier usage
#
slopegraph <- function(startpts, endpts, labels) {
        
        x0 <- c()
        y0 <- c()
        x1 <- c()
        y1 <- c()
        
        startyear <- 1
        stopyear <- 2
        xoffset <- 0.1
        yoffset <- 0
        ystartprev <- 0
        ystopprev <- 0
        ythreshold <- ( max(startpts) - min(startpts) ) * 0.025
        
        for (i in length(startpts):1) {
                
                ystartdiff <- (startpts[i]+yoffset) - ystartprev
                if (abs(ystartdiff) < ythreshold) {
                        yoffset <- yoffset + (ythreshold-ystartdiff)
                }
                
                # Calculate slope
                slope <- (endpts[i] - startpts[i]) / (stopyear - startyear)
                
                # Intercept
                intercept <- startpts[i] + yoffset
                
                # Start and stop coordinates for lines
                ystart <- intercept
                ystop <- slope * (stopyear-startyear) + intercept
                ystopdiff <- ystop - ystopprev
                if (abs(ystopdiff) < ythreshold) {
                        yoffset <- yoffset + (ythreshold-ystopdiff)
                        intercept <- startpts[i] + yoffset
                        ystart <- intercept
                        ystop <- slope * (stopyear-startyear) + intercept
                }
                
                # Draw the line for current country
                x0 <- c(x0, startyear)
                y0 <- c(y0, ystart)
                x1 <- c(x1, stopyear)
                y1 <- c(y1, ystop)
                
                
                ystartprev <- ystart
                ystopprev <- ystop
        }
        
        ymin <- min(c(startpts, endpts))
        ymax <- max(c(startpts, endpts)) + yoffset
        
        par(family="serif", mar=c(0,0,0,0))
        plot(0, 0, type="n", main="", xlab="", ylab="", xlim=c(.5,2.5), ylim=c(ymin,ymax*1.1), bty="n", las=1, axes=FALSE)
        segments(x0, y0, x1, y1)
        text(x0, y0, rev(startpts), pos=2, cex=0.8)
        text(x0-xoffset, y0, rev(labels), pos=2, cex=0.8)
        text(x1, y1, rev(endpts), pos=4, cex=0.8)
        text(x1+xoffset, y1, rev(labels), pos=4, cex=0.8)
        
        # Year labels
        text(startyear, ymax*1.1, deparse(substitute(startpts)), cex=0.8, pos=2, offset=1)
        text(stopyear, ymax*1.1, deparse(substitute(endpts)), cex=0.8, pos=4, offset=0.5)
}



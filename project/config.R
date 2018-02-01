##----------------------------------------------------------------------
## Report settings
library(knitr)
opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    fig.width = 9,
    fig.height = 4,
    dev.args = list(family = "Palatino"))

##----------------------------------------------------------------------
## Load the lattice packages and customize outputs graphics
library(lattice)
library(latticeExtra)
library(grid)

## http://www.magesblog.com/2013/04/how-to-change-alpha-value-of-colours-in.html
add.alpha <- function(col, alpha = 1){
    apply(sapply(col, col2rgb)/255, 2,
          function(x) rgb(x[1], x[2], x[3], alpha = alpha))
}

## Define Colors
mycol <- c(1, "#377EB8", "#E41A1C", "#4DAF4A",
           "#ff00ff", "#FF7F00", "#984EA3", "#FFFF33", "#808080")
myreg <- colorRampPalette(c(mycol[3],  "gray90", mycol[2]))(100)

## Trellis graphical style.
ps <- list(
    superpose.symbol = list(
        col = mycol, pch = 1,
        fill = add.alpha(mycol, alpha = 0.4)),
    box.rectangle = list(col = 1, fill = c("gray70")),
    box.umbrella = list(col = 1, lty = 1),
    box.dot = list(pch = "|"),
    dot.symbol = list(col = 1, pch = 19),
    dot.line = list(col = "gray50", lty = 3),
    plot.symbol = list(col = 1),
    plot.line = list(col = 1),
    plot.polygon = list(col = "gray95"),
    superpose.line = list(col = mycol, lty = 1),
    superpose.polygon = list(col = mycol),
    strip.background = list(col = c("gray90", "gray70")),
    regions = list(col = myreg),
    par.sub.text = list(
        font = 1, just = "left", cex = 0.9,
        x = grid::unit(10, "mm"))
    )

trellis.par.set(ps)

## Alternative settings
ac <- list(pad1 = 0.5, pad2 = 0.5, tck = 0.5)
ps2 <- list(
    layout.widths = list(
        left.padding = 0.25,
        right.padding = -1,
        ylab.axis.padding = 0),
    layout.heights = list(
        bottom.padding = 0.25,
        top.padding = 0,
        axis.xlab.padding = 0,
        xlab.top = 0),
    axis.components = list(
        bottom = ac, top = ac,
        left = ac, right = ac)
)

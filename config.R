#-----------------------------------------------------------------------
# Setup (general settings for outputs)
library(knitr)
library(xtable)

options(digits = 3, OutDec = ",",
        xtable.caption.placement = "top",
        xtable.booktabs = TRUE,
        xtable.sanitize.text.function = identity)
opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    echo = FALSE,
    results = "hide",
    ## dev = "tikz",
    fig.width = 7,
    fig.height = 5,
    out.width = "\\textwidth",
    fig.align = "center",
    fig.pos = "!htb",
    dev.args = list(family = "Palatino"))

#-----------------------------------------------------------------------
# Load the lattice packages and customize outputs graphics
library(lattice)
library(latticeExtra)
library(grid)

trellis.par.set(canonical.theme(color = FALSE))
ac <- list(pad1 = 0.5, pad2 = 0.65, tck = 0.5)
trellis.par.set(
    reference.line = list(col = "gray90"),
    strip.background = list(col = c("gray85", "gray75")),
    box.rectangle = list(col = 1, fill = c("gray80")),
    box.umbrella = list(col = 1, lty = 1),
    par.sub.text = list(
        font = 1, just = "left", cex = 0.9,
        x = grid::unit(10, "mm")),
    box.dot = list(pch = "|"),
    plot.polygon = list(col = "gray90"),
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

# Objects to show custom legends
ltys <- trellis.par.get("superpose.line")$lty
pchs <- trellis.par.get("superpose.symbol")$pch
cols <- trellis.par.get("superpose.polygon")$col

panel.beeswarm <- function (x, y, subscripts, spread, ...) {
    xx <- x
    yy <- y
    aux <- by(cbind(yy, xx, subscripts),
              INDICES = xx,
              FUN = function(i) {
                  or <- order(i[, 1])
                  ys <- i[or, 1]
                  yt <- table(ys)
                  dv <- sapply(unlist(yt), FUN = function(j) {
                      seq(from = 1, to = j, length.out = j) - (j + 1)/2
                  })
        if (!is.list(dv)) {
            dv <- as.list(dv)
        }
        xs <- i[or, 2] + spread * do.call(c, dv)
        cbind(x = xs, y = ys, subscripts = subscripts[or])
    })
    aux <- do.call(rbind, aux)
    panel.xyplot(aux[, 1], aux[, 2], subscripts = aux[, 3], ...)
}

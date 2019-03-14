#-----------------------------------------------------------------------
# Load packages and general customizations
## ---- setup

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(lattice)
library(latticeExtra)
source("../config.R")

#-----------------------------------------------------------------------
# Explain dispersion through point process.
## ---- processo-pontual

mygrid <- expand.grid(xc = 1:3, yc = 1:3)
mygrid <- data.frame(mygrid)
sp::coordinates(mygrid) <- ~xc + yc

set.seed(20124689)
equi <- sp::spsample(mygrid, n = 100, type = "random")
unde <- sp::spsample(mygrid, n = 100, type = "stratified")
over <- sp::spsample(mygrid, n = 100, type = "clustered",
                     nclusters = 20)
## unde <- sp::spsample(mygrid, n = 100, type = "aligned")

coords <- purrr::map_dfr(
    .x = list("equi" = equi, "over" = over, "unde" = unde),
    .id = "caso",
    .f = function(x) {
        colnames(x@coords) <- c("x", "y")
        as.data.frame(x@coords)
    })

col <- "gray50"
xyplot(y ~ x | caso,
       layout = c(NA, 1),
       as.table = TRUE,
       pch = 19,
       xlab = "",
       ylab = "",
       scales = list(
           y = list(draw = FALSE),
           x = list(at = pretty(coords$x),
                    labels = rep("", length(pretty(coords$x))))
       ),
       strip = strip.custom(
           factor.levels = c("Equidispersion",
                             "Overdispersion",
                             "Underdispersion")),
           # factor.levels = c("Equidispersão",
           #                   "Superdispersão",
           #                   "Subdispersão")),
       panel = function(x, y, ...) {
           panel.grid(h = 10, v = 10, col = col, lty = 2)
           panel.xyplot(x, y, ...)
       },
       data = coords)
subs <- paste0("(", letters[1:3], ")")
poss <- c(0, 1, 2)/3 + 0.02
purrr::walk(seq(poss), function(i) {
    grid.text(subs[i], x = poss[i], y = .03, gp = gpar(cex = .9))
})

#-----------------------------------------------------------------------
# Descriptive analysis of the cotton bools data set
## ---- desc-cotton

cotton <- read_delim("../data/cotton.txt", delim = "\t") %>%
    mutate(est = fct_inorder(factor(est)))
cotton

xyplot(ncap ~ des | est,
       data = cotton,
       layout = c(NA, 1),
       as.table = TRUE,
       type = c("p", "smooth", "g"),
       xlab = "Níveis de desfolha artificial",
       ylab = "Número de capulhos",
       spread = 0.05,
       panel = panel.beeswarm)

## ---- desc-cotton2
cotton <- read_delim("../data/cotton.txt", delim = "\t") %>%
    mutate(est = fct_inorder(factor(est)))
levels(cotton$est) <- c("vegetative",
                        "flower bud",
                        "blossom",
                        "fig",
                        "cotton boll")
cotton

cotton_summary <- cotton %>%
    group_by(des, est) %>%
    summarise(me = mean(ncap), va = var(ncap))

xy1 <- xyplot(ncap ~ des | est,
              data = cotton,
              layout = c(NA, 3),
              as.table = TRUE,
              type = c("p", "smooth", "g"),
              xlab = "Artificial defoliation level",
              ylab = "Number of bolls produced",
              sub = "(a)",
              spread = 0.05,
              panel = panel.beeswarm)

lim <- extendrange(unlist(cotton_summary[, c("me", "va")]))
xy2 <- xyplot(va ~ me,
              xlim = lim,
              ylim = lim,
              type = c("g", "p", "r"),
              sub = "(b)",
              xlab = "Sample mean",
              ylab = "Sample variance",
              data = cotton_summary,
              panel = function(...) {
                  panel.xyplot(...)
                  panel.abline(a = 0, b = 1, lty = 2)
              })

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)

#-----------------------------------------------------------------------
# Descriptive analysis of the soybeans data set
## ---- desc-soya

# Load data
soybean <- read_delim("../data/soybean.txt", delim = "\t") %>%
    mutate(umid = factor(umid)) %>%
    slice(-74)
soybean

fvls <- paste0("Umidade : ", levels(soybean$umid), "%")
xyplot(ngra + nvag ~ K | umid,
       data = soybean,
       xlab = "Nível de adubação potássica",
       ylab = "Contagem",
       type = c("p", "g", "a"),
       jitter.x = TRUE,
       key = list(
           type = "b",
           divide = 1,
           lines = list(pch = pchs[1:2], lty = ltys[1:2]),
           text = list(c("Nº de grãos",
                         "Nº de vagens"))),
       layout = c(NA, 1),
       strip = strip.custom(factor.levels = fvls))

## ---- desc-soya2

# Load data
soybean <- read_delim("../data/soybean.txt", delim = "\t") %>%
    mutate(umid = factor(umid)) %>%
    slice(-74)
soybean

soybean_summary <-
    soybean %>%
    gather(key = "var", value = "count", ngra, nvag) %>%
    group_by(K, umid, var) %>%
    summarise(me = mean(count), va = var(count))

fvls <- paste0("Soil moisture : ", levels(soybean$umid), "%")
xy1 <- xyplot(ngra + nvag ~ K | umid,
              data = soybean,
              xlab = "Potassium fertilization level",
              ylab = "Count",
              sub = "(a)",
              type = c("p", "g", "a"),
              jitter.x = TRUE,
              layout = c(NA, 2),
              as.table = TRUE,
              key = list(
                  type = "b",
                  divide = 1,
                  lines = list(pch = pchs[1:2], lty = ltys[1:2]),
                  text = list(c("Number of grains",
                                "Number of pods"))),
              strip = strip.custom(factor.levels = fvls))

lim <- extendrange(unlist(soybean_summary[, c("me", "va")]))
xy2 <- xyplot(va ~ me | var,
              xlim = lim,
              ylim = lim,
              type = c("g", "p", "r"),
              sub = "(b)",
              xlab = "Sample mean",
              ylab = "Sample variance",
              data = soybean_summary,
              layout = c(NA, 2),
              strip = strip.custom(
                  factor.levels = c("Number of grains",
                                    "Number of pods")
              ),
              panel = function(...) {
                  panel.xyplot(...)
                  panel.abline(a = 0, b = 1, lty = 2)
              })

print(xy1, position = c(0, 0, 0.6, 1), more = TRUE)
print(xy2, position = c(0.6, 0, 1, 0.95), more = FALSE)

#-----------------------------------------------------------------------
# Descriptive analysis of the epilepsy data set

## ---- load-epilepsy
epilepsy <- read_delim("../data/epilepsy.csv", delim = ";") %>%
    mutate(trt = factor(trt, labels = c("Placebo", "Tratamento")),
           sex = factor(sex),
           race = factor(race),
           date0 = lubridate::dmy(date0)) %>%
    rename("week" = studyweek)

## ---- desc-epilepsy
epilepsy_summary <- epilepsy %>% group_by(trt, week) %>%
    summarise(ave = mean(nseizw), med = median(nseizw))
xyplot(nseizw ~ week | trt,
       groups = id,
       lty = 1,
       col = "gray70",
       type = c("l", "g"),
       scales = list(x = "free"),
       xlab = "Tempo (semanas)",
       ylab = "Número de convulsões",
       # key = list(columns = 2,
       #            text = list(paste(c("Average", "Median"),
       #                               "profile")),
       #            lines = list(lty = c(1, 2), lwd = 2)),
       data = epilepsy) +
    xyplot(ave + med ~ week | trt,
           type = c("l"),
           lwd = 2,
           scales = list(x = "free"),
           data = epilepsy_summary)

## ---- desc-epilepsy2
epilepsy <- read_delim("../data/epilepsy.csv", delim = ";") %>%
    mutate(trt = factor(trt, labels = c("Placebo", "Treatment")),
           sex = factor(sex),
           race = factor(race),
           date0 = lubridate::dmy(date0)) %>%
    rename("week" = studyweek)

epilepsy_summary <- epilepsy %>% group_by(trt, week) %>%
    summarise(ave = mean(nseizw), med = median(nseizw))
xyplot(nseizw ~ week | trt,
       groups = id,
       lty = 1,
       col = "gray70",
       type = c("l", "g"),
       scales = list(x = "free"),
       xlab = "Time (weeks)",
       ylab = "Number of seizures",
       # key = list(columns = 2,
       #            text = list(paste(c("Average", "Median"),
       #                               "profile")),
       #            lines = list(lty = c(1, 2), lwd = 2)),
       data = epilepsy) +
    xyplot(ave + med ~ week | trt,
           type = c("l"),
           lwd = 2,
           scales = list(x = "free"),
           data = epilepsy_summary)

#-----------------------------------------------------------------------
# Descriptive analysis of the bromelia data set
## ---- desc-bromelia

# Load data set
comps <- c("Casca de Pinus", "Casca de Eucaliptos", "Coxim",
           "Fibra de Coco", "Xaxim")
bromelia <- read_delim("../data/bromelia.txt", delim = "\t") %>%
    mutate(treat = factor(treat, labels = comps),
           block = factor(block)) %>%
    mutate()

# Visualize profiles
bromelia_summary <- bromelia %>% group_by(treat, time) %>%
  summarise(ave = mean(nleaves), var = var(nleaves))

xy1 <- xyplot(nleaves ~ time | treat,
              groups = block,
              type = c("g", "l"),
              xlab = "Tempo (dias)",
              ylab = "Número de folhas",
              as.table = TRUE,
              layout = c(3, 2),
              col = "gray60",
              lty = 1,
              sub = "(a)",
              # key = list(columns = 1,
              #            text = list("Perfil médio"),
              #            lines = list(lty = 1, lwd = 2)),
              data = bromelia) +
    xyplot(ave ~ time | treat,
           type = c("l"),
           lwd = 2,
           data = bromelia_summary)

lim <- extendrange(unlist(log(bromelia_summary[, c("ave", "var")] + 0.1)))
xy2 <- xyplot(log(var + 0.1) ~ log(ave),
              xlim = lim,
              ylim = lim,
              type = c("g", "p", "r"),
              sub = "(b)",
              xlab = expression("Logaritmo da média"~"amostral"~(log(bar(y)))),
              ylab = expression("Logaritmo da variância"~"amostral"~(log(s^2))),
              data = bromelia_summary,
              par.settings = list(
                  layout.heights = list(bottom.padding = 4),
                  par.sub.text = list(
                      just = "left",
                      y = grid::unit(-6.6, "mm"))),
              panel = function(...) {
                  panel.xyplot(...)
                  panel.abline(a = 0, b = 1, lty = 2)
              })

print(xy1, position = c(0, 0, 0.6, 1), more = TRUE)
print(xy2, position = c(0.6, 0, 1, 0.95), more = FALSE)

#-----------------------------------------------------------------------
# Descriptive analysis of the whitefly data set
## ---- desc-whitefly

# Load data set
whitefly <- read_delim("../data/whitefly.txt", delim = "\t") %>%
    filter(grepl("BRS", x = cult)) %>%
    mutate(cult  = fct_inorder(factor(cult)),
           bloco = fct_inorder(factor(bloco)))

# Visualize profiles
whitefly_summary <- whitefly %>% group_by(cult, dias) %>%
    summarise(ave_nsup = mean(nsup), var_nsup = var(nsup),
              ave_nmed = mean(nmed), var_nmed = var(nmed),
              ave_ninf = mean(ninf), var_ninf = var(ninf))

flvs <- paste("Terço", c("inferior", "médio", "superior"))
xy1 <- useOuterStrips(
    xyplot(ninf + nmed + nsup ~ dias | cult,
           outer = TRUE,
           groups = bloco,
           type = c("g", "l"),
           xlab = "Tempo de infestação (dias)",
           ylab = "Número de ninfas de mosca-branca",
           sub = "(a)",
           col = "gray50",
           lty = 1,
           data = whitefly) +
    xyplot(ave_ninf + ave_nmed + ave_nsup ~ dias | cult,
           outer = TRUE,
           type = c("l"),
           lwd = 2,
           data = whitefly_summary),
    strip.left = strip.custom(factor.levels = flvs)
)

aux <- whitefly_summary %>%
    gather(key = posa, value = ave, ave_nsup, ave_nmed, ave_ninf) %>%
    gather(key = posb, value = var, var_nsup, var_nmed, var_ninf) %>%
    mutate(lave = log(ave), lvar = log(var)) %>%
    ungroup()
index <- apply(aux[, 7:8], 1L, function(x) any(is.infinite(x)))

lim <- extendrange(unlist(aux[!index, c("lave", "lvar")]))
xy2 <- xyplot(lvar ~ lave | posa,
              data = aux[!index, ],
              type = c("g", "p", "r"),
              # xlab = expression(log(bar(y))),
              # ylab.right = expression(log(s^2)),
              xlab = list("média", col = "white"),
              ylab = "",
              sub = "(b)",
              layout = c(NA, 3),
              strip = FALSE,
              ylim = lim,
              xlim = lim,
              strip.left = strip.custom(
                  factor.levels = flvs,
                  bg = "gray75"),
              panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(0, 1, lty = 2)
              })

print(xy1, position = c(0, 0, 0.77, 1), more = TRUE)
print(xy2, position = c(0.75, 0, 1, 0.95), more = FALSE)

## ---- desc-whitefly2

# Load data set
whitefly <- read_delim("../data/whitefly.txt", delim = "\t") %>%
    filter(grepl("BRS", x = cult)) %>%
    mutate(cult  = fct_inorder(factor(cult)),
           bloco = fct_inorder(factor(bloco)))

# Visualize profiles
whitefly_summary <- whitefly %>% group_by(cult, dias) %>%
    summarise(ave_nsup = mean(nsup), var_nsup = var(nsup),
              ave_nmed = mean(nmed), var_nmed = var(nmed),
              ave_ninf = mean(ninf), var_ninf = var(ninf))

flvs <- paste(c("lower", "middle", "upper"), "third")
xy1 <- useOuterStrips(
    xyplot(ninf + nmed + nsup ~ dias | cult,
           outer = TRUE,
           groups = bloco,
           type = c("g", "l"),
           xlab = "Time of infestation (days)",
           ylab = "Number of whitefly nymphs",
           sub = "(a)",
           col = "gray50",
           lty = 1,
           data = whitefly) +
    xyplot(ave_ninf + ave_nmed + ave_nsup ~ dias | cult,
           outer = TRUE,
           type = c("l"),
           lwd = 2,
           data = whitefly_summary),
    strip.left = strip.custom(factor.levels = flvs)
)

aux <- whitefly_summary %>%
    gather(key = posa, value = ave, ave_nsup, ave_nmed, ave_ninf) %>%
    gather(key = posb, value = var, var_nsup, var_nmed, var_ninf) %>%
    mutate(lave = log(ave), lvar = log(var)) %>%
    ungroup()
index <- apply(aux[, 7:8], 1L, function(x) any(is.infinite(x)))

lim <- extendrange(unlist(aux[!index, c("lave", "lvar")]))
xy2 <- xyplot(lvar ~ lave | posa,
              data = aux[!index, ],
              type = c("g", "p", "r"),
              # xlab = expression(log(bar(y))),
              # ylab.right = expression(log(s^2)),
              xlab = list("média", col = "white"),
              ylab = "",
              sub = "(b)",
              layout = c(NA, 3),
              strip = FALSE,
              ylim = lim,
              xlim = lim,
              strip.left = strip.custom(
                  factor.levels = flvs,
                  bg = "gray75"),
              panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(0, 1, lty = 2)
              })

print(xy1, position = c(0, 0, 0.77, 1), more = TRUE)
print(xy2, position = c(0.75, 0, 1, 0.95), more = FALSE)

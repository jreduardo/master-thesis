#' @name eerjmaster
#' @docType package
#' @title Codes and data for master's thesis of Eduardo Jr
#' @description Code implementation and real data used to the master's
#'     thesis od Eduardo Jr that proposes flexible models for count
#'     data.
NULL

#' @name bromelia
#' @docType data
#' @title Alternative Substrates for Growth of Bromeliads
#' @description Xaxim is used in the past for containers to grow
#'     bromeliads and orchids whose was commercially prohibited since
#'     2001. Experiments have been conducted to find alternative
#'     container material. This dataset was produced from an experiment
#'     set up in a randomized complete block design with five
#'     alternative components to substrate pots (with turfa and perlita)
#'     for bromeliads. Plots with 8 plants, initially, were followed
#'     during 435 days after planting and the total number of leaves per
#'     plot was recorded at five dates.
#'
#' @format A data frame with 120 observations and 5 variables, where
#'
#'     \describe{
#'
#'     \item{\code{nleaves}}{Number of leaves per plot.}
#'     \item{\code{nplots}}{Number of plots.}
#'     \item{\code{days}}{Days after planting.}
#'     \item{\code{treat}}{Components to alternative substrates. The
#'     substrates were Xaxim, Pinus bark, Eucalyptus bark, Coxim, and
#'     Coir fiber, mixed with peat and perlite.}
#'     \item{\code{block}}{Indicates the block.}
#'     }
#'
#' @keywords datasets
#' @usage data(bromelia)
#' @examples
#'
#' data(bromelia)
#' str(bromelia)
NULL

#' @name cotton
#' @docType data
#' @title Artifitial Defoliation in Cotton Phenology
#' @description Several sources may lead to defoliation in the cotton
#'     crop, e.g. pests, diseases of foliage and mechanical
#'     injuries. This dataset comes from a greenshouse factorial
#'     experiment performed to evaluate the effect of artifical
#'     defoliation in the total number of cotton bolls at different
#'     growth stages. The experimental unit was a pot with 2 cotton
#'     plants. The response variable is the number of bolls produced at
#'     the end of the crop cycle.
#'
#' @format A data frame with 125 observations and 3 variables, where
#'
#'     \describe{
#'
#'     \item{\code{bolls}}{Number of cotton bolls.}
#'     \item{\code{defol}}{Levels of the artificial defoliation (percent
#'     in leaf area removed with scissors).}
#'     \item{\code{phenol}}{Phenological growth stages of the cotton
#'     plants.}
#'     }
#'
#' @keywords datasets
#' @usage data(cotton)
#' @examples
#'
#' data(cotton)
#' str(cotton)
NULL

#' @name soybean
#' @docType data
#' @title Soil Moisture and Potassium Doses in Soybean Crop
#' @description The tropical soils, usually poor in potassium, demand
#'     potassium fertilization when cultivated with soybean to obtain
#'     satisfactory yields. This dataset results of a greenhouse
#'     factorial experiment set up in a randomized complete block design
#'     with five blocks. The aim of this study was to evaluate the
#'     effects of potassium doses and soil humidity levels on soybean
#'     agronomic characteristics. The experimental unit was a pot with 2
#'     soybean plants and the response variables are the numbers of
#'     grains, pods and viable pods recorded after the plants reach
#'     physiological maturity and dry out naturally.
#'
#' @format A data frame with 74 observations and 6 variables, where
#'
#'     \describe{
#'
#'     \item{\code{grains}}{Number of grains.}
#'     \item{\code{pods}}{Number of pods.}
#'     \item{\code{vpods}}{Number of viable pods.}
#'     \item{\code{K}}{Potassium doses (10 mg\eqn{^{-2}}
#'     dm\eqn{^{-3}}).}
#'     \item{\code{soil}}{Soil moisture levels (percentual of the total
#'     porosity).}
#'     \item{\code{block}}{Indicates the block.}
#'     }
#'
#' @keywords datasets
#' @usage data(soybean)
#' @examples
#'
#' data(soybean)
#' str(soybean)
NULL

#' @name whitefly
#' @docType data
#' @title Damage of Whitefly in Soybean Crop
#' @description The whitefly causes several damage in the soybean crop
#'     by sucking the plant sap, transmission of viruses and promoting
#'     sooty mold. This dataset comes from an experiment to evaluate the
#'     effect of the whitefly \emph{Bemisia tabaci} infestation in the
#'     differents cultivars of soybean. The experiment was
#'     performed in a greenhouse conditions where randomized blocks were
#'     infested with adults whiteflies. The experimental unit was two
#'     pots with soybean plants that were followed during 38 days. The
#'     number of nymphs in the upper, middle and lower third of the
#'     plants were recorded weekly.
#' @format A data frame with 96 observations and 6 variables, where
#'
#'     \describe{
#'
#'     \item{\code{nupper}}{Number of whitefly nymphs in the upper
#'     third.}
#'     \item{\code{nmiddle}}{Number of whitefly nymphs in the middle
#'     third.}
#'     \item{\code{nlower}}{Number of whitefly nymphs in the upper
#'     third.}
#'     \item{\code{days}}{Days of infestation (o days indicates no
#'     infestation).}
#'     \item{\code{cult}}{Initials of the soybean cultivar.}
#'     \item{\code{block}}{Indicates the block.}
#'     }
#'
#' @keywords datasets
#' @usage data(whitefly)
#' @examples
#'
#' data(whitefly)
#' str(whitefly)
NULL

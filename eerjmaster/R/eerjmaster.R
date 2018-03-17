#' @name eerjmaster
#' @docType package
#' @title Codes and data for master's thesis of Eduardo Jr
#' @description Code implementation and real data used to the master's
#'     thesis od Eduardo Jr that proposes flexible models for count
#'     data.
NULL

#' @name bromelia
#' @docType data
#' @title Alternative substrates for growth of bromeliads.
#' @description Xaxim is used in the past for containers to grow
#'     bromeliads and orchids whose was commercially prohibited since
#'     2001. Experiments have been conducted to find alternative
#'     container material. This dataset was produced from an experiment
#'     set up in a randomized complete block design with five
#'     alternative components to substrate pots (with turfa and perlita)
#'     for bromeliads. Plots with 8 plants, initially, were followed
#'     during 435 days after planting and the total number of leaves per
#'     plot was recorded at five dates.
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

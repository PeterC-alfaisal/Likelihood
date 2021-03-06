
# This file is automatically generated, you probably don't want to edit this

cttOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "cttOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            rows = NULL,
            cols = NULL,
            counts = NULL,
            pll = FALSE,
            nul = 1,
            alt = 0,
            lint = 2,
            ciWidth = 95,
            varA = FALSE,
            cc = FALSE,
            pcRow = FALSE,
            pcCol = FALSE, ...) {

            super$initialize(
                package="Likelihood",
                name="ctt",
                requiresData=TRUE,
                ...)

            private$..rows <- jmvcore::OptionVariable$new(
                "rows",
                rows,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..cols <- jmvcore::OptionVariable$new(
                "cols",
                cols,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..counts <- jmvcore::OptionVariable$new(
                "counts",
                counts,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"),
                default=NULL)
            private$..pll <- jmvcore::OptionBool$new(
                "pll",
                pll,
                default=FALSE)
            private$..nul <- jmvcore::OptionNumber$new(
                "nul",
                nul,
                min=-100000000000,
                max=100000000000,
                default=1)
            private$..alt <- jmvcore::OptionNumber$new(
                "alt",
                alt,
                min=-100000000000,
                max=100000000000,
                default=0)
            private$..lint <- jmvcore::OptionNumber$new(
                "lint",
                lint,
                min=1,
                max=10,
                default=2)
            private$..ciWidth <- jmvcore::OptionNumber$new(
                "ciWidth",
                ciWidth,
                min=50,
                max=99.9,
                default=95)
            private$..varA <- jmvcore::OptionBool$new(
                "varA",
                varA,
                default=FALSE)
            private$..cc <- jmvcore::OptionBool$new(
                "cc",
                cc,
                default=FALSE)
            private$..pcRow <- jmvcore::OptionBool$new(
                "pcRow",
                pcRow,
                default=FALSE)
            private$..pcCol <- jmvcore::OptionBool$new(
                "pcCol",
                pcCol,
                default=FALSE)

            self$.addOption(private$..rows)
            self$.addOption(private$..cols)
            self$.addOption(private$..counts)
            self$.addOption(private$..pll)
            self$.addOption(private$..nul)
            self$.addOption(private$..alt)
            self$.addOption(private$..lint)
            self$.addOption(private$..ciWidth)
            self$.addOption(private$..varA)
            self$.addOption(private$..cc)
            self$.addOption(private$..pcRow)
            self$.addOption(private$..pcCol)
        }),
    active = list(
        rows = function() private$..rows$value,
        cols = function() private$..cols$value,
        counts = function() private$..counts$value,
        pll = function() private$..pll$value,
        nul = function() private$..nul$value,
        alt = function() private$..alt$value,
        lint = function() private$..lint$value,
        ciWidth = function() private$..ciWidth$value,
        varA = function() private$..varA$value,
        cc = function() private$..cc$value,
        pcRow = function() private$..pcRow$value,
        pcCol = function() private$..pcCol$value),
    private = list(
        ..rows = NA,
        ..cols = NA,
        ..counts = NA,
        ..pll = NA,
        ..nul = NA,
        ..alt = NA,
        ..lint = NA,
        ..ciWidth = NA,
        ..varA = NA,
        ..cc = NA,
        ..pcRow = NA,
        ..pcCol = NA)
)

cttResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "cttResults",
    inherit = jmvcore::Group,
    active = list(
        freqs = function() private$.items[["freqs"]],
        text = function() private$.items[["text"]],
        cttma = function() private$.items[["cttma"]],
        ctt = function() private$.items[["ctt"]],
        ctt2 = function() private$.items[["ctt2"]],
        ctt3 = function() private$.items[["ctt3"]],
        plotc = function() private$.items[["plotc"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Odds Ratio")
            self$add(jmvcore::Table$new(
                options=options,
                name="freqs",
                title="Contingency Table",
                columns=list(),
                clearWith=list(
                    "rows",
                    "cols",
                    "counts")))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="Log likelihood ratio analysis"))
            self$add(jmvcore::Table$new(
                options=options,
                name="cttma",
                title="Marginal main effects and interaction analyses",
                rows=4,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Component", 
                        `type`="text"),
                    list(
                        `name`="Value", 
                        `title`="Expected value", 
                        `type`="number"),
                    list(
                        `name`="S", 
                        `type`="number"),
                    list(
                        `name`="G", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `refs`=list(
                            "EdwardsCT")))))
            self$add(jmvcore::Table$new(
                options=options,
                name="ctt",
                title="Odds Ratio analyses",
                rows=3,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="Value", 
                        `title`="OR Value", 
                        `type`="number"),
                    list(
                        `name`="ordiff", 
                        `title`="Difference", 
                        `type`="number"),
                    list(
                        `name`="S", 
                        `type`="number"),
                    list(
                        `name`="G", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="ctt2",
                title="Support and likelihood-based confidence intervals for OR",
                rows=2,
                columns=list(
                    list(
                        `name`="Interval", 
                        `title`="Type of interval", 
                        `type`="text"),
                    list(
                        `name`="Level", 
                        `type`="text"),
                    list(
                        `name`="OR", 
                        `type`="number"),
                    list(
                        `name`="Lower", 
                        `type`="number"),
                    list(
                        `name`="Upper", 
                        `type`="number", 
                        `refs`=list(
                            "Aitkin")))))
            self$add(jmvcore::Table$new(
                options=options,
                name="ctt3",
                title="Variance analysis",
                visible=FALSE,
                rows=1,
                clearWith=list(
                    "cc"),
                columns=list(
                    list(
                        `name`="var", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="Sv", 
                        `title`="S", 
                        `type`="number"),
                    list(
                        `name`="X2", 
                        `title`="\u03C7\u00B2", 
                        `type`="number"),
                    list(
                        `name`="dfv", 
                        `title`="df", 
                        `type`="number"),
                    list(
                        `name`="pv", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="pv1", 
                        `title`="1 - p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `refs`=list(
                            "EdwardsVA")))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotc",
                title="Likelihood function for OR with S-level support interval",
                width=500,
                height=400,
                renderFun=".plotc",
                visible=FALSE))}))

cttBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "cttBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "Likelihood",
                name = "ctt",
                version = c(1,0,0),
                options = options,
                results = cttResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = TRUE,
                requiresMissings = FALSE)
        }))

#' LR Odds Ratio
#'
#' Analysis of a 2 x 2 contingency table using the odds ratio
#' 
#' @param data the data as a data frame
#' @param rows the variable to use as the rows in the contingency table (not
#'   necessary when providing a formula, see the examples)
#' @param cols the variable to use as the columns in the contingency table
#'   (not necessary when providing a formula, see the examples)
#' @param counts the variable to use as the counts in the contingency table
#'   (not necessary when providing a formula, see the examples)
#' @param pll .
#' @param nul .
#' @param alt .
#' @param lint .
#' @param ciWidth a number between 50 and 99.9 (default: 95), width of the
#'   confidence intervals to provide
#' @param varA .
#' @param cc .
#' @param pcRow \code{TRUE} or \code{FALSE} (default), provide row percentages
#' @param pcCol \code{TRUE} or \code{FALSE} (default), provide column
#'   percentages
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$freqs} \tab \tab \tab \tab \tab a proportions table \cr
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$cttma} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$ctt} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$ctt2} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$ctt3} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plotc} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$freqs$asDF}
#'
#' \code{as.data.frame(results$freqs)}
#'
#' @export
ctt <- function(
    data,
    rows,
    cols,
    counts = NULL,
    pll = FALSE,
    nul = 1,
    alt = 0,
    lint = 2,
    ciWidth = 95,
    varA = FALSE,
    cc = FALSE,
    pcRow = FALSE,
    pcCol = FALSE,
    formula) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("ctt requires jmvcore to be installed (restart may be required)")

    if ( ! missing(formula)) {
        if (missing(counts))
            counts <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="lhs",
                type="vars",
                subset="1")
        if (missing(rows))
            rows <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="rhs",
                type="vars",
                subset="1")
        if (missing(cols))
            cols <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="rhs",
                type="vars",
                subset="2")
    }

    if ( ! missing(rows)) rows <- jmvcore::resolveQuo(jmvcore::enquo(rows))
    if ( ! missing(cols)) cols <- jmvcore::resolveQuo(jmvcore::enquo(cols))
    if ( ! missing(counts)) counts <- jmvcore::resolveQuo(jmvcore::enquo(counts))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(rows), rows, NULL),
            `if`( ! missing(cols), cols, NULL),
            `if`( ! missing(counts), counts, NULL))

    for (v in rows) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in cols) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- cttOptions$new(
        rows = rows,
        cols = cols,
        counts = counts,
        pll = pll,
        nul = nul,
        alt = alt,
        lint = lint,
        ciWidth = ciWidth,
        varA = varA,
        cc = cc,
        pcRow = pcRow,
        pcCol = pcCol)

    analysis <- cttClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}


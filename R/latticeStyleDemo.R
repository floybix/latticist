## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

latticeStyleDemo <-
    function(type = c("plot", "superpose", "polygons", "regions"))
{
    type <- match.arg(type, several.ok = TRUE)
    ## set up keys
    linesKey <- list(c("Main", "2nd", "3rd", "4th", "5th", "6th"),
                     lines = TRUE, points = FALSE, type = "b",
                     columns = 3, between = 1)
    polygonKey <- list(c("Main", "2nd", "3rd", "4th", "5th", "6th"),
                       rectangles = TRUE, points = FALSE,
                       columns = 3, between = 1)
    ## create a list of trellis objects
    objs <- list()
    if ("plot" %in% type) {
        objs$plot.symbol <-
            xyplot(ozone ~ wind^2, environmental,
                   type = c("p", "smooth"),
                   panel = function(x, y, ...) {
                       try(panel.refline(h = 0))
                       panel.xyplot(x, y, ...)
                       lims <- current.panel.limits()
                       panel.abline(h = mean(lims$ylim))
                       panel.bwplot(x, rep(quantile(lims$ylim, 0.9), length(x)),
                                    box.width = diff(lims$ylim) * 0.08)
                       panel.usertext(mean(lims$xlim), mean(lims$ylim),
                                      "This is user.text; \n that is add.line", pos = 3)
                   })
    }
    if ("polygons" %in% type) {
        objs$polygons <- barchart(margin.table(Titanic, c(1,4)),
                                  legend = list(top = list(fun = "drawSimpleKey", args = polygonKey)))
    }
    if ("superpose" %in% type) {
        objs$superpose <-
            xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width, iris,
                   type = c("p", "r"), jitter.x = TRUE, jitter.y = TRUE, factor = 5,
                   legend = list(bottom = list(fun = "drawSimpleKey", args = linesKey)))
    }
    if ("regions" %in% type) {
        objs$regions <- levelplot(volcano[c(TRUE,FALSE), c(TRUE,FALSE)],
                                  scales = list(draw = FALSE))
    }
    ## merge multiple panels into one display
    if (length(objs) == 1)
        obj <- objs[[1]]
    else obj <- do.call("c", objs)
    rm(objs)

    obj <- update(obj, as.table = TRUE, main = "Lattice style demo",
                  xlab = expression(NULL), ylab = "I am a ylab")
    if (length(type) > 1) {
        obj <- update(obj, scales = list(x = list(draw = FALSE)),
                      legend = list(
                      right = if ("regions" %in% type)
                      list(fun = "draw.colorkey", args = list(list(at=0:100))),
                      bottom = if ("superpose" %in% type)
                      list(fun = "drawSimpleKey", args = linesKey),
                      top = if ("polygons" %in% type)
                      list(fun = "drawSimpleKey", args = polygonKey)
                      ))
    }
    obj
}

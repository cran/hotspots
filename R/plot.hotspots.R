plot.hotspots <-
function(x, pch = par("pch"), ...) {
if (!inherits(x, "hotspots")) 
	stop("use only with \"hotspots\" objects")
h = TRUE ; c = TRUE
if (is.null(x$positive.cut)) h = FALSE
if (is.null(x$negative.cut)) c = FALSE

color <- rep("black", length(x$data))
if (h) color[x$data > min(x$positive.cut)] <- "red"
if (c) color[x$data < max(x$negative.cut)] <- "blue"

pc <- rep(pch, length(x$data))

if (h)
	{upper <- max(c(x$data,x$positive.cut))} else
		upper <- max(x$data)
if (c)
	{lower <- min(c(x$data,x$negative.cut))} else
		lower <- min(x$data)
lim <- c(lower-(upper-lower)*0.1, upper+(upper-lower)*0.1)

densityplot(~c(NA, x$data), col = c("black", color), pch = c(NA, pc),
	xlim = lim, xlab = x$dataset_name,
	panel = function(x,...) {
		panel.densityplot(x,...,)
		if (h) panel.abline(v = x$positive.cut, col = "red")
		if (c) panel.abline(v = x$negative.cut, col = "blue") }, ... ) }

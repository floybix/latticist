## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

writeCallAndRef <- function(plotCall, plotNum)
{
  plotStr <- deparse(plotCall, control = NULL)
  plotStr <- toString(paste(plotStr, collapse = ""),
                      width = 43)
  cat("\\verb#", plotStr, "#", "\n", sep = "")
  cat("$\\to$~\\emph{p.}~\\pageref{code:", plotNum, "}",
      "\n", sep = "")
  cat("\\label{plot:", plotNum, "}", "\n", sep = "")
  cat("\n")
}

writePlotCallsAppendix <- function(plotCalls)
{
    for (i in seq_along(plotCalls)) {
        x <- plotCalls[[i]]
        cat(paste("\\subsection{", i, "}", sep = ""),
            "\\begin{frame}[fragile]",
            "\\frametitle{Appendix: Code}",
            paste("\\label{code:", i, "}", sep = ""),
            paste("Code to produce the plot on ",
                  "page~\\pageref{plot:", i, "}:", sep = ""),
            "\\smallskip ",
            "\\begin{Schunk}",
            "\\begin{Soutput}",
            paste(deparse(x, control = NULL, width = 35),
                  collapse = "\n"),
            "\\end{Soutput}",
            "\\end{Schunk}",
            "\\end{frame}",
            "",
            sep = "\n")
    }
}

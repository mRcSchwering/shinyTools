#' GetCaptures
#'
#' Split a chr string by capture groups according to a regular expression provided.
#'
#' The string is returned as a matrix of start and stop position of substrings.
#' These substrings are labelled by which capture group they belong to.
#' Substrings outside a capture are labelled as 0.
#'
#' Nested capturing groups do not work.
#'
#' @family CapturePattern module functions
#'
#' @param s    chr string
#' @param pat  chr string containing a regular expression with at least 1 capture '(', ')'
#'
#' @return  Num matrix indexing substrings of s. Each columns represents a substring.
#'          Row 1 is the label, Row 2 is the start, row 3 the stop letter of each substring.
#'          Substrings which are part of a capture are labelled with increasing numbers as they appear in s beginning with 1.
#'          Substrings which are not part of a capture are labelled with 0.
#'          If nothing was captured in chr s, NULL is returned.
#'
#' @examples
#' s <- ">ENSG00000139083_8_61387.34117"
#' pat <- "^.*(SG.+?)1.*_(.+?)_.*$"
#'
#' m <- GetCaptures(s, pat)
#' apply(m, 2, function(x) substr(s, x[2], x[3]))
#'
#' @export
#'
GetCaptures <- function(s, pat){
  cs <- regexec(pat, s)[[1]]
  if( cs[1] == -1 ) return(NULL)
  m <- sapply(2:length(cs), function(x) matrix(c(x - 1, cs[x], cs[x] + attr(cs, "match.length")[x] - 1), ncol = 1))
  if( any(is.na(m[, 1])) ) stop("No captures found in regular expression.")
  bl <- 1:nchar(s) %in% if(ncol(m) > 1) unlist(apply(m, 2, function(x) x[2]:x[3])) else m[2, 1] : m[3, 1]

  gap <- FALSE
  o <- NULL
  for( n in 1:length(bl) ){
    if( !bl[n] ){
      if( gap ) o[2, ncol(o)] <- n else {gap <- TRUE; o <- cbind(o, c(n, n))}
    } else gap <- FALSE
  }
  if(!is.null(o)) o <- rbind(rep(0, ncol(o)), o)

  out <- sapply(sort(c(o[2, ], m[2, ])), function(x) if(x %in% o[2, ]) o[, o[2, ] == x] else m[, m[2, ] == x])
  rownames(out) <- c("capture", "start", "stop")
  out
}

#' Update and Re-fit a Model Call
#' 
#' This function can be used to fit a new cub model in which has changed some arguments.
#' 
#' @param object an existing fit obtained from cub function.
#' @param ... additionally arguments to the call or arguments with changed values.
#' @param evaluate if true evaluate the new call else return the call.
#' 
#' @examples
#' # The first model
#' mod1 <- cub(pi.fo = global ~ 1, xi.fo = ~ 1, m=7, data=univer)
#' mod1
#' # Modifying right sides of pi and xi formulas
#' update2.cub(mod1, pi.fo = global ~ gender, xi.fo = ~ lage + gender)
#' # Modifying the dataset
#' update2.cub(mod1, xi.fo = ~ 1, m=7, data=univer[1:50, ])
#'
#' @export
#' 
update2.cub <- function(object, ..., evaluate=TRUE) {
  if (is.null(call <- getCall(object))) 
    stop("need an object with call component")
  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }
  if (evaluate) 
    eval(call, parent.frame())
  else call
}
#' @export
update1.cub <- function (object, 
                        formula., 
                        what = c("pi", "xi"),
                        parameter= NULL,
                        evaluate = TRUE) 
{
  call <- object$call
  if (is.null(call)) 
    stop("need an object with call component")
  
  if (!missing(formula.)) 
  {
    what <- if (!is.null(parameter))  {
      match.arg(parameter, choices=c("pi", "xi"))
    } 
    else  match.arg(what)
  }
  
  if (what == "pi") 
  { call$pi.fo <- update.formula(formula(object, what), formula.) }
  
  else if (what == "xi")  
  {
    call[[paste(what, "fo", sep=".")]] <- 
      if (length(update.formula(formula(object, what), formula.))==2)
        update.formula(formula(object, what), formula.)
    else
      update.formula(formula(object, what), formula.)[-2]
  }
  
  if (evaluate) 
    eval(call, parent.frame())
  else call
}

#' @export
formula.cub <- function(x, what = c("pi", "xi"),
                        parameter= NULL, ... ) 
{
  what <- if (!is.null(parameter))  {
    match.arg(parameter, choices=c("pi", "xi"))} 
  else  match.arg(what)
  if (!what%in%x$parameters) stop(paste(what, "is not a parameter in the object","\n")) 
  fo <- x[[paste(what, "fo", sep=".")]]
  ## the problem is when "." is in the formula, if true get formula from terms
  if (length(fo)==2 && "."%in%strsplit(as.character(fo),split="")[[2]])# no resp 
    fo <- formula(x[[paste(what, "terms", sep=".")]])
  if (length(fo)==3 &&  "."%in%strsplit(as.character(fo), split = "")[[3]])# "."%in%strsplit(as.character(fo), split = "")[[3]]
    fo <- formula(x[[paste(what, "terms", sep=".")]])
  fo
}



#' @rdname iProjection
#' @export

setClass(
  Class = "iProjection",
  representation = representation(
    design = "formula",
    conditionMatrix = "matrix",
    conditions = "numeric",
    iProjection = "numeric",
    reference = "numeric"
  )
)

setValidity(
  Class = "iProjection",
  method = function(object){
    errors <- character()
    ## check whether design is formula
    if( ! is(design(object), "formula")){
      msg <-"The 'design' slot must be a formula."
      errors <- c(errors, msg)
    }

    if (! (is(conditionMatrix(object), "matrix") & is.numeric(conditionMatrix(object)))){
      msg <- "The 'conditionMatrix' slot must be a numeric matrix."
      errors <- c(errors, msg)
    }

    if (! (is(conditions(object), "numeric") & is(conditions(object), "vector"))){
      msg <- "The 'conditions' slot must be a numeric vector."
      errors <- c(errors, msg)
    }

    if (! (is(iProjection(object), "numeric") & is(iProjection(object), "vector"))){
      msg <- "The 'iProjection' slot must be a numeric vector."
      errors <- c(errors, msg)
      sumIProjection = sum(iProjection(object))
      if (sumIProjection != 1){
        msg <- paste0("The entries in the 'iProjection' slot sum to ", sumIProjection, ". Should be 1." )
        errors <- c(errors, msg)
      }
    }

    if (! (is(reference(object), "numeric") & is(reference(object), "vector"))){
      msg <- "The 'reference' slot must be a numeric vector."
      errors <- c(errors, msg)
    }

    lengthIProjection <- length(iProjection(object))
    lengthReference <- length(reference(object))
    lengthConditions <- length(conditions(object))
    ncolConditionMatrix <- NCOL(conditionMatrix(object))
    nrowConditionMatrix <- NROW(conditionMatrix(object))

    if (lengthIProjection != ncolConditionMatrix){
      msg <- paste0(
        "The number of entries in the 'iProjection' slot is ", lengthIProjection,
        ". Should be equal to the number of columns in the 'conditionMatrix' slot = ",
        ncolConditionMatrix, "."
      )
      errors <- c(errors, msg)
    }

    if (lengthReference != ncolConditionMatrix){
      msg <- paste0(
        "The number of entries in the 'iReference' slot is ", lengthIReference,
        ". Should be equal to the number of columns in the 'conditionMatrix' slot = ",
        ncolConditionMatrix, "."
      )
      errors <- c(errors, msg)
    }

    if (lengthConditions != nrowConditionMatrix){
      msg <- paste0(
        "The number of entries in the 'conditions' slot is ", lengthConditions,
        ". Should be equal to the number of rows in the 'conditionMatrix' slot = ",
        nrowConditionMatrix, "."
      )
      errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else return(errors)
  }
)
#' iProjection class
#'
#' The \code{iProjection} class represents an \eqn{i}-projection from a reference distribution.
#'
#' @slot design design formula describing the conditions
#' @slot conditions values resulting from applying conditions onto the data
#' @slot mat matrix representation of the conditions
#' @slot iProjection i-projection
#' @slot reference reference distribution
#'
#' @param totemData totemData object with the empirical distribution.
#' @param design a \code{formula}, expresses how the target (left hand side) should depend
#' on attributes (right hand side).
#' @param reference numeric vector, reference distribution (default: uniform distribution).
#'
#' @details The parameter 'design' (\code{formula} object) encodes the relationships between the attributes. The operators
#' \enumerate{
#'   \item '|' denote independence, e.g. \code{design = ~ x | y}  signifies \eqn{p(x,y) = p(x)\cdotp(y)}.
#'   \item '&' denote dependence of the left and right hand side, e.g. \code{design = ~ x & y} signifies that \eqn{p(x,y)}
#'   cannot be necessarily factorized.
#' }
#' The typical operator precedence are used, i.e. '&' before '|'. By using brackets, attributes can be grouped,
#' e.g. \code{design = ~ x & (y | z)} signifies \eqn{p(x,y,z) = p(x|y,z)\cdot p(y)\cdot p(z)}.
#'
#'
#' It is also possible to use the \code{mean()} function to calculate (numerical) expected values. e.g \code{design = ~ mean(x) & y},
#' which signifies that the mean of \eqn{x} depends on \eqn{y}.
#'
#' Finally, it is possible to calculate new attributes, e.g. \code{design = ~ mean(x/y^2) & z}, which signifies that that the mean
#' of \eqn{\frac{x}{y^2}} depends on \eqn{y}, or higher central moments, e.g. \code{design = ~ mean((x - mean(x))^2) |z}, which signifies that the 2nd
#' central moment of \eqn{x} (the variance) is indpendent of \eqn{z}
#'
#'
#' @aliases iProjection-class
#' @rdname iProjection
#' @author Orestis Loukas & Ho Ryun Chung
#' @export
iProjection <- function(totemData, design, reference = NULL){
  ## check input
  if (! is(totemData, "totemData")){
    stop("'totemData' must be a totemData object")
  }
  if (is.null(reference)){
    reference <- rep(1/ nrow(data(totemData)), nrow(data(totemData)))
  } else{
    if (! (is(reference, "numeric") & is(reference, "vector"))){
      stop("The 'reference' slot must be a numeric vector.")
    }
    lengthReference <- length(reference)
    if (lengthReference != nrow(data(totemData))){
      stop("The provided 'reference' has length = ", lengthReference, ". Must be ", nrow(data(totemData)))
    }
  }
  if (! is(design, "formula")){
    stop("'design' must be a formula")
  } else{

    designVars <- all.vars(design)
    if (! all(designVars %in% colnames(data(totemData)))){
      stop("All attributes in design formula must be columns in the 'data' slot of the totemData object")
    }
    if (design[[1]] != "~"){
      stop("'design' must start with '~'")
    }

    ## construct conditionMatrix from the design formula
    ## we need to parse the formula
    ## ~ x + group, x and group are independent
    ## conditions: marginal x, marginal group
    ##
    ## ~ x + group + mean(x):group, x and group are dependent
    ## conditions: marginal x, marginal group, mean x|group
    deparsedFormula <- deparseFormula(design)




    ## check for rank
    ##
  }

}


deparseFormula <- function(x){
  if (rlang::is_syntactic_literal(x)) {
    return(x)
  } else if (is_symbol(x)) {
    return(x)
  }
  lapply(x, deparseFormula)

}



#' Set up available designs for NOWAC
#'
#' @return vector with available designs
#'
#' @author Bjorn Fjukstad, \email{bjorn@cs.uit.no}
#'
#' @seealso \code{\link{selectDesign}}
#'
#' @keywords design
#'
#' @export
getDesigns <- function() {
    return(c("case-control", "cross-sectional"))
}

#' Get hospital name
#'
#' Helper function to translate the hospital code (a number between 1 and 11) to
#' a name, such as Tromso, Nodo or Molde.
#'
#' @param code Integer code of the hospital
#'
#' @return string Hospital name
#'
#' @author Bjorn Fjukstad, \email{bjorn@cs.uit.no}
#'
#' @seealso \code{\link{nowaclite}}
#'
#' @keywords hospital
#'
#' @examples
#' hospital_name <- getHospital(1)
#'
#' @export
getHospital <- function(code) {
  if (is.na(code)) {
    return(NA)
  }
  hospital <- switch(code, "Tromso", "Bodo", "Buskerud (Drammen)", "Fredrikstad",
                "Haukeland", "Molde", "Radiumhospitalet", "St. Olavs hospital",
                "Stavanger", "Tonsberg", "Radium/Ulleval")
  if (is.null(hospital)) {
    stop("Invalid hospital code. Should be between 1 and 11.")
  }
  return(hospital)
}

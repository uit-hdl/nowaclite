# Data subset functions -----------------------------------
#' Subset column from data
#'
#' @param samples Dataset with samples
#' @param column Column for the subset
#' @param levels Get levels (TRUE) or values (FALSE)
#'
#' @return subset of the data
#'
#' @author Nikita Shvetsov, \email{nikita.shvetsov@uit.no}
#'
#' @seealso \code{\link{selectDisease}, \link{selectDesign}, \link{selectSubCohort}}
#'
#' @keywords select subset column
#'
#' @examples
#' getSamples
#' getColumnFromData(samples, "LPNR")
#'
#' getDiseases
#' getColumnFromData(samples, "Disease", levels=TRUE)
#'
#' getSubCohorts (Project)
#' getColumnFromData(samples, "Project", levels=TRUE)
#'
#' @export
getColumnFromData <- function(dataset, column, levels=FALSE) {
  if (levels) return (levels(dataset[[column]]))
  else return (dataset[[column]])
}


#' Select samples with specified diseases
#'
#' @param samples Dataset with samples
#' @param diseases Vector of diseases
#'
#' @return samples with specified diseases
#'
#' @author Bjorn Fjukstad, \email{bjorn@cs.uit.no}
#'
#' @seealso \code{\link{getColumnFromData}}
#'
#' @keywords select diseases
#'
#' @examples
#' diseases <- c("breast cancer")
#' samples <- selectDisease(samples, diseases)
#'
#' @export
selectDisease <- function(samples, diseases) {
    available_diseases <- getColumnFromData(samples, "Disease", levels=TRUE)
    inter <- intersect(diseases, available_diseases)
    if (!identical(diseases, inter)) {
        error_msg <- "Disease not available. Select one or more from: "
        disease_string <- paste0(available_diseases, collapse=", ")
        error_msg <- paste0(error_msg, disease_string)
        stop(error_msg)
    }
    samples <- samples[getColumnFromData(samples, 'Disease') %in% diseases, ]
    return (samples)
}


#' Select samples within specified design
#'
#' @param samples Dataset with samples
#' @param design String, identifiying design
#'
#' @return samples with specified design
#'
#' @author Bjørn Fjukstad, \email{bjorn@cs.uit.no}
#'
#' @seealso \code{\link{getColumnFromData}}
#'
#' @keywords select design
#'
#' @examples
#' samples <- selectDesign(samples, "case-control")
#'
#' @export
selectDesign <- function(samples, design) {
    if (design == "case-control") {
        cc_status <- c("case", "ctrl")
    } else if (design == "cross-sectional") {
        cc_status <- c("ctrl")
    } else {
        stop("Invalid design. Select either 'case-control' or 'cross-selectional'")
    }
    samples <- samples[getColumnFromData(samples, 'CaseControlStatus') %in% cc_status, ]
    return (samples)
}


#' Select samples within specified sub-cohorts
#'
#' Select samples within specified sub-cohorts (also can be referred as Projects)
#'
#' @param samples Dataset with samples
#' @param design String, identifiying sub-cohorts
#'
#' @return samples within specified sub-cohorts
#'
#' @author Bjorn Fjukstad, \email{bjorn@cs.uit.no}
#'
#' @seealso \code{\link{getColumnFromData}}
#'
#' @keywords select sub-cohort
#'
#' @examples
#'
#' samples <- selectSubCohort(samples, c("Hospital CC1", "Hospital CC2", "Hospital CC3"))
#'
#' @export
selectSubCohort <- function(samples, sub_cohorts) {
    available_sub_cohorts <- getColumnFromData(samples, "Project", levels=TRUE)
    inter <- intersect(sub_cohorts, available_sub_cohorts)
    if (!identical(sub_cohorts, inter)) {
        error_msg <- "Sub cohort not available. Select one or more from: "
        cohort_string <- paste0(available_sub_cohorts, collapse=", ")
        error_msg <- paste0(error_msg, cohort_string)
        stop(error_msg)
    }
    samples <- samples[getColumnFromData(samples, "Project") %in% sub_cohorts, ]
    return(samples)
}


#' Select outro data
#'
#' Select outro data (cancer registry or questionaire data)
#'
#' @param samples Dataset with samples
#' @param outro Outer dataset (registry or questionaire)
#' @param sLNR Samples Id for samples dataset used for merging
#' @param oLNR Samples Id for outro dataset used for merging
#'
#' @return data related to evaluated samples
#'
#' @author Nikita Shvetsov, \email{nikita.shvetsov@uit.no}
#'
#' @seealso \code{\link{getColumnFromData}}
#'
#' @keywords questionaires outro cancer registry merging
#'
#' @examples
#' quest_data <- getOutroData(samples, two_page_quest)
#' registry_data <- getOutroData(samples, cancer_registry_2018, sLNR=LPNR, oLNR=LPNR)
#'
#' @export
getOutroData <- function(samples, outro, sLNR='LabNumber', oLNR='labnr') {
    return (outro[getColumnFromData(outro, oLNR) %in% getColumnFromData(samples, sLNR), ])
}

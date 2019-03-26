#' Creates lumi object from expression and control data file.
#'
#' Removes user-defined bad saples, remove suffixes
#' and returns the object. Gene expression and controls files should be
#' in folder, specified in \code{input_path} argument.
#'
#' @param input_path Path to the folder, containing data
#' @param exprs_filename Gene expression filename
#' @param ctrl_data_filename Controls data filename
#' @param bad_samples Sample names that should be excluded
#' @param remove_suffix Remove samples with suffix in their name
#' @param output_path Path for the output result
#'
#' @return Lumi object (lobj)
#'
#' @author Bjorn Fjukstad, \email{bjorn@cs.uit.no}
#'
#' @seealso \code{\link{generateDatasetFile}}
#'
#' @keywords lumi make object
#'
#' @examples
#' lumi_object <- makeLumi("/path/to/gene_expression_data_folder/",
#'                          "gene_expression_filename.csv",
#'                          "controls_data_filename.csv",
#'                          bad_samples=c("Sample1","Sample2"),
#'                          remove_suffix=c("_a", "_b"),
#'                          output_path="/path/to/folder/with/results/")
#'
#' @export
makeLumi <- function(input_path,
                      exprs_filename,
                      ctrl_data_filename,
                      bad_samples=NULL,
                      remove_suffix=NULL,
                      output_path="~/") {
    lobj <- lumi::lumiR(file.path(paste0(input_path, exprs_filename)),
                        convertNuID=TRUE,
                        lib.mapping="lumiHumanIDMapping",
                        inputAnnotation=FALSE,
                        QC=FALSE)

    # Control data name without file extension (csv or txt)
    ctrl_probe_name <- unlist(strsplit(ctrl_data_filename, "\\."))[1]
    path <- unlist(strsplit(input_path, "/"))
    dataset_name <- path[length(path)]
    ctrl_probe_name <- paste0(dataset_name, "_", ctrl_probe_name)

    # Fixes control data file. Some of there have an unnamed
    # column with row numbers crashing the addControlData
    # function below.
    control_filename <- fixTroubledFile(file.path(paste0(input_path, ctrl_data_filename)),
                                         path=output_path,
                                         sep="\t",
                                         dataset_name=ctrl_probe_name)

    lobj <- lumi::addControlData2lumi(control_filename, lobj)

    if (!is.null(bad_samples)) {
      to_remove <- Biobase::sampleNames(lobj) %in% bad_samples
      lobj <- lobj[,-which(to_remove)]
    }

    if (!is.null(remove_suffix)) {
      bad_suffixes <- paste(remove_suffix, collapse="|")
      Biobase::sampleNames(lobj) <- gsub(bad_suffixes, "", Biobase::sampleNames(lobj))
    }

    return(lobj)
  }

#' Generate RData datasets from raw files.
#'
#' Creates RData files from expression file, control file, overview and
#' samplesheet.
#'
#' @param input_path Path to the folder, containing data
#' @param exprs_filename Gene expression filename
#' @param overview_filename Overview filename
#' @param samplesheet_filename Samplesheet filename
#' @param ctrl_data_filename Controls data filename
#' @param bad_samples Sample names that should be excluded
#' @param remove_suffix Remove samples with suffix in their name
#' @param output_path Path for the output result
#' @param output_name Name for output
#' @param single_file Save as separate files or all-in-one (single file)
#'
#' @return NULL
#'
#' @author Bjorn Fjukstad, \email{bjorn@cs.uit.no}
#'
#' @seealso \code{\link{makeLumi}}
#'
#' @keywords dataset generate gene expression
#'
#' @examples
#' generateDatasetFile(input_path="/path/to/gene_expression_data_folder/",
#'                       exprs_filename="gene_expression_filename.csv",
#'                       overview_filename="overview_filename.csv",
#'                       samplesheet_filename="samplesheet_filename.csv"
#'                       ctrl_data_filename="controls_data_filename.csv"
#'                       remove_suffix=c("_a", "_b"),
#'                       bad_samples=c("Sample1", "Sample2"),
#'                       output_path="/path/to/folder/with/results/",
#'                       output_name="dataset_name",
#'                       single_file=TRUE)
#'
#' @export
generateDatasetFile <- function(input_path,
                                  exprs_filename,
                                  overview_filename,
                                  samplesheet_filename,
                                  ctrl_data_filename,
                                  bad_samples=NULL,
                                  remove_suffix=NULL,
                                  output_path,
                                  output_name,
                                  single_file=FALSE) {
  lobj <- makeLumi(input_path,
                    exprs_filename,
                    ctrl_data_filename,
                    bad_samples,
                    remove_suffix,
                    input_path)

  samplesheet <- read.table(paste(input_path, samplesheet_filename, sep=""),
                            sep=",",
                            header=TRUE,
                            check.names=FALSE,
                            skip=7)

  samplesheet <- samplesheet[, -c(4, 5, 6)]

  if (overview_filename == "") {
    create_overview <- FALSE
    warning("No overview file specified. Output dataset will not contain an overview object.\n")
  } else {
    create_overview <- TRUE
  }

  if (create_overview) {
    overview <- read.table(paste(input_path, overview_filename, sep=""),
                           sep=";",
                           header=TRUE,
                           check.names=FALSE)

    # Only samples from lumi obj
    overview <- overview[overview$Sample_ID %in% sampleNames(lobj), ]
  }

  # Select only negative controls
  CData.all <- getControlData(lobj)
  negative_controls <- CData.all[CData.all$controlType == "NEGATIVE",]

  # Create output directory if it does not exist
  dir.create(output_path, recursive=TRUE)

  lobj_name=paste0(output_path, output_name, "_lobj.rda")
  if (create_overview) {
    overview_name <- paste0(output_path, output_name, "_overview.rda")
  }
  samplesheet_name <- paste0(output_path, output_name, "_samplesheet.rda")
  negative_controls_name <- paste0(output_path, output_name, "_negative_controls.rda")

  if (single_file){

    # Since we're going to generate dataset files for multiple projects
    # we're prefixing the names of the lobj, overview, samplesheet and
    # negative_controls objects with the output name. E.g.
    # biopsies_all_lobj, biopsies_all_overview etc.

    lobj_name <- paste(output_name, "lobj", sep="_")
    assign(lobj_name, lobj)
    if (create_overview){
      overview_name <- paste(output_name, "overview", sep="_")
      assign(overview_name, overview)
    }

    samplesheet_name <- paste(output_name, "samplesheet", sep="_")
    assign(samplesheet_name, samplesheet)

    negative_controls_name <- paste(output_name, "negative_controls", sep="_")
    assign(negative_controls_name, negative_controls)

    if (create_overview){
      save(list=c(eval(lobj_name),
                  eval(overview_name),
                  eval(samplesheet_name),
                  eval(negative_controls_name)
                  ), file=paste0(output_path, output_name, ".rda"))
    } else {
        save(list=c(eval(lobj_name),
            eval(samplesheet_name),
            eval(negative_controls_name)
      ), file=paste0(output_path, output_name, ".rda"))
    }
  } else {
    save(lobj, file=lobj_name)
    if (create_overview){
      save(overview, file=overview_name)
    }
    save(samplesheet, file=samplesheet_name)
    save(negative_controls, file=negative_controls_name)
  }
  cat(paste("Created dataset", output_name))
}


#' Checks if the file is ready for lumi processing
#'
#' Check to see if the file contains numbers as row names. These files will
#' break the addControlData2lumi function call and must be fixed. We remove
#' all row names and return the filename of the new fixed file. If the file
#' is fine we don't do anything and return the original filename.
#'
#' @param filename Full filename of the data to check
#' @param path Output path for fixed dataset
#' @param sep Separator used for splitting data
#' @param dataset_name Name of the fixed dataset
#'
#' @return Filename of fixed or checked file
#'
#' @author Bjorn Fjukstad, \email{bjorn@cs.uit.no}
#'
#' @seealso \code{\link{generateDatasetFile}}
#'
#' @keywords fix rownames
#'
#' @examples
#'control_filename <- fixTroubledFile(/path/to/control/dataset.csv)),
#'                                      path="/output/path",
#'                                      sep="\t",
#'                                      dataset_name="control_dataset_name")
#'
#' @export
fixTroubledFile <- function(filename,
                              path="~/",
                              sep=";",
                              dataset_name="") {
  # Read file line by line, get number of columns and number of
  # data entries in the second line.
  file <- readLines(filename)
  rows <- strsplit(file, split="\n")
  num_columns <- length(unlist(strsplit(rows[[1]], split=sep)))
  num_data_entries <- length(unlist(strsplit(rows[[2]], split=sep)))

  if (num_columns == num_data_entries) {
    cat("Control data file was ok.")
    return(filename)
  }

  # Pretty regex! What it does is that it substitutes the first
  # occurance of a number (any length) followed by a ';' on
  # every line in the file.  This way we're removing the row
  # names from the file.
  data_rows <- rows[2:length(unlist(rows))]
  without_row_names <- sub(paste0("[0-9]*", sep), "", data_rows)

  # Write contents to file and return filename
  new_filename <- paste0(path, dataset_name, ".csv")

  cat(rows[[1]], sep="\n", file=new_filename)
  cat(without_row_names, sep="\n", file=new_filename, append=TRUE)

  cat(paste0("Wrote control data file to ", new_filename, "\n"))

  return(new_filename)
}


#' Initialize course directory.
#'
#' @return Write out YAML file (course.yml) that collects course information.
#' @export
#'
#' @examples
#' \dontrun{
#'   course_init()
#' }
course_init <- function() {

  if (file.exists("course.yml")) {
    message("Course already set up.")
    invisible()
  }

  institution <- readline(prompt = "Institution: [R University]: ")
  if (institution == "") institution <- "R University"

  course <- readline(prompt = "Course title: ")

  year <- readline(prompt = paste0("Year: [", format(Sys.Date(), "%Y"), "]:"))
  if (year == "") year <- format(Sys.Date(), "%Y")

  language <- readline(prompt = "Language: [en]: ")
  if (language == "") language <- "en"

  logo <- readline(prompt = "Relative path to logo: [NULL]: ")

  reglength <- readline(prompt = "Number of digits in the registration ID: [7]: ")
  reglength <- if (reglength == "") 7 else as.integer(reglength)

  register <- readline(prompt = "Path to registration list (csv): [register.csv]: ")
  register <- if (register == "") "register.csv"

  message("Note: ", register, " must have at least 3 columns: registration, name, id\n",
          "Please make sure that your file conforms to this rule.")

  problems_dir <- readline(prompt = "Path to problems dir: [problems]: ")
  if (problems_dir == "") problems_dir <- "problems"

  nops_dir <- readline(prompt = "Path to create NOPS exams: [nops]: ")
  if (nops_dir == "") nops_dir <- "nops"

  for (d in c(problems_dir, nops_dir)){
    suppressWarnings(res <- dir.create(d, recursive = TRUE))
    if (res) message(d, " created.")
  }
  message("*****")
  message("Package {examtools} produces random seed for an individual \n",
          "NOPS exam, by mixing common random seed and the date of \n",
          "individual examination when producing NOPS exams. \n",
          "Please keep this randomly picked 'common seed' secret!")
  seed = sample(1:10000, 1)

  samples_dir <- file.path(dirname(problems_dir), "samples")
  copy_sample <- readline(prompt = paste0("Copy samples to ",
                                          samples_dir,": [Y]es/No: "))
  if (copy_sample[[1]] %in% c("", "y", "Y")) {
    dir.create(samples_dir)
    exercises <- list.files(system.file("exercises", package = "exams"),
                            pattern = "Rmd$", full.names = TRUE)
    file.copy(exercises, file.path(samples_dir, basename(exercises)))
  }

  yml <- list(
    institution = institution,
    course = course,
    year = year,
    language = language,
    logo = logo,
    reglength = reglength,
    register = register,
    problems_dir = problems_dir,
    nops_dir = nops_dir,
    seed = seed
  )

  yaml::write_yaml(yml, "course.yml")

  message("Course information is saved to ", "course.yml")

}


course_check <- function() {
  if (!file.exists("course.yml")) {
    stop("course.yml does not exist. Please initialize with\n",
         "   course_init()")
  }

  mandatory_fields <- c("institution",
                        "course",
                        "language",
                        "reglength",
                        "register",
                        "problems_dir",
                        "nops_dir",
                        "seed")

  yml <- yaml::read_yaml("course.yml")
  missing <- list()
  for (field in mandatory_fields){
    if (is_empty(yml[[field]])) {
      missing[length(missing) + 1] <- field
    }
  }
  if (length(missing) > 0) {
    stop("Following fields are missing: ", paste(missing, collapse = " "),
         "\nPlease fix ", "course.yml")
  }
  invisible(yml)
}





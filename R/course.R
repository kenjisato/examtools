
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
  if (logo == "") logo <- NULL

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

  message("Package {examtools} mixes common random seed and the date of \n",
          "individual examination when producing NOPS exams.",
          "Please keep this randomly picked 'seed' secret!")
  seed = sample(1:10000, 1)

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

  message("Course information is saved to ", "course.yml")
  yaml::write_yaml(yml, "course.yml")
}

course_check <- function() {

}

exam_create <- function() {

}

exam_scan <- function() {

}

exam_evaluate <- function() {

}



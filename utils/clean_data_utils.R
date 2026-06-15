read_group <- function(filename, lecture_type_value, practice_type_value, group_value) {
  path <- file.path(data_raw_dir, filename)
  
  if (!file.exists(path)) {
    stop("Raw file not found: ", path)
  }
  
  read_csv(path, show_col_types = FALSE) |>
    mutate(
      lecture_type = lecture_type_value,
      practice_type = practice_type_value,
      group = group_value,
      source_file = filename
    )
}

first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  as.character(x[1])
}

is_nonempty_text <- function(x) {
  !is.na(x) & str_trim(as.character(x)) != ""
}
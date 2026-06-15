parse_r_from_label <- function(label) {
  if (is.na(label)) return(NA_real_)
  
  label_clean <- as.character(label) |>
    str_replace_all("\u2212", "-")
  
  matched <- str_match(
    label_clean,
    regex("Correlation\\s*=\\s*(-?\\s*[0-9.]+)", ignore_case = TRUE)
  )
  
  if (is.na(matched[1, 2])) return(NA_real_)
  
  num_str <- matched[1, 2] |>
    str_remove_all("\\s")
  
  abs(as.numeric(num_str))
}

extract_add_points_stats <- function(answer_str) {
  tryCatch({
    pts <- jsonlite::fromJSON(answer_str)
    
    if (is.null(pts) || length(pts) == 0 || nrow(pts) == 0) {
      return(list(
        n_points = 0L,
        final_r = NA_real_,
        final_slope = NA_real_
      ))
    }
    
    last <- pts[nrow(pts), ]
    
    list(
      n_points = nrow(pts),
      final_r = last$after$r,
      final_slope = last$after$slope
    )
  }, error = function(e) {
    list(
      n_points = NA_integer_,
      final_r = NA_real_,
      final_slope = NA_real_
    )
  })
}

parse_correct_answer <- function(img_label, stat_type) {
  if (is.na(img_label) || is.na(stat_type)) return(NA_real_)
  
  pattern <- paste0(stat_type, "\\s*=\\s*(-?[0-9.]+)")
  m <- regmatches(
    img_label,
    regexpr(pattern, img_label, ignore.case = TRUE)
  )
  
  if (length(m) == 0 || m == "") return(NA_real_)
  
  as.numeric(
    sub(
      paste0(stat_type, "\\s*=\\s*"),
      "",
      m,
      ignore.case = TRUE
    )
  )
}
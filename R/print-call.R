s4_dict <- collections::Queue$new()

print_call <- function(name, ..., result = NULL) {
  args <- list(...)

  is_drv <- purrr::map_lgl(args, inherits, "DBIDriver")
  is_conn <- purrr::map_lgl(args, inherits, "DBIConnection")
  is_result <- purrr::map_lgl(args, inherits, "DBIResult")

  if (is.call(name)) {
    call <- rlang::eval_tidy(rlang::quo(as.call(list(name, ...))))
  } else {
    call <- rlang::eval_tidy(rlang::quo(call(name, find_s4_dict(args[[1]]), !!!args[-1])))
  }
  on.exit(print(styler::style_text(deparse(call, width.cutoff = 80))))

  force(result)
  if (!is.null(result)) {
    new_obj <- add_s4_dict(result)
    call <- call("<-", new_obj, call)
  }

  invisible(call)
}

find_s4_dict <- function(x) {
  all_s4 <- s4_dict$as_list()
  for (i in seq_along(all_s4)) {
    s4_i <- all_s4[[i]]
    if (identical(s4_i$obj, x)) {
      return(s4_i$name)
    }
  }

  stop("Not found.")
}

add_s4_dict <- function(x) {
  if (inherits(x, "DBIResult")) prefix <- "res"
  else if (inherits(x, "DBIConnection")) prefix <- "conn"
  else if (inherits(x, "DBIDriver")) prefix <- "drv"
  else stop("Unknown class")

  all_s4 <- s4_dict$as_list()
  all_names <- purrr::map_chr(purrr::map(all_s4, "name"), rlang::as_string)

  prefix_names <- grep(paste0("^", prefix), all_names, value = TRUE)
  suffixes <- as.integer(gsub(paste0("^", prefix), "", prefix_names))

  max_prefix <- max(c(suffixes, 0L))

  new_name <- as.name(paste0(prefix, max_prefix + 1L))

  s4_dict$push(
    list(
      obj = x,
      name = new_name
    )
  )

  new_name
}

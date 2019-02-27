s4_dict <- collections::Stack$new()

log_call <- function(call) {
  quo <- enquo(call)
  expr <- quo_get_expr(quo)
  env <- quo_get_env(quo)

  args <- purrr::map(expr[-1], ~ eval_tidy(., env))

  args <- purrr::map(args, find_s4_dict)
  new_call <- call2(expr[[1]], !!!args)
  #on.exit(print(styler::style_text(deparse(new_call, width.cutoff = 80))))
  on.exit({
    cat(deparse(new_call, width.cutoff = 80), sep = "\n")
    if (isTRUE(result$visible)) {
      print(result$value)
    }
  })

  visible_quo <- rlang::new_quosure(call2(withVisible, expr), env)
  result <- eval_tidy(visible_quo)

  new_obj <- add_s4_dict(result$value)
  if (!is.null(new_obj)) {
    new_call <- call2("<-", new_obj, new_call)
  }

  if (result$visible) {
    result$value
  } else {
    invisible(result$value)
  }
}

find_s4_dict <- function(x) {
  all_s4 <- s4_dict$as_list()
  for (i in seq_along(all_s4)) {
    s4_i <- all_s4[[i]]
    if (identical(s4_i$obj, x)) {
      return(s4_i$name)
    }
  }

  # Not found
  x
}

add_s4_dict <- function(x) {
  if (inherits(x, "DBIResult")) prefix <- "res"
  else if (inherits(x, "DBIConnection")) prefix <- "conn"
  else if (inherits(x, "DBIDriver")) prefix <- "drv"
  else return(NULL)

  # Doesn't work yet (?)
  #if (!is.null(find_s4_dict(x))) return(NULL)

  all_s4 <- s4_dict$as_list()
  all_names <- purrr::map_chr(purrr::map(all_s4, "name"), as_string)

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

clear_s4_dict <- function() {
  # https://github.com/randy3k/collections/issues/3
  while (s4_dict$size() > 0) s4_dict$pop()
}

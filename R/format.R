fmt <- function(..., .envir = parent.frame()) {
  if(requireNamespace("cli", quietly = TRUE)) {
    cli::format_inline(..., .envir = .envir)
  } else {
    glue::glue(..., .envir = .envir, .transformer = fmt_transformer)
  }
}

fmt_transformer <- function(text, envir) {
  m <- regexpr("^\\.[a-z]+\\s*", text, ignore.case = TRUE)
  value <- if(m != -1) {
    # extract the modifier
    modifier <- sub("\\s*$", "", regmatches(text, m))

    # interpolate the text (need another call to glue)
    regmatches(text, m) <- ""
    text <- glue::glue(text, .envir = envir)

    switch(modifier,
      .arg = glue::backtick(text),
      .q = glue::single_quote(text),
      {
        abort(glue::glue("unknown glue modifier {glue::single_quote(modifier)}"))
      }
    )
  } else {
    eval(str2expression(text), envir)
  }
  if(length(value)>1) {
    value <- glue::glue_collapse(value, sep = ", ", width = 40, last = " and ")
  }

  value
}

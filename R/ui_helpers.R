# Small shared UI helpers (plain Shiny tags — no bslib version surprises).

money0 <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "—", paste0("$", formatC(x, format = "f", digits = 0, big.mark = ",")))
}
num1 <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "—", formatC(x, format = "f", digits = 1, big.mark = ","))
}

fmt_time <- function(epoch) format(as.POSIXct(as.numeric(epoch), origin = "1970-01-01"),
                                   "%H:%M:%S")

# Render a data.frame as a Bootstrap table inside a renderUI() block.
html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(shiny::p(class = "pg-muted", "—"))
  cells <- function(r) lapply(df[r, ], function(v) shiny::tags$td(as.character(v)))
  shiny::tags$table(class = "table table-sm",
    shiny::tags$thead(shiny::tags$tr(lapply(names(df), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(df)), function(r) shiny::tags$tr(cells(r)))))
}

# One labelled statistic for the result card.
stat_block <- function(label, value) shiny::div(
  style = "display:inline-block; min-width:130px; margin:.3rem 1rem .3rem 0;",
  shiny::div(class = "pg-muted", style = "font-size:.85rem;", label),
  shiny::div(style = "font-size:1.4rem; font-weight:600;", value)
)

base_url <- function(session) {
  cd   <- session$clientData
  port <- if (!is.null(cd$url_port) && nzchar(cd$url_port)) paste0(":", cd$url_port) else ""
  paste0(cd$url_protocol, "//", cd$url_hostname, port, cd$url_pathname)
}

# Small shared UI helpers (plain Shiny tags — no bslib version surprises).

money0 <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "—", paste0("$", formatC(x, format = "f", digits = 0, big.mark = ",")))
}
# Prices show to the cent, so a student who types $19.50 sees $19.50.
money2 <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "—", paste0("$", formatC(x, format = "f", digits = 2, big.mark = ",")))
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

# A styled countdown. `tl` = seconds left (NA -> nothing). Colour shifts
# green -> amber -> red; at 0 it reads "0:00".
countdown_tag <- function(tl, size = "2.2rem", note = NULL) {
  if (length(tl) != 1 || is.na(tl)) return(NULL)
  col <- if (tl <= 0) "#dc3545" else if (tl <= 15) "#fd7e14" else "#198754"
  txt <- if (tl <= 0) "0:00" else sprintf("%d:%02d", tl %/% 60, floor(tl) %% 60)
  shiny::div(
    style = sprintf("font-weight:700;font-variant-numeric:tabular-nums;line-height:1;color:%s;font-size:%s;", col, size),
    txt,
    if (!is.null(note)) shiny::div(style = "font-size:.85rem;font-weight:500;color:#6c757d;", note))
}

base_url <- function(session) {
  cd   <- session$clientData
  port <- if (!is.null(cd$url_port) && nzchar(cd$url_port)) paste0(":", cd$url_port) else ""
  paste0(cd$url_protocol, "//", cd$url_hostname, port, cd$url_pathname)
}

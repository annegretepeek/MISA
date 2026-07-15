# global.R

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(arrow)
library(ggplot2)
library(plotly)
library(zoo)
library(htmltools)

# ---- data paths ------------------------------------------------------------
DATA_DIR <- "data"
PATH_ACTIVE_COMPANIES <- file.path(DATA_DIR, "active_companies.parquet")
PATH_COMPANY_YEAR     <- file.path(DATA_DIR, "company_year")
PATH_EMTA_QUARTERLY   <- file.path(DATA_DIR, "emta_quarterly")

# ---- lazy datasets ---------------------------------------------------------
# active_companies is small and is kept in memory for selectize choices.
# company_year and emta_quarterly stay lazy; data is loaded only after collect().
active_companies <- read_parquet(PATH_ACTIVE_COMPANIES) %>%
  mutate(registrikood = as.character(registrikood)) %>%
  arrange(nimi)

company_year_ds <- open_dataset(PATH_COMPANY_YEAR) 
emta_quarterly_ds <- open_dataset(PATH_EMTA_QUARTERLY)

maakonnad <- active_companies %>%
  distinct(maakond) %>%
  filter(!is.na(maakond), maakond != "") %>%
  arrange(maakond) %>%
  pull(maakond)

ettevotted <- active_companies %>%
  transmute(label = paste0(nimi, " (", registrikood, ")"), value = registrikood)

# ---- helpers ---------------------------------------------------------------
fmt_num <- function(x, digits = 0) {
  prettyNum(round(x, digits), big.mark = " ", scientific = FALSE)
}

kymne_aste <- function(x) {
  max_vaartus <- suppressWarnings(max(abs(x[is.finite(x)]), na.rm = TRUE))

  if (!is.finite(max_vaartus)) {
    return(list(div = 1, label = ""))
  }

  if (max_vaartus > 1e9) {
    list(div = 1e9, label = "(miljardites)")
  } else if (max_vaartus > 1e6) {
    list(div = 1e6, label = "(miljonites)")
  } else if (max_vaartus > 1000) {
    list(div = 1000, label = "(tuhandetes)")
  } else {
    list(div = 1, label = "")
  }
}

add_derived_metrics <- function(df) {
  df %>%
    mutate(
      aeg = zoo::as.yearqtr(paste(aasta, kvartal), format = "%Y %q"),
      kaive_tootaja = if_else(!is.na(tootajad) & tootajad > 0, kaive / tootajad, NA_real_),
      tmaksud_tootaja = if_else(!is.na(tootajad) & tootajad > 0, toomaksud / tootajad, NA_real_),
      rmaksud_kaive = if_else(!is.na(kaive) & kaive > 0, 100 * rmaksud / kaive, NA_real_),
      tmaksud_kaive = if_else(!is.na(kaive) & kaive > 0, 100 * toomaksud / kaive, NA_real_)
    )
}

safe_one <- function(x) {
  if (length(x) == 0 || all(is.na(x))) NA else x[which(!is.na(x))[1]]
}

plot_metric <- function(df, metric, title, unit = "", percent = FALSE) {
  req(nrow(df) > 0)

  y <- df[[metric]]
  scale <- if (percent) list(div = 1, label = "%") else kymne_aste(y)
  y_lab <- if (percent) "%" else paste(unit, scale$label)

  p <- ggplot(
    df,
    aes(
      x = aeg,
      y = .data[[metric]] / scale$div,
      color = grupp,
      group = grupp,
      text = paste0(
        grupp, "\n",
        format(aeg, "%Y Q%q"), "\n",
        title, ": ", fmt_num(.data[[metric]], if (percent) 1 else 0),
        if (percent) "%" else ifelse(unit == "€", " €", "")
      )
    )
  ) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 0.8, na.rm = TRUE) +
    theme_bw() +
    labs(title = title, x = NULL, y = y_lab) +
    guides(color = guide_legend(title = NULL)) +
    scale_x_yearqtr(format = "%Y Q%q") +
    expand_limits(y = 0)

  ggplotly(p, tooltip = "text") %>%
    config(displayModeBar = FALSE)
}

#' Daily and hourly activity visuals
#'
#' Two sub-panels:
#' events per day and events per hour.
#'
#' @param path Path to directory containing \code{BirdNET.xlsx}
#' @param taxon Taxon name (matched against the \code{Taxon} column)
#' @param model Model used to classify; one of \code{"BirdNET_V2.4"} or
#'   \code{"Perch v2"}
#'
#' @return A \code{patchwork} object with two side-by-side ggplot panels
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate hour as_date
#' @importFrom readxl read_xlsx
#' @importFrom tidyr complete
#' @importFrom egg theme_presentation
#' @importFrom patchwork plot_layout plot_annotation
#' @export
#'
birdNET_graph <- function(path, taxon, model = c("BirdNET v2.4", "Perch v2", "combined")) {

  model <- match.arg(model)
  # Load existing workbook
  if (model == 'BirdNET v2.4') {
    xlsx    <- 'BirdNET.xlsx'
  } else if (model == 'Perch v2') {
    xlsx <- 'Perch.xlsx'
  } else if (model == 'combined') {
    xlsx <- basename(path)
    path <- dirname(path)
  }

  # Silence R CMD CHECK notes for NSE column names
  Taxon <- T1 <- hh <- dd <- Verification <- n <- NULL

  ## 1. Load & prepare data
  df <- readxl::read_xlsx(file.path(path, xlsx)) |>
    dplyr::filter(Taxon == taxon) |>
    dplyr::mutate(
      hh = lubridate::hour(T1),
      dd = lubridate::as_date(T1),
      Verification = dplyr::case_when(
        Verification == TRUE  | tolower(as.character(Verification)) == "t" ~ "Verified",
        Verification == FALSE | tolower(as.character(Verification)) == "f" ~ "Rejected",
        Verification == '?' ~ "?",
        .default = "Not verified"
      )
    )

  ## 2. Error rate subtitle
  verified_df <- dplyr::filter(df, Verification %in% c("Verified", "Rejected"))
  n_sample    <- nrow(verified_df)
  n_rejected  <- sum(verified_df$Verification == "Rejected")

  if (n_sample > 0) {
    reliability_rate <- round(100 - (100 * n_rejected / n_sample), 1)
    subtitle_text <- paste0(
      "Reliability: ", reliability_rate, "%",
      "  (n = ", n_sample, " verified detections)"
    )
  } else {
    subtitle_text <- "Reliability: no verified detections"
  }

  ## 3. Shared fill scale
  fill_scale <- scale_fill_manual(
    values = c(
      Verified      = "#4daf4a",
      Rejected      = "#e41a1c",
      `?`           = 'darkgray',
      "Not verified" = "#377eb8"
    ),
    guide = guide_legend(title = "Verification")
  )

  ## Plot A: events per day
  plot_day <- df |>
    dplyr::count(dd, Verification) |>
    tidyr::complete(dd, Verification, fill = list(n = 0)) |>
    ggplot(aes(x = dd, y = n, fill = Verification)) +
    geom_bar(stat = "identity", col = "black", linewidth = 0.3) +
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    fill_scale +
    labs(
      x = "Date",
      y = "Events"
    ) +
    egg::theme_presentation(base_size = 20) +
    theme(panel.grid    = element_blank(),
          axis.text.x   = element_text(angle = 45, hjust = 1))

  ## 5. Plot B: events per hour
  plot_hour <- df |>
    dplyr::count(hh, Verification) |>
    tidyr::complete(hh = 0:23, Verification, fill = list(n = 0)) |>
    ggplot(aes(x = hh, y = n, fill = Verification)) +
    geom_bar(stat = "identity", col = "black", linewidth = 0.3) +
    scale_x_continuous(breaks = seq(0, 23, by = 3), expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    fill_scale +
    labs(
      x = "Hour",
      y = ""
    ) +
    egg::theme_presentation(base_size = 20) +
    theme(panel.grid    = element_blank())


  ## 6. Combine with patchwork
  combined <- (plot_day + plot_hour) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title    = taxon,
      subtitle = subtitle_text,
      caption  = paste0(
        nrow(df), " event(s)  (", model, ")\n",
        min(df$T1), " - ", max(df$T1)),
      theme = theme(plot.title    = element_text(size = 18, face = "bold"),
                    plot.subtitle = element_text(size = 14, face = "italic"),
                    plot.caption  = element_text(size = 12)))
  return(combined)
}

#' Heatmap of BirdNET detections
#'
#' @inheritParams birdNET_graph
#' @param db data frame or path to xlsx file
#' @export
#'
birdNET_heatmap <- function(db, taxon) {

  message(db)
  message(taxon)

  Taxon <- T1 <- Verification <- dd <- hh <- NULL

  if (is.data.frame(db)) {
    df <- db
  } else {
    df <- readxl::read_xlsx(db)
  }

  df <- df |>
    dplyr::filter(Taxon == taxon) |>
    dplyr::mutate(
      hh = lubridate::hour(T1),
      dd = lubridate::as_date(T1),
      Verification = dplyr::case_when(
        Verification == TRUE  | tolower(as.character(Verification)) == "t" ~ "T",
        Verification == FALSE | tolower(as.character(Verification)) == "f" ~ "F",
        Verification == '?' ~ "?",
        .default = "Not verified"
      )
    )

  verified_df <- dplyr::filter(df, Verification %in% c("T", "F"))
  n_sample    <- nrow(verified_df)
  n_rejected  <- sum(verified_df$Verification == "F")

  if (n_sample > 0) {
    reliability_rate <- round(100 - (100 * n_rejected / n_sample), 1)
    subtitle_text <- paste0(
      "Reliability: ", reliability_rate, "%",
      "  (n = ", n_sample, " verified detections)"
    )
  } else {
    subtitle_text <- "Reliability: no verified detections"
  }

  heat_df <- df |>
    dplyr::count(dd, hh) |>
    tidyr::complete(
      dd  = seq(min(dd), max(dd), by = "day"),
      hh  = 0:23,
      fill = list(n = 0)
    ) |>
    dplyr::mutate(n = dplyr::na_if(n, 0))

  p <-
    ggplot(heat_df, aes(x = dd, y = hh, fill = n)) +
    geom_tile(color = "black", linewidth = 0.02) +
    geom_text(aes(label = ifelse(n > 0, n, "")), size = 3, na.rm = T) +
    scale_x_date(
      date_labels = "%d.%m",
      date_breaks = "1 day",
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = 0:23,
      expand = c(0, 0),
      trans  = "reverse"       # Stunde 0 oben, 23 unten
    ) +
    scale_fill_gradient(
      low  = "#f7fcb9",
      high = "#41ab5d",
      na.value = 'white',
      guide = 'none',
      name = NULL) +
    labs(x        = "Date",
         y        = "Hour",
         title    = paste(taxon, subtitle_text),
         caption  = paste0(nrow(df), " event(s)", "\n",
                           min(df$T1), " - ", max(df$T1))) +
    egg::theme_presentation(base_size = 14) +
    theme(panel.grid   = element_blank(),
          axis.text.x  = element_text(angle = 45, hjust = 1),)
  return(p)
}

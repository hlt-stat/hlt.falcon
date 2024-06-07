#' Count occurrences of a variable in a dataframe
#'
#' This function counts the occurrences of a variable in a dataframe, and calculates the proportion of each occurrence.
#'
#' @param df The input dataframe.
#' @param .var The variable to count occurrences.
#' @param .N_col The column to count. This parameter is ignored by lintr.
#' @param df_denom An optional dataframe for denominator. Default is NULL.
#' @param denom A character vector for denominator. Default is c("N_s", "N_col", "n").
#' @param id_var The variable for id. Default is "USUBJID".
#' @param arm_var The variable for arm. Default is "ARM".
#' @return A dataframe with the count and proportion of each occurrence of the variable.
#' @export
#' @importFrom dplyr filter select distinct mutate arrange
#' @examples
#' \dontrun{
#' a_count_occurrences_trtem_ae(df, .var = "AGEGR1")
#' }
a_count_occurrences_trtem_ae <- function(df,
                                         .var,
                                         .N_col, # nolint
                                         df_denom = NULL,
                                         denom = c("N_s", "N_col", "n"),
                                         id_var = "USUBJID",
                                         arm_var = "ARM") {
  df <- df %>% dplyr::filter(TRTEMFL == "Y")
  occurrences <- df[[.var]]
  ids <- factor(df[[id_var]])
  has_occurrence_per_id <- base::table(occurrences, ids) > 0
  n_ids_per_occurrence <- as.list(base::rowSums(has_occurrence_per_id))
  lvls <- names(n_ids_per_occurrence)

  denom <- base::match.arg(denom)
  if (denom == "N_s" && is.null(df_denom)) {
    stop("If using subgroup population counts, `df_denom` must be specified.") # nocov
  }
  dn <- base::switch(denom,
               N_s = lapply(
                 lvls,
                 function(x) {
                   df_denom %>%
                     dplyr::filter(.data[[.var]] == x, .data[[arm_var]] == df[[arm_var]][1]) %>%
                     dplyr::select(USUBJID) %>%
                     dplyr::distinct() %>%
                     nrow()
                 }
               ),
               n = base::nlevels(ids),
               N_col = .N_col
  )
  if (denom == "N_s") names(dn) <- lvls

  x_stats <- lapply(
    lvls,
    function(x) {
      i <- n_ids_per_occurrence[[x]]
      denom <- if (denom == "N_s") dn[[x]] else dn
      if (i == 0 && denom == 0) c(0, 0) else c(i, i / denom)
    }
  )
  names(x_stats) <- names(n_ids_per_occurrence)

  in_rows(
    .list = x_stats,
    .formats = tern::format_count_fraction
  )
}

#' Load libraries and data
#'
#' This function loads the necessary libraries and data for the analysis.
#'
#' @return A list containing the dataframe for analysis and the original adsl dataframe.
#' @export
#' @examples
#' \dontrun{
#' data <- load_libraries_and_data()
#' }
load_libraries_and_data <- function() {
  adsl <- random.cdisc.data::cadsl %>%
    dplyr::mutate(AGEGR1 = as.factor(dplyr::case_when(
      AGE >= 17 & AGE < 65 ~ ">=17 to <65",
      AGE >= 65 ~ ">=65",
      AGE >= 65 & AGE < 75 ~ ">=65 to <75",
      AGE >= 75 ~ ">=75"
    )) %>% formatters::with_label("Age Group, years")) %>%
    formatters::var_relabel(AGE = "Age, years")

  adae <- random.cdisc.data::cadae

  df <- dplyr::left_join(adsl, adae, by = intersect(names(adsl), names(adae)))
  df
  list(df = df, adsl = adsl)
}

#' Create Table 22
#'
#' This function creates Table 22, which is a demographic table in clinical trial reports.
#'
#' @param df The input dataframe.
#' @param adsl The ADSL dataframe.
#' @return A dataframe that represents Table 22.
#' @export
#' @examples
#' \dontrun{
#' table_22 <- create_table_22(df, adsl)
#' }
create_table_22 <- function(df, adsl) {
  tbl <- falcon::make_table_22(df = df, alt_counts_df = adsl, denom = "N_s")
  ft <- rtables::tt_to_flextable(tbl)
  df_tbl <- ft$body$dataset

  df_tbl <- df_tbl %>%
    dplyr::mutate(V1 = ifelse(V1 %in% c("F", "M", ">=17 to <65", ">=65", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "OTHER", "UNKNOWN", "HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "NOT REPORTED", "UNKNOWN"), paste0("  ", V1), V1))

  df_tbl  # Return df_tbl
}

#' Create Sassy Report
#'
#' This function creates a sassy report based on the input dataframe.
#'
#' @param df_tbl The input dataframe.
#' @param titles The titles for the report.
#' @param footnotes The footnotes for the report.
#' @param page_header The header for each page.
#' @param page_footer The footer for each page.
#' @param headers The headers for the spanning columns.
#' @param column_labels The labels for the columns.
#' @param file The file name for the report. Default is "fda-table-22.rtf".
#' @return The path of the created report.
#' @export
#' @examples
#' \dontrun{
#' report_path <- create_sassy_report(
#'   df_tbl,
#'   headers,
#'   titles,
#'   footnotes,
#'   page_header,
#'   page_footer,
#'   column_labels
#' )
#' }
create_sassy_report <- function(df_tbl, titles, footnotes, page_header, page_footer, headers,column_labels, file = "fda-table-22.rtf") {
  tbl <- reporter::create_table(df_tbl, first_row_blank = TRUE, borders = "bottom") %>%
    reporter::column_defaults(from = "V1", to = "V4", align = "left", width = 1.5) %>%
    reporter::spanning_header(2, 4, label = headers) %>%
    reporter::define(var = "V1", label = column_labels[[1]], width = 2.5) %>%
    reporter::define(var = "V2", label = column_labels[[2]], align = "right") %>%
    reporter::define(var = "V3", label = column_labels[[3]], align = "right") %>%
    reporter::define(var = "V4", label = column_labels[[4]], align = "right") %>%
    reporter::titles(titles, borders = "bottom", bold = TRUE) %>%
    reporter::footnotes(footnotes)


  rpt <- reporter::create_report(file = file,
                       paper_size = "RD4",
                       output_type = "RTF",
                       font = "Times", font_size = 10) %>%
    reporter::page_header(width = 2,
                right = page_header,
                blank_row = "below") %>%
    reporter::set_margins(top = 1.26, bottom = 1.26, left = 1, right = 1) %>%
    reporter::add_content(tbl, align = "centre") %>%
    reporter::page_footer(left = page_footer, right = "Page [pg] of [tpg]")

  res <- reporter::write_report(rpt)

  res$modified_path
}


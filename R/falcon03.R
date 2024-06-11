#' Load libraries and preprocess data for Falcon 03
#'
#' This function loads the necessary libraries and preprocesses the ADSL data for the Falcon 03 analysis.
#' The preprocessing includes setting a seed for reproducibility, adding missing values to the RANDDT column,
#' and adding new variables (ENRLDT, SCRNFL, SCRNFRS, SCRNFAILFL) to the dataframe.
#'
#' @return A dataframe named adsl

load_libraries_and_data_03 <- function() {
    # Preprocess adsl data
    set.seed(1)

    adsl <- random.cdisc.data::cadsl

    adsl[["RANDDT"]][sample(seq_len(nrow(adsl)), 100)] <- NA

    adsl <- adsl %>%
        dplyr::mutate(
            ENRLDT = .data$RANDDT,
            SCRNFL = "Y",
            SCRNFRS = factor(sample(
                c("Inclusion/exclusion criteria not met", "Patient noncompliance", "Consent withdrawn", "Other"),
                size = nrow(adsl), replace = TRUE
            ), levels = c("Inclusion/exclusion criteria not met", "Patient noncompliance", "Consent withdrawn", "Other")),
            SCRNFAILFL = ifelse(is.na(.data$ENRLDT), "Y", "N")
        )

    adsl[["SCRNFRS"]][adsl[["SCRNFL"]] == "N" | !is.na(adsl[["ENRLDT"]])] <- NA

    return(adsl)
}

#' Create Disposition Table
#'
#' This function creates a disposition table for clinical trial reports, often referred to as Table 03.
#' It uses the `make_table_03` function from the `falcon` package, which is not exported and therefore accessed directly.
#'
#' @param adsl The ADSL dataframe containing the subject-level analysis data.
#' @return A dataframe that represents the disposition table.
#' @export
#' @examples
#' \dontrun{
#' disposition_table <- create_table_03(adsl)
#' }
create_table_03 <- function(adsl) {
    tbl <- falcon::make_table_03(df = adsl, scrnfl_var = "SCRNFL", scrnfailfl_var = "SCRNFAILFL", scrnfail_var = "SCRNFRS")
    ft <- rtables::tt_to_flextable(tbl)
    df_tbl <- ft$body$dataset
    df_tbl <- df_tbl %>%
        dplyr::mutate(V1 = ifelse(V1 %in% c("Inclusion/exclusion criteria not met", "Patient noncompliance", "Consent withdrawn", "Other"), paste0("  ", V1), V1))
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

create_sassy_report_03 <- function(df_tbl, titles, footnotes, page_header, page_footer, headers,column_labels, file = "fda-table-03.rtf") {
  tbl <- reporter::create_table(df_tbl, first_row_blank = TRUE, borders = "bottom") %>%
    reporter::column_defaults(from = "V1", to = "V3", align = "left", width = 1.5) %>%
    reporter::spanning_header(2, 4, label = headers) %>%
    reporter::define(var = "V1", label = column_labels[[1]], width = 2.5) %>%
    reporter::define(var = "V2", label = column_labels[[2]], align = "right") %>%
    reporter::define(var = "V3", label = column_labels[[3]], align = "right") %>%
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


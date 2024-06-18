#' @title Make Table 02
#' @description This function creates a table with specified variables and labels. The labels are passed as an argument to the function. The function also modifies the 'V1' column in the resulting table based on the labels passed.
#' @details
#' * `df` must contain the variables specified by `vars`, `arm_var`, and `saffl_var`.
#' * If specified, `alt_counts_df` must contain the variables specified by `arm_var`, `id_var`, and `saffl_var`.
#' * Flag variables (i.e. `XXXFL`) are expected to have two levels: `"Y"` (true) and `"N"` (false). Missing values in
#'   flag variables are treated as `"N"`.
#' * Columns are split by arm. Overall population column is included by default (see `lbl_overall` argument).
#' * Information from either ADSUB or ADVS is generally included into `df` prior to analysis.
#' * Numbers in table for non-numeric variables represent the absolute numbers of patients and fraction of `N`.
#' * All-zero rows are removed by default (see `prune_0` argument).
#' @describeIn make_table_02 Create FDA table 2 using functions from `rtables` and `tern`.
#' @importFrom tern df_explicit_na format_auto analyze_vars 
#' @importFrom rtables basic_table 
#' @importFrom falcon alt_counts_df_preproc	 basic_table_annot make_table_02  
#' @param df A data frame.
#' @param alt_counts_df An alternative counts data frame. Default is NULL.
#' @param show_colcounts Logical. If TRUE, column counts are shown. Default is TRUE.
#' @param arm_var The arm variable. Default is "ARM".
#' @param saffl_var The saffl variable. Default is "SAFFL".
#' @param vars A character vector of variable names. Default is c("AGE", "HEIGHTBL", "WEIGHTBL","BMIBL").
#' @param vars_no_n A character vector of variable names without 'n'. Default is c("SEX", "ETHNICC").
#' @param lbl_vars A named character vector of variable labels. Default is formatters::var_labels(df, fill = TRUE)[c(vars, vars_no_n)].
#' @param lbl_overall The label for overall. Default is "Total Population".
#' @param na_rm Logical. If TRUE, NA values are removed. Default is FALSE.
#' @param prune_0 Logical. If TRUE, 0 values are pruned. Default is TRUE.
#' @param annotations Annotations for the table. Default is NULL.
#' @param .labels A named character vector of labels for the statistics. Default is c(n = "Number (Missing)", mean_sd = "Mean (SD)", median = "Median", quantiles = c("Q1. Q3"), range =c("Min. Max"), count_fraction = "n").
#' @return A table.
#' @export
#' @examples
#' \dontrun{
#' vars <- c("AGE", "SEX", "ETHNICC")
#' .labels <- c(n = "例数 (缺失)", mean_sd = "均值(标准差)")
#' make_table_02_huarun(df = data.frame(), vars = vars, .labels = .labels)
#' }
make_table_02_huarun <- function (df, alt_counts_df = NULL, show_colcounts = TRUE, arm_var = "ARM",
                                  saffl_var = "SAFFL",
                                  vars = c("AGE", "HEIGHTBL", "WEIGHTBL","BMIBL"),
                                  vars_no_n = c("SEX", "ETHNICC"),
                                  lbl_vars = formatters::var_labels(df, fill = TRUE)[c(vars, vars_no_n)],
                                  lbl_overall = "Total Population",
                                  na_rm = FALSE,
                                  prune_0 = TRUE,
                                  annotations = NULL,
                                  .labels = c(n = "Number (Missing)", mean_sd = "Mean (SD)", median = "Median",
                                              quantiles = c("Q1. Q3"), range =c("Min. Max"), count_fraction = "n (%)")) {
    checkmate::assert_subset(c(vars, vars_no_n, arm_var, saffl_var), names(df))
    df <- df %>% filter(.data[[saffl_var]] == "Y") %>% df_explicit_na()
    alt_counts_df <- alt_counts_df_preproc(alt_counts_df, arm_var, saffl_var)

    format_stat <- function(stat) {
        function(x) format_auto(df[[stat]], stat)(x)
    }

    lyt <- basic_table_annot(show_colcounts, annotations) %>%
        split_cols_by_arm(arm_var, lbl_overall) %>%
        analyze_vars(vars = vars,
                     var_labels = lbl_vars[vars],
                     show_labels = "visible",
                     .formats = c("mean_sd" = "xx.x (xx.x)", "median" = "xx.x", "quantiles" = "xx.x - xx.x", "range" = "xx.x - xx.x", "count_fraction" = "xx. (xx.%)"),
                     .labels = .labels,
                     na.rm = na_rm) %>%
        append_topleft("Characteristic")
    tbl <- build_table(lyt, df = df, alt_counts_df = alt_counts_df)

    if (prune_0)
        tbl <- prune_table(tbl)

    tbl
}

#' @title Load Libraries and Data
#' @description This function loads the necessary libraries and data for the analysis. It imports the 'haven' package to read the 'adsl' and 'advs' data from the specified paths, filters the 'advs' data for the baseline visit, selects the 'USUBJID' and 'AVAL' columns, and then joins the 'adsl' and filtered 'advs' data on the 'USUBJID' column. The paths to the 'adsl' and 'advs' data are currently hard-coded and need to be updated for different datasets.
#' @return A list containing two dataframes: 'anl', which is the result of joining the 'adsl' and filtered 'advs' data on the 'USUBJID' column, and the original 'adsl' dataframe.
#' @import haven
#' @export
#' @examples
#' \dontrun{
#' data <- load_libraries_and_data()
#' # 'data' is a list containing 'anl' and 'adsl' dataframes
#' }

load_libraries_and_data <- function() {
    adsl <- haven::read_sas("C:/Users/JJ/Desktop/github/data/adsl.sas7bdat")
    advs <- haven::read_sas("C:/Users/JJ/Desktop/github/data/advs.sas7bdat")
    adsl <- adsl
    advs <- advs %>%
    dplyr::filter(AVISIT == "BASELINE") %>%
    dplyr::select("USUBJID", "AVAL")
    anl <- dplyr::left_join(adsl, advs, by = "USUBJID")
}

#' Create Demographic Table
#'
#' This function creates a demographic table for clinical trial reports, often referred to as Table 02.
#' It uses the `make_table_02_huarun` function, which generates a table with specified variables and labels. 
#' The function also modifies the 'V1' column in the resulting table based on the labels passed.
#'
#' @param df The input dataframe containing the raw data.
#' @param vars A character vector of variable names. Default is c("AGE", "SEX", "ETHNICC", "HEIGHTBL", "WEIGHTBL","BMIBL").
#' @param lbl_vars A named character vector of variable labels. Default is c(AGE = "年龄（岁）", SEX = "性别, n", ETHNICC = "民族, n", HEIGHTBL = "身高（cm）", WEIGHTBL = "体重（kg）", BMIBL = "基线体重指数（kg/m2)").
#' @param .labels A named character vector of labels for the statistics. Default is c(n = "例数 (缺失)", mean_sd = "均值(标准差)", median = "中位数", quantiles = "Q1. Q3", range = "最小值. 最大值", count_fraction = "n").
#' @return A dataframe that represents the demographic table.
#' @export
#' @examples
#' \dontrun{
#' vars <- c("AGE", "SEX", "ETHNICC")
#' demographic_table <- create_table_02(df, vars = vars)
#' }
create_table_02 <- function(df, vars = c("AGE", "SEX", "ETHNICC", "HEIGHTBL", "WEIGHTBL","BMIBL"), lbl_vars, .labels) {
    checkmate::assert_subset(vars, names(df))
    checkmate::assert_subset(names(lbl_vars), vars)
    
    tbl <- make_table_02_huarun(df, vars = vars, lbl_vars = lbl_vars, .labels = .labels)
    
    ft <- rtables::tt_to_flextable(tbl)
    df_tbl <- ft$body$dataset
    df_tbl <- df_tbl %>%
        dplyr::mutate(V1 = ifelse(V1 %in% .labels, paste0("  ", V1), V1))

    df_tbl
}



#' Create Sassy Report
#'
#' This function creates a sassy report based on the input dataframe. The dataframe should have 9 columns.
#'
#' @param df_tbl The input dataframe. It should have 9 columns.
#' @param titles The titles for the report.
#' @param footnotes The footnotes for the report.
#' @param page_header The header for each page.
#' @param page_footer The footer for each page.
#' @param headers The headers for the spanning columns.
#' @param column_labels The labels for the columns. It should be a list of 9 elements.
#' @param file The file name for the report. Default is "fda-table-02.rtf".
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

create_sassy_report <- function(df_tbl, titles, footnotes, page_header, page_footer, headers,column_labels, file = "../fda-table-02.rtf") {
  tbl <- reporter::create_table(df_tbl, first_row_blank = TRUE, borders = "bottom") %>%
    reporter::column_defaults(from = "V1", to = "V9", align = "left", width = 1.5) %>%
    reporter::spanning_header(2, 4, label = headers) %>%
    reporter::define(var = "V1", label = column_labels[[1]], width = 2.5) %>%
    reporter::define(var = "V2", label = column_labels[[2]], align = "right") %>%
    reporter::define(var = "V3", label = column_labels[[3]], align = "right") %>%
    reporter::define(var = "V4", label = column_labels[[4]], align = "right") %>%
    reporter::define(var = "V5", label = column_labels[[5]], align = "right") %>%
    reporter::define(var = "V6", label = column_labels[[6]], align = "right") %>%
    reporter::define(var = "V7", label = column_labels[[7]], align = "right") %>%
    reporter::define(var = "V8", label = column_labels[[8]], align = "right") %>%
    reporter::define(var = "V9", label = column_labels[[9]], align = "right") %>%
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


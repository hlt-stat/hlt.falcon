make_table_02_huarun <-
  function (df, alt_counts_df = NULL, show_colcounts = TRUE, arm_var = "ARM",
          saffl_var = "SAFFL", vars = c("SEX", "AGE", "AGEGR1", "RACE",
                                        "ETHNIC", "COUNTRY"), lbl_vars = formatters::var_labels(df,
                                                                                                fill = TRUE)[vars], lbl_overall = "Total Population",
          na_rm = FALSE, prune_0 = TRUE, annotations = NULL)
{
  checkmate::assert_subset(c(vars, arm_var, saffl_var), names(df))
  df <- df %>% filter(.data[[saffl_var]] == "Y") %>% df_explicit_na()
  alt_counts_df <- alt_counts_df_preproc(alt_counts_df, id_var,
                                         arm_var, saffl_var)
  lyt <- basic_table_annot(show_colcounts, annotations) %>%
    split_cols_by_arm(arm_var, lbl_overall) %>% analyze_vars(vars = vars,
                                                             var_labels = lbl_vars, show_labels = "visible",
                                                             .stats = c("n", "mean_sd", "median", "quantiles", "median_range"),
                                                             .formats = NULL,
                                                             .labels = c(n = "例数 (缺失)", mean_sd = "均值(标准差)", median = "中位数",
                                                                         quantiles = c("Q1. Q3"), median_range =c("最小值. 最大值") ),
                                                             na.rm = na_rm) %>% append_topleft("Characteristic")
  tbl <- build_table(lyt, df = df, alt_counts_df = alt_counts_df)
  if (prune_0)
    tbl <- prune_table(tbl)
  tbl
}



# Load Libraries & Data
library(falcon)
library(dplyr)

adsl <- random.cdisc.data::cadsl
advs <- random.cdisc.data::cadvs

# Pre-Processing - Add any variables needed in your table to df
adsl <- adsl

advs <- advs %>%
  filter(AVISIT == "BASELINE", VSTESTCD == "TEMP") %>%
  select("USUBJID", "AVAL")

anl <- left_join(adsl, advs, by = "USUBJID")

# Output Table
make_table_02_hlt(
  df = anl,
  vars = c("SEX", "AGE", "RACE", "ETHNIC", "AVAL"),
  lbl_vars = c(
    "Sex", "Age, years", "Race", "Ethnicity",
    "Baseline Temperature (C)"
  )
)

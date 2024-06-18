## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)
utils::globalVariables(c(".data", "TRTEMFL", "USUBJID", "V1", "id_var"))

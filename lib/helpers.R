
remove_fr_columns <- function(dataset) {
  dataset %>%
    select(
      -DESCRIP_F,
      -TITLE_F,
      -DEPT_F,
      -INDICATORFRA,
      -SUBINDICATORFRA
    )
}

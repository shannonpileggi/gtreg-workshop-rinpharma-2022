library(gtreg)
library(tidyverse)

# ------------------------------------------------------------------------------
# First adverse event tables
# ------------------------------------------------------------------------------

# tbl_ae_count() ---------------------------------------------------------------
df_adverse_events |>
  tbl_ae_count(
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  )

# tbl_ae() ---------------------------------------------------------------
df_adverse_events |>
  tbl_ae(
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  )

# tbl_ae() with id_df -----------------------------------------------------------
df_adverse_events |>
  tbl_ae(
    id = patient_id,
    id_df = df_patient_characteristics,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  )

# tbl_ae() with id_df & strata -------------------------------------------------
t1 <- df_adverse_events |>
  tbl_ae(
    id = patient_id,
    id_df = df_patient_characteristics,
    strata = trt,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  )

# tbl_ae_focus() ---------------------------------------------------------------
df_adverse_events |>
  tbl_ae_focus(
    id = patient_id,
    id_df = df_patient_characteristics,
    ae = adverse_event,
    soc = system_organ_class,
    include = c(any_complication, grade3_complication)
  )

# ------------------------------------------------------------------------------
# Modified adverse event tables
# ------------------------------------------------------------------------------

# tbl_ae() with add_overall() --------------------------------------------------
t1|>
  add_overall(across = 'by') |>
  bold_labels()


# tbl_ae() with modified headers------------------------------------------------
t1|>
  add_overall(across = 'by') |>
  bold_labels() |>
  modify_header(
    all_ae_cols() ~ "**Grade {by}**"
  ) |>
  modify_spanning_header(
    all_ae_cols(TRUE, TRUE) ~ "**{strata}**  \nN = {n}"
  )


# ------------------------------------------------------------------------------
# Other tabling functions
# ------------------------------------------------------------------------------

# tbl_reg_summary() ------------------------------------------------------------
df_patient_characteristics |>
  tbl_reg_summary(
    by = trt,
    include = c(marker, status)
  ) |>
  bold_labels()


df_patient_characteristics |>
  select(trt, marker, status) |>
  tbl_reg_summary(
    by = trt
  ) |>
  bold_labels()

# tbl_listing() ------------------------------------------------------------
df_adverse_events |>
  head(n = 10) |>
  select(system_organ_class, adverse_event, grade, drug_attribution, patient_id) |>
  arrange(adverse_event, desc(grade)) |>
  tbl_listing(
    group_by = system_organ_class
  ) |>
  bold_labels()

# ------------------------------------------------------------------------------
# Table shells
# ------------------------------------------------------------------------------

# uniform shell ----------------------------------------------------------------
df_adverse_events |>
  tbl_ae(
    id = patient_id,
    id_df = df_patient_characteristics,
    strata = trt,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade,
    digits = style_xxx,
    zero_symbol = NULL
  ) |>
  bold_labels() |>
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
  modify_spanning_header(all_ae_cols(TRUE, TRUE) ~ "**{strata}**  \nN = xx")

# custom shell -----------------------------------------------------------------
df_adverse_events |>
  tbl_ae(
    id = patient_id,
    id_df = df_patient_characteristics,
    strata = trt,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade,
    digits = list(
      style_xxx, # style for n
      function(x) style_xxx(x, width = 4, digits = 1) # style for %
    ),
    zero_symbol = NULL
  ) |>
  bold_labels() |>
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
  modify_spanning_header(all_ae_cols(TRUE, TRUE) ~ "**{strata}**  \nN = xx")

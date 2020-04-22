## Style as close as possible to BioC's style using styler
bioc_style <- styler::tidyverse_style(indent_by = 4)
bioc_style$indention$update_indention_ref_fun_dec <- function(pd_nested) {
    if (pd_nested$token[1] == "FUNCTION") {
        seq <- rlang::seq2(3, nrow(pd_nested) - 2)
        pd_nested$indention_ref_pos_id[seq] <- pd_nested$pos_id[nrow(pd_nested)]
        pd_nested$indent[seq] <- pd_nested$indent[seq] + 4
    }
    pd_nested
}

styler::style_pkg(transformers = bioc_style)
## Style vignettes too
sapply(dir("vignettes", "Rmd$", full.names = TRUE), styler::style_file, transformers = bioc_style)

## This messes up quotes
# formatR::tidy_dir("R", indent = 4, width.cutoff = 70)

## Update docs
devtools::document()

## Update style for this directory
styler::style_dir("dev", transformers = bioc_style)

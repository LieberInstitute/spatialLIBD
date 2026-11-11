# Define the app's palette policy separately from plotting and reactive wiring.
# Keep historical dataset palettes, available discrete palette choices, category
# color mapping, and model-boxplot colors together.

app_paper_colors <- function(short = FALSE) {
    colors <- spatialLIBD::libd_layer_colors[-length(spatialLIBD::libd_layer_colors)]
    if (short) names(colors) <- gsub("ayer", "", names(colors))
    colors
}

app_layer_colors <- function(sce, group_var) {
    colors <- sce[[paste0(group_var, "_colors")]]
    if (!is.null(colors)) return(colors)

    groups <- unique(as.character(sce[[group_var]]))
    groups <- groups[!is.na(groups)]
    libd_colors <- spatialLIBD::libd_layer_colors
    short_names <- gsub("ayer", "", names(libd_colors))
    if (all(groups %in% names(libd_colors)) || all(groups %in% short_names)) {
        return(get_colors(libd_colors, groups))
    }
    get_colors(colors = NULL, clusters = groups)
}

app_legacy_colors <- function(variable) {
    if (variable %in% c("Maynard", "Martinowich")) {
        return(c(
            WM = "#b2df8a", `6` = "#e41a1c", `1` = "#377eb8", `5` = "#4daf4a",
            `4` = "#ff7f00", `2_3` = "gold", `5_6` = "#a65628", `4_5` = "#999999",
            `1_6` = "black", `3` = "grey", `2` = "white", `1_5` = "purple"
        ))
    }
    if (variable %in% c("layer_guess", "layer_guess_reordered")) {
        return(app_paper_colors())
    }
    if (variable %in% c("layer_guess_reordered_short", "spatialLIBD")) {
        return(app_paper_colors(TRUE))
    }
    NULL
}

app_palette_choices <- function(spe, variable) {
    labels <- spe[[variable]]
    supplied <- paste0(variable, "_colors")
    supplied <- intersect(supplied, colnames(colData(spe)))
    palettes <- paletteer::palettes_d_names
    palettes <- palettes[palettes$length >= length(unique(labels)), ]
    choices <- c(supplied, paste0(palettes$package, "::", palettes$palette))
    list(choices = choices, selected = if (length(supplied)) {
        supplied[[1]]
    } else {
        app_preferred(choices, "Polychrome::palette36")
    })
}

app_discrete_colors <- function(spe, variable, palette, reverse = FALSE) {
    legacy <- app_legacy_colors(variable)
    if (!is.null(legacy)) {
        return(legacy)
    }
    labels <- spe[[variable]]
    if (palette %in% colnames(colData(spe))) {
        colors <- spe[[palette]]
        colors <- colors[unique(names(colors))]
        if (is.factor(labels)) colors <- colors[levels(labels)]
        return(colors)
    }
    colors <- paletteer::paletteer_d(palette,
        n = length(unique(labels)),
        direction = if (reverse) -1 else 1
    )
    names(colors) <- unique(labels)
    colors
}

app_boxplot_colors <- function(palette) {
    colors <- switch(palette,
        viridis = c(
            "grey80", "grey90", viridisLite::viridis(4)[2:1],
            viridisLite::viridis(4)[3:4]
        ),
        paper = c("grey80", "grey90", "palegreen3", "springgreen2", "darkorange2", "orange1"),
        bluered = c("grey90", "grey60", "skyblue2", "royalblue3", "tomato2", "firebrick4")
    )
    as.list(setNames(colors, c(
        "col_bkg_box", "col_bkg_point", "col_low_box",
        "col_low_point", "col_high_box", "col_high_point"
    )))
}

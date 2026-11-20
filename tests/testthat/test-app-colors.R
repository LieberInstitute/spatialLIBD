test_that("layer palettes prefer custom colors, then LIBD, then discrete fallbacks", {
    sce <- list(domain = c("Layer2", "Layer1", "Layer2", NA_character_))
    sce$domain_colors <- c(Layer1 = "black", Layer2 = "white")
    expect_identical(app_layer_colors(sce, "domain"), sce$domain_colors)

    sce$domain_colors <- NULL
    colors <- app_layer_colors(sce, "domain")
    expect_identical(colors[c("Layer1", "Layer2")], libd_layer_colors[c("Layer1", "Layer2")])
    sce$domain <- c("L2", "L1", NA_character_)
    colors <- app_layer_colors(sce, "domain")
    expect_identical(unname(colors[c("L1", "L2")]), unname(libd_layer_colors[c("Layer1", "Layer2")]))

    # Exercise both sides of the small-palette boundary and the supported maximum.
    for (n in c(12L, 13L, 36L)) {
        groups <- sprintf("domain%02d", seq_len(n))
        sce$domain <- c(rev(groups), groups[1], NA_character_)
        colors <- app_layer_colors(sce, "domain")
        expect_setequal(names(colors), groups)
        expect_length(unique(colors), n)
        expect_false(anyNA(grDevices::col2rgb(colors)))
        expect_identical(app_layer_colors(list(domain = groups), "domain"), colors)
        if (n == 12L) {
            expect_equal(unname(colors[c("domain01", "domain12")]), c("#b2df8a", "purple"))
        } else {
            expect_equal(unname(as.character(colors)),
                unname(as.character(paletteer::paletteer_d("Polychrome::palette36", n))))
        }
    }
})

test_that("model colors preserve domain colors and handle pairwise and ANOVA tests", {
    sce <- list(domain = c("A", "B", "C"),
        domain_colors = c(A = "black", B = "white", C = "red", `A-C` = "gold"))
    enrichment <- app_model_colors(sce, "domain", c("B", "A"), "enrichment")
    expect_identical(enrichment[c("B", "A")], c(B = "white", A = "black"))
    comparisons <- c("A-B", "B-A", "A-C", "A-missing", "unrecognized")
    colors <- app_model_colors(sce, "domain", comparisons, "pairwise")
    # Black/white have a neutral Lab midpoint, independent of comparison direction.
    expect_identical(colors, setNames(c("#777777", "#777777", "gold", "grey60", "grey60"), comparisons))
    expect_identical(app_model_colors(sce, "domain", c("all", "adjusted"), "anova"),
        c(all = "grey60", adjusted = "grey60"))

    # Abbreviated comparison labels resolve full LIBD domain names.
    sce <- list(domain = c("Layer1", "Layer2"))
    colors <- app_model_colors(sce, "domain", c("L1-L2", "Layer1-Layer2"), "pairwise")
    expect_identical(unname(colors[1]), unname(colors[2]))
    expect_false(anyNA(colors))
    expect_true(all(colors != "grey60"))

    # Generated domain colors are shared across enrichment and pairwise models.
    sce <- list(domain = c("Tumor", "Stroma"))
    palette <- app_model_colors(sce, "domain", sce$domain, "enrichment")
    colors <- app_model_colors(sce, "domain", c("Tumor-Stroma", "Stroma-Tumor"), "pairwise")
    expect_identical(names(palette), c("Stroma", "Tumor"))
    expect_identical(unname(colors[1]), unname(colors[2]))
    expect_true(all(colors != "grey60"))
    expect_false(any(colors %in% palette))
})

test_that("layer reduced dimensions apply domain palettes only to categorical data", {
    sce <- SingleCellExperiment(assays = list(logcounts = matrix(seq_len(18), nrow = 3)))
    sce$custom <- factor(rep(c("Layer1", "Layer2"), 3))
    sce$custom_colors <- c(Layer1 = "black", Layer2 = "white")[as.character(sce$custom)]
    sce$libd <- as.character(sce$custom)
    sce$generic <- rep(c("Tumor", "Stroma"), 3)
    sce$score <- seq_len(ncol(sce))
    reducedDim(sce, "PCA") <- cbind(seq_len(ncol(sce)), c(2, 4, 1, 6, 3, 5))
    expected <- list(
        custom = c(Layer1 = "black", Layer2 = "white"),
        libd = libd_layer_colors[c("Layer1", "Layer2")],
        generic = c(Stroma = "#b2df8a", Tumor = "#e41a1c")
    )
    shiny::testServer(function(input, output, session) {
        feature <- app_layer_dimensions_server(input, output, session, sce)
    }, {
        session$setInputs(layer_which_dim = "PCA", layer_reduced_dim_ncomponents = 2,
            layer_reduced_dim_theme_size = 12, layer_reduced_dim_point_size = 2)
        for (variable in names(expected)) {
            session$setInputs(layer_which_dim_color = variable)
            built <- suppressMessages(ggplot2::ggplot_build(feature$plot()))
            expect_true(built$plot$scales$get_scales("colour")$is_discrete())
            expect_equal(built$data[[1]]$colour,
                unname(expected[[variable]][as.character(sce[[variable]])]))
        }
        session$setInputs(layer_which_dim_color = "score")
        built <- ggplot2::ggplot_build(feature$plot())
        expect_false(built$plot$scales$get_scales("colour")$is_discrete())
        expect_gt(length(unique(built$data[[1]]$colour)), 2L)
    })
})

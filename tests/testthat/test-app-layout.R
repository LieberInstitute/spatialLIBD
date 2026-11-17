# The plot container must reserve the space occupied by the rendered image;
# decorative spacing after a shorter container cannot prevent overlap.
test_that("static plot containers reserve their full display height", {
    opts <- app_test_options()
    testthat::local_mocked_bindings(get_golem_options = function(which = NULL) {
        if (is.null(which)) opts else opts[[which]]
    }, .package = "golem")
    doc <- xml2::read_html(htmltools::renderTags(app_ui())$html)
    for (id in c("histology", "gene", "editImg_plot", "layer_boxplot",
                 "layer_gene_set_plot", "layer_tstat_cor_plot")) {
        node <- xml2::xml_find_first(doc, paste0("//*[@id='", id, "']"))
        expect_match(xml2::xml_attr(node, "style"), "height:600px", fixed = TRUE,
                     info = id)
    }
})

test_that("embedding containers grow with the requested number of components", {
    with_app({
        session$setInputs(layer_reduced_dim_ncomponents = 2)
        expect_match(output$layer_reduced_dim_ui$html, "height:600px", fixed = TRUE)
        session$setInputs(layer_reduced_dim_ncomponents = 6)
        expect_match(output$layer_reduced_dim_ui$html, "height:800px", fixed = TRUE)
    })
})

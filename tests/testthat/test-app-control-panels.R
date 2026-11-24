test_that("each plotting or editing tab owns its controls and documentation has no sidebar", {
    opts <- app_test_options()
    testthat::local_mocked_bindings(get_golem_options = function(which = NULL) {
        if (is.null(which)) opts else opts[[which]]
    }, .package = "golem")
    doc <- xml2::read_html(htmltools::renderTags(app_ui())$html)
    tab <- function(value) {
        link <- xml2::xml_find_first(doc, paste0("//a[@data-value='", value, "']"))
        id <- substring(xml2::xml_attr(link, "href"), 2)
        xml2::xml_find_first(doc, paste0("//*[@id='", id, "']"))
    }
    controls <- function(value) xml2::xml_attr(xml2::xml_find_all(tab(value), ".//input[@id] | .//select[@id]"), "id")
    interactive_ids <- controls("interactive")
    grid_ids <- controls("grid")
    editor_ids <- controls("edit_image")
    expect_true(all(c("interactive_controls-pointsize_scale", "interactive_controls-image_opacity", "label_manual_ann") %in% interactive_ids))
    expect_false(any(c("grid_pointsize", "grid_controls-guidepointsize", "editImg_sample") %in% interactive_ids))
    expect_true(all(c("grid_pointsize", "grid_controls-guidepointsize", "grid_controls-geneid", "grid_imageid") %in% grid_ids))
    expect_false(any(c("interactive_controls-pointsize_scale", "interactive_controls-imageid", "label_manual_ann") %in% grid_ids))
    expect_true(all(c("editImg_sample", "editImg_brightness", "editImg_imageid") %in% editor_ids))
    expect_false(any(grepl("^(interactive_controls-|grid_)", editor_ids)))
    for (value in c("interactive", "grid", "edit_image")) {
        expect_length(xml2::xml_find_all(tab(value), ".//*[contains(concat(' ', normalize-space(@class), ' '), ' well ')]"), 1)
    }
    expect_length(xml2::xml_find_all(tab("documentation"), ".//*[contains(concat(' ', normalize-space(@class), ' '), ' well ')]"), 0)
    expect_gte(length(xml2::xml_find_all(tab("edit_image"), ".//button[contains(@aria-label, 'About ')]")), 18)
    # Plot actions stay beside the plots rather than in the settings sidebar.
    sidebar <- function(value) xml2::xml_find_first(tab(value), ".//*[contains(concat(' ', normalize-space(@class), ' '), ' well ')]")
    expect_length(xml2::xml_find_all(sidebar("interactive"), ".//*[@id='label_manual_ann'] | .//*[@role='toolbar']"), 0)
    expect_length(xml2::xml_find_all(doc, "//*[@id='spot_viewer']/preceding-sibling::*[@role='toolbar']"), 1)
    expect_length(xml2::xml_find_all(doc, "//*[@id='label_manual_ann']/preceding::div[@id='spot_viewer']"), 1)
    expect_length(xml2::xml_find_all(sidebar("grid"), ".//*[@id='grid_update'] | .//*[@id='downloadPlotGrid']"), 0)
    expect_length(xml2::xml_find_all(doc, "//*[@id='grid_static']/preceding-sibling::*[@role='toolbar']//*[@id='grid_update' or @id='downloadPlotGrid']"), 2)
    expect_length(xml2::xml_find_all(tab("edit_image"), ".//details[@class='sl-appearance']/summary/span[@class='sl-appearance-show']"), 2)
    ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//input[@id] | //select[@id] | //textarea[@id]"), "id")
    expect_false(anyDuplicated(ids) > 0)
})

test_that("interactive and grid controls have independent state and saved grid settings", {
    with_app({
        initial_gene <- grid_controls$input$geneid
        grid_before <- grids$grid()
        app_set_inputs(session, geneid = rownames(f$spe)[2], minCount = Inf,
            clustercolor_direction = TRUE, alphalevel = 0.2, imageid = "")
        expect_identical(grid_controls$input$geneid, initial_gene)
        expect_identical(grids$grid(), grid_before)
        app_set_inputs(session, grid_update = 1)
        expect_identical(grids$grid()$stem, grid_before$stem)
        viewer <- app_viewer_message(session)
        frame <- interactive$frame()
        app_set_inputs(session, target = "grid_controls", geneid = rownames(f$spe)[3],
            minCount = -Inf, alphalevel = 0.7, genecolor = "magma")
        expect_identical(grids$grid()$stem, grid_before$stem)
        app_set_inputs(session, grid_update = 2)
        expect_match(grids$grid()$stem, rownames(f$spe)[3], fixed = TRUE)
        expect_identical(app_viewer_message(session), viewer)
        expect_identical(interactive$frame(), frame)
        app_set_inputs(session, spot_tabs = "edit_image")
        app_set_inputs(session, spot_tabs = "interactive")
        expect_identical(interactive_controls$input$geneid, rownames(f$spe)[2])
        expect_identical(grid_controls$input$geneid, rownames(f$spe)[3])
    })
})

test_that("editor sample and grid image choices do not change the interactive view", {
    with_app({
        sample <- interactive_controls$input$sample
        viewer <- app_viewer_message(session)
        app_set_inputs(session, editImg_sample = unique(f$spe$sample_id)[2], editImg_brightness = 0,
            grid_imageid = "lowres")
        expect_identical(interactive_controls$input$sample, sample)
        expect_identical(app_viewer_message(session), viewer)
        expect_true(all(images$preview() %in% c("#000000", "#000000ff")))
        expect_identical(interactive_controls$input$imageid, "lowres")
    })
})

test_that(
    "multi_gene_z_score",
    {
        #   With two good columns but 1 zero-variance column, the zero-variance
        #   column should be dropped with a warning
        cont_mat = matrix(c(1, 0, 3, 3, 2, -5), ncol = 3)
        colnames(cont_mat) = c('good1', 'bad', 'good2')
        expect_warning(
            multi_gene_z_score(cont_mat),
            "Dropping features\\(s\\) 'bad' which have no expression variation"
        )

        #   NAs should be correctly removed from columns (as long as 2 non-NAs remain
        #   in at least 2 columns), and the result should have no NAs
        cont_mat = matrix(c(1, NA, 3, NA, 2, 0), ncol = 2)
        colnames(cont_mat) = c('good1', 'good2')
        expect_equal(any(is.na(multi_gene_z_score(cont_mat))), FALSE)

        #   With only one good column, an error should be thrown
        cont_mat = matrix(c(1, NA, 3, 4, 2, 2), ncol = 3)
        colnames(cont_mat) = c('bad1', 'good', 'bad2')
        expect_error(
            multi_gene_z_score(cont_mat),
            "After dropping features with no expression variation, less than 2 features were left"
        )
    }
)

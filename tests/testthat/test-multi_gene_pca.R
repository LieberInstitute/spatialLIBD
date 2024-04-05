test_that(
    "multi_gene_pca",
    {
        #   With two good columns but 1 zero-variance column, the zero-variance
        #   column should be dropped with a warning
        cont_mat <- matrix(c(1, 0, 3, 3, 2, -5), ncol = 3)
        colnames(cont_mat) <- c("good1", "bad", "good2")
        expect_warning(
            multi_gene_pca(cont_mat),
            "Dropping features\\(s\\) 'bad' which have NAs or no expression variation"
        )

        #   With two good columns but 1 zero-variance column, the zero-variance
        #   column should be dropped with a warning
        cont_mat <- matrix(c(1, NA, 3, 4, 2, -5), ncol = 3)
        colnames(cont_mat) <- c("bad", "good1", "good2")
        expect_warning(
            multi_gene_pca(cont_mat),
            "Dropping features\\(s\\) 'bad' which have NAs or no expression variation"
        )

        #   With only one good column, an error should be thrown
        cont_mat <- matrix(c(1, NA, 3, 4, 2, 2), ncol = 3)
        colnames(cont_mat) <- c("bad1", "good", "bad2")
        expect_error(
            multi_gene_pca(cont_mat),
            "After dropping features with NAs or no expression variation, less than 2 features were left"
        )
    }
)

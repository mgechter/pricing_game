#!/usr/bin/env Rscript
# Run the engine test suite.  From the project root:  Rscript scripts/run_tests.R
res <- testthat::test_dir("tests/testthat", reporter = "progress",
                          stop_on_failure = FALSE)
df    <- as.data.frame(res)
fails <- sum(df$failed) + sum(as.logical(df$error))
cat(sprintf("\n%d test(s): %d failed, %d errored\n",
            nrow(df), sum(df$failed), sum(as.logical(df$error))))
if (fails > 0) quit(status = 1)

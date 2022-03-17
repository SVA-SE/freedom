library(freedom)

ob <- post_fr(0.5, 0.5)
stopifnot(ob > 0.5)

ob1 <- tools::assertError(post_fr(-0.1, 0.5))[[1]]$message
ob2 <- tools::assertError(post_fr(1.1, 0.5))[[1]]$message
ex <- "The prior probability of freedom cannot be greater than 1 or less than 0"
stopifnot(length(grep(ex, ob1)) == 1L)
stopifnot(length(grep(ex, ob2)) == 1L)

ob1 <- tools::assertError(post_fr(0.1, -0.1))[[1]]$message
ob2 <- tools::assertError(post_fr(0.1, 1.1))[[1]]$message
ex <- "System sensitivity cannot be greater than 1 or less than 0"
stopifnot(length(grep(ex, ob1)) == 1L)
stopifnot(length(grep(ex, ob2)) == 1L)

ob <- prior_fr(0.9, 0.01)
stopifnot(ob < 0.9)

ob1 <- tools::assertError(prior_fr(0.1, -0.1))[[1]]$message
ob2 <- tools::assertError(prior_fr(0.1, 1.1))[[1]]$message
ex <- paste("The annual probability of introduction cannot",
            "be greater than 1 or less than 0")
stopifnot(length(grep(ex, ob1)) == 1L)
stopifnot(length(grep(ex, ob2)) == 1L)

ob1 <- tools::assertError(prior_fr(-0.1, 0.1))[[1]]$message
ob2 <- tools::assertError(prior_fr(1.1, 0.1))[[1]]$message
ex <- paste("The posterior probability of freedom cannot",
            "be greater than 1 or less than 0")
stopifnot(length(grep(ex, ob1)) == 1L)
stopifnot(length(grep(ex, ob2)) == 1L)

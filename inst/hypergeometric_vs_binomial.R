library(freedom)

n_tested <- 1:100
herd_size <- 1000
Se <- seq(0,1,by = 0.01)
dp <- seq(0,1, by = 0.01)
df <- do.call("rbind",lapply(dp, function(z){
    do.call("rbind",lapply(Se, function(y){
        finite <- do.call("c",lapply(n_tested, function(x){
            hse_finite(n_tested = x, N = herd_size, test_Se = y, dp = z)
        }))
        infinite <- do.call("c",lapply(n_tested, function(x){
            hse_infinite(n_tested = x, test_Se = y, dp = z)
        }))
        df <- data.frame(n_tested = n_tested,
                         herd_size = herd_size,
                         test_Se = y,
                         dp = z,
                         finite = finite,
                         infinite = infinite)
        return(df)
    }))
}))
df$difference <- df$finite - df$infinite

plot(df$difference[df$n_tested<2])
plot(df$dp, df$difference)

pdf(file = "~/Desktop/fig1.pdf")
hist(df$difference)
dev.off()

png(file = "~/Desktop/fig2.png")
plot(df$test_Se, df$difference)
dev.off()

temp <- df[df$test_Se > 0.7 & df$dp > 0.5 & df$n_tested < 3,]

pdf(file = "~/Desktop/fig3.pdf")
hist(temp$difference)
dev.off()

t_test_tantrum <- function(n = 100, group_size = 2, reps = 100, seed = 42) {
  set.seed(seed)
  
  results <- data.frame(test = integer(), p_value = double(), sig = logical())
  
  for (i in 1:reps) {
    data <- rnorm(n)  
    group <- sample(rep(LETTERS[1:group_size], length.out = n))
    df <- data.frame(value = data, group = factor(group))
    
    test_result <- t.test(value ~ group, data = df)
    
    results <- rbind(results, data.frame(
      test = i,
      p_value = test_result$p.value,
      sig = test_result$p.value < 0.05
    ))
  }
  
  cat("\n T-Test Tantrum Results\n")
  cat("------------------------\n")
  cat("Total tests: ", nrow(results), "\n")
  cat("Significant (p < 0.05): ", sum(results$sig), "\n")
  if (sum(results$sig) > 0) {
    cat("UH OH! False discoveries detected! \n")
  } else {
    cat("Nothing significant. Boring but honest.\n")
  }
  
  
  library(ggplot2)
  ggplot(results, aes(x = test, y = p_value, color = sig)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    scale_color_manual(values = c("grey70", "firebrick")) +
    theme_minimal() +
    labs(
      title = "t_test_tantrum(): with p-Hack",
      x = "Test #",
      y = "p-value",
      color = "Significant?"
    )
}


t_test_tantrum(n = 1000, group_size = 2, reps = 400, seed=2908)

t_test_tantrum(n = 10, group_size = 2, reps = 400, seed=2910)

t_test_tantrum(n = 5, group_size = 2, reps = 10, seed=2910)

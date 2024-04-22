# Install GitHub version of cfbfastR into your workflow

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)


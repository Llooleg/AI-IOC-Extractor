if [ "$(git log -1 --format='%an')" = "HelgiL" ]; then
  git commit --amend --reset-author --no-edit
fi
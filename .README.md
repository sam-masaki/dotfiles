# dotfiles

Now modified to use the technique described in [this article](www.atlassian.com/git/tutorials/dotfiles). To clone to a new machine, run
```
git clone --bare git@github.com:sam-masaki/dotfiles ~/.cfg
git --git-dir=$HOME/.cfg/ --work-tree=$HOME
```
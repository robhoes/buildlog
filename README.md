
## Create a buildlog alias

If you would like to type something like 'git blog' instead of 'git log |
buildlog', then add the following line to your ~/.gitconfig:

> [alias]
>        blog = !sh -c 'git log | buildlog'


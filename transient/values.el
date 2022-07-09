((magit-branch-configure)
 (magit-fetch "--prune")
 (magit-pull "--rebase" "--autostash")
 (magit-push "--force-with-lease"))

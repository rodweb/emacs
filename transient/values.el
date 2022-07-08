((magit-branch-configure)
 (magit-fetch "--prune")
 (magit-pull "--ff-only")
 (magit-push "--force-with-lease"))

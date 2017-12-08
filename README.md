# .config
My (@cpcallen) personal dotfiles collection.

To install on a new machine:

    git clone https://github.com/cpcallen/.config.git && .config/setup

If you see complaints about existing files/directories then you will
need to reconcile your local dotfiles with the versions in
`~/.config`, remove the local copy, and then re-run `~/.config/setup`
to create symlinks to the git-controlled versions.

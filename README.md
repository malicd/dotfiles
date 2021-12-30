# dotfiles

## Prerequisite

### `nodejs`
Install [nodejs](https://nodejs.org/en/) for [CoC](https://github.com/neoclide/coc.nvim):

```
curl -sL install-node.now.sh/lts | bash
```

or
```
curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
sudo apt-get install -y nodejs
```

### `vim`
Clone latest [vim](https://github.com/vim/vim), compile it and install locally in `$HOME/bin`:
```
./configure --with-python3-command=python3.6 \
            --with-python3-config-dir=/usr/lib64/python3.6/config-3.6m-x86_64-linux-gnu/ \
            --with-features=huge  \
            --enable-fail-if-missing \
            --enable-multibyte \
            --enable-largefile \
            --enable-python3interp \
            --prefix=$HOME
make -j
make install
```

### `zsh`
Clone latest [zsh](https://github.com/zsh-users/zsh/), compile it and install locally in `$HOME/bin`:
```
./Util/preconfig
./configure --prefix=$HOME
make -j
make install
```

Either add `[ -f $HOME/bin/zsh ] && exec $HOME/bin/zsh -l` to `.profile` or start the session with `ssh -t uname@hostname $HOME/bin/zsh`.

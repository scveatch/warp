# Warp 

# Installation
Run the following to automatically download the tool and add it to your path. You may need to run `source ~/.bashrc` to
refresh your path.

```
curl -sSL https://github.com/scveatch/warp/raw/main/scripts/install.sh | bash
```

# Notes to self: 
Permissions errors are fixed and release is tentatively stable. Need to configure for more machines -- tested 
on a docker container running debian, but need to ensure configuration for alpine / mac. Then, extensions -- 
yay!

# Extensions: 
 - Keep track of commonly used directories to add to the program proactively. 
 - Add tags to group directories and projects. 
 - fzf matching when jumping. 
 - colored output 
 - undo / remove last used warp point (useful for misspelled names)
 - Show current warp point in terminal (Actually useful??)
 - Shortcuts (warp -1 to last point, warp -2 to the one before that, etc.)
 - nvim integration? Would be nice to be able to warp to directory and immediately open nvim.


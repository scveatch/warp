#!/usr/bin/env bash

# ------------------------------------------------------------ 
# warp.sh -- A Shell wrapper for the Haskell warp binary. 
#
# Defines _warp_main() which: 
#   - calls warp add/remove/list via the binary. 
#   - resolves warp targets and cd's into them. 
#   - functions in bash and zsh. 
#   - is sourced by ~/.local/bin/_warp
# ------------------------------------------------------------ 


# Path to compiled Haskell Binary
WARP_BIN=~/.local/bin/warp

# ---------------
# Main Dispatcher
# ---------------
_warp_main() {
    local cmd="$1"

    # --- warp add <name> [path] --- 
    if [ "$cmd" = "add" ]; then 
        local name="$2"
        local dir="${3:-$PWD}"
        "$WARP_BIN" add "$name" "$dir"
        return 
    fi

    # --- warp remove <name> --- 
    if [ "$cmd" = "remove" ]; then 
        "$WARP_BIN" remove "$2"
        return 
    fi 

    # --- warp list --- 
    if [ "$cmd" = "list" ]; then 
        "$WARP_BIN" list
        return 
    fi

    # --- Default: resolve warp point --- 
    if [ -z "$cmd" ]; then 
        echo "warp: missing target (try `warp list`)"
        return 1 
    fi 
    local dest
    dest=$("$WARP_BIN" "$1") || return 1
    # if valid directory -> cd 
    if [ -d "$dest" ] && [ $# -eq 1 ];  then
        cd "$dest" || return 1 
    else 
        echo "$dest" 
    fi
}

_warp_complete() {
    local cur points

    # Current word being completed
    cur="${COMP_WORDS[COMP_CWORD]}"

    # Ensure WARP_BIN is set (path to your warp binary)
    : "${WARP_BIN:=warp}"

    # List all warp points (extract only names)
    points=$("$WARP_BIN" list | awk -F: '{print $1}')

    # Generate completions for the current word
    COMPREPLY=( $(compgen -W "$points" -- "$cur") )
}
# Attach the function to the 'warp' command
complete -F _warp_complete warp

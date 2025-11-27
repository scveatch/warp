#!/usr/bin/env bash

# Usage 
#   warp add <name> <path>    -> add a warp point
#   warp remove <name>        -> remove a warp point
#   warp list                 -> list all warp points
#   warp <name>               -> jump to a warp point

# Path to compiled Haskell Binary
WARP_BIN="$PWD/warp-bin"
# WARP_BIN="$PWD/home/spencer/projects/warp/.stack-work/install/x86_64-linux/564e52e6df8f51aef529d8a08062b06ff21bd4e00b399c682c2f92bd548bf078/9.8.4/bin/warp-bin"

warp(){
    # Warp Add
    if [ "$1" = "add" ]; then 
        local name="$2"
        local dir="${3:-$PWD}"
        "$WARP_BIN" add "$name" "$dir"
        return
    fi

    # Warp Remove
    if [ "$1" = "remove" ]; then 
        "$WARP_BIN" remove "$2"
        return 
    fi

    # Warp List
    if [ "$1" = "list" ]; then 
        "$WARP_BIN" list
        return 
    fi

    # Default -- Resolve Warp Point
    local dest
    dest=$("WARP_BIN" "$1") || return 1
    if [ -d "$dest" ] && [ $# -eq 1 ];  then
        cd "$dest" || return 1 
    else 
        echo "$dest" 
    fi
}

_warp_complete() {
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"
    # List all warp points from the Haskell binary
    local points
    points=$("$WARP_BIN" list | awk '{print $1}')  # assuming 'name: path'
    COMPREPLY=( $(compgen -W "$points" -- "$cur") )
}

complete -F _warp_complete warp


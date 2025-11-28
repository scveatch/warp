#!/usr/bin/env bash

# Usage 
#   warp add <name> <path>    -> add a warp point
#   warp remove <name>        -> remove a warp point
#   warp list                 -> list all warp points
#   warp <name>               -> jump to a warp point

# Path to compiled Haskell Binary
WARP_BIN="$PWD/warp"

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
    local cur points

    # Current word being completed
    cur="${COMP_WORDS[COMP_CWORD]}"

    # Ensure WARP_BIN is set (path to your warp binary)
    : "${WARP_BIN:warp}"

    # List all warp points (extract only names)
    points=$("$WARP_BIN" list | awk -F: '{print $1}')

    # Generate completions for the current word
    COMPREPLY=( $(compgen -W "$points" -- "$cur") )
}
# Attach the function to the 'warp' command
complete -F _warp_complete warp

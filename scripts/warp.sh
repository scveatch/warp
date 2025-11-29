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
# Command alias 
# ---------------
warp() { _warp_main "$@"; }
wp()   { _warp_main "$@"; }

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


# ------------------------------------
# Tab completion for warp (bash + zsh)
# ------------------------------------

_warp_completions(){
    local cur points
    WARP_COMMANDS="add remove rename list"

    # Bash 
    if [ -n "${BASH_VERSION:-}" ]; then 
        cur="${COMP_WORDS[COMP_CWORD]}"
    # zsh
    elif [ -n "${ZSH_VERSION:-}" ]; then
        cur="${words[CURRENT]}"
        emulate -L bash
    else 
        return
    fi

    # List warp points
    points=$("$WARP_BIN" list 2>/dev/null | awk -F: '{print $1}')

    options="$WARP_COMMANDS $points"

    if [[ -z "$cur" ]]; then
        COMPREPLY=( $options )
    else
        COMPREPLY=( $(compgen -W "$options" -- "$cur") )
    fi
}

# Register Completion
if [ -n "${BASH_VERSION:-}" ]; then
    complete -F _warp_completions warp
    complete -F _warp_completions wp
elif [ -n "${ZSH_VERSION:-}" ]; then
    compdef _warp_completions warp
    compdef _warp_completions wp
fi

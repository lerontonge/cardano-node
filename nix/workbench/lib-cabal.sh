echo 'workbench:  cabal-inside-nix-shell mode enabled, calling cardano-* via 'cabal run' (instead of using Nix store)' >&2

function workbench-prebuild-executables()
{
    local local_changes=

    git diff --exit-code --quiet && echo -n ' ' || echo -n '[31mlocal changes + '
    git log -n1 --alternate-refs --pretty=format:"%Cred%cr %Cblue%h %Cgreen%D %Cblue%s%Creset" --color
    echo

    echo -n "workbench:  prebuilding executables (because of useCabalRun): "
    for exe in tx-generator cardano-cli cardano-node cardano-topology locli
    do echo -n "$exe "
       cabal -v0 build -- exe:$exe 2>&1 >/dev/null |
           { grep -v 'Temporary modify'; true; } || return 1
    done
    echo
}

function cardano-cli() {
    cabal -v0 run exe:cardano-cli -- "$@"
}

function cardano-node() {
    cabal -v0 run exe:cardano-node "$@"
}

function cardano-topology() {
    cabal -v0 run exe:cardano-topology "$@"
}

function locli() {
  cabal -v0 run exe:locli "$@"
}

function tx-generator() {
  cabal -v0 run exe:tx-generator "$@"
}

export WORKBENCH_EXTRA_FLAGS="$WORKBENCH_EXTRA_FLAGS --cabal"

export -f cardano-cli cardano-node cardano-topology locli tx-generator

workbench-prebuild-executables

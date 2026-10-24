typecheck_watch:
    cd critters && ghcid
remove_saves:
    rm critters/games/*_save*
run game_file save_file interface:
    cd critters && cabal run critters -- {{ quote(game_file) }} --save {{ quote(save_file) }} --interface {{ quote(interface) }}
test:
    cd critters && cabal test

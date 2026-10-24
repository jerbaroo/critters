typecheck_watch:
    cd critters && ghcid
remove_saves:
    cd critters && rm **/*_save_*
run game_file save_file interface:
    cd critters && cabal run critters -- {{ quote(game_file) }} --save {{ quote(save_file) }} --interface {{ quote(interface) }}
test:
    cd critters && cabal test

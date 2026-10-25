remove_saves game_file:
    rm -f {{ quote(game_file) }}/*_save_*
run game_file save_name interface:
    cabal run --project-dir=critters exe:critters -- {{ quote(game_file) }} --save {{ quote(save_name) }} --interface {{ quote(interface) }}
test:
    cd critters && cabal test
watch_logs game_file save_name:
    tail -f {{ quote(game_file + "_save_" + save_name) }}
watch_typecheck:
    cd critters && ghcid

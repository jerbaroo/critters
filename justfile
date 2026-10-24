run game_file save_file interface:
    cd critters && cabal run critters -- {{ quote(game_file) }} --save {{ quote(save_file) }} --interface {{ quote(interface) }}

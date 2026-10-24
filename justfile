run game_file save_file:
    cd critters && cabal run critters -- {{ quote(game_file) }} --save {{ quote(save_file) }}

run game_file:
    cd critters && cabal run critters -- {{ quote(game_file) }}

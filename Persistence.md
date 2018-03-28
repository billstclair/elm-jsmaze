# JSMaze Persistence

This file documents the persistence mechanism used by JSMaze.

Persistence is all done via [billstclair/elm-localstorage](http://package.elm-lang.org/packages/billstclair/elm-localstorage/latest). On the server, this also requires the [node-localstorage](https://www.npmjs.com/package/node-localstorage) NPM package.

`JSMaze.EncodeDecode` contains all the functions that convert between `Value` and different data structures.

`JSMaze.Persistence` actually does all the persisting.

# Philosophy

Everything is persisted all the time. On the client, this means that the part of the `Model` that cannot be computed is saved on every `update`. On the server, it means that every change to a board or player is persisted before it is sent out to the clients.

# Keys

A `Board` is persisted separately from its `Player`s. This is so that not much needs to be written when a player moves.

`Board.id` is world global. It is created by appending the date and time with an optional index, to make it unique.

    "2018-03-27T23:29:03"
    "2018-03-27T23:29:03-2"
    
A persisted board key is "B:" prepended to the board id:
    
    "B:2018-03-27T23:29:03"
    "B:2018-03-27T23:29:03-2"

`Player.id` is unique to a board. It is the player name followed by an optional index, to make it unique:

    "Bill"
    "Bill-12"
    
A persisted player key is "P:", followed by the board-id, followed by a "/" and the player id:

    "P:2018-03-27T23:29:03/Bill"
    "P:2018-03-27T23:29:03/Bill-12"

# Values

The Values are `EncodeDecode.encodeBoard` for boards and `EncodeDecode.decodePlayer` for players.

# Accounts (server only)

TBD

# Images

TBD

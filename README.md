Way back in 2012, when I first heard about [Node.JS](https://nodejs.org/), I recreated a maze game that I played on the PDP-10 and Imlac terminals at MIT in the late 1970s. In that game, a bunch of players ran around a shared maze, shooting at each other.

That game was up at [jsmaze.com](jsmaze.com) for many years, but I took it down recently, to save the $15/month that its AWS VM was costing me. I spent a little time attempting to make it work again, but the libraries it depends on have changed incompatibily, and having been spoiled by Elm, I didn't want to spend the effort it would take to figure it out.

This project is a remake, from scratch, in Elm.

See [persistence.md](https://github.com/billstclair/elm-jsmaze/blob/master/Persistence.md) for information about how boards and players are persisted.

You can run it from `elm reactor`, but you'l only get session persistence:

    cd .../elm-jsmaze
    elm reactor

Then aim your browser at http://localhost:8000/src/ReactorMaze.elm

To update the site with real persistence:

    cd .../elm-jsmaze
    bin/build
    
Then aim your browser at file:///.../elm-jsmaze/site/index.html

Bill St. Clair<br/>
15 March, 2018


# JSMaze Design

## Why?

1. I had a great time playing the maze game on the Imlac terminals late at night at MIT. 

2. I remember "Put that there," at the Architecture Machine Group (Now the MIT Media Lab), and want a place to put things, so I can find them later, by just going to where I put them. I can find my things with my eyes closed, but if somebody moves the furniture, I'll bump into it, even with my eyes open.

3. I'm hoping to charge a small membership fee, maybe $5/year, for advanced features. Get 100,000 people to pay $5/year, and I can work on JSMaze full time, for the rest of my life.

## What?

The game as of 1 October 2019 is very similar to the old MIT game, except there's no networked play, and no shooting. Those are both things that I included in the raw JavaScript version, many years ago, but are not my initial focus.

These tasks are in roughly the order I intend to do them.

1. Multiple named mazes

   You can edit now, but you should be able to save multiple edits, each with its own name and saved state, and return to any one at will.

1. Paste images and video URLs on the walls. Make them visible with perspective.

1. Networked play

   Multiple people in the same maze, each with a custom appearance. Some able to decorate the walls.

1. Chat. First typed, then audio and video. End-to-end encrypted, if the participants wish that.

1. War mode

   People in war mode may shoot at each other. The old MIT maze result of getting shot were to reappear somewhere random in the maze. There are lots of possibilties, including a scripting language to determine that.

   Those in war mode cannot see, be seen, or interact with those NOT in war mode. If only the real world worked that way.

1. Store text on a wall. Paint on a wall. In addition to pasted images and video URLs.

1. A real scene renderer, so that I can support large rooms, not just the one unit wide halls that the current simple line-drawing renderer can do.

## How?

1. The whole thing will be written in [Elm](https://elm-lang.org/), even the server, using [billstclair/elm-websocket-framework package](http://package.elm-lang.org/packages/billstclair/elm-websocket-framework/latest).

1. The first thing to do is to finish the upgrade to Elm 0.19. I'm close.

1. Use Amazon S3 for individual persistence and sharing of maze designs, drawings, character images, and sculptures (after WebGL rendering), either at Amazon itself, or using the Digital Ocean or other API-compatible system.

1. Also allow sharing through simple copy and paste of JSON representations.

1. Convert the CSS to a simple CSS file, so that I can find a real visual designer to make it pretty.

1. Some sort of payment method for some part of the system, maybe more than some limited time per day of server connection.

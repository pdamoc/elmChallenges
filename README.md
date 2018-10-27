# elmChallenges

This is a learning project that provides some interesting challenges for the Elm programming language, intended to be completed after reading the [Elm Architecture guide](http://guide.elm-lang.org/architecture/index.html).

The challenges were provided by Gil Mizrahi. 

The example code that comes with these challenges requires Elm 0.19 

## Challenge No. 1

Create a program that does the following:

The program will print "Right" in the middle of the screen if the mouse cursor is on the right half of the screen or will print "Left" instead in middle of the screen if the mouse cursor in on the left half of the screen. Try to add different background and foreground colors as well :) also, resizing the browser window should not break the application.

Build instructions: `elm make src/challenge1.elm --output=src/challenge1.js` and then open `src/challenge1.html`. 

## Challenge No. 2 

Write a program that fills the screen with small blue circles, produced (pseudo) randomly and over time (choose how much time to wait between generating a new circle).


Build instructions: `elm make src/challenge2.elm --output=src/challenge2.js` and then open `src/challenge2.html`. 

## Challenge No. 3 

Add two new abilities:
 
- when the 'P' key on the keyboard is pressed, pause the generation of new circles and when pressed again, resume.
- when the 'R' key on the keyboard is pressed, reset and start the animation from the beginning.

Extra: try to add two buttons which will do the same things as 'P' and 'R'.

Build instructions: `elm make src/challenge3.elm --output=src/challenge3.js` and then open `src/challenge3.html`. 

## Challenge No. 4

Write a program that asks the user for a Github username (from a text field) and displays this user's name, avatar and lists his/her programming languages. Fetch the information only after the user stops typing for one second. 


Build instructions: `elm make src/challenge4.elm --output=src/challenge4.js` and then open `src/challenge4.html`. 

## Challenge No. 5

Write a snake clone. Make it save the top score for each user using local storage. Make it possible to start the game again without refreshing the page.

Build instructions: `elm make src/challenge5.elm --output=src/challenge5.js` and then open `src/challenge5.html`. 

## Challenge No. 6 

With the previous challenge, separate the high score from the rest of the application in its own module so it can be reused in another game. Separate the game into its own module so it can be loaded from another application. 

## Solutions on Ellie

- Challenge No. 1: https://ellie-app.com/3GLYq5bGBrTa1
- Challenge No. 2: https://ellie-app.com/3JRgRxpsmrga1
- Challenge No. 3: https://ellie-app.com/3JRgcdfNbcqa1
- Challenge No. 4: https://ellie-app.com/3JRkNHxXSBKa1
- Challenge No. 5: https://ellie-app.com/3JR928vMzJBa1


The update for `0.19` was done by [Bryan Jennings](https://github.com/bryanjenningz).
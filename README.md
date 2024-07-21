# Connect4 - Haskell

### GUI & Text Version
<img title="GUI" alt="Haskell Connect4 GUI" src="/readme/gui.png" width ="40%">  <img title="text" alt="Haskell Connect4 Text" src="/readme/textual.png" width ="40%">


## Run the game using the Haskell Tool Stack:

https://docs.haskellstack.org/en/stable/#__tabbed_2_3

### From the main directory, run:

**$ stack run -- `<args>`**

### Available arguments are:

**--text**: Runs the game as text in the terminal (default)
<br>
**--gui**: Runs a graphical version of the game using GLOSS
<br>
**--rows n**: Sets the number of rows (default is 8)
<br>
**--columns n** Sets the number of columns (default is 8)
<br>

## Game Mechanics & Controls:

Connect4 is a two player game where each player takes turns dropping a coin into a chosen column. A player wins when they have 4 adjacent coins (in any orientation - vertically, horizontally, or diagonally).

The game starts with player Red.

### Controls

**Choose a column**: Type the number of the row you wish to select. They are numbered from left to right, 0 to n-1.
<br>
**Drop a coin**: Press ENTER once a number has been select. If the number was invalid, you will be prompted to choose again.
<br>
**Reset the game (GUI only)**: Press R at any point to reset the game back to its empty state.
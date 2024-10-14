# Jigsaw Sudoku

This repository contains an interactive Jigsaw Sudoku game developed in Haskell. It was created as a final project for the COMP3258 Functional Programming course (2019-2020).

## Introduction

Jigsaw Sudoku is a variation of the classic Sudoku puzzle. Instead of regular 3x3 subgrids, the subgrids in Jigsaw Sudoku can have irregular shapes. This Haskell program allows users to play, solve, and interact with Jigsaw Sudoku puzzles through a command-line interface.

## Features

- Load puzzles from a file
- Save progress to a file
- Display the puzzle in the terminal
- Make, undo, and redo moves
- Get hints
- Solve the puzzle automatically

## Installation

To run this project, ensure you have [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) installed.

1. Clone the repository:
```bash
git clone https://github.com/yourusername/jigsaw-sudoku.git
```
2. Navigate to the project directory:
```bash
cd jigsaw-sudoku
```
3. Compile the project:
```bash
ghc --make Main.hs -o jigsaw-sudoku
```

## Usage

Run the compiled program with:
```bash
./jigsaw-sudoku
```

Follow the on-screen instructions to interact with the game.

## Game Options

Upon starting the game, the following options are available:

1. **Load file**: Load a puzzle from a file.
2. **Quit game**: Exit the program.
3. **Save file**: Save the current puzzle state to a file.
4. **Show puzzle**: Display the current state of the puzzle.
5. **Make a move**: Input a number into the puzzle.
6. **Undo move**: Revert the last move.
7. **Redo move**: Reapply a previously undone move.
8. **Solve**: Automatically solve the puzzle.
9. **Hint**: Provide a hint for the next move.

## How to Play

- A Jigsaw Sudoku grid contains irregularly shaped regions.
- Fill in the grid such that:
  - Each row contains the numbers 1 to N exactly once.
  - Each column contains the numbers 1 to N exactly once.
  - Each irregularly shaped region contains the numbers 1 to N exactly once.
- Use the game options to interact with the puzzle, make moves, and solve it.

## Puzzle File Format

The puzzle file should contain two lines:
1. The size of the grid (e.g., `9` for a 9x9 grid).
2. The initial state of the puzzle, with blank cells represented by `.` and filled cells represented by their respective numbers.

Example:
```text
9
1..4.7..3....5.9......8....9...4..5.....2....6...1...8.2......3.4...6....3..7..1
```

## Future Improvements

Potential future enhancements for this project include:

- Adding a graphical user interface (GUI) for a more user-friendly experience.
- Supporting larger grid sizes and different shapes for more variety.
- Improving the solver with advanced techniques for better performance.

## Author

This project was developed by Shubhankar Agrawal as part of the COMP3258 Functional Programming course.

- **Email**: [shubhankar.a31@gmail.com](mailto:shubhankar.a31@gmail.com)
- **UID**: 3035345306

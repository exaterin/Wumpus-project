# Wumpus World in Prolog

This repository contains a simple implementation of the Wumpus World game, a famous game in the world of Artificial Intelligence (AI), implemented in Prolog.

## Overview

In Wumpus World, a player navigates through a grid-based cave, where each grid cell may contain one of four things:

- The player itself
- A pit
- Gold
- A dangerous Wumpus

The goal of the game is to grab the gold and get out of the cave without falling into a pit or encountering the Wumpus. The game is turn-based, and on each turn the player can move to an adjacent cell.

This Prolog-based Wumpus World implementation can read a map from a text file, where each cell's contents are represented by a letter: "s" for the player, "p" for a pit, "g" for gold, and "w" for the Wumpus.

## How to Run

The main predicate `run/1` takes as argument the name of a file that contains the map, and outputs a sequence of moves that leads to a solution, if one exists.

For instance, to run the game with a map defined in a file called map, you would call `run('map')`.

## Implementation

The player is guided by a breadth-first search (BFS) algorithm that explores all possible moves from the current state. The algorithm keeps track of visited states to avoid revisiting them, and stops when it reaches a state where the player has the gold and is back at the starting position.

The `make_move/3` predicate is used to find all possible next states, given the current state and map size. 

The `move_player/5` predicate moves the player in one of the four directions, as long as the destination cell is within the map bounds and does not contain a pit.

The `goal_test/1` predicate checks if the current state is a goal state: a state where the map does not contain gold (because the player has picked it up) and the player is back at the starting position.

Finally, the `print_path/2` predicate is used to output the solution path, showing the sequence of moves from the starting state to the goal state.

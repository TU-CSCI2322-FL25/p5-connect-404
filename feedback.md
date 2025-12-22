Connect 404

Functionality (64/73 points)
  * Game mechanics:                                 20 points
  * Exact game solver:                                 15 points
  * Cut-off depth solver:                                 12 points
  * Reasonable evaluation function:                 1.5/2 points
	 * You only rate the top row and diagonal in each set of 4 columns. 
  * Avoiding unnecessary work:                            0/3 points
	 * Not done
  * Command-line interface:                         9/10 points
	 * Help flag incomplete - looks like it just doesn’t get called unless a file is specified.
  * Move and verbose flags:                         2/5 points
	 * Doesn’t look like you have the pretty-printing (story 5) at all, and verbose does nothing.
  * Error-handling:                                 1/5 points
	 * Incomplete error handling on reading, using catMabyes, unsafe tail, fromJust
	 * Multiple fromMaybe’s which should be case expressions, but some aren’t ever checked.
	 * getMove has a default move, which should be the Maybe.
	 * Errors for updateGame checked but crashed instead of handled.
	 * updateColumn is missing cases.
  * Makefile:                                        0.5/1 point
	 * Still called cluster
  * Interactive Mode:                                2/5 points
	 * Still requires a file, prints in file format.
	 * reading moves is unsafe, crashes if move is invalid.

Design (20/27 points)
  * Well-designed data types                        8 points
  * Well-decomposed functions                        6/10 points
	 * Dispatch is nice, but the guards should really be on a second line. Also, it checks for errors after main does similar checks.
	 * Missing IO game reading action
	 * getDepth has default but is always called in an ‘if’
  * Good module decomposition                        2 points
	 * Only two modules, but at least read code is in Main.
  * Good variable names                                2 points
  * Efficient/idiomatic code                        2/5 points
	 * Some ugly functions and workarounds to avoid pattern matching.

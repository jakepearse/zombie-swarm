-module(pythagoras).
-author("Robert Hales rsjh3@kent.ac.uk").
-export([pyth/4]).
pyth(X,Y,X2,Y2)->
	math:sqrt(math:pow((X-X2),2)+math:pow((Y-Y2),2)).

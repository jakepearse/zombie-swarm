-module(trigstuff).
-author("Robert Hales rsjh3@kent.ac.uk").
-export([findcoordinates/4]).
findcoordinates(0,Speed,X,Y)->
	{X,Y + Speed};
findcoordinates(360,Speed,X,Y)->
	{X,Y + Speed};
findcoordinates(90,Speed,X,Y)->
	{X + Speed,Y};
findcoordinates(180,Speed,X,Y)->
	{X,Y - Speed};
findcoordinates(270,Speed,X,Y)->
	{X - Speed,Y};
findcoordinates(Bearing,Speed,X,Y) when Bearing > 0, Bearing < 90 ->
	NewX =X + (math:sin(Bearing*0.0174532925) * Speed),
	NewY =Y + (math:cos(Bearing*0.0174532925) * Speed),
	{NewX,NewY};
findcoordinates(Bearing,Speed,X,Y) when Bearing > 90, Bearing < 180 ->
	Adjbearing = Bearing - 90,
	NewX =X + (math:cos(Adjbearing*0.0174532925) * Speed),
	NewY =Y - (math:sin(Adjbearing*0.0174532925) * Speed),
	{NewX,NewY};
findcoordinates(Bearing,Speed,X,Y) when Bearing > 180, Bearing < 270 ->
	Adjbearing = Bearing - 180,
	NewX =X - (math:sin(Adjbearing*0.0174532925) * Speed),
	NewY =Y - (math:cos(Adjbearing*0.0174532925) * Speed),
	{NewX,NewY};
findcoordinates(Bearing,Speed,X,Y) when Bearing > 270, Bearing < 360 ->
	Adjbearing = Bearing - 270,
	NewX =X - (math:cos(Adjbearing*0.0174532925) * Speed),
	NewY =Y + (math:sin(Adjbearing*0.0174532925) * Speed),
	{NewX,NewY};
findcoordinates(_Bearing,_Speed,X,Y)->
	{X,Y}.
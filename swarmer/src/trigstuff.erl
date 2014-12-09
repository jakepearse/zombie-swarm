-module(trigstuff).
-author("Robert Hales rsjh3@kent.ac.uk").
-define(MOVEMENT_DISTANCE, 1).
-export([findcoordinates/3]).
findcoordinates(0,X,Y)->
	{X,Y + ?MOVEMENT_DISTANCE};
findcoordinates(360,X,Y)->
	{X,Y + ?MOVEMENT_DISTANCE};
findcoordinates(90,X,Y)->
	{X + ?MOVEMENT_DISTANCE,Y};
findcoordinates(180,X,Y)->
	{X,Y - ?MOVEMENT_DISTANCE};
findcoordinates(270,X,Y)->
	{X - ?MOVEMENT_DISTANCE,Y};
findcoordinates(Bearing,X,Y) when Bearing > 0, Bearing < 90 ->
	NewX =X + (math:sin(Bearing*0.0174532925) * ?MOVEMENT_DISTANCE),
	NewY =Y + (math:cos(Bearing*0.0174532925) * ?MOVEMENT_DISTANCE),
	{NewX,NewY};
findcoordinates(Bearing,X,Y) when Bearing > 90, Bearing < 180 ->
	Adjbearing = Bearing - 90,
	NewX =X + (math:cos(Adjbearing*0.0174532925) * ?MOVEMENT_DISTANCE),
	NewY =Y - (math:sin(Adjbearing*0.0174532925) * ?MOVEMENT_DISTANCE),
	{NewX,NewY};
findcoordinates(Bearing,X,Y) when Bearing > 180, Bearing < 270 ->
	Adjbearing = Bearing - 180,
	NewX =X - (math:sin(Adjbearing*0.0174532925) * ?MOVEMENT_DISTANCE),
	NewY =Y - (math:cos(Adjbearing*0.0174532925) * ?MOVEMENT_DISTANCE),
	{NewX,NewY};
findcoordinates(Bearing,X,Y) when Bearing > 270, Bearing < 360 ->
	Adjbearing = Bearing - 270,
	NewX =X - (math:cos(Adjbearing*0.0174532925) * ?MOVEMENT_DISTANCE),
	NewY =Y + (math:sin(Adjbearing*0.0174532925) * ?MOVEMENT_DISTANCE),
	{NewX,NewY};
findcoordinates(_Bearing,X,Y)->
	{X,Y}.
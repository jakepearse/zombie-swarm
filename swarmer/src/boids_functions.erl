-module(boids_functions).
-author("Robert Hales rsjh3@kent.ac.uk").
-define(SUPER_EFFECT, 0.03).
-define(FLOCKING_EFFECT,0.05).
-define(VELOCITY_EFFECT,0.04).
-export([super_attractor/4,collision_avoidance/4,
	flocking/3,flocking/6]).

super_attractor(X,Y,Attx,Atty) ->
	Changex = X + (?SUPER_EFFECT*(Attx-X)),
	Changey = Y + (?SUPER_EFFECT*(Atty-Y)),
	{Changex,Changey}.

collision_avoidance(X,Y,Otherx,Othery) ->
	Changex = X + (X - Otherx),
	Changey = Y + (Y - Othery),
	{Changex,Changey}.

flocking([],X,Y) ->
	{X,Y};
flocking(Next,X,Y) ->
	flocking(Next,0,0,0,X,Y).
flocking([],Count,Totalx,Totaly,X,Y)->
	Avgx = Totalx/Count,
	Avgy = Totaly/Count,
	Changex = ((Avgx - X)* ?FLOCKING_EFFECT),
	Changey = ((Avgy - Y)* ?FLOCKING_EFFECT),
	{Changex,Changey};
flocking([H|Ts],Count,Totalx,Totaly,X,Y)->
	{EX,EY} = H,
	Newcount = Count + 1,
	Newtotalx = Totalx + EX,
	Newtotaly = Totaly + EY,
	flocking(Ts,Newcount,Newtotalx,Newtotaly,X,Y).

velocity([],X,Y) ->
	{X,Y};
velocity(Next,X,Y) ->
	velocity(Next,0,0,0,X,Y).
velocity([],Count,Totalx,Totaly,X,Y)->
	Avgx = Totalx/Count,
	Avgy = Totaly/Count,
	Changex = ((Avgx - X)* ?VELOCITY_EFFECT),
	Changey = ((Avgy - Y)* ?VELOCITY_EFFECT),
	{Changex,Changey};
velocity([H|Ts],Count,Totalx,Totaly,X,Y)->
	{EX,EY} = H,
	Newcount = Count + 1,
	Newtotalx = Totalx + EX,
	Newtotaly = Totaly + EY,
	velocity(Ts,Newcount,Newtotalx,Newtotaly,X,Y).
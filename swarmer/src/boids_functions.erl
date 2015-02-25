-module(boids_functions).
-author("Robert Hales rsjh3@kent.ac.uk").
-define(SUPER_EFFECT, 0.03).
-define(FLOCKING_EFFECT,0.05).
-define(VELOCITY_EFFECT,0.04).
-export([super_attractor/4,super_repulsor/4,collision_avoidance/4,
	flocking/3,flocking/6,velocity/3,velocity/6,limit_speed/5,limit_speed/4]).

%Changes the velocity of the entity to move towards a super attractor.
super_attractor(X,Y,Attx,Atty) ->
	Changex = X + (?SUPER_EFFECT*(Attx-X)),
	Changey = Y + (?SUPER_EFFECT*(Atty-Y)),
	{Changex,Changey}.

%Changes the velocity of the entity to move away from a super repulsor.
super_repulsor(X,Y,Repx,Repy) ->
	Changex = X - (?SUPER_EFFECT*(Repx-X)),
	Changey = Y - (?SUPER_EFFECT*(Repy-Y)),
	{Changex,Changey}.

%Prevents entities from colliding.
collision_avoidance(X,Y,Otherx,Othery) ->
	Changex = X + (X - Otherx),
	Changey = Y + (Y - Othery),
	{Changex,Changey}.

%Makes an entity move towards the average locations of the entities in its flock.
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

%Makes an entity match velocity with other entities in it's flock.
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

%Limits the speed of an entity.
limit_speed(Limit,X,Y,Velx,Vely)->
	limit_speed(Limit,pythagoras:pyth(X,Y,(X+Velx),(Y+Vely)),Velx,Vely).
limit_speed(Limit,Speed,Velx,Vely) when Limit < Speed ->
	Limiter = Limit/Speed,
	{Limiter*Velx,Limiter*Vely};
limit_speed(_Limit,_Speed,Velx,Vely) ->
	{Velx,Vely}.

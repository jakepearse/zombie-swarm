-module(pso).
-export([velocity/10,zombie_target/3]).

% This is the pso movement function it needs 10 variables
% it returns X velocity -- how far to move along the X axis & Y velocity (could be negative),
% after you work out your new position, you need to store the velocity to pass back into the function next time
% MaxVelocity is a velocity damper - used by clamp/2 it stop particles accelerating past thier goal,
% Inertia, accelration try 0.7,
% CurrentXPosition & CurrentYPosition,
% These can start at 0, but should move away from zero over time
velocity(MaxVelocity, Inertia, CurrentPositionX, CurrentPositionY, CurrentX_velocity, CurrentY_velocity, PreviousBestX, PreviousBestY, TargetX, TargetY) ->
  VelocityX = clamp(MaxVelocity, Inertia * CurrentX_velocity + random:uniform() * (PreviousBestX - CurrentPositionX) + random:uniform() * (TargetX - CurrentPositionX)),
  VelocityY = clamp(MaxVelocity, Inertia * CurrentY_velocity + random:uniform() * (PreviousBestY - CurrentPositionY) + random:uniform() * (TargetY - CurrentPositionY)),
  {VelocityX,VelocityY}.
  
clamp(MaxVelocity,Velocity) when Velocity < 0 andalso Velocity > MaxVelocity * -1 -> Velocity;
clamp(MaxVelocity,Velocity) when Velocity < 0 -> MaxVelocity;
clamp(MaxVelocity,Velocity) when Velocity < MaxVelocity -> Velocity;
clamp(_,MaxVelocity) -> MaxVelocity.

% I haven't tested this yet but its about right ...
zombie_target(ZombieX,ZombieY,ListOfHumans) ->
  [Target|_OtherStuff] = lists:keysort(1,lists:map(fun({HumanPid,{X,Y}}) ->
    {swarm_libs:pyth(ZombieX,ZombieY,X,Y),HumanPid,{X,Y}} end, ListOfHumans)),
  Target.

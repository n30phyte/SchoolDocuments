#const steps=6.   %solved if no further constraints

car(car1;car2).

location(loc1;loc2).

at(car1,loc1,0).
at(car2,loc2,0).

at(ferry,loc1,0).

goal(T):-
  at(car1,loc2,T),
  at(car2,loc1,T).


%#show empty/2.
%#show occupied/2.

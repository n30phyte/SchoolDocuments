#const steps=7.

car(car1;car2).

location(loc1;loc2).

at(car1,loc1,0).
at(car2,loc1,0).

at(ferry,loc1,0).

goal(T):-
  at(car1,loc2,T),
  at(car2,loc2,T).


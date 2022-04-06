#const steps=16.

car(car1;car2;car3;car4;car5).

location(loc1;loc2;loc3).

at(car1,loc1,0).
at(car2,loc1,0).
at(car3,loc2,3).
at(car4,loc3,1).
at(car5,loc3,2).

at(ferry,loc1,0).

goal(T):-
  at(car1,loc3,T),
  at(car2,loc2,T),
  at(car3,loc1,T),
  at(car4,loc2,T),
  at(car5,loc1,T).

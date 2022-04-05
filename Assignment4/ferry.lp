% Initially, there are cars at various locations, and there is a
% ferry at some location. The ferry can only transport one car at
% a time and the goal is to transport all cars to their
% destinations. No paralell actions are allowed.

#show board/3.
#show move/4.
#show unboard/3.
%#show at/3.
%#show in/3.
%#show moving/3.


time(0..steps).

% Clingo processes "safe programs": any variable occuring in a
% negative literal of rule r must appear in a positive atom in the body of r.
%
% There is nothing wrong to use domains predicates. One may first write
% these predicates and then comment the unnessary ones out (as shown).
% Nothing wrong to leave them there.
% Not all of them can be removed, specially when there is a possibility
% a variable can be instantiated (during grounding) to something unintended.
% You may discover this during debugging using the "#show" diretive.


% actions
{board(Car,Loc,T)} :-
        car(Car),
%       location(Loc), time(T),
        empty(ferry,T),
        at(Car,Loc,T),
        at(ferry,Loc,T),
        not moving(ferry,Loc,T),
        not goal(T).

{unboard(Car,Loc,T)} :-
        car(Car),
%       location(Loc), time(T),
        in(ferry,Car,T),
        at(ferry,Loc,T),
        not moving(ferry,Loc,T),
        not goal(T).

{move(ferry,From,To,T)} :-
%       car(Car),location(From), time(T),
        location(To),
        at(ferry,From,T),
        From != To,
        not goal(T).

moving(ferry,From,T):-     % irrelevant of where ferry moves to
%       location(From),location(Loc), time(T),
        at(ferry,From,T),
        move(ferry,From,Loc,T).

% Below is the wrong code to define empty: it says if there exists
% a Car not in ferry, then ferry is empty.
%
% empty(ferry,T):-
%       car(Car), time(T),
%       not in(ferry,Car,T).

empty(ferry,T):- time(T), not occupied(ferry,T).
occupied(ferry,T) :- in(_,_,T).


%fluents
in(ferry,Car,T+1):-   %an action causes a property to hold
%       car(Car), location(Loc), time(T),
        at(ferry,Loc,T),
        board(Car,Loc,T).

in(ferry,Car,T+1):-
%       car(Car),
        time(T),
        in(ferry,Car,T),
        not affected0(Car,T).

affected0(Car,T) :-
%       time(T), car(Car), location(Loc),
        unboard(Car,Loc,T).

% !!! Cannot replace the above by below - it says that
% Car is in ferry at T+1 if at T there is a location Loc
% s.t. Car is not unboarded - not intended!
%
% in(ferry,Car,T+1):-
%        car(Car), time(T),
%        in(ferry,Car,T),
%        not unboard(Car,Loc,T).

at(ferry,Loc,T+1):-
%       car(Car), location(Loc), time(T),
        at(ferry,Loc,T),
        board(Car,Loc,T).

at(ferry,Loc,T+1):-
%       car(Car), location(Loc), time(T),
        at(ferry,Loc,T),
        unboard(Car,Loc,T).

at(ferry,Loc,T+1):-
%       location(Loc),
        time(T),
        at(ferry,Loc,T), %if we don't have tis line, what could happen?
                         %A: ferry can be everywhere
        not moving(ferry,Loc,T).

at(ferry,To,T+1):-
%       location(To),location(From),
        time(T),
        at(ferry,From,T),
        move(ferry,From,To,T).

at(Car,Loc,T+1):-
        car(Car),  % not commented out - don't want Car instantied to ferry
%       location(Loc), time(T),
        unboard(Car,Loc,T).

at(Car,Loc,T+1):-    %frame axiom
        car(Car), location(Loc), time(T),
        at(Car,Loc,T),
        not board(Car,Loc,T).

goal(T+1):-  time(T), goal(T).
     %once goal is achieved, goal(T) is true for all T > k.

goal :- time(T), goal(T).
:- not goal.


% The code above works for the input file, ferryIn0.lp and ferryIn1.lp,
% but not ferryIn2.lp.
%
% Discover what is wrong. Consider adding the following constraints:
% 
% 1. ferry cannot be moved to two different locations at the same time
% 2. it's not possible to board different cars at the same time and same location. 







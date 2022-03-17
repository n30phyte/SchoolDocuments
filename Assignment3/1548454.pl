% Spec: a parameter headed with + means it is an input parameter--it's bound when the predicate is called, and a parameter headed with - is an output parameter--it's a variable when the predicate is called and bound to a value when the predicate is proved. A parameter headed by ? means it can be either input or output parameter. 

% Question 1
% setDifference(+S1,+S2,-S3)
% S1 and S2 are lists of atoms, S3 is list of atoms that are in S1 but not in S2. Assume S1 is a set.
% Ex:
% setDifference([a,b,c,d,e,g], [b,a,c,e,f,q], S).
% S=[d,g]

% Check if Item is in List

% True when item is in head of list
isMember(H, [H|T]).

% Called when item is not in head of list
isMember(H, [_|T]) :-
    isMember(H, T).

setDifference(LFirst, LSecond, LOut) :- 
    findall(X, (isMember(X, LFirst), \+ isMember(X, LSecond)), LOut).

% Question 2
% swap(+L, -R)
% L is list of atoms, R is list of atoms from L swapped
% Ex:
% swap([a,1,b,2], W).
% W = [1,a,2,b].
% swap([a,1,b], W).
% W = [1,a,b].

swap([H], [H]) :- !.

swap([A, B], [B, A]) :- !.

swap([A, B | Tin], [B, A | Tout]) :-
    swap(Tin, Tout).

% Question 3
% filter(+L,+OP,+N,-L1)
% L is possibly nested list of numbers
% OP is either equal, greaterThan or lessThan
% N is a number
% L1 is a flat list of all numbers in L that satisfies condition formed by OP and N
% Ex:
% filter([3,4,[5,2],[1,7,3]],greaterThan,3,W).
% W= [4,5,7]
% filter([3,4,[5,2],[1,7,3]],equal,3,W).
% W= [3,3]
% filter([3,4,[5,2],[1,7,3]],lessThan,3,W).
% W= [2,1]

% % No items in list
% flattenList([], []) :- !.

% % Has items in list
% flattenList([H|T], LOut) :- 
%     !, 
%     flattenList(L ).

% % Only one item in list
% flattenList([H], [H]).

% filter(LIn, equal, N, LOut):- .

% filter(LIn, greaterThan, N, LOut).

% filter(LIn, lessThan, N, LOut).

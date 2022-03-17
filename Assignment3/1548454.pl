% Spec: a parameter headed with + means it is an input parameter--it's bound when the predicate is called, and a parameter headed with - is an output parameter--it's a variable when the predicate is called and bound to a value when the predicate is proved. A parameter headed by ? means it can be either input or output parameter. 

% Question 1
% setDifference(+S1,+S2,-S3)
% S1 and S2 are lists of atoms, S3 is list of atoms that are in S1 but not in S2. Assume S1 is a set.
% Ex:
% setDifference([a,b,c,d,e,g], [b,a,c,e,f,q], S).
% S=[d,g]

% Check if Item is in List

% True when item is in head of list
isMember(H, [H|_]).

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

% appendList(+L1, +L2, -L1andL2)

% If L1 is empty, L2 is just L2.
appendList([], L2, L2).

% If L1 is not empty, append H from L1 to output list, and add in T3 from defined inside predicate
appendList([H1 | T1], L2, [H1 | T3]) :-
    % Append T1 to L2 and return value in T3
    appendList(T1, L2, T3).

% No items in list
flattenList([], []) :- !.

% Has items in list
flattenList([H|T], LOut) :-
    !,
    flattenList(H, Left),
    flattenList(T, Right),
    appendList(Left, Right, LOut).

% List is single atom.
flattenList(H, [H]).

filter(LIn, equal, N, LOut) :-
    flattenList(LIn, LFlat),
    !, findall(X, (isMember(X, LFlat), X == N), LOut).

filter(LIn, greaterThan, N, LOut) :-
    flattenList(LIn, LFlat),
    !, findall(X, (isMember(X, LFlat), X > N), LOut).

filter(LIn, lessThan, N, LOut) :-
    flattenList(LIn, LFlat),
    !, findall(X, (isMember(X, LFlat), X < N), LOut).

% Question 4
% countAll(+L, -N)
% L is flat list of atoms
% N is list of pairs, with the first entry being the atom and the second entry being the count of the atoms.
% Ex:
% countAll([a,b,e,c,c,b],N).
% N = [[a,1],[e,1],[b,2],[c 2]]

countAtom(_, [], 0).

countAtom(H, [H|T], N) :- 
    countAtom(H, T, M), 
    N is M + 1, !.

countAtom(A, [_|T], N) :-
    countAtom(A, T, N), !.

removeAtom(_, [], []).

% Atom is head
removeAtom(H, [H|T], L) :-
    removeAtom(H, T, L), !.

% Atom not head
removeAtom(A, [H|T], [H|L]) :-
    removeAtom(A, T, L), !.

countAll([], L).

countAll([H|T], LOut) :-
    countAtom(H, T, N),
    removeAtom(H, T, LTemp),
    LOut is [[H, N + 1], LOut],
    countAll(T, LTemp), !.




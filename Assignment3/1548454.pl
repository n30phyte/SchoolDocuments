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

% countRemoveAtom(+A, +LIn -LOut, -N)
% A: Atom to count and remove
% LIn: List in
% LOut: List out
% N: Count of atom
countRemoveAtom(_, [], [], 1).

% Atom is head
countRemoveAtom(H, [H|T], L, N) :-
    countRemoveAtom(H, T, L, M),
    N is M + 1, !.

% Atom not head
countRemoveAtom(A, [H|T], [H|L], N) :-
    countRemoveAtom(A, T, L, N), !.

countUnsorted([], []).

countUnsorted([H|T1], [[H, N]| L]) :-
    countRemoveAtom(H, T1, T2, N),
    countUnsorted(T2, L), !.

% Taken from https://kti.mff.cuni.cz/~bartak/prolog/sorting.html

sortList(List,Sorted):-  
    i_sort(List,[],Sorted), !.

i_sort([],Acc,Acc).

i_sort([H|T],Acc,Sorted):- 
    insert(H,Acc,NAcc),
    i_sort(T,NAcc,Sorted).

insert([X, M],[[Y, N]|T],[[Y, N]|NT]) :- 
    M > N,
    insert([X, M],T,NT).

insert([X, M],[[Y, N]|T],[[X, M],[Y, N]|T]) :-
    M =< N.

insert(X,[],[X]).

countAll(LIn, LOut) :-
    countUnsorted(LIn, LTemp), 
    sortList(LTemp, LOut).

% sub(+L,+S,-L1),
% L is nested list of atoms, S is a list of pairs with substitutes, L1 is output

kvFind(_, [], _).

kvFind(K, [[K,V] | _], V).

kvFind(K, [[_,_] | T1], V) :-
    kvFind(K ,T1, V), !.

% sub([], _, L2).

% sub([H1|T1], L1, [H2|L2]) :-.

% Question 7
node(_).

edge(_, _).

% Define bidirectional connection
connected(NameA, NameB) :-
    edge(NameA, NameB);
    edge(NameB, NameA).

% Check if A certain node connects to all nodes in the list
connects(_, []).
connects(NameA, [NameH|T]) :-
    connected(NameA, NameH),
    connects(NameA, T).

% True if all nodes in the list connected
allConnected([]).
allConnected([NameH|T]) :-
    connects(NameH, T),
    allConnected(T).

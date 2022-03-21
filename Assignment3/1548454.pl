%%% Question 1
%% setDifference(+S1,+S2,-S3)
%% S1 and S2 are lists of atoms, S3 is list of atoms that are in S1 but not in S2. Assume S1 is a set.
setDifference(LFirst, LSecond, LOut) :- 
    findall(X, (isMember(X, LFirst), \+ isMember(X, LSecond)), LOut).

%% Q1 Helpers
%% isMember(+A, +L)
%% True when atom is in list
isMember(H, [H|_]). % True when item in head of list

isMember(H, [_|T]) :- % True when item is not in head of list
    isMember(H, T).

%%% Question 2
%% swap(+L, -R)
%% L is list of atoms, R is list of atoms from L swapped
swap([H], [H]) :- !.
swap([A, B], [B, A]) :- !.
swap([A, B | T1], [B, A | T2]) :-
    swap(T1, T2).

%%% Question 3
%% filter(+L,+OP,+N,-L1)
%% L is possibly nested list of numbers
%% OP is either equal, greaterThan or lessThan
%% N is a number
%% L1 is a flat list of all numbers in L that satisfies condition formed by OP and N
filter(LIn, equal, N, LOut) :-
    flattenList(LIn, LFlat),
    !, findall(X, (isMember(X, LFlat), X == N), LOut).

filter(LIn, greaterThan, N, LOut) :-
    flattenList(LIn, LFlat),
    !, findall(X, (isMember(X, LFlat), X > N), LOut).

filter(LIn, lessThan, N, LOut) :-
    flattenList(LIn, LFlat),
    !, findall(X, (isMember(X, LFlat), X < N), LOut).

%% Q3 Helpers
% appendList(+L1, +L2, -L1andL2)
appendList([], L2, L2).
appendList([H1 | T1], L2, [H1 | T3]) :-
    appendList(T1, L2, T3).

% flattenList(+L1, -L2)
flattenList([], []) :- !.
flattenList([H|T], LOut) :-
    !, flattenList(H, Left),
    flattenList(T, Right),
    appendList(Left, Right, LOut).
flattenList(H, [H]).

%%% Question 4
%% countAll(+L, -N)
%% L is flat list of atoms
%% N is list of pairs, with the first entry being the atom and the second entry being the count of the atoms.
countAll(LIn, LOut) :-
    countUnsorted(LIn, LTemp), 
    sortList(LTemp, LOut).

%% Q4 Helpers
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
    insertionSort(List,[],Sorted), !.

insertionSort([],Acc,Acc).
insertionSort([H|T],Acc,Sorted):- 
    insert(H,Acc,NAcc),
    insertionSort(T,NAcc,Sorted).
insert([X, M],[[Y, N]|T],[[Y, N]|NT]) :- 
    M > N,
    insert([X, M],T,NT).
insert([X, M],[[Y, N]|T],[[X, M],[Y, N]|T]) :-
    M =< N.
insert(X,[],[X]).

%%% Question 5
%% sub(+L,+S,-L1),
%% L is nested list of atoms, S is a list of pairs with substitutes, L1 is output
sub([], _, []).
sub([H1|T1], L1, [H2|L2]) :- 
    atom(H1),
    replace(H1, L1, H2),
    sub(T1, L1, L2), !.

sub([H1|T1], L1, [H2|L2]) :- 
    \+ atom(H1),
    sub(H1, L1, H2),
    sub(T1, L1, L2).


%% Q5 Helpers
kvFind(_, [], _).
kvFind(K, [[K,V] | _], V).
kvFind(K, [[_,_] | T1], V) :-
    kvFind(K ,T1, V), !.

replace(A, [], A).

replace(A, [[A, V]|_], V) :- !.

replace(A, [[_, _]|T], V) :-
    replace(A, T, V), !.



%%% Question 6
%% clique(-L)
%% L is list of all nodes that are a clique
clique(Nodes) :-
    findall(X, node(X), AllNodes),
    mkSubset(AllNodes, Nodes),
    allConnected(Nodes).

%% Q6 Helpers
mkSubset([],[]).
mkSubset([X|S], [X|L]) :-
    mkSubset(S, L).
mkSubset([_|S], L) :-
    mkSubset(S, L).

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

%%% Question 7
%% convert(+L1, -L2)
convert([], []).
convert([q|T1], [q|T2]) :-
    betweenQuote(T1, T2), !.
convert(L1, L2) :-
    preQuote(L1, L2), !.

% Q7 Helpers
preQuote([], []).
preQuote([e|T], L2) :-
    preQuote(T, L2), !.
preQuote([q|T], [q|L2]) :-
    betweenQuote(T, L2).
preQuote([_|T], [w|L2]) :-
    preQuote(T, L2).

betweenQuote([], []).
betweenQuote([H|T], [H|T2]) :-
    member(q, [H|T]),
    betweenQuote(T, T2), !.

betweenQuote([H|T], T2) :-
    \+ member(q, [H|T]),
    preQuote([H|T], T2), !.

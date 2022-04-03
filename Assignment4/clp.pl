:- use_module(library(clpfd)).

% Question 1

scaledResult(Semester, Type, Score, Scaled) :-
    setup(Semester, Type, Total, Percent),
    Scaled is (Score / Total) * 100 * Percent.

query1(Semester, Name, Total) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final),
    scaledResult(Semester, as1, As1, As1Scaled),
    scaledResult(Semester, as2, As2, As2Scaled),
    scaledResult(Semester, as3, As3, As3Scaled),
    scaledResult(Semester, as4, As4, As4Scaled),
    scaledResult(Semester, midterm, Midterm, MidtermScaled),
    scaledResult(Semester, final, Final, FinalScaled),
    Total is (As1Scaled + As2Scaled + As3Scaled + As4Scaled + MidtermScaled + FinalScaled).

query2(Semester, L) :-
    findall(Name,
        (c325(Semester, Name, _, _, _, _, Midterm, Final),
        setup(Semester, midterm, MidtermTotal, _),
        setup(Semester, final, FinalTotal, _),
        (Final / FinalTotal) > (Midterm / MidtermTotal))
        , L).
    
query3(Semester, Name, as1, NewMark) :-
    retract(c325(Semester, Name, _, As2, As3, As4, Midterm, Final)),
    assert(c325(Semester, Name, NewMark, As2, As3, As4, Midterm, Final)).

query3(Semester, Name, as2, NewMark) :-
    retract(c325(Semester, Name, As1, _, As3, As4, Midterm, Final)),
    assert(c325(Semester, Name, As1, NewMark, As3, As4, Midterm, Final)).

query3(Semester, Name, as3, NewMark) :-
    retract(c325(Semester, Name, As1, As2, _, As4, Midterm, Final)),
    assert(c325(Semester, Name, As1, As2, NewMark, As4, Midterm, Final)).

query3(Semester, Name, as4, NewMark) :-
    retract(c325(Semester, Name, As1, As2, As3, _, Midterm, Final)),
    assert(c325(Semester, Name, As1, As2, As3, NewMark, Midterm, Final)).


query3(Semester, Name, midterm, NewMark) :-
    retract(c325(Semester, Name, As1, As2, As3, As4, _, Final)),
    assert(c325(Semester, Name, As1, As2, As3, As4, NewMark, Final)).

query3(Semester, Name, final, NewMark) :-
    retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, _)),
    assert(c325(Semester, Name, As1, As2, As3, As4, Midterm, NewMark)).

% Question 2

% Question 3

add_letters([Head], Count, Sum) :-
    Exponent is (10 ** Count),
    Sum = Head * Exponent.

add_letters([Head|Tail], Count, Sum) :-
    Exponent is (10 ** Count),
    Count1 is Count + 1,
    Sum = Sum1 + Head * Exponent,
    add_letters(Tail, Count1, Sum1).

add_letters(Word, Sum) :-
    reverse(Word, RWord),
    add_letters(RWord, 0, Sum).

encrypt(Word1, Word2, Word3) :-
    append([Word1, Word2, Word3], L),
    list_to_set(L, Letters),
    [Head1|_] = Word1,
    [Head2|_] = Word2,
    [Head3|_] = Word3,
    !,
    Letters ins 0..9,
    all_different(Letters),
    add_letters(Word1, Sum1),
    add_letters(Word2, Sum2),
    add_letters(Word3, Sum3),
    Sum1 + Sum2 #= Sum3,
    Head1 #\= 0,
    Head2 #\= 0,
    Head3 #\= 0,
    label(Letters).

% Question 4

xalldistinct2([]).

xalldistinct2([H|T]) :-
    maplist(#\=(H), T),
    xalldistinct2(T).

xall-distinct(L) :-
    maplist(xalldistinct2, L).

grid(N, L) :-
    length(L, N),
    maplist(same_length(L), L).

xtranspose([[]|_], []).
xtranspose(Original, [NewRow|RestRows]) :-
    transpose_first_column(Original, NewRow, RestMatrix),
    xtranspose(RestMatrix, RestRows).

%% Take nested list in A, add to B and move the rest into C
transpose_first_column([], [], []).
transpose_first_column([[HInner | TInner] | TOuter], [HInner | HOther], [TInner | TOther]) :-
    transpose_first_column(TOuter, HOther, TOther).

sudoku(Rows) :-
    grid(9, Rows),
        % Rows now is a 9x9 grid of variables
    append(Rows, Vs),
        % Vs is a list of all 9*9 variables in Rows
    Vs ins 1..9,
    xall-distinct(Rows),
            % Variables of each row get distinct values
    xtranspose(Rows, Columns),
        % get the columns of 9x9 grid
    xall-distinct(Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        % need references to rows
    blocks(As, Bs, Cs),
        % deal with three rows at a time
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

problem(P) :-
    P = [[1,_,_,8,_,4,_,_,_],
    [_,2,_,_,_,_,4,5,6],
    [_,_,3,2,_,5,_,_,_],
    [_,_,_,4,_,_,8,_,5],
    [7,8,9,_,5,_,_,_,_],
    [_,_,_,_,_,6,2,_,3],
    [8,_,1,_,_,_,7,_,_],
    [_,_,_,1,2,3,_,8,_],
    [2,_,5,_,_,_,_,_,9]].

t(Rows) :-
    problem(Rows),
    sudoku(Rows),
    maplist(labeling([ff]), Rows),
    maplist(writeln, Rows).

% Question 5

% paper(Id, CoAuthor1, CoAuthor2, Subject).
% reviewer(Name, Subject1, Subject2).

paper(1,lily,xxx,ai).
paper(2,peter,john,database).
paper(3,ann,xxx,theory).
paper(4,ken,lily,network).
paper(5,kris,xxx,games).

reviewer(lily,theory,network).
reviewer(john,ai,theory).
reviewer(peter,database,network).
reviewer(ann,theory,network).
reviewer(kris,theory,games).
reviewer(ken,database,games).
reviewer(bill,database,ai).
reviewer(jim,theory,games).

workLoadAtMost(2).

assoc([],[],_).
assoc([N|R],[E|W],Dom) :-
   find0(N,Dom,E),
   assoc(R,W,Dom).

find0(1,[F|_],F) :- !.
find0(N,[_|R],E) :- 
    N1 is N-1,
    find0(N1,R,E).


assign(W1, W2) :-
    workLoadAtMost(N),
    findall(X, reviewer(X, _, _), LReviewers),
    findall(X, paper(X, _, _, _), LPapers),
    length(LReviewers, ReviewersCount),
    length(LPapers, PaperCount),
    length(W1, PaperCount),
    length(W2, PaperCount),
    !,
    W1 ins 1..ReviewersCount,
    W2 ins 1..ReviewersCount,
    allAssignable(W1, LReviewers),
    allAssignable(W2, LReviewers),
    not(hasDouble(W1, W2))

hasDouble([H], [H]) :- !.
hasDouble([H|_], [H|_]):- !.
hasDouble([H1|T1], [H2|T2]) :- 
    hasDouble(T1, T2).

allAssignable(LDPersons, LReviewers) :-
    assoc(LDPersons, LPersons, LReviewers),
    assignable(LPersons, 1).

assignable(Person, Paper) :-
    reviewer(Person, PersonTopic1, PersonTopic2),
    paper(Paper, PaperPerson1, PaperPerson2, PaperTopic),
    Person \= PaperPerson1, Person \= PaperPerson2,
    (PersonTopic1 = PaperTopic; PersonTopic2 = PaperTopic).



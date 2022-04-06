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

%% When doing arc consistency checking for the A1 Variable
%% (top left corner), the subunit (3x3 box) consistency
%% checking will remove 1, 3 and 9 from the domain of A1.
%% Column-wise AC for A1 will remove 7, 8 and 9
%% Row-wise AC for A1 will remove 2, 3 and 6

%% In A4 (top left corner of top middle box), the subunit
%% checking will remove 2, 3, 5, 6 and 8.
%% Column-wise AC will remove 1, 2, 3, 6, 7, 8
%% Row-wise AC will remove 2, 3 and 6

%% In E5 (middle box), subunit AC will remove 1, 2, 7 and 8
%% Column-wise AC will remove 1 and 2
%% Row-wise AC will remove 7 and 8

%% In B5, subunit AC will remove 2, 3, 5, 6, 8
%% Column-wise AC will remove 1, 2
%% Row-wise AC will remove 1, 3, 5, 9

%% In E2, subunit AC will remove 6, 7, 8
%% Column-wise AC will remove nothing
%% Row-wise AC will remove 7, 8

%% In example 2, when you pick E5 and do local AC, no
%% values can be removed from the domain, as there are
%% no other numbers in the neighbouring boxes within that 3x3.

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

split(L, A, B) :-
    append(A, B, L),
    length(A, N),
    length(B, N).

indexOf(List, Item, Index) :-
    nth1(Index, List, Item), !.

indexOf(_, _, -1).

makeCardinalityPair(N, N, [N-W]) :- 
    workLoadAtMost(M),
    W #=< M,
    !.
makeCardinalityPair(N, StartN, [StartN-W|T]) :-
    N1 is StartN + 1,
    workLoadAtMost(M),
    W #=< M,
    makeCardinalityPair(N, N1, T), !.

getAllReviewers(LReviewers) :-
    findall(X, reviewer(X, _, _), LReviewers).

getAllPapers(LPapers) :-
    findall(X, paper(X, _, _, _), LPapers).

getReviewersForTopic(Topic, LEligible) :-
    findall(X, (reviewer(X, Topic, _) ; reviewer(X, _, Topic)), LFound),
    getAllReviewers(LReviewers),
    maplist(indexOf(LReviewers), LFound, LEligible).

assign(W1, W2) :-
    % Gather facts and domains
    getAllReviewers(LReviewers),
    getAllPapers(LPapers),
    % Calculate lengths if necessary
    length(LReviewers, ReviewersCount),
    length(LPapers, PaperCount),
    N2 is PaperCount * 2,
    !,
    % Set reviewer workload constraints
    makeCardinalityPair(ReviewersCount, 1, ReviewPairs),
    length(AllMappings, N2),
    global_cardinality(AllMappings, ReviewPairs),
    split(AllMappings, W1T, W2T),
    maplist(#\=, W1T, W2T),
    addConstraints(W1T),
    addConstraints(W2T),
    label(AllMappings),
    maplist(indexOf(LReviewers), W1, W1T),
    maplist(indexOf(LReviewers), W2, W2T).
    % label(AllMappings).

addConstraints(LAssignments) :-
    addConstraints(1, LAssignments).
addConstraints(N, [H]) :-
    constrain(N, H).
addConstraints(N, [H|T]) :-
    constrain(N, H),
    N1 is N + 1,
    addConstraints(N1, T).

constrain(NumPaper, NumReviewer) :-
    % Constrain reviewers
    getAllReviewers(LReviewers),
    paper(NumPaper, Person1, Person2, Topic),
    getReviewersForTopic(Topic, LEligible),
    indexOf(LReviewers, Person1, NumPerson1),
    indexOf(LReviewers, Person2, NumPerson2),
    list_to_fdset(LEligible, SEligible),
    NumReviewer in_set SEligible,
    NumReviewer #\= NumPerson1,
    NumReviewer #\= NumPerson2.

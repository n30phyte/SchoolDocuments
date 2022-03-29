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
    length(Word1, N),
    length(Word2, N1),
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

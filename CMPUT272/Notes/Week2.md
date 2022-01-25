# Week 2

## Chapter 2.3

### Arguments

A sequence of statements

All statements except the last are assumptions
The last statement is the conclusion.

Valid arguments are whenever all the assumptions are true, and so is the conclusion

#### Forms

##### Modus Ponen

$p \implies q$
$p$
$\therefore q$

##### Modus Tollens

$p \implies q$
$\sim q$
$\therefore \sim p$

##### Elimination

$p \lor q$
$\sim p$
$\therefore q$

##### Transitivity

$p \implies q$
$q \implies r$
$\therefore p \implies r$

##### Conjunction

$p$
$q$
$\therefore p \land q$

##### Contradiction

$p \implies c$
$\therefore \sim p$

##### Division into cases

$p \lor q$
$p \implies r$
$q \implies r$

##### Generalization

$p$
$\therefore p \lor q$

##### Specialization

$p \land q$
$\therefore p$

#### Example

$\sim p \land q$
$r \implies p$
$\sim r \implies s$
$s \implies t$
$\therefore t$

##### Solution

First statement argues $\sim p$ (spec)
Second statement argues $\sim r$ (mod. tol.)
Third statement and 4th statement argues $\sim r \implies t$ (trans.)
Trans. statement and mod. tol. statement argue $t$
$\therefore t$

#### Invalid Arguments

You can have a valid argument with the wrong conclusion. Valid arguments can have false conclusion.

##### False Argument Example

If I am teaching 272, then I know everything
I am teaching 272
**therefore I know everything**

#### Other common errors

##### Converse error

If bob is a cheater then he sits in the back row
Bob sits in the back row
**hence bob is a cheater**

_It is possible for Bob to not be a cheater, and sitting in the back row $backrow \implies cheater$ truth table_

##### Inverse error

If interest rates are going up, then stock market prices will go down
Interest rates are not going up.
**therefore stock market prices will not go down.**

$p \implies q$
$\sim q$
$\therefore \sim q$

#### Hypothetical Reasoning

$P_1, P_2, P_3, ... P_n$
$q \implies r$

Assume all assumptions are true, then check if conclusion still true.
if $q$ is false, then conclusion is always true regardless of assumptions.

This can be reduced to:
Check if assumptions are all true, and see if you can derive r from that.

##### Hypothetical reasoning example

$u \implies r$
$(r \land s) \implies (p \lor t)$
$q \implies (u \land s)$
$\sim t$
$\therefore q \implies p$

By Hypothetical reasoning:

1. $u \implies r$
2. $(r \land s) \implies (p \lor t)$
3. $q \implies (u \land s)$
4. $\sim t$
5. $q$
$\therefore p$

###### Hypothetical reasoning solution

a) (5) and (3) by modus ponen $u \land s$
b) by specialization of a) $u$ and $s$
c) (1) and (b) by modus ponen $r$
d) (b) and (c) by conj $r \land s$
e) (2) and (d) by modus ponen $(p \lor t)$
d) (e) and (4) by elimination $p$
QED

##### Knights and Knaves

Knights always tell the truth
Knaves always lie

A: B is a Knight
B: A and I are of opposite types
What are A and B

If A is a knight, then the arguments fall apart, hence A is a knave.
This implies both of them are knaves because that's the only consistent solution.

## Chapter 2.4

### Digital Logic Circuit Design



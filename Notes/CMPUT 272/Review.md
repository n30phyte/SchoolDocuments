Question 2:
$P(n): \sum_{i=1}^{n} i = \frac{(2n+1)^2}{8}$
a)  Prove that for all $k \geq 1: P(k) \implies P(k+1)$
    Let $k \geq 1$ be an arb int and assume $P(k)$
    $\sum_{i=1}^{k} i = \frac{(2k+1)^2}{8}$
    Prove $P(k+1)$:
     $\sum_{i=1}^{k+1} i = \sum_{i=1}^k i + (k+1)$

Question 4:
R is a relation on X, $|x| \geq 2$. Sym, trans, not refl.
$\bar{R} = X \times X - R$
a) There is no $x \in X$ such that $(x, x) \in R$
    Since not reflexive,
    $~\forall x \in X, (x,x) \in R$
    $\exists x \in x, (x,x) \not\in R$
    *Exists*, not *for all*.
b) $\bar{R}$ is reflexive
    $\forall x \in X, (x, x) \in \bar{R} \cong \forall x\in X, (x,x) \not\in R$
    hence $\bar{R}$ Not reflexive.
c) $\bar{R}$ is symmetric
    $\forall x,y \in X, (x,y) \in R \iff (y,x) \in R$
    If (x,y) is in R bar, it is **not** in R and vice versa, hence this is true.

Question 5:
f: x -> y, g: y -> z
a) If g o f is onto, then g is onto:
Statement seems to be correct.

onto: $\forall z \in Z, \exists x \in X$
      $g(f(x)) = z$
goal: $\forall z \in Z, \exists y \in Y: g(y) = z$
      given $z \in Z$, let $y=f(x)$ where x is the value for which $g(f(x)) = z$ then $g(y) = z$

p603 q7:
For all $n \geq 0$
$(nC0) ^ 2 + (nC1)^2 + ... + (nCn)^2 = 2nCn$
$(nC0)(nCn) + (nC1)(nCn-1)+ ... + (nCn)(nC0) = (2nCn)$

p604 q37:
$\forall n \geq 0: 3^n = nC0 + 2(nC1) + 2^2(nC2) +...+2^n(nCn)$
soln:
$(1+2)^n$ when expanded opens into the same terms on the right.

p278 q24:
Use well-ordering principle to prove that given any integer $n \geq 1$, there exists an odd integer m and a nonnegative integer k such that $n = 2^k \dot m$

$Let S contain all non negative integers k such that 2^k|n$
$Let k* be the largest element in S$
$2^k*|n -> \frac{n}{2^k*} is odd will be m$
# Week 1

## Chapter 2.1, 2.2

- Statement: Declarative sentence that is either true or false, can't be both.
  - "Two plus two is six" is a **false** statment.
  - "x plus x is greater than zero" is **not** a statement, as it depends on the value of x.
  - Can be assigned to variable (p, q, r, a, b)
  - Primitive: Simplest form, and no way to reduce or split.
  - Compound: Multiple statements joined using operators.
    - Negation (**~**, $\neg$)
    - Conjunction (AND, **$\land$**)
    - Disjunction (OR, **$\lor$**)
    - $p \lor q \land r$ is ambiguous, and is illegal. Must be written as $(p \lor q) \land r$ or $p \lor (q \land r)$
- Argument: Sequence of statements trying to show that the final assertion (conclusion) is true.

- Logic simplification:
  - DeMorgan's Laws
    - $\sim (p \lor q) \equiv \sim p \land\sim q$
  - A Tautology (**t**) is a statement that is always true
    - $p \lor \sim p$
  - A Contradiction (**c**) is a statement that is always false
    - $p \land \sim p$
  - Commutative Law
    - $p \lor q \equiv q \lor p$
  - Associative Law
    - $p \land (q \land r) \equiv (p \land q) \land r$
    - $p \lor (q \lor r) \equiv (p \lor q) \lor r$
  - Distributive Law
    - $p \land (q \lor r) \equiv (p \land q) \lor (p \land r)$
    - $p \lor (q \land r) \equiv (p \lor q) \land (p \lor r)$
  - Negation
    - $p \lor \sim p \equiv \textbf{t}$
    - $p \land \sim p \equiv \textbf{c}$
  - Double Negation
    - $\sim (\sim p) \equiv p$
  - Absorption
    - $p \lor (p\land q) \equiv p$
    - $p \land (p\lor q) \equiv p$

- Conditional Statement
  - if p then q, ($p \implies q$)
    - Truth of P implies truth of q
      - "If you call me tonight(p), I will bring your book tomorrow(q)"

| p   | q   | $p \implies q$ |
| --- | --- | -------------- |
| T   | T   | T              |
| T   | F   | F              |
| F   | T   | T              |
| F   | F   | T              |

- Conditional extras
  - Contrapositive:
    - $\sim q \implies \sim p \equiv p \implies q$
  - Converse
    - $q \implies p \neq p \implies q$
  - Inverse
    - $\sim p \implies \sim q \neq p \implies q$
  - if p is true, q must be true.
    - the opposite is not valid.
  - if q is true, p **might** be true.
- Biconditional Statement
  - $p \iff q \equiv (p \implies q) \land (q \implies p)$

| p   | q   | $p \iff q$ |
| --- | --- | ---------- |
| T   | T   | T          |
| T   | F   | F          |
| F   | T   | F          |
| F   | F   | T          |

Order of Operations:
$\sim, \land\;and\;\lor, \implies\;and\;\iff$, highest to lowest

Example:

```math
p \land q \implies r \equiv p \implies (q \implies r)

(p \land q) \implies r \equiv \sim (p \land q) \lor r
```

The only time arguments are invalid is when all assumptions are true but conclusion is false.

e.g. p implies q is true
     p is true
     hence q is true
     recall
| p   | q   | $p \implies q$ |
| --- | --- | -------------- |
| T   | T   | T              |
| T   | F   | F              |
| F   | T   | T              |
| F   | F   | T              |

Check validity:
Look at rows where assumptions are true, i.e. row 1 (p is true, p implies q is true).
Conclusion is true at row 1

Arguments

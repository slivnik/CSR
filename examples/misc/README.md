# CSR: small examples

- <code>exampleABCn.csr</code>: the context-sensitive reduction system for $ \{ a^n b^n c^n ; n \ge 1 \} $ taken from D. Grune, C.J. Jacobs, Parsing Techniques, A Practical Guide, 2nd Edition, Monographs in Computer Science, Springer-Verlag, 2008.
- <code>exampleABCnL-</code>$k$<code>.csr</code> (for $ k = 1,2,3 $): the context-sensitive reduction systems for $ { a^n b^n c^n ; n \ge 1 } $ performing reductions close to the beginning of the sentenital form being parsed.
- <code>exampleABCnR-</code>$k$<code>.csr</code> (for $ k = 1,2,3 $): the context-sensitive reduction systems for $ { a^n b^n c^n ; n \ge 1 } $ performing reductions close to the end of the sentenital form being parsed.
- <code>exampleABCn.2</code>: traces of parsing <code>a a b b c c</code> using <code>exampleABCn.csr</code>.
- <code>exampleABCn.3</code>: traces of parsing <code>a a a b b b c c c</code> using <code>exampleABCnL-</code>$k$<code>.csr</code> and <code>exampleABCnR-</code>$k$<code>.csr</code> (for $ k = 1,2,3 $).


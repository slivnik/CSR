# CSR: source code

**Tools needed:**

- [Glasgow Haskell Compiler](https://www.haskell.org/ghc/),
- [Alex](https://www.haskell.org/alex/) (the lexical analyser generator for Haskell) and 
- [Happy](https://www.haskell.org/happy/) (the parser generator for Haskell).

(Alex and Happy are needed to read the file containing the context-sensitive reduction system only.)

**Compiling:** Just type <code>make</code> to produce the executable <code>CSRparser</code>.

**Running:** The program, i.e., <code>CSRparser</code>, takes two arguments:

- the name of the file containing the context-sensitive reduction system and
- the name of the algorithm (<code>naive</code>, <code>bjump</code> or <code>dfa</code>).

It reads tokens from the standard input and prints the trace of the parsing process to the standard output.

For example, type

> echo "a a b b c c" | ./CSRparser \.\./examples/misc/exampleABCn.csr dfa

to get the following ouput:

> CONTEXT-SENSITIVE REDUCTION SYSTEM:
>
> symbs: [ ] s a b c S Q
> terms: [ ] a b c
> goal:  s
> reductions:
> (1) a b c --> S
> (2) a S Q --> S
> (3) b b c c --> b Q c
> (4) Q c --> c Q
>
>DFA PARSER:
>
> 0 . 
>
> 0 [ 0 a 1 a 1 b 2 b 3 c 4 c 8 . 
>  7 compares and applying 1: (3) b b c c --> b Q c
> 0 [ 0 a 1 a 1 . b - Q - c -
>
> 0 [ 0 a 1 a 1 b 2 Q 6 c 7 . 
> 3 compares and applying 2: (4) Q c --> c Q
> 0 [ 0 a 1 a 1 b 2 . c - Q -
> 
> 0 [ 0 a 1 a 1 b 2 c 9 . Q -
> 1 compares and applying 3: (1) a b c --> S
> 0 [ 0 a 1 . S - Q -
> 
> 0 [ 0 a 1 S 10 Q 11 . 
> 2 compares and applying 4: (2) a S Q --> S
> 0 [ 0 . S -
> 
> 0 [ 0 S 0 ] 0 .
> 2 compares
> 
> 15 total compares

To understand the output above, consider the following excerpt:

> 0 [ 0 a 1 a 1 . b - Q - c -
>
> 0 [ 0 a 1 a 1 b 2 Q 6 c 7 . 
> 3 compares and applying 2: (4) Q c --> c Q
> 0 [ 0 a 1 a 1 b 2 . c - Q -

The dot (<code>.</code>) denotes the current position.  Thus the step starts in state <code>1</code> right before <code>b Q c</code> that were added by reduction applying <code>b b c c --> b Q c</code>.  The parser moves over <code>b Q c</code> and applies reduction <code>Q c --> c Q</code>.  The current position is now right before the newly added <code>c Q</code> that replaced <code>Q c</code>.


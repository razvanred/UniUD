# ProgettoLC parteB Riccardo Cavasin Relazione

Calcolo la collezione canonica di insiemi di item LR(0) sulla grammatica aumentata:

0. E' $\rightarrow$ E
1. E $\rightarrow$ L `=` R
2. E $\rightarrow$ L `pp`
3. E $\rightarrow$ `pp` L
4. L $\rightarrow$ `id`
5. L $\rightarrow$ L `[` E `]`
6. R $\rightarrow$ `num`
7. R $\rightarrow$ L
8. R $\rightarrow$ R `+` R
9. R $\rightarrow$ L `pp`
10. R $\rightarrow$ `pp` L

## Automa LR(0)

Per ogni stato sono distinti i kernel items (sopra), dagli altri items (sotto). Gli items di tipo $A\rightarrow\alpha\ \bullet$ che generano un'azione reduce sono evidenziati in grassetto. È stato aggiunto un simbolo "$" che rappresenta l'EOF, la fine dell'input.

* $I_0=$ closure($\{$E' $\rightarrow$ $\bullet$ E \$$\}$)
  * E' $\rightarrow$ $\bullet$ E $
  ---
  * E $\rightarrow$ $\bullet$ L `=` R
  * E $\rightarrow$ $\bullet$ L `pp`
  * E $\rightarrow$ $\bullet$ `pp` L
  * L $\rightarrow$ $\bullet$ `id`
  * L $\rightarrow$ $\bullet$ L `[` E `]`
* $I_1=$ goto($I_0$, E)
  * **E' $\rightarrow$ E $\bullet$ $**
* $I_2=$ goto($I_0$, L) $=$ goto($I_7$, L)
  * E $\rightarrow$ L $\bullet$ `=` R
  * E $\rightarrow$ L $\bullet$ `pp`
  * L $\rightarrow$ L $\bullet$ `[` E `]`
* $I_3=$ goto($I_0$, `pp`) $=$ goto($I_7$, `pp`)
  * E $\rightarrow$ `pp` $\bullet$ L
  ---
  * L $\rightarrow$ $\bullet$ `id`
  * L $\rightarrow$ $\bullet$ L `[` E `]`
* $I_4=$ goto($I_0$, `id`) $=$ goto($I_3$, `id`) $=$ goto($I_5$, `id`) $=$ goto($I_7$, `id`) $=$ goto($I_{12}$, `id`) $=$ goto($I_{14}$, `id`)
  * **L $\rightarrow$ `id` $\bullet$**
* $I_5=$ goto($I_2$, `=`)
  * E $\rightarrow$ L `=` $\bullet$ R
  ---
  * R $\rightarrow$ $\bullet$ `num`
  * R $\rightarrow$ $\bullet$ L
  * R $\rightarrow$ $\bullet$ R `+` R
  * R $\rightarrow$ $\bullet$ L `pp`
  * R $\rightarrow$ $\bullet$ `pp` L
  * L $\rightarrow$ $\bullet$ `id`
  * L $\rightarrow$ $\bullet$ L `[` E `]`
* $I_6=$ goto($I_2$, `pp`)
  * **E $\rightarrow$ L `pp` $\bullet$**
* $I_7=$ goto($I_2$, `[`) $=$ goto($I_8$, `[`) $=$ goto($I_{11}$, `[`) $=$ goto($I_{16}$, `[`)
  * L $\rightarrow$ L `[` $\bullet$ E `]`
  ---
  * E $\rightarrow$ $\bullet$ L `=` R
  * E $\rightarrow$ $\bullet$ L `pp`
  * E $\rightarrow$ $\bullet$ `pp` L
  * L $\rightarrow$ $\bullet$ `id`
  * L $\rightarrow$ $\bullet$ L `[` E `]`
* $I_8=$ goto($I_3$, L)
  * **E $\rightarrow$ `pp` L $\bullet$**
  * L $\rightarrow$ L $\bullet$ `[` E `]`
* $I_9=$ goto($I_5$, R)
  * **E $\rightarrow$ L `=` R $\bullet$**
  * R $\rightarrow$ R $\bullet$ `+` R
* $I_{10}=$ goto($I_5$, `num`) $=$ goto($I_{14}$, `num`)
  * **R $\rightarrow$ `num` $\bullet$**
* $I_{11}=$ goto($I_5$, L) $=$ goto($I_{14}$, L)
  * **R $\rightarrow$ L $\bullet$**
  * R $\rightarrow$ L $\bullet$ `pp`
  * L $\rightarrow$ L $\bullet$ `[` E `]`
* $I_{12}=$ goto($I_5$, `pp`) $=$ goto($I_{14}$, `pp`)
  * R $\rightarrow$ `pp` $\bullet$ L
  ---
  * L $\rightarrow$ $\bullet$ `id`
  * L $\rightarrow$ $\bullet$ L `[` E `]`
* $I_{13}=$ goto($I_7$, E)
  * L $\rightarrow$ L `[` E $\bullet$ `]`
* $I_{14}=$ goto($I_9$, `+`) $=$ goto($I_{18}$, `+`)
  * R $\rightarrow$ R `+` $\bullet$ R
  ---
  * R $\rightarrow$ $\bullet$ `num`
  * R $\rightarrow$ $\bullet$ L
  * R $\rightarrow$ $\bullet$ R `+` R
  * R $\rightarrow$ $\bullet$ L `pp`
  * R $\rightarrow$ $\bullet$ `pp` L
  * L $\rightarrow$ $\bullet$ `id`
  * L $\rightarrow$ $\bullet$ L `[` E `]`
* $I_{15}=$ goto($L_{11}$, `pp`)
  * **R $\rightarrow$ L `pp` $\bullet$**
* $I_{16}=$ goto($I_{12}$, L)
  * **R $\rightarrow$ `pp` L $\bullet$**
  * L $\rightarrow$ L $\bullet$ `[` E `]`
* $I_{17}=$ goto($I_{13}$, `]`)
  * **L $\rightarrow$ L `[` E `]` $\bullet$**
* $I_{18}=$ goto($I_{14}$, R)
  * **R $\rightarrow$ R `+` R $\bullet$**
  * R $\rightarrow$ R $\bullet$ `+` R

## Tabella di parsing SLR

Contrassegnato con "*" nella tabella c'è l'unico conflitto shift/reduce (s14/r8) nella tabella di parsing. Il conflitto è dato dall'ambiguità dell'operatore `+`. Come specificato dalla consegna, si sceglie lo shift per ottenere l'associatività a destra. Eccetto per questo conflitto, la grammatica è SLR(1).

| first                                     | follow                                               |
| ----------------------------------------- | ---------------------------------------------------- |
| $first($E'$)=$ $\{$ `pp`, `id` $\}$       | $follow($E'$)=$ $\{$ $ $\}$                          |
| $first($E$)=$ $\{$ `pp`, `id` $\}$        | $follow($E$)=$ $\{$ `]`, $ $\}$                      |
| $first($L$)=$ $\{$ `id` $\}$              | $follow($L$)=$ $\{$ `=`, `pp`, `[`, `]`, $, `+` $\}$ |
| $first($R$)=$ $\{$ `num`, `pp`, `id` $\}$ | $follow($R$)=$ $\{$ `+`, `]`, $ $\}$                 |

| stato \ action$\{$ | `+`  | `=` | `[`  | `]` | `id` | `num` | `pp` |   $   | $\}\,$ goto$\{$ | E'  |  E  |  L  |  R  | $\}$ |
| ------------------ | :--: | :-: | :--: | :-: | :--: | :---: | :--: | :---: | --------------- | :-: | :-: | :-: | :-: | ---- |
| $I_{0}$            |  e2  | e3  |  e4  | e2  |  s4  |  e6   |  s3  |  e1   |                 | e0  |  1  |  2  | e0  |      |
| $I_{1}$            | e0!  | e0! | e0!  | e0! | e0!  |  e0!  | e0!  | *acc* |                 | e0  | e0  | e0  | e0  |      |
| $I_{2}$            | e14  | s5  |  s7  | e2  | e18  |  e6   |  s6  |  e1   |                 | e0  | e0  | e0  | e0  |      |
| $I_{3}$            | e14  | e2  |  e4  | e2  |  s4  |  e5   |  e2  |  e1   |                 | e0  | e0  |  8  | e0  |      |
| $I_{4}$            |  r4  | r4  |  r4  | r4  | e8^  |  e8^  |  r4  |  r4   |                 | e0  | e0  | e0  | e0  |      |
| $I_{5}$            | e15  | e2  |  e2  | e2  |  s4  |  s10  | s12  |  e1   |                 | e0  | e0  | 11  |  9  |      |
| $I_{6}$            | e14  | e16 |  e2  | r2  |  e2  |  e2   |  e2  |  r2   |                 | e0  | e0  | e0  | e0  |      |
| $I_{7}$            | e14  | e3  |  e4  | e19 |  s4  |  e6   |  s3  |  e1   |                 | e0  | 13  |  2  | e0  |      |
| $I_{8}$            | e14  | e16 |  s7  | r3  |  e2  |  e2   |  e2  |  r3   |                 | e0  | e0  | e0  | e0  |      |
| $I_{9}$            | s14  | e17 | e19  | r1  |  e7  |  e7   |  e2  |  r1   |                 | e0  | e0  | e0  | e0  |      |
| $I_{10}$           |  r6  | e9^ | e9^  | r6  | e9^  |  e9^  | e9^  |  r6   |                 | e0  | e0  | e0  | e0  |      |
| $I_{11}$           |  r7  | e17 |  s7  | r7  |  e2  |  e10  | s15  |  r7   |                 | e0  | e0  | e0  | e0  |      |
| $I_{12}$           |  e2  | e2  |  e4  | e2  |  s4  |  e5   |  e2  |  e1   |                 | e0  | e0  | 16  | e0  |      |
| $I_{13}$           | e0!  | e0! |  e2  | s17 | e0!  |  e0!  |  e2  |  e1   |                 | e0  | e0  | e0  | e0  |      |
| $I_{14}$           |  e2  | e2  |  e4  | e11 |  s4  |  s10  | s12  |  e1   |                 | e0  | e0  | 11  | 18  |      |
| $I_{15}$           |  r9  | e17 | e12^ | r9  | e12^ | e12^  | e12^ |  r9   |                 | e0  | e0  | e0  | e0  |      |
| $I_{16}$           | r10  | e17 |  s7  | r10 |  e2  |  e7   |  e2  |  r10  |                 | e0  | e0  | e0  | e0  |      |
| $I_{17}$           |  r5  | r5  |  r5  | r5  | e13^ | e13^  |  r5  |  r5   |                 | e0  | e0  | e0  | e0  |      |
| $I_{18}$           | s14* | e17 | e19  | r8  |  e7  |  e7   |  e2  |  r8   |                 | e0  | e0  | e0  | e0  |      |
| $I_{19}$           | s14* | e17 | e19  | r8  |  e7  |  e7   |  e2  |  r8   |                 | e0  | e0  | e0  | e0  |      |

### Errori

| n°   | azioni                                                                               |
| ---- | ------------------------------------------------------------------------------------ |
| e0   | *print*: internal error<br>exit                                                      |
| e1   | *print*: program ended too early<br>*exit*                                           |
| e2   | *print*: unexpected {lookahead}<br>*skip*                                            |
| e3   | *print*: lvalue missing in assignment<br>*push goto($I$, L)*                         |
| e4   | *print*: accessor without identifier<br>*push goto($I$, L)*                          |
| e5   | *print*: `num` is not a valid lvalue<br>*skip*, *push goto($I$, L)*                  |
| e6   | *print*: `num` is allowed only in a assignment rvalue<br>*skip*                      |
| e7   | *print*: missing `+` operator<br>*push goto($I$, `+`)*                               |
| e8^  | //bubble up<br>r4                                                                    |
| e9^  | //bubble up<br>r6                                                                    |
| e10  | *print*: missing `+` operator<br>r7, *push goto($I$, `+`)*                           |
| e11  | *print*: right `+` operand missing<br>*pop*                                          |
| e12^ | //bubble up<br>r9                                                                    |
| e13^ | //bubble up<br>r5                                                                    |
| e14  | *print*: `+` is allowed only in a assignment rvalue<br>*skip*                        |
| e15  | *print*: left `+` operand missing<br>*push goto($I$, R)*                             |
| e16  | *print*: {symbol[0] symbol[1]} is not a valid lvalue<br>*pop 2*, *push goto($I$, L)* |
| e17  | *print*: only one assignment per expression<br>*skip*                                |
| e18  | *print*: unexpected {lookahead} (add "=")<br>*push goto($I$, `=`)*                   |
| e19  | *print*: accessor cannot be empty<br>*push goto($I$, E)*                             |

### Commenti sulla gestione degli errori

In alcuni casi, si è cercato di rimandare la gestione degli errori per poter dare messaggi più significativi. Questo approccio ha lo svantaggio di essere più complicato da realizzare correttamente e prono ad errori. Le error routines che fanno "bubbling up" sono state marcate con "^". Inoltre, non vengono mai inseriti stati non validi nello stack, quindi la sezione goto della tabella ha solo errori interni.

Alcune routine d'errore fanno riferimento agli ultimi $0\dots n$ simboli nello stack partendo dalla cima.

Per come è fatto il parser SLR generato da questa grammatica, è impossibile gestire il caso d'errore `id [ id + id + ]` come "`id` `[` R `]`" senza aggiungere stati. Questo perché il parser ha già una regola di riduzione da R a "L `=` R" sul lookahead `]`.

## Esecuzione d'esempio

| stack                                                     | symbols                   |                                  input | action                                               |
| --------------------------------------------------------- | ------------------------- | -------------------------------------: | ---------------------------------------------------- |
| $I_0$                                                     |                           | `id [ id + id + ] = num id pp + num $` | s4                                                   |
| $I_0$ $I_4$                                               | `id`                      |    `[ id + id + ] = num id pp + num $` | r4                                                   |
| $I_0$ $I_2$                                               | L                         |    `[ id + id + ] = num id pp + num $` | s7                                                   |
| $I_0$ $I_2$ $I_7$                                         | L `[`                     |      `id + id + ] = num id pp + num $` | s4                                                   |
| $I_0$ $I_2$ $I_7$ $I_4$                                   | L `[` `id`                |         `+ id + ] = num id pp + num $` | r4                                                   |
| $I_0$ $I_2$ $I_7$ $I_2$                                   | L `[` L                   |         `+ id + ] = num id pp + num $` | `+` is allowed only in a assignment rvalue<br>*skip* |
| $I_0$ $I_2$ $I_7$ $I_2$                                   | L `[` L                   |           `id + ] = num id pp + num $` | unexpected id (add "=")<br>*push goto($I$, `=`)*     |
| $I_0$ $I_2$ $I_7$ $I_2$ $I_5$                             | L `[` L (`=`)             |           `id + ] = num id pp + num $` | s4                                                   |
| $I_0$ $I_2$ $I_7$ $I_2$ $I_5$ $I_4$                       | L `[` L (`=`) `id`        |              `+ ] = num id pp + num $` | r4                                                   |
| $I_0$ $I_2$ $I_7$ $I_2$ $I_5$ $I_{11}$                    | L `[` L (`=`) L           |              `+ ] = num id pp + num $` | r7                                                   |
| $I_0$ $I_2$ $I_7$ $I_2$ $I_5$ $I_9$                       | L `[` L (`=`) R           |              `+ ] = num id pp + num $` | s14                                                  |
| $I_0$ $I_2$ $I_7$ $I_2$ $I_5$ $I_9$ $I_{14}$              | L `[` L (`=`) R `+`       |                `] = num id pp + num $` | right `+` operand missing<br>*pop*                   |
| $I_0$ $I_2$ $I_7$ $I_2$ $I_5$ $I_9$                       | L `[` L (`=`) R           |                `] = num id pp + num $` | r1                                                   |
| $I_0$ $I_2$ $I_7$ $I_{13}$                                | L `[` E                   |                `] = num id pp + num $` | s17                                                  |
| $I_0$ $I_2$ $I_7$ $I_{13}$ $I_{17}$                       | L `[` E `]`               |                  `= num id pp + num $` | r5                                                   |
| $I_0$ $I_2$                                               | L                         |                  `= num id pp + num $` | s5                                                   |
| $I_0$ $I_2$ $I_5$                                         | L `=`                     |                    `num id pp + num $` | s10                                                  |
| $I_0$ $I_2$ $I_5$ $I_{10}$                                | L `=` `num`               |                        `id pp + num $` | r9^                                                  |
| $I_0$ $I_2$ $I_5$ $I_{9}$                                 | L `=` R                   |                        `id pp + num $` | missing `+` operator<br>*push goto($I$, `+`)*        |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$                        | L `=` R (`+`)             |                        `id pp + num $` | s4                                                   |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$ $I_4$                  | L `=` R (`+`) `id`        |                           `pp + num $` | r4                                                   |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$ $I_11$                 | L `=` R (`+`) L           |                           `pp + num $` | s15                                                  |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$ $I_11$ $I_{15}$        | L `=` R (`+`) L `pp`      |                              `+ num $` | r9                                                   |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$ $I_18$                 | L `=` R (`+`) R           |                              `+ num $` | s14                                                  |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$ $I_18$ $I_14$          | L `=` R (`+`) R `+`       |                                `num $` | s10                                                  |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$ $I_18$ $I_14$ $I_{10}$ | L `=` R (`+`) R `+` `num` |                                    `$` | r6                                                   |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$ $I_18$ $I_14$ $I_{18}$ | L `=` R (`+`) R `+` R     |                                    `$` | r8                                                   |
| $I_0$ $I_2$ $I_5$ $I_{9}$ $I_{14}$ $I_18$                 | L `=` R (`+`) R           |                                    `$` | r8                                                   |
| $I_0$ $I_2$ $I_5$ $I_9$                                   | L `=` R                   |                                    `$` | r1                                                   |
| $I_0$ $I_1$                                               | E                         |                                    `$` | acc                                                  |

## Calcolo lookahead per parser LALR

Uso le varianti di closure($I$) e goto($I$, X) con lookahead per calcolare i lookahead generati spontaneamente e i lookahead propagati. L'algoritmo per il calcolo dei lookahead utilizza un simbolo non terminale fittizio `#` per calcolare contemporaneamente entrambi.

* $I_0$
  * closure($\{$E' $\rightarrow$ $\bullet$ E, `#` $\}$)
    * E' $\rightarrow$ $\bullet$ E, `#`
    * E $\rightarrow$ $\bullet$ L `=` R, `#`
    * E $\rightarrow$ $\bullet$ L `pp`, `#`
    * E $\rightarrow$ $\bullet$ `pp` L, `#`
    * L $\rightarrow$ $\bullet$ `id`, `=`/`pp`
    * L $\rightarrow$ $\bullet$ L `[` E `]`, `=`/`pp`
* $I_1$
  * closure($\{$E' $\rightarrow$ E $\bullet$, `#` $\}$)
    * E' $\rightarrow$ E $\bullet$, `#`
* $I_2$
  * closure($\{$E $\rightarrow$ L $\bullet$ `=` R, `#` $\}$)
    * E $\rightarrow$ L $\bullet$ `=` R, `#`
  * closure($\{$E $\rightarrow$ L $\bullet$ `pp`, `#` $\}$)
    * E $\rightarrow$ L $\bullet$ `pp`, `#`
  * closure($\{$L $\rightarrow$ L $\bullet$ `[` E `]`, `#` $\}$)
    * L $\rightarrow$ L $\bullet$ `[` E `]`, `#`
* $I_3$
  * closure($\{$E $\rightarrow$ `pp` $\bullet$ L, `#` $\}$)
    * E $\rightarrow$ `pp` $\bullet$ L, `#`
    * L $\rightarrow$ $\bullet$ `id`, `#`/`[`
    * L $\rightarrow$ $\bullet$ L `[` E `]`, `#`/`[`
* $I_4$
  * closure($\{$L $\rightarrow$ `id` $\bullet$, `#` $\}$)
    * L $\rightarrow$ `id` $\bullet$, `#`
* $I_5$
  * closure($\{$E $\rightarrow$ L `=` $\bullet$ R, `#` $\}$)
    * E $\rightarrow$ L `=` $\bullet$ R, `#`
    * R $\rightarrow$ $\bullet$ `num`, `#`/`+`
    * R $\rightarrow$ $\bullet$ L, `#`/`+`
    * R $\rightarrow$ $\bullet$ R `+` R, `#`/`+`
    * R $\rightarrow$ $\bullet$ L `pp`, `#`/`+`
    * R $\rightarrow$ $\bullet$ `pp` L, `#`/`+`
    * L $\rightarrow$ $\bullet$ `id`, `#`/`+`/`pp`
    * L $\rightarrow$ $\bullet$ L `[` E `]`, `#`/`+`/`pp`
* $I_6$
  * closure($\{$E $\rightarrow$ L `pp` $\bullet$, `#` $\}$)
    * E $\rightarrow$ L `pp` $\bullet$, `#`
* $I_7$
  * closure($\{$L $\rightarrow$ L `[` $\bullet$ E `]`, `#` $\}$)
    * L $\rightarrow$ L `[` $\bullet$ E `]`, `#`
    * E $\rightarrow$ $\bullet$ L `=` R, `]`
    * E $\rightarrow$ $\bullet$ L `pp`, `]`
    * E $\rightarrow$ $\bullet$ `pp` L, `]`
    * L $\rightarrow$ $\bullet$ `id`, `=`/`pp`
    * L $\rightarrow$ $\bullet$ L `[` E `]`, `=`/`pp`
* $I_8$
  * closure($\{$E $\rightarrow$ `pp` L $\bullet$, `#` $\}$)
    * E $\rightarrow$ `pp` L $\bullet$, `#`
  * closure($\{$L $\rightarrow$ L $\bullet$ `[` E `]`, `#` $\}$)
    * L $\rightarrow$ L $\bullet$ `[` E `]`, `#`
* $I_9$
  * closure($\{$E $\rightarrow$ L `=` R $\bullet$, `#` $\}$)
    * E $\rightarrow$ L `=` R $\bullet$, `#`
  * closure($\{$R $\rightarrow$ R $\bullet$ `+` R, `#` $\}$)
    * R $\rightarrow$ R $\bullet$ `+` R, `#`
* $I_{10}$
  * closure($\{$R $\rightarrow$ `num` $\bullet$, `#` $\}$)
    * R $\rightarrow$ `num` $\bullet$, `#`
* $I_{11}$
  * closure($\{$R $\rightarrow$ L $\bullet$, `#` $\}$)
    * R $\rightarrow$ L $\bullet$, `#`
  * closure($\{$R $\rightarrow$ L $\bullet$ `pp`, `#` $\}$)
    * R $\rightarrow$ L $\bullet$ `pp`, `#`
  * closure($\{$L $\rightarrow$ L $\bullet$ `[` E `]`, `#` $\}$)
    * L $\rightarrow$ L $\bullet$ `[` E `]`, `#`
* $I_{12}$
  * closure($\{$R $\rightarrow$ `pp` $\bullet$ L, `#` $\}$)
    * R $\rightarrow$ `pp` $\bullet$ L, `#`
    * L $\rightarrow$ $\bullet$ `id`, `#`/`[`
    * L $\rightarrow$ $\bullet$ L `[` E `]`, `#`/`[`
* $I_{13}$
  * closure($\{$L $\rightarrow$ L `[` E $\bullet$ `]`, `#` $\}$)
    * L $\rightarrow$ L `[` E $\bullet$ `]`, `#`
* $I_{14}$
  * closure($\{$R $\rightarrow$ R `+` $\bullet$ R, `#` $\}$)
    * R $\rightarrow$ R `+` $\bullet$ R, `#`
    * R $\rightarrow$ $\bullet$ `num`, `#`/`+`
    * R $\rightarrow$ $\bullet$ L, `#`/`+`
    * R $\rightarrow$ $\bullet$ R `+` R, `#`/`+`
    * R $\rightarrow$ $\bullet$ L `pp`, `#`/`+`
    * R $\rightarrow$ $\bullet$ `pp` L, `#`/`+`
    * L $\rightarrow$ $\bullet$ `id`, `#`/`+`/`pp`/`[`
    * L $\rightarrow$ $\bullet$ L `[` E `]`, `#`/`+`/`pp`/`[`
* $I_{15}$
  * closure($\{$R $\rightarrow$ L `pp` $\bullet$, `#` $\}$)
    * R $\rightarrow$ L `pp` $\bullet$, `#`
* $I_{16}$
  * closure($\{$R $\rightarrow$ `pp` L $\bullet$, `#` $\}$)
    * R $\rightarrow$ `pp` L $\bullet$, `#`
  * closure($\{$L $\rightarrow$ L $\bullet$ `[` E `]`, `#` $\}$)
    * L $\rightarrow$ L $\bullet$ `[` E `]`, `#`
* $I_{17}$
  * closure($\{$L $\rightarrow$ L `[` E `]` $\bullet$, `#` $\}$)
    * L $\rightarrow$ L `[` E `]` $\bullet$, `#`
* $I_{18}$
  * closure($\{$R $\rightarrow$ R `+` R $\bullet$, `#` $\}$)
    * R $\rightarrow$ R `+` R $\bullet$, `#`
  * closure($\{$R $\rightarrow$ R $\bullet$ `+` R, `#` $\}$)
    * R $\rightarrow$ R $\bullet$ `+` R, `#`

La variante di goto($I$, X) ottiene risultati equivalenti a quelli già ottenuti nell'automa LR(0).

L'algoritmo per il calcolo dei lookahead converge al 5° passo. L'ultimo passo aggiunge solo $ allo stato $I_{18}$ ed è stato collassato.

| stato    | item $\quad$ lookahead$\{$                                           |            init            |             pass 1              |                 pass 2                 |                     pass 3                     |                  pass 4 (+1)                   | $\}$ |
| -------- | -------------------------------------------------------------------- | :------------------------: | :-----------------------------: | :------------------------------------: | :--------------------------------------------: | :--------------------------------------------: | ---- |
| $I_{0}$  | E' $\rightarrow$ $\bullet$ E                                                       |             $              |                $                |                   $                    |                       $                        |                       $                        |      |
| $I_{1}$  | E' $\rightarrow$ E $\bullet$                                                       |                            |                $                |                   $                    |                       $                        |                       $                        |      |
| $I_{2}$  | E $\rightarrow$ L $\bullet$ `=` R<br>E $\rightarrow$ L $\bullet$ `pp`<br>L $\rightarrow$ L $\bullet$ `[` E `]` | `]`<br>`]`<br>`=`/`pp`/`[` | `]`/$<br>`]`/\$<br>`=`/`pp`/`[` |    `]`/$<br>`]`/\$<br>`=`/`pp`/`[`     |        `]`/$<br>`]`/\$<br>`=`/`pp`/`[`         |        `]`/$<br>`]`/\$<br>`=`/`pp`/`[`         |      |
| $I_{3}$  | E $\rightarrow$ `pp` $\bullet$ L                                                   |            `]`             |              `]`/$              |                 `]`/$                  |                     `]`/$                      |                     `]`/$                      |      |
| $I_{4}$  | L $\rightarrow$ `id` $\bullet$                                                     |      `=`/`pp`/`[`/`+`      |      `=`/`pp`/`[`/`+`/`]`       |         `=`/`pp`/`[`/`+`/`]`/$         |             `=`/`pp`/`[`/`+`/`]`/$             |             `=`/`pp`/`[`/`+`/`]`/$             |      |
| $I_{5}$  | E $\rightarrow$ L `=` $\bullet$ R                                                  |                            |               `]`               |                 `]`/$                  |                     `]`/$                      |                     `]`/$                      |      |
| $I_{6}$  | E $\rightarrow$ L `pp` $\bullet$                                                   |                            |               `]`               |                 `]`/$                  |                     `]`/$                      |                     `]`/$                      |      |
| $I_{7}$  | L $\rightarrow$ L `[` $\bullet$ E `]`                                              |                            |        `=`/`pp`/`[`/`+`         |         `=`/`pp`/`[`/`+`/$/`]`         |             `=`/`pp`/`[`/`+`/$/`]`             |             `=`/`pp`/`[`/`+`/$/`]`             |      |
| $I_{8}$  | E $\rightarrow$ `pp` L $\bullet$<br>L $\rightarrow$ L $\bullet$ `[` E `]`                        |          <br>`[`           |         `]`<br>`[`/`]`          |          `]`/\$<br>`[`/`]`/$           |              `]`/\$<br>`[`/`]`/$               |              `]`/\$<br>`[`/`]`/$               |      |
| $I_{9}$  | E $\rightarrow$ L `=` R $\bullet$<br>R $\rightarrow$ R $\bullet$ `+` R                           |          <br>`+`           |             <br>`+`             |             `]`<br>`+`/`]`             |              `]`/\$<br>`+`/`]`/$               |              `]`/\$<br>`+`/`]`/$               |      |
| $I_{10}$ | R $\rightarrow$ `num` $\bullet$                                                    |            `+`             |               `+`               |                `+`/`]`                 |                   `+`/`]`/$                    |                   `+`/`]`/$                    |      |
| $I_{11}$ | R $\rightarrow$ L $\bullet$<br>R $\rightarrow$ L $\bullet$ `pp`<br> L $\rightarrow$ L $\bullet$ `[` E `]`      | `+`<br>`+`<br>`+`/`pp`/`[` |   `+`<br>`+`<br>`+`/`pp`/`[`    | `+`/`]`<br>`+`/`]`<br>`+`/`pp`/`[`/`]` | `+`/`]`/\$<br>`+`/`]`/\$<br>`+`/`pp`/`[`/`]`/$ | `+`/`]`/\$<br>`+`/`]`/\$<br>`+`/`pp`/`[`/`]`/$ |      |
| $I_{12}$ | R $\rightarrow$ `pp` $\bullet$ L                                                   |            `+`             |               `+`               |                `+`/`]`                 |                   `+`/`]`/$                    |                   `+`/`]`/$                    |      |
| $I_{13}$ | L $\rightarrow$ L `[` E $\bullet$ `]`                                              |                            |                                 |            `=`/`pp`/`[`/`+`            |            `=`/`pp`/`[`/`+`/`$`/`]`            |            `=`/`pp`/`[`/`+`/`$`/`]`            |      |
| $I_{14}$ | R $\rightarrow$ R `+` $\bullet$ R                                                  |                            |               `+`               |                  `+`                   |                    `+`/`]`                     |                   `+`/`]`/$                    |      |
| $I_{15}$ | R $\rightarrow$ L `pp` $\bullet$                                                   |                            |               `+`               |                  `+`                   |                    `+`/`]`                     |                   `+`/`]`/$                    |      |
| $I_{16}$ | R $\rightarrow$ `pp` L $\bullet$<br>L $\rightarrow$ L $\bullet$ `[` E `]`                        |          <br>`[`           |         `+`<br>`[`/`+`          |             `+`<br>`[`/`+`             |             `+`/`]`<br>`[`/`+`/`]`             |          `+`/`]`/\$<br>`[`/`+`/`]`/$           |      |
| $I_{17}$ | L $\rightarrow$ L `[` E `]` $\bullet$                                              |                            |                                 |                                        |                `=`/`pp`/`[`/`+`                |            `=`/`pp`/`[`/`+`/`$`/`]`            |      |
| $I_{18}$ | R $\rightarrow$ R `+` R $\bullet$<br>R $\rightarrow$ R $\bullet$ `+` R                           |          <br>`+`           |             <br>`+`             |               `+`<br>`+`               |                   `+`<br>`+`                   |         `+`/`]`/ (/\$)<br>`+`/`]` (/$)         |      |

## Tabella di parsing LALR

Le tabelle di parsing sono uguali. La grammatica (disambiguata con l'associatività a destra dell'operatore `+`) era SLR(1), quindi è anche LALR(1).

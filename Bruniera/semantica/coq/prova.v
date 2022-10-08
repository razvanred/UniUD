Fixpoint ack (n m : nat) : nat :=
  match n with
  | O => S m
  | S p => let fix ackn (m : nat) :=
               match m with
               | O => ack p 1
               | S q => ack p (ackn q)
               end
           in ackn m
  end.

Example test1: (ack 1 2) = 4.
Proof. compute. reflexivity. Qed.

Example test2: (ack 3 4) = 125.
Proof. compute. reflexivity. Qed.

Fixpoint ack_flip (b : bool) (n m : nat) : nat*bool :=
  match n with
  | O => (S m, negb b)
  | S p => let fix ackn (b' : bool) (m : nat) :=
               match m with
               | O => ack_flip (negb b') p 1
               | S q => let (a, b'') := (ackn (negb b') q)
                  in ack_flip b'' p a
               end
           in ackn b m
  end.

Example test3: (ack_flip false 0 0) = (ack 0 0, Nat.even 0).
Proof. compute. reflexivity. Qed.
Example test4: (ack_flip false 1 1) = (ack 1 1, Nat.even 1).
Proof. compute. reflexivity. Qed.
Example test5: (ack_flip false 2 2) = (ack 2 2, Nat.even 2).
Proof. compute. reflexivity. Qed.
Example test6: (ack_flip false 3 3) = (ack 3 3, Nat.even 3).
Proof. compute. reflexivity. Qed.

(**
Lemma simplification0: 
forall p m : nat, forall b : bool, 
let fix ackn (b' : bool) (m : nat) :=
   match m with
   | O => ack_flip (negb b') p 1
   | S q => let (a, b'') := (ackn (negb b') q)
      in ack_flip b'' p a
   end
in ackn b m = ack_flip b (S p) m.
Proof. intros. compute. simpl. reflexivity. Qed.

Lemma simplification1: 
forall p m : nat, m > 0 -> forall b : bool, 
let fix ackn (b' : bool) (m : nat) :=
   match m with
   | O => ack_flip (negb b') p 1
   | S q => let (a, b'') := (ackn (negb b') q)
      in ack_flip b'' p a
   end
in ackn b (S m) = 
let (a, b'' ) := (ack_flip b (S p) m)
in ack_flip b'' p a.
Proof. intros.
  induction p.
  - compute. reflexivity.
  

Lemma eavenness0:
forall n, (ack_flip false 0 n) = (S n, Nat.even 0).
Proof. compute. reflexivity. Qed.

Lemma eavenness1:
forall n, (ack_flip false 1 n) = (ack 1 n, Nat.even 1).
Proof. induction n.
  - compute. reflexivity.
  - compute. apply simplification. **)

Theorem ackerness :
forall n m : nat, (fst (ack_flip false n m) = ack n m).
Proof. intros n m. induction n as [| n' IH0].
  - compute. reflexivity.
  - induction m as [| m' IH1].
    + Admitted.

Theorem eavenness0 :
forall n : nat, ack_flip false 1 n = (ack 1 n, false).
Proof. intros. destruct n.
  - compute. reflexivity.
  - assert ().

Theorem eavenness :
forall n : nat, (ack_flip false n n) = (ack n n, Nat.even n).
Proof. intros.
  induction n as [| n' IH].
    - compute. reflexivity.
    - intros. simpl.
      assert (ack_flip false (S n') (S n') = 
      (let fix ackn (n : nat) :=
               match n with
               | O => ack n' 1
               | S q => ack n' (ackn q)
               end
           in ack n' (ackn n')
           , negb (Nat.even n'))).
           {simpl.  }
      
  
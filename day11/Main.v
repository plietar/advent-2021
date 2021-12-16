Require Import String.
Require Import List.
Require Import Coq.Arith.Compare_dec.
Require Import Coq.Arith.PeanoNat.
Import ListNotations.
Require Import Strings.String.
Require Import Ascii.

Definition grid := list (list nat).
Definition solve (input: list (list nat)): unit := tt.

Definition phase1 : grid -> grid := map (map S).
Definition phase3 : grid -> grid := map (map (fun x => if ge_dec x 10 then 0 else x)).

Fixpoint initialFlashes'' (x: nat) (l: list nat) :=
  match l with (o::os) => if o =? 10 then x :: initialFlashes'' (S x) os else initialFlashes'' (S x) os | nil => nil end.

Fixpoint initialFlashes' (y: nat) (l: list (list nat)) :=
  match l with (o::os) => map (fun n => (n, y)) (initialFlashes'' 0 o) ++ initialFlashes' (S y) os | nil => nil end.

Definition initialFlashes := initialFlashes' 0.

Definition checkFlash (center: bool) (o: nat) (x: nat) :=
  if o =? 9 then if center then nil else [x] else nil.

Fixpoint applyFlash'' (center: bool) (a: nat) (x: nat) (row: list nat) :=
  match a, row with
  | 0, (o :: os) =>
      (S o :: os, checkFlash false o x)

  | 1, (o :: os) =>
      let (os', flashes) := applyFlash'' center 0 (S x) os in
      ((if center then o else S o) :: os', checkFlash center o x ++ flashes)

  | 2, (o :: os) =>
      let (os', flashes) := applyFlash'' center 1 (S x) os in
      (S o :: os', checkFlash false o x ++ flashes)

  | (S a'), (o :: os) =>
      let (os', flashes) := applyFlash'' center a' (S x) os in
      (o :: os', flashes)

  | _, _ => (row, nil)
  end.

Fixpoint applyFlash' (a b: nat) (y: nat) (g: grid) : (grid * list (nat * nat)) :=
  match b, g with
  | 0, (r :: rs) =>
      let (r', flashes) := applyFlash'' false a 0 r in
      (r' :: rs, map (fun x => (x, y)) flashes)
  | 1, (r :: rs) =>
      let (r', flashes) := applyFlash'' true a 0 r in
      let (rs', flashes') := applyFlash' a 0 (S y) rs in
      (r' :: rs', map (fun x => (x, y)) flashes ++ flashes')
  | 2, (r :: rs) =>
      let (r', flashes) := applyFlash'' false a 0 r in
      let (rs', flashes') := applyFlash' a 1 (S y) rs in
      (r' :: rs', map (fun x => (x, y)) flashes ++ flashes')
  | (S b'), (r :: rs) =>
      let (rs', flashes) := applyFlash' a b' (S y) rs in
      (r :: rs', flashes)

  | _, _ => (g, nil)
  end.

Definition applyFlash (f: nat * nat) (g: grid) : (grid * list (nat * nat)) :=
  applyFlash' (S (fst f)) (S (snd f)) 0 g.

Fixpoint phase2 (fuel: nat) (flashes: list (nat * nat)) (g: grid) : (grid * nat) :=
  match fuel, flashes with
  | O, _ | _, nil => (g, 0)
  | S fuel', f::flashes' =>
      let (g', flashes'') := applyFlash f g in
      let (g'', n) := phase2 fuel' (flashes' ++ flashes'') g' in
      (g'', S n)
  end.

Definition step (fuel: nat) (g: grid) :=
  let g' := phase1 g in
  let (g'', n) := phase2 fuel (initialFlashes g') g' in
  (phase3 g'', n).

Definition gridSize' (g: list nat) : nat := Datatypes.length g.
Definition gridSize (g: list (list nat)) : nat := list_sum (map gridSize' g).

Fixpoint part1' (fuel: nat) (count: nat) (g: grid) :=
  match count with
  | O => 0
  | S count' =>
      let (g', n) := step fuel g in
      n + part1' fuel count' g'
  end.

Fixpoint part2' (fuel: nat) (count: nat) (tick: nat) (g: grid) :=
  match count with
  | O => 0
  | S count' =>
      let (g', n) := step fuel g in
      if n =? gridSize g' then tick
      else part2' fuel count' (S tick) g'
  end.

Definition part1 (fuel: nat) := part1' fuel 100.
Definition part2 (fuel: nat) := part2' fuel fuel 1.

Fixpoint parseRow (s: string) :=
  match s with
  | EmptyString => nil
  | String c s' => (nat_of_ascii c - 48) :: parseRow s'
  end.

Fixpoint lines (s: string) :=
  match s with
  | EmptyString => nil
  | String "010" s' => EmptyString :: lines s' (* 010 is \n *)
  | String c s' =>
      match lines s' with
      | nil => [String c EmptyString]
      | l :: ls => String c l :: ls
      end
  end.

Definition parse (s: string) : grid := map parseRow (lines s).

Require Coq.extraction.Extraction.
Extraction Language OCaml.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extraction "build/solver.ml" parse part1 part2.

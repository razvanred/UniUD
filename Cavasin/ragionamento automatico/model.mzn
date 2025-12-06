include "diffn_nonstrict.mzn";
include "geost.mzn";
include "all_different.mzn";

opt bool: use_circle_packing;
bool: circle_packing = use_circle_packing default false;
opt bool: use_diffn;
bool: diffN = use_diffn default false;
opt bool: use_geost_bb;
bool: geostBB = use_geost_bb default false;
constraint assert(diffN \/ geostBB -> not circle_packing, "\"use_diffn\" and \"use_geost_bb\" can't be used for circle packing");
constraint assert(not (diffN /\ geostBB), "\"use_diffn\" and \"use_geost_bb\" are mutually exclusive options");

% definitions
type uint = 0..infinity;
type Coord = record(0..max (box_size in box_sizes) (box_size.width): x, 0..max (box_size in box_sizes) (box_size.length): y);
type Batch = record(uint: capacity, uint: height, uint: diameter, uint: quantity);
type Bottle = record(index_set(batches): batch) ++ var record(opt index_bottles: box);
type BoxSize = record(uint: width, uint: length, uint: height, uint: min_occupancy);
function uint: capacity(index_bottles: i) = batches[bottles[i].batch].capacity;
function uint: height(index_bottles: i) = batches[bottles[i].batch].height;
function uint: diameter(index_bottles: i) = batches[bottles[i].batch].diameter;
function var index_bottles: box_(index_bottles: i) = deopt(bottles[i].box);
function var uint: distance(var Coord: p1, var Coord: p2) = (p1.x - p2.x)^2 + (p1.y - p2.y)^2;
function var BoxSize: box_size(var index_bottles: i) = box_sizes[boxes[i]];

predicate occupancy(var BoxSize: box_size, var set of index_bottles: domain) =
  let {
      % 1ml = 1cm³
      var uint: occupancy = sum (j in domain) (capacity(j));
  } in (occupancy*100/(box_size.width*box_size.length*box_size.height) >= box_size.min_occupancy);

predicate packed(list of var Coord: positions, var set of index_bottles: domain) =
  if diffN then
    forall (i in domain) (bounded(positions[i], diameter(i), box_size(box_(i))))
    /\ forall (i in index_set(box_sizes)) (
      let {
        var set of uint: boxed_bottles = {j | j in domain where bottles[j].box = i};
        list of var uint: rect_widths = [if i in boxed_bottles then diameter(i) else 0 endif | i in index_bottles];
      } in diffn_nonstrict(
        [position.x | position in positions],
        [position.y | position in positions],
        rect_widths,
        [diameter(i) | i in index_bottles]
      )
    )
  elseif geostBB then
    abort("\"geost_bb\" implementation impossible: argument 2 expects type array[int,int] of int, but type array[int,int] of var int given")
    % /\ forall (i in index_set(box_sizes)) (
    %   let {
    %     var set of uint: boxed_bottles = {j | j in domain where bottles[j].box = i};
    %     array[index_set(batches), 1..2] of var uint: rectSizes = [(i,j): if i in boxed_bottles then batches[i].diameter else 0 endif | i in index_set(batches), j in 1..2];
    %     array[index_set(batches), 1..2] of uint: rectOffsets = [(i,j): 0 | i in index_set(batches), j in 1..2];
    %     array[index_set(batches)] of set of index_set(batches): shapes = [{bottle.batch} | bottle in bottles];
    %     array[index_set(positions), 1..2] of var uint: positions = [(i,j): if j = 1 then positions[i].x else positions[i].y endif | i in index_set(positions), j in 1..2];
    %     list of index_set(batches): objects = [bottle.batch | bottle in bottles];
    %   } in geost_bb(
    %     2,
    %     rectSizes,
    %     rectOffsets,
    %     shapes,
    %     positions,
    %     objects,
    %     [0, 0],
    %     [box_size(i).width, box_size(i).length]
    %   )
    % )
  else
    forall (i in domain) (
      bounded(positions[i], diameter(i), box_size(box_(i)))
      /\ forall (j in domain where i < j) (
        bottles[i].box = bottles[j].box -> no_overlap(positions[i], diameter(i), positions[j], diameter(j))
      )
    )
  endif;

predicate bounded(var Coord: pos, uint: diameter, var BoxSize: box_size) =
  if circle_packing then
    pos.x+(diameter/2) <= box_size.width /\ pos.y+(diameter/2) <= box_size.length
  else
    pos.x+diameter <= box_size.width /\ pos.y+diameter <= box_size.length
  endif;

predicate no_overlap(var Coord: pos1, uint: diameter1, var Coord: pos2, uint: diameter2) =
  if circle_packing then
    distance(pos1, pos2) >= (diameter1+diameter2)^2
  else
    pos1.x+diameter1 <= pos2.x
    \/ pos1.y+diameter1 <= pos2.y
    \/ pos2.x+diameter2 <= pos1.x
    \/ pos2.y+diameter2 <= pos1.y
  endif;

% parameters
uint: demijohns_number;
uint: demijohn_capacity;
list of Batch: batches;
list of BoxSize: box_sizes;

% variables
set of uint: index_bottles = 1..sum (batch in batches) (batch.quantity);
list of Bottle: bottles::add_to_output = let {
  array[index_bottles] of index_set(batches): bottles = [i | i in index_set(batches), _ in 1..batches[i].quantity];
  array[index_bottles] of var opt index_bottles: boxes;
} in [(batch: bottles[i], box: boxes[i]) | i in index_bottles];
array[index_bottles] of var index_set(box_sizes): boxes::add_to_output;
array[index_bottles] of var Coord: positions::add_to_output;

var set of index_bottles: used_bottles::add_to_output = {i | i in index_bottles where occurs(bottles[i].box)};
var set of index_bottles: used_boxes::add_to_output = {box_(i) | i in index_bottles where occurs(bottles[i].box)};

var 0..demijohns_number*demijohn_capacity: bottled_wine::add_to_output = sum (i in used_bottles) (capacity(i));

% symmetry breaking
constraint forall (i in used_boxes where i > 1) (boxes[i-1] in used_boxes);
constraint forall (i in index_bottles) (not (i in used_bottles) -> positions[i] = (x: 0, y: 0));

%test
% constraint all_different([box_(i) | i in used_bottles]);
% constraint forall (i in index_bottles) (i in used_bottles);
% constraint not (4 in used_boxes) ;
% constraint not (1 in used_boxes) /\ not (5 in used_boxes) ;
%end test

% constraints
constraint forall (i in used_bottles) (height(i) <= box_size(box_(i)).height);

constraint packed(positions, used_bottles);

constraint forall (i in used_boxes) (
  let {
    var BoxSize: box_size = box_size(i);
    var set of uint: boxed_bottles = {j | j in used_bottles where bottles[j].box = i};
  } in occupancy(box_size, boxed_bottles));

% objective
solve
  % ::relax_and_reconstruct(
  %   % [deopt(j) | j in [deopt(bottles[i].box) | i in used_bottles] ++
  %   [positions[i].x | i in index_bottles] ++
  %   [positions[i].y | i in index_bottles], 60)
  % ::seq_search([int_search([bottles[j].box | j in index_bottles], indomain_random, first_fail),
  %   int_search([positions[i].x | i in index_bottles], indomain_random, input_order),
  %   int_search([positions[i].y | i in index_bottles], indomain_random, input_order)])
  % ::restart_luby(2)
  % maximize bottled_wine;
  maximize let {
    uint: unit = length(bottles) + 1;
    var 0..length(bottles): sub = length(bottles) - card(used_boxes);
  } in bottled_wine*unit + sub;


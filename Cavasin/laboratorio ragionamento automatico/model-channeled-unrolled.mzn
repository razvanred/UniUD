% definitions
type uint = 0..infinity;
type Coord = record(0..max (box_size in box_sizes) (box_size.width): x, 0..max (box_size in box_sizes) (box_size.length): y);
type BottleSize = record(uint: capacity, uint: height, uint: diameter);
type Batch = BottleSize ++ record(uint: quantity);
type Bottle = BottleSize ++ var record(opt index_boxes: box, Coord: position);
type BoxSize = record(uint: width, uint: length, uint: height, uint: min_occupancy);
function var bool: boxed(index_bottles: i) = occurs(bottles[i].box);

function var BoxSize: box_size_(var opt index_boxes: i) = boxes[deopt(i)];

predicate occupancy(index_boxes: i) =
  let {
      var BoxSize: box_size = box_size_(i);
      % 1ml = 1cm³
      var uint: occupancy = sum (j in index_bottles) ((bottles[j].box = i)*bottles[j].capacity);
  } in (occupancy*100/(box_size.width*box_size.length*box_size.height) >= box_size.min_occupancy);

predicate packed2D(index_bottles: i) =
  let {
    Bottle: bottle = bottles[i];
    var BoxSize: box_size = box_size_(bottle.box);
  } in bottle.position.x+bottle.diameter <= box_size.width
    /\ bottle.position.y+bottle.diameter <= box_size.length
    /\ forall (j in index_bottles) (
      i < j /\ bottle.box = bottles[j].box ->
      no_overlap(bottle.position, bottle.diameter, bottles[j].position, bottles[j].diameter)
    );

predicate no_overlap(var Coord: pos1, uint: diameter1, var Coord: pos2, uint: diameter2) =
  pos1.x+diameter1 <= pos2.x
  \/ pos1.y+diameter1 <= pos2.y
  \/ pos2.x+diameter2 <= pos1.x
  \/ pos2.y+diameter2 <= pos1.y;

% parameters
uint: demijohns_number;
uint: demijohn_capacity;
list of Batch: batches;
list of BoxSize: box_sizes;

% variables
set of uint: index_bottles = 1..sum (batch in batches) (batch.quantity);
array[index_bottles] of Bottle: bottles::add_to_output = let {
  array[index_bottles] of index_set(batches): bottles = [i | i in index_set(batches), _ in 1..batches[i].quantity];
  array[index_bottles] of var opt index_boxes: boxes;
  array[index_bottles] of var Coord: positions;
} in [(capacity: batches[bottles[i]].capacity, height: batches[bottles[i]].height, diameter: batches[bottles[i]].diameter, box: boxes[i], position: positions[i]) | i in index_bottles];

set of uint: index_boxes = 1..length(box_sizes)*card(index_bottles);
array[index_boxes] of BoxSize: boxes::add_to_output = [box_size | box_size in box_sizes, _ in index_bottles];
var set of index_boxes: used_boxes;

var 0..demijohns_number*demijohn_capacity: bottled_wine::add_to_output = sum (i in index_bottles) (boxed(i)*bottles[i].capacity);

% constraints
constraint forall (i in index_bottles) (boxed(i) -> bottles[i].box in used_boxes);

constraint forall (i in index_bottles) (boxed(i) -> bottles[i].height <= box_size_(bottles[i].box).height);

constraint forall (i in index_bottles) (boxed(i) -> packed2D(i));

constraint forall (i in index_boxes) (i in used_boxes -> occupancy(i));

% objective
solve
  maximize let {
    uint: unit = card(index_bottles)+1;
    var {0} union index_bottles: used_boxes = card(used_boxes);
    var {0} union index_bottles: sub = card(index_bottles)-used_boxes;
  } in bottled_wine*unit+sub;

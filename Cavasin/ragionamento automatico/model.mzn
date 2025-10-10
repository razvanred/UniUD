% Damigiane bis
% Si dispone di n damigiane uguali tra loro contenenti ciascuna k litri di vino (dati di input)
% e di h bottiglie b_1,...,b_h di capacità c_1,...,c_h (si pensi a casi realistici, in cui la bottiglia
% è tipicamente da 1/3 di litro, 1/2 litro, 3/4 litro, 1 litro, 1 litro e 1/2, etc.) Anche questi
% sono dati di input.
% Ci sono poi degli scatoloni per le bottiglie (ce ne sono illimitati) che si vuole spedire (non
% importa se con camion treno o altro). Gli scatoloni hanno dei limiti massimi di capienza
% in termini di bottiglie (si faccia qualche casistica ragionevole basata sui diametri delle
% bottiglie).
% Vogliamo riempire le bottiglie e imballarle in scatoloni "quasi" pieni. Il "quasi" significa
% che sono ammessi scatoloni che contengono un po' meno della quota massima (poni tu a
% caso, che so 1 litro).
% Le bottiglie vanno riempite per intero in base alla loro capacità ma non è necessario
% usarle tutte.
% L'obiettivo è di caricare, con i vincoli sopra e con le bottiglie a disposizione, la massima
% quantità di vino negli scatoloni.
% Se l'ultimo scatolone fosse troppo vuoto allora non viene trasportato, dunque quel vino
% per ora rimane in azienda (e dunque fa calare la quantità totale spedita).
% Si giochi un po' coi parametri per creare delle istanze ragionevoli.

include "diffn_nonstrict.mzn";
include "geost.mzn";

opt bool: use_circle_packing;
bool: circlePacking = use_circle_packing default false;
opt bool: use_diffn;
bool: diffN = use_diffn default false;
opt bool: use_geost_bb;
bool: geostBB = use_geost_bb default false;
constraint assert(diffN \/ geostBB -> not circlePacking, "\"use_diffn\" and \"use_geost_bb\" can't be used for circle packing");
constraint assert(not (diffN /\ geostBB), "\"use_diffn\" and \"use_geost_bb\" are mutually exclusive options");

% definitions
type uint = 0..infinity;
type ufloat = 0.0..infinity;
type Coord = record(0..max (boxSize in boxSizes) (boxSize.width): x, 0..max (boxSize in boxSizes) (boxSize.length): y);
type Batch = record(ufloat: capacity, uint: height, uint: diameter, uint: quantity);
type Bottle = record(index_set(batches): batch) ++ var record(opt index_bottles: box);
type BoxSize = record(uint: width, uint: length, uint: height, uint: minCoverage);
ufloat: pi = 3.141592653589793115997963468544185161590576171875;
function ufloat: capacity(index_bottles: i) = batches[bottles[i].batch].capacity;
function uint: height(index_bottles: i) = batches[bottles[i].batch].height;
function uint: diameter(index_bottles: i) = batches[bottles[i].batch].diameter;
function var index_bottles: box_(index_bottles: i) = deopt(bottles[i].box);
function var ufloat: distance(var Coord: p1, var Coord: p2) = sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2);
function var BoxSize: boxSize_(var index_bottles: i) = boxSizes[deopt(boxes[i])];

predicate coverage(var BoxSize: boxSize, var set of index_bottles: domain) =
  if circlePacking then
      let {
          var ufloat: area = boxSize.width * boxSize.length;
          var ufloat: coverage = pi*sum (j in domain) ((diameter(j)/2)^2);
      } in (coverage/area*100 >= boxSize.minCoverage)
  else
      let {
          var uint: area = boxSize.width * boxSize.length;
          var uint: coverage = sum (j in domain) (diameter(j)^2);
      } in (coverage/area*100 >= boxSize.minCoverage)
  endif;

predicate packed(list of var Coord: positions, var set of index_bottles: domain) =
  if diffN then
    forall (i in domain) (bounded(positions[i], diameter(i), boxSize_(box_(i))))
    /\ forall (i in index_set(boxSizes)) (
      let {
        var set of uint: boxedBottles = {j | j in domain where bottles[j].box = i};
        list of var uint: rectWidths = [if i in boxedBottles then diameter(i) else 0 endif | i in index_bottles];
      } in diffn_nonstrict(
        [position.x | position in positions],
        [position.y | position in positions],
        rectWidths,
        [diameter(i) | i in index_bottles]
      )
    )
  elseif geostBB then
    abort("\"geost_bb\" implementation impossible: argument 2 expects type array[int,int] of int, but type array[int,int] of var int given")
    % /\ forall (i in index_set(boxSizes)) (
    %   let {
    %     var set of uint: boxedBottles = {j | j in domain where bottles[j].box = i};
    %     array[index_set(batches), 1..2] of var uint: rectSizes = [(i,j): if i in boxedBottles then batches[i].diameter else 0 endif | i in index_set(batches), j in 1..2];
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
    %     [boxSize(i).width, boxSize(i).length]
    %   )
    % )
  else
    forall (i in domain) (
      bounded(positions[i], diameter(i), boxSize_(box_(i)))
      /\ forall (j in domain where i < j) (
        bottles[i].box = bottles[j].box -> no_overlap(positions[i], diameter(i), positions[j], diameter(j))
      )
    )
  endif;

predicate bounded(var Coord: pos, uint: diameter, var BoxSize: boxSize) =
  if circlePacking then
    pos.x+(diameter/2) <= boxSize.width /\ pos.y+(diameter/2) <= boxSize.length
  else
    pos.x+diameter <= boxSize.width /\ pos.y+diameter <= boxSize.length
  endif;

predicate no_overlap(var Coord: pos1, uint: diameter1, var Coord: pos2, uint: diameter2) =
  if circlePacking then
    distance(pos1, pos2) >= diameter1+diameter2
  else
    pos1.x+diameter1 <= pos2.x
    \/ pos1.y+diameter1 <= pos2.y
    \/ pos2.x+diameter2 <= pos1.x
    \/ pos2.y+diameter2 <= pos1.y
  endif;

% parameters
uint: demijohnsNumber;
uint: demijohnCapacity;
list of Batch: batches;
list of BoxSize: boxSizes;

% variables
var 0.0..demijohnsNumber*demijohnCapacity: bottledWine = sum (i in usedBottles) (capacity(i));
set of uint: index_bottles = 1..sum (batch in batches) (batch.quantity);
list of Bottle: bottles::add_to_output = let {
  array[index_bottles] of var opt index_bottles: boxes;
} in [(batch: i, box: boxes[i+j]) | i in index_set(batches), j in 0..batches[i].quantity-1];
array[index_bottles] of var opt index_set(boxSizes): boxes::add_to_output;

var set of index_bottles: usedBottles = {i | i in index_bottles where occurs(bottles[i].box)};
var set of index_bottles: usedBoxes = {i | i in index_bottles where occurs(boxes[i])};
% var set of index_bottles: usedBoxes = {deopt(bottles[i].box) | i in index_bottles where occurs(bottles[i].box)};
array[index_bottles] of var Coord: positions;

% symmetry breaking
constraint forall (i in index_bottles) (occurs(bottles[i].box) <-> occurs(boxes[bottles[i].box]));
constraint forall (i in index_bottles) (not occurs(bottles[i].box) -> positions[i] = (x: 0, y: 0));
constraint forall (i in index_bottles where i > 1) (occurs(boxes[i]) -> occurs(boxes[i-1]));

% constraints
constraint packed(positions, usedBottles);

constraint forall (i in usedBottles) (height(i) <= boxSize_(box_(i)).height);

constraint forall (i in usedBoxes) (
  let {
      var BoxSize: boxSize = boxSize_(i);
      var set of uint: boxedBottles = {j | j in usedBottles where bottles[j].box = i};
  } in coverage(boxSize, boxedBottles));

% objective
solve
  % ::relax_and_reconstruct(
  %   % [deopt(j) | j in [deopt(bottles[i].box) | i in usedBottles] ++
  %   [positions[i].x | i in index_bottles] ++
  %   [positions[i].y | i in index_bottles], 60)
  % ::seq_search([int_search([bottles[j].box | j in index_bottles], indomain_random, first_fail),
  %   int_search([positions[i].x | i in index_bottles], indomain_random, input_order),
  %   int_search([positions[i].y | i in index_bottles], indomain_random, input_order)])
  % ::restart_luby(2)
  maximize bottledWine;


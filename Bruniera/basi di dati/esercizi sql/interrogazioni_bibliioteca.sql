set search_path to biblioteca;

select distinct nome
from socio
	join ha_letto on socio.ci = ha_letto.ci
where sesso = 'F';

select libro.titolo
from libro
	join genere on libro.genere = genere.nome
where genere.sala = 'A'

select distinct libro.autore, libro.genere
from socio
	join ha_letto on socio.ci = ha_letto.ci
	join libro on ha_letto.isbn = libro.isbn
where sesso = 'M';

select libro.titolo
from socio
	join ha_letto on socio.ci = ha_letto.ci
	join libro on ha_letto.isbn = libro.isbn
where socio.nome = 'Ellade Pedone'
	and libro.genere = 'giallo';

select libro.titolo, genere.sala
from libro
	join genere on libro.genere = genere.nome;

select libro.titolo, genere.sala
from libro
	left outer join genere on libro.genere = genere.nome;

select ha_letto1.ci, ha_letto2.ci
from ha_letto as ha_letto1 
	join ha_letto as ha_letto2 on ha_letto1.isbn = ha_letto2.isbn
where ha_letto1.ci < ha_letto2.ci;

select distinct ha_letto.ci
from ha_letto 
	join libro on ha_letto.isbn = libro.isbn
where libro.genere is null;

select distinct ha_letto.ci
from ha_letto
	join libro on ha_letto.isbn = libro.isbn
	join genere on libro.genere = genere.nome
where genere.sala = 'A';
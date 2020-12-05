DROP SCHEMA IF EXISTS movies CASCADE;

CREATE SCHEMA movies;

SET search_path TO movies;


-------------------------- Definizione delle tabelle e dei vincoli intrarelazionali
CREATE TABLE movie(	  
	mid integer PRIMARY KEY -- diversa sintassi, imposto il vincolo sulla colonna
,	title varchar(50) NOT NULL
,	year integer 
,	director varchar(50)  
);

COMMENT ON COLUMN movie.year IS 'Talvolta l''anno non è noto';
COMMENT ON COLUMN movie.director IS 'Talvolta il regista non è noto';

CREATE TABLE reviewer(	  
	rid integer PRIMARY KEY
,	name varchar(50) NOT NULL 
);

CREATE TABLE rating(	  
	rid integer NOT NULL
,	mid integer NOT NULL
,	stars integer NOT NULL
,	ratingDate date NOT NULL
,	CONSTRAINT pk_rating PRIMARY KEY (rid, mid, ratingDate)
);


--------------------------  Aggiungo i vincoli interrelazionali
ALTER TABLE rating
ADD CONSTRAINT fk_rating_reviewer FOREIGN KEY (rid)
REFERENCES reviewer (rid) 
ON DELETE CASCADE ON UPDATE RESTRICT;

ALTER TABLE rating
ADD CONSTRAINT fk_rating_movie FOREIGN KEY (mid)
REFERENCES movie (mid) 
ON DELETE CASCADE ON UPDATE RESTRICT;


-------------------------- Importazione dei dati
insert into Movie values(101, 'Gone with the Wind', 1939, 'Victor Fleming');
insert into Movie values(102, 'Star Wars', 1977, 'George Lucas');
insert into Movie values(103, 'The Sound of Music', 1965, 'Robert Wise');
insert into Movie values(104, 'E.T.', 1982, 'Steven Spielberg');
insert into Movie values(105, 'Titanic', 1997, 'James Cameron');
insert into Movie values(106, 'Snow White', 1937, null);
insert into Movie values(107, 'Avatar', 2009, 'James Cameron');
insert into Movie values(108, 'Raiders of the Lost Ark', 1981, 'Steven Spielberg');

insert into Reviewer values(201, 'Sarah Martinez');
insert into Reviewer values(202, 'Daniel Lewis');
insert into Reviewer values(203, 'Brittany Harris');
insert into Reviewer values(204, 'Mike Anderson');
insert into Reviewer values(205, 'Chris Jackson');
insert into Reviewer values(206, 'Elizabeth Thomas');
insert into Reviewer values(207, 'James Cameron');
insert into Reviewer values(208, 'Ashley White');

insert into Rating values(201, 101, 2, '2011-01-22'); 
insert into Rating values(201, 101, 4, '2011-01-27'); 
insert into Rating values(202, 106, 4, '2011-01-29');
insert into Rating values(203, 103, 2, '2011-01-20');
insert into Rating values(203, 108, 4, '2011-01-12');
insert into Rating values(203, 108, 2, '2011-01-30');
insert into Rating values(204, 101, 3, '2011-01-09');
insert into Rating values(205, 103, 3, '2011-01-27');
insert into Rating values(205, 104, 2, '2011-01-22');
insert into Rating values(205, 108, 4, '2011-01-27');
insert into Rating values(206, 107, 3, '2011-01-15');
insert into Rating values(206, 106, 5, '2011-01-19');
insert into Rating values(207, 107, 5, '2011-01-20');
insert into Rating values(208, 104, 3, '2011-01-02');

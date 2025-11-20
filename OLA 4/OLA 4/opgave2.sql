Create database `opg3`;

use `opg3`;
create table `biler` (
`car_id` int not null,
primary key (`car_id`),
`makemodel` varchar(99),
`prices` int default null,
`forhandler` varchar(50),
`adresse` varchar(99),
`cvr`varchar(20),
`links`varchar(500),
`beskrivelse` varchar(5000)
);

create table `forhandler` (
`forhandler_id` int not null,
primary key (`forhandler_id`),
`forhandler` 	varchar(99),
`adresse` 		varchar(99),
`cvr` 			varchar(99)
);

drop table `autocamper`;
create table `autocamper` (
`makemodel`		varchar(200),
`prices`		varchar(200),
`forhandler`	varchar(200),
`modelår` 		varchar(10),
`f_reg` 		varchar(10),
`km_tal` 		varchar(20),
`drivmiddel` 	varchar(20),
`bs_forbrug` 	varchar(20),
`CO2` 			varchar(20),
`euronorm` 		int not null,
`periodisk_afg` varchar(50),
`ydelse` 		varchar(50),
`acceleration` 	varchar(50),
`tophastighed` 	varchar(50),
`geartype` 		varchar(50),
`antal_gear` 	int not null,
`trækvægt` 		varchar(50),
`farve` 		varchar(20),
`nypris`		varchar(50),
`kategori`		varchar(50),
`type`			varchar(20),
`bag_str`		varchar(50),
`vægt`			varchar(50),
`bredde`		varchar(50),
`længde`		varchar(50),
`højde`			varchar(50),
`lastevne`		varchar(50),
`max_træk_m_b`	varchar(50),
`trækhjul`		varchar(20),
`cylindre`		int not null,
`abs_brems`		varchar(50),
`esp`			varchar(5),
`airbags`		int not null,
`tank_kap`		varchar(20),
`døre`			varchar(10),
`beskrivelse`	varchar(5000),
`cvr`			int not null,
`links` 		varchar(300)
);

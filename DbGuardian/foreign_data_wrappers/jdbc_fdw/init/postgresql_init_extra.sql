-- SET consistant time zones;
SET timezone = 'PST8PDT';
DROP TABLE IF EXISTS tbl01;
DROP TABLE IF EXISTS tbl02;
DROP TABLE IF EXISTS tbl03;
DROP TABLE IF EXISTS tbl04;
DROP TABLE IF EXISTS tbl05;
DROP TABLE IF EXISTS test_explicit_cast;
DROP TABLE IF EXISTS tbl06;

CREATE TABLE tbl01 (id bigint primary key, c1 INT);
CREATE TABLE tbl02 (id char(255) primary key, c1 INT, c2 float8, c3 boolean);
CREATE TABLE tbl03 (id timestamp primary key, c1 INT);
CREATE TABLE tbl04 (id INT primary key, c1 float8, c2 bigint, c3 text, c4 boolean, c5 timestamp);
create table tbl05(id int, v bytea);
INSERT INTO tbl04 VALUES (1, 31.12, 128912, 'anystring', true, '2000-01-01 00:00:00');
INSERT INTO tbl04 VALUES (2, 2565.56, 6565, 'example', false, '2000-01-01 00:00:00');
INSERT INTO tbl04 VALUES (3, -121.122, 1829812, 'thing', true, '2000-01-01 00:00:00');
INSERT INTO tbl04 VALUES (4, 55.23, 523, '!)@(#)!_#!', false, '1990-11-01 00:00:00');
INSERT INTO tbl04 VALUES (5, -1.12, 22342, '(!)JAWLFJ', false, '2010-11-01 00:00:00');
INSERT INTO tbl04 VALUES (6, 45021.21, 2121, 'example', false, '1999-10-01 00:00:00');
INSERT INTO tbl04 VALUES (7, 121.9741, 23241, 'thing', false, '2010-10-01 00:00:00');
INSERT INTO tbl04 VALUES (8, 75, 316, 'example', false, '1999-10-01 10:10:00');
INSERT INTO tbl04 VALUES (9, 6867.34, 8916, 'thing', false, '2010-10-01 10:10:00');
CREATE TABLE test_explicit_cast(id int, c1 text);
CREATE TABLE tbl06 (id INT primary key, c1 float8, c2 bigint, c3 text, c4 boolean, c5 timestamp);
INSERT INTO tbl06 VALUES (1, 31.12, 128912, 'anystring', true, '2000-01-01 00:00:00');
INSERT INTO tbl06 VALUES (2, 2565.56, 6565, 'example', false, '2000-01-01 00:00:00');
INSERT INTO tbl06 VALUES (3, -121.122, 1829812, 'thing', true, '2000-01-01 00:00:00');
INSERT INTO tbl06 VALUES (4, 55.23, 523, '!)@(#)!_#!', false, '1990-11-01 00:00:00');
INSERT INTO tbl06 VALUES (5, -1.12, 22342, '(!)JAWLFJ', false, '2010-11-01 00:00:00');
INSERT INTO tbl06 VALUES (6, 45021.21, 2121, 'example', false, '1999-10-01 00:00:00');
INSERT INTO tbl06 VALUES (7, 121.9741, 23241, 'thing', false, '2010-10-01 00:00:00');
INSERT INTO tbl06 VALUES (8, 75, 316, 'example', false, '1999-10-01 10:10:00');
INSERT INTO tbl06 VALUES (9, 6867.34, 8916, 'thing', false, '2010-10-01 10:10:00');
INSERT INTO tbl06 VALUES (10, 31.12, 128912, 'group_1', true, '2000-01-01 00:00:00');
INSERT INTO tbl06 VALUES (11, 2565.56, 6565, 'group_1', false, '2000-01-01 00:00:00');
INSERT INTO tbl06 VALUES (12, -121.122, 1829812, 'group_1', true, '2000-01-01 00:00:00');
INSERT INTO tbl06 VALUES (13, 55.23, 523, 'group_2', false, '1990-11-01 00:00:00');
INSERT INTO tbl06 VALUES (14, -1.12, 22342, 'group_2', false, '2010-11-01 00:00:00');
INSERT INTO tbl06 VALUES (15, 45021.21, 2121, 'group_3', false, '1999-10-01 00:00:00');
INSERT INTO tbl06 VALUES (16, 121.9741, 23241, 'group_3', false, '2010-10-01 00:00:00');
INSERT INTO tbl06 VALUES (17, 75, 316, 'group_3', false, '1999-10-01 10:10:00');
INSERT INTO tbl06 VALUES (18, 6867.34, 8916, 'group_4', false, '2010-10-01 10:10:00');
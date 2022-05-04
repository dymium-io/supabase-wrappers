--
-- PostgreSQL database dump
--

-- Dumped from database version 11.6
-- Dumped by pg_dump version 12.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', true);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: dymium; Type: DATABASE; Schema: -; Owner: admin
--

CREATE DATABASE dymium WITH TEMPLATE = template0 ENCODING = 'UTF8' ;


ALTER DATABASE dymium OWNER TO admin;

\connect dymium

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: attom; Type: SCHEMA; Schema: -; Owner: dymium
--

CREATE SCHEMA dymium;


GRANT CONNECT ON DATABASE dymium TO postgres;
GRANT ALL ON DATABASE dymium TO dymium;


--
-- PostgreSQL database dump complete
--

